{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module GHCID.Metrics
  ( -- * Metrics Types
    GHCIDMetrics(..)
  , ProcessMetrics(..)
  , RequestMetrics(..)
  , HealthMetrics(..)
  
    -- * Metrics Collection
  , initializeMetrics
  , collectProcessMetrics
  , collectRequestMetrics
  , collectHealthMetrics
  
    -- * Metrics Reporting
  , reportMetrics
  , exportPrometheusMetrics
  , logMetricsSummary
  
    -- * Metrics Configuration
  , MetricsConfig(..)
  , defaultMetricsConfig
  
    -- * Metric Updates
  , recordProcessStart
  , recordProcessStop
  , recordProcessError
  , recordRequestProcessed
  , recordRequestError
  , recordHealthCheck
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, formatTime, defaultTimeLocale)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (Handle, hFlush)

-- Internal imports
import GHCID.ProcessRegistry (CabalURI(..), GHCIDStatus(..))
import Utils.Logging

-- | Process-level metrics
data ProcessMetrics = ProcessMetrics
  { pmProcessesStarted :: Int
  , pmProcessesStopped :: Int
  , pmProcessesRunning :: Int
  , pmProcessesFailed :: Int
  , pmProcessesRestarted :: Int
  , pmAverageUptime :: Double  -- seconds
  , pmTotalUptime :: Double    -- seconds
  , pmProcessStatusCounts :: Map GHCIDStatus Int
  } deriving (Show, Eq)

-- | Request-level metrics
data RequestMetrics = RequestMetrics
  { rmTotalRequests :: Int
  , rmSuccessfulRequests :: Int
  , rmFailedRequests :: Int
  , rmAverageResponseTime :: Double  -- milliseconds
  , rmRequestsByType :: Map Text Int
  , rmErrorsByType :: Map Text Int
  , rmRequestsPerSecond :: Double
  } deriving (Show, Eq)

-- | Health check metrics
data HealthMetrics = HealthMetrics
  { hmHealthChecksPerformed :: Int
  , hmHealthyProcesses :: Int
  , hmUnhealthyProcesses :: Int
  , hmFailedHealthChecks :: Int
  , hmAverageHealthCheckTime :: Double  -- milliseconds
  , hmLastHealthCheckTime :: Maybe UTCTime
  } deriving (Show, Eq)

-- | Overall GHCID metrics
data GHCIDMetrics = GHCIDMetrics
  { metricsStartTime :: UTCTime
  , metricsLastUpdate :: UTCTime
  , processMetrics :: ProcessMetrics
  , requestMetrics :: RequestMetrics
  , healthMetrics :: HealthMetrics
  , customMetrics :: Map Text Double  -- Extensible custom metrics
  } deriving (Show, Eq)

-- | Metrics configuration
data MetricsConfig = MetricsConfig
  { metricsEnabled :: Bool
  , metricsLogInterval :: Int        -- seconds
  , metricsExportPrometheus :: Bool
  , metricsPrometheusPort :: Int
  , metricsRetentionPeriod :: Int    -- seconds
  , metricsVerboseLogging :: Bool
  } deriving (Show, Eq)

-- | Default metrics configuration
defaultMetricsConfig :: MetricsConfig
defaultMetricsConfig = MetricsConfig
  { metricsEnabled = True
  , metricsLogInterval = 60  -- 1 minute
  , metricsExportPrometheus = False
  , metricsPrometheusPort = 9090
  , metricsRetentionPeriod = 3600  -- 1 hour
  , metricsVerboseLogging = False
  }

-- | Initialize metrics collection
initializeMetrics :: MetricsConfig -> IO (TVar GHCIDMetrics)
initializeMetrics config = do
  startTime <- getCurrentTime
  let initialMetrics = GHCIDMetrics
        { metricsStartTime = startTime
        , metricsLastUpdate = startTime
        , processMetrics = emptyProcessMetrics
        , requestMetrics = emptyRequestMetrics
        , healthMetrics = emptyHealthMetrics
        , customMetrics = Map.empty
        }
  
  metricsVar <- newTVarIO initialMetrics
  
  when (metricsEnabled config) $ do
    logInfo "GHCID metrics collection initialized"
    when (metricsVerboseLogging config) $
      logInfo $ "Metrics config: " <> T.pack (show config)
  
  return metricsVar

-- | Empty process metrics
emptyProcessMetrics :: ProcessMetrics
emptyProcessMetrics = ProcessMetrics 0 0 0 0 0 0.0 0.0 Map.empty

-- | Empty request metrics
emptyRequestMetrics :: RequestMetrics
emptyRequestMetrics = RequestMetrics 0 0 0 0.0 Map.empty Map.empty 0.0

-- | Empty health metrics
emptyHealthMetrics :: HealthMetrics
emptyHealthMetrics = HealthMetrics 0 0 0 0 0.0 Nothing

-- | Record a process start event
recordProcessStart :: TVar GHCIDMetrics -> CabalURI -> IO ()
recordProcessStart metricsVar cabalURI = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let pm = processMetrics metrics
        pm' = pm 
          { pmProcessesStarted = pmProcessesStarted pm + 1
          , pmProcessesRunning = pmProcessesRunning pm + 1
          }
    in metrics 
         { processMetrics = pm'
         , metricsLastUpdate = now
         }
  
  logInfo $ "Metrics: Process started " <> getCabalURI cabalURI

-- | Record a process stop event
recordProcessStop :: TVar GHCIDMetrics -> CabalURI -> Double -> IO ()
recordProcessStop metricsVar cabalURI uptime = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let pm = processMetrics metrics
        newTotalUptime = pmTotalUptime pm + uptime
        newCount = pmProcessesStopped pm + 1
        newAverage = if newCount > 0 then newTotalUptime / fromIntegral newCount else 0.0
        pm' = pm 
          { pmProcessesStopped = newCount
          , pmProcessesRunning = max 0 (pmProcessesRunning pm - 1)
          , pmAverageUptime = newAverage
          , pmTotalUptime = newTotalUptime
          }
    in metrics 
         { processMetrics = pm'
         , metricsLastUpdate = now
         }
  
  logInfo $ "Metrics: Process stopped " <> getCabalURI cabalURI <> " (uptime: " <> T.pack (show uptime) <> "s)"

-- | Record a process error event
recordProcessError :: TVar GHCIDMetrics -> CabalURI -> Text -> IO ()
recordProcessError metricsVar cabalURI errorMsg = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let pm = processMetrics metrics
        pm' = pm 
          { pmProcessesFailed = pmProcessesFailed pm + 1
          , pmProcessesRunning = max 0 (pmProcessesRunning pm - 1)
          }
    in metrics 
         { processMetrics = pm'
         , metricsLastUpdate = now
         }
  
  logWarn $ "Metrics: Process error " <> getCabalURI cabalURI <> ": " <> errorMsg

-- | Record a processed request
recordRequestProcessed :: TVar GHCIDMetrics -> Text -> Double -> Bool -> IO ()
recordRequestProcessed metricsVar requestType responseTime success = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let rm = requestMetrics metrics
        newTotal = rmTotalRequests rm + 1
        newSuccessful = if success then rmSuccessfulRequests rm + 1 else rmSuccessfulRequests rm
        newFailed = if success then rmFailedRequests rm else rmFailedRequests rm + 1
        
        -- Update average response time
        oldAverage = rmAverageResponseTime rm
        oldCount = rmTotalRequests rm
        newAverage = if oldCount > 0
          then (oldAverage * fromIntegral oldCount + responseTime) / fromIntegral newTotal
          else responseTime
        
        -- Update request type counts
        newByType = Map.insertWith (+) requestType 1 (rmRequestsByType rm)
        
        rm' = rm
          { rmTotalRequests = newTotal
          , rmSuccessfulRequests = newSuccessful
          , rmFailedRequests = newFailed
          , rmAverageResponseTime = newAverage
          , rmRequestsByType = newByType
          }
    in metrics
         { requestMetrics = rm'
         , metricsLastUpdate = now
         }
  
  let status = if success then "success" else "failure"
  logInfo $ "Metrics: Request " <> requestType <> " " <> status <> " (" <> T.pack (show responseTime) <> "ms)"

-- | Record a request error
recordRequestError :: TVar GHCIDMetrics -> Text -> Text -> IO ()
recordRequestError metricsVar requestType errorType = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let rm = requestMetrics metrics
        newErrors = Map.insertWith (+) errorType 1 (rmErrorsByType rm)
        rm' = rm { rmErrorsByType = newErrors }
    in metrics
         { requestMetrics = rm'
         , metricsLastUpdate = now
         }
  
  logWarn $ "Metrics: Request error " <> requestType <> " - " <> errorType

-- | Record a health check
recordHealthCheck :: TVar GHCIDMetrics -> CabalURI -> Bool -> Double -> IO ()
recordHealthCheck metricsVar cabalURI healthy checkTime = do
  now <- getCurrentTime
  atomically $ modifyTVar metricsVar $ \metrics ->
    let hm = healthMetrics metrics
        newTotal = hmHealthChecksPerformed hm + 1
        newHealthy = if healthy then hmHealthyProcesses hm + 1 else hmHealthyProcesses hm
        newUnhealthy = if healthy then hmUnhealthyProcesses hm else hmUnhealthyProcesses hm + 1
        newFailed = if healthy then hmFailedHealthChecks hm else hmFailedHealthChecks hm + 1
        
        -- Update average check time
        oldAverage = hmAverageHealthCheckTime hm
        oldCount = hmHealthChecksPerformed hm
        newAverage = if oldCount > 0
          then (oldAverage * fromIntegral oldCount + checkTime) / fromIntegral newTotal
          else checkTime
        
        hm' = hm
          { hmHealthChecksPerformed = newTotal
          , hmHealthyProcesses = newHealthy
          , hmUnhealthyProcesses = newUnhealthy
          , hmFailedHealthChecks = newFailed
          , hmAverageHealthCheckTime = newAverage
          , hmLastHealthCheckTime = Just now
          }
    in metrics
         { healthMetrics = hm'
         , metricsLastUpdate = now
         }
  
  let status = if healthy then "healthy" else "unhealthy"
  logInfo $ "Metrics: Health check " <> getCabalURI cabalURI <> " " <> status <> " (" <> T.pack (show checkTime) <> "ms)"

-- | Collect current process metrics
collectProcessMetrics :: [CabalURI] -> Map CabalURI GHCIDStatus -> IO ProcessMetrics
collectProcessMetrics activeURIs statusMap = do
  let runningCount = length activeURIs
  let statusCounts = Map.fromListWith (+) [(status, 1) | status <- Map.elems statusMap]
  
  return ProcessMetrics
    { pmProcessesStarted = 0  -- This would be maintained by the metrics system
    , pmProcessesStopped = 0
    , pmProcessesRunning = runningCount
    , pmProcessesFailed = 0
    , pmProcessesRestarted = 0
    , pmAverageUptime = 0.0
    , pmTotalUptime = 0.0
    , pmProcessStatusCounts = statusCounts
    }

-- | Collect current request metrics
collectRequestMetrics :: TVar GHCIDMetrics -> IO RequestMetrics
collectRequestMetrics metricsVar = do
  metrics <- readTVarIO metricsVar
  let rm = requestMetrics metrics
  
  -- Calculate requests per second
  now <- getCurrentTime
  let elapsedSeconds = realToFrac $ diffUTCTime now (metricsStartTime metrics)
  let rps = if elapsedSeconds > 0 then fromIntegral (rmTotalRequests rm) / elapsedSeconds else 0.0
  
  return rm { rmRequestsPerSecond = rps }

-- | Collect current health metrics  
collectHealthMetrics :: TVar GHCIDMetrics -> IO HealthMetrics
collectHealthMetrics metricsVar = do
  metrics <- readTVarIO metricsVar
  return $ healthMetrics metrics

-- | Report all metrics to logs
reportMetrics :: TVar GHCIDMetrics -> IO ()
reportMetrics metricsVar = do
  metrics <- readTVarIO metricsVar
  now <- getCurrentTime
  
  let uptime = diffUTCTime now (metricsStartTime metrics)
  let uptimeStr = formatTime defaultTimeLocale "%H:%M:%S" uptime
  
  logInfo "=== GHCID Metrics Report ==="
  logInfo $ "Uptime: " <> T.pack uptimeStr
  logInfo $ "Last Update: " <> T.pack (show $ metricsLastUpdate metrics)
  
  -- Process metrics
  let pm = processMetrics metrics
  logInfo "Process Metrics:"
  logInfo $ "  Started: " <> T.pack (show $ pmProcessesStarted pm)
  logInfo $ "  Stopped: " <> T.pack (show $ pmProcessesStopped pm) 
  logInfo $ "  Running: " <> T.pack (show $ pmProcessesRunning pm)
  logInfo $ "  Failed: " <> T.pack (show $ pmProcessesFailed pm)
  logInfo $ "  Average Uptime: " <> T.pack (show $ pmAverageUptime pm) <> "s"
  
  -- Request metrics
  let rm = requestMetrics metrics
  logInfo "Request Metrics:"
  logInfo $ "  Total: " <> T.pack (show $ rmTotalRequests rm)
  logInfo $ "  Successful: " <> T.pack (show $ rmSuccessfulRequests rm)
  logInfo $ "  Failed: " <> T.pack (show $ rmFailedRequests rm)
  logInfo $ "  Average Response Time: " <> T.pack (show $ rmAverageResponseTime rm) <> "ms"
  logInfo $ "  Requests/Second: " <> T.pack (show $ rmRequestsPerSecond rm)
  
  -- Health metrics
  let hm = healthMetrics metrics
  logInfo "Health Metrics:"
  logInfo $ "  Health Checks: " <> T.pack (show $ hmHealthChecksPerformed hm)
  logInfo $ "  Healthy: " <> T.pack (show $ hmHealthyProcesses hm)
  logInfo $ "  Unhealthy: " <> T.pack (show $ hmUnhealthyProcesses hm)
  logInfo $ "  Average Check Time: " <> T.pack (show $ hmAverageHealthCheckTime hm) <> "ms"
  
  logInfo "=== End Metrics Report ==="

-- | Export metrics in Prometheus format
exportPrometheusMetrics :: TVar GHCIDMetrics -> Handle -> IO ()
exportPrometheusMetrics metricsVar handle = do
  metrics <- readTVarIO metricsVar
  let pm = processMetrics metrics
  let rm = requestMetrics metrics
  let hm = healthMetrics metrics
  
  -- Process metrics
  T.hPutStrLn handle "# HELP ghcid_processes_started_total Total number of GHCID processes started"
  T.hPutStrLn handle "# TYPE ghcid_processes_started_total counter"
  T.hPutStrLn handle $ "ghcid_processes_started_total " <> T.pack (show $ pmProcessesStarted pm)
  
  T.hPutStrLn handle "# HELP ghcid_processes_running Current number of running GHCID processes"
  T.hPutStrLn handle "# TYPE ghcid_processes_running gauge"
  T.hPutStrLn handle $ "ghcid_processes_running " <> T.pack (show $ pmProcessesRunning pm)
  
  T.hPutStrLn handle "# HELP ghcid_processes_failed_total Total number of failed GHCID processes"
  T.hPutStrLn handle "# TYPE ghcid_processes_failed_total counter"
  T.hPutStrLn handle $ "ghcid_processes_failed_total " <> T.pack (show $ pmProcessesFailed pm)
  
  -- Request metrics
  T.hPutStrLn handle "# HELP ghcid_requests_total Total number of MCP requests processed"
  T.hPutStrLn handle "# TYPE ghcid_requests_total counter"
  T.hPutStrLn handle $ "ghcid_requests_total " <> T.pack (show $ rmTotalRequests rm)
  
  T.hPutStrLn handle "# HELP ghcid_request_duration_ms Average request duration in milliseconds"
  T.hPutStrLn handle "# TYPE ghcid_request_duration_ms gauge"
  T.hPutStrLn handle $ "ghcid_request_duration_ms " <> T.pack (show $ rmAverageResponseTime rm)
  
  -- Health metrics
  T.hPutStrLn handle "# HELP ghcid_health_checks_total Total number of health checks performed"
  T.hPutStrLn handle "# TYPE ghcid_health_checks_total counter"
  T.hPutStrLn handle $ "ghcid_health_checks_total " <> T.pack (show $ hmHealthChecksPerformed hm)
  
  hFlush handle

-- | Log a concise metrics summary
logMetricsSummary :: TVar GHCIDMetrics -> IO ()
logMetricsSummary metricsVar = do
  metrics <- readTVarIO metricsVar
  let pm = processMetrics metrics
  let rm = requestMetrics metrics
  
  logInfo $ "GHCID Metrics - Processes: " 
    <> T.pack (show $ pmProcessesRunning pm) <> " running, "
    <> T.pack (show $ pmProcessesStarted pm) <> " started, "
    <> T.pack (show $ pmProcessesFailed pm) <> " failed | Requests: "
    <> T.pack (show $ rmTotalRequests rm) <> " total, "
    <> T.pack (show $ rmSuccessfulRequests rm) <> " success, "
    <> T.pack (show $ rmFailedRequests rm) <> " failed"

-- JSON instances for metrics (for API export)
instance ToJSON ProcessMetrics where
  toJSON ProcessMetrics{..} = object
    [ "processes_started" .= pmProcessesStarted
    , "processes_stopped" .= pmProcessesStopped
    , "processes_running" .= pmProcessesRunning
    , "processes_failed" .= pmProcessesFailed
    , "processes_restarted" .= pmProcessesRestarted
    , "average_uptime_seconds" .= pmAverageUptime
    , "total_uptime_seconds" .= pmTotalUptime
    , "process_status_counts" .= Map.mapKeys (T.pack . show) pmProcessStatusCounts
    ]

instance ToJSON RequestMetrics where
  toJSON RequestMetrics{..} = object
    [ "total_requests" .= rmTotalRequests
    , "successful_requests" .= rmSuccessfulRequests
    , "failed_requests" .= rmFailedRequests
    , "average_response_time_ms" .= rmAverageResponseTime
    , "requests_by_type" .= rmRequestsByType
    , "errors_by_type" .= rmErrorsByType
    , "requests_per_second" .= rmRequestsPerSecond
    ]

instance ToJSON HealthMetrics where
  toJSON HealthMetrics{..} = object
    [ "health_checks_performed" .= hmHealthChecksPerformed
    , "healthy_processes" .= hmHealthyProcesses
    , "unhealthy_processes" .= hmUnhealthyProcesses
    , "failed_health_checks" .= hmFailedHealthChecks
    , "average_health_check_time_ms" .= hmAverageHealthCheckTime
    , "last_health_check_time" .= hmLastHealthCheckTime
    ]

instance ToJSON GHCIDMetrics where
  toJSON GHCIDMetrics{..} = object
    [ "start_time" .= metricsStartTime
    , "last_update" .= metricsLastUpdate
    , "process_metrics" .= processMetrics
    , "request_metrics" .= requestMetrics
    , "health_metrics" .= healthMetrics
    , "custom_metrics" .= customMetrics
    ]
