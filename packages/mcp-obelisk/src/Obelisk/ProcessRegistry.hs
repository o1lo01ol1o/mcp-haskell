{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Obelisk.ProcessRegistry
  ( ProcessRegistry
  , createRegistry
  , startObeliskWatch
  , stopObeliskWatch
  , getObeliskStatus
  , getObeliskMessages
  , getLastLogLine
  , listProjects
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import Obelisk.Types
import System.Directory (doesDirectoryExist)
import System.IO (Handle, hClose, hIsEOF)
import System.Process.Typed

-- | Internal representation of a running ob watch process.
data ObeliskHandle = ObeliskHandle
  { ohProcess :: Process () Handle Handle
  , ohStatus :: TVar ObeliskStatus
  , ohBuffer :: TVar (Seq Text)
  , ohLastMessage :: TVar (Maybe Text)
  , ohStdoutReader :: Async ()
  , ohStderrReader :: Async ()
  }

newtype ProcessRegistry = ProcessRegistry (TVar (Map ProjectId ObeliskHandle))

bufferLimit :: Int
bufferLimit = 4000

createRegistry :: IO ProcessRegistry
createRegistry = ProcessRegistry <$> newTVarIO Map.empty

startObeliskWatch :: ProcessRegistry -> ProjectId -> IO (Either Text StartResult)
startObeliskWatch (ProcessRegistry registryVar) projectId@(ProjectId projectPath) = do
  exists <- doesDirectoryExist projectPath
  if not exists
    then pure $ Left $ "Project path not found: " <> T.pack projectPath
    else do
      wasRunning <- atomically $ Map.member projectId <$> readTVar registryVar
      when wasRunning $ void $ stopObeliskWatch (ProcessRegistry registryVar) projectId

      statusVar <- newTVarIO ObStarting
      bufferVar <- newTVarIO Seq.empty
      lastMessageVar <- newTVarIO Nothing

      let processConfig =
            setWorkingDir projectPath
              $ setStdout createPipe
              $ setStderr createPipe
              $ setStdin inherit
              $ setCreateGroup True
              $ proc "ob" ["watch"]

      startResult <- try @SomeException $ startProcess processConfig
      case startResult of
        Left ex -> pure $ Left $ "Failed to start ob watch: " <> T.pack (show ex)
        Right process -> do
          stdoutReader <- async $ pumpOutput statusVar bufferVar lastMessageVar (getStdout process)
          stderrReader <- async $ pumpError lastMessageVar (getStderr process)

          let handle = ObeliskHandle
                { ohProcess = process
                  , ohStatus = statusVar
                  , ohBuffer = bufferVar
                  , ohLastMessage = lastMessageVar
                  , ohStdoutReader = stdoutReader
                  , ohStderrReader = stderrReader
                }

          atomically $ modifyTVar' registryVar (Map.insert projectId handle)
          let startMsg = if wasRunning then "ob watch restarted" else "ob watch started"
          pure $ Right $ StartResult True startMsg projectPath

stopObeliskWatch :: ProcessRegistry -> ProjectId -> IO (Either Text StopResult)
stopObeliskWatch (ProcessRegistry registryVar) projectId@(ProjectId projectPath) = do
  maybeHandle <- atomically $ do
    registry <- readTVar registryVar
    case Map.lookup projectId registry of
      Nothing -> pure Nothing
      Just handle -> do
        writeTVar registryVar (Map.delete projectId registry)
        pure $ Just handle
  case maybeHandle of
    Nothing -> pure $ Right $ StopResult True "ob watch not running" projectPath
    Just ObeliskHandle {..} -> do
      cancel ohStdoutReader
      cancel ohStderrReader
      void $ try @SomeException $ stopProcess ohProcess
      void $ try @SomeException $ do
        hClose (getStdout ohProcess)
        hClose (getStderr ohProcess)
      atomically $ writeTVar ohStatus ObStopped
      pure $ Right $ StopResult True "ob watch stopped" projectPath

getObeliskStatus :: ProcessRegistry -> ProjectId -> IO ObeliskStatus
getObeliskStatus (ProcessRegistry registryVar) projectId = do
  registry <- readTVarIO registryVar
  case Map.lookup projectId registry of
    Nothing -> pure ObStopped
    Just ObeliskHandle {..} -> readTVarIO ohStatus

getObeliskMessages :: ProcessRegistry -> ProjectId -> Maybe Int -> IO (Either Text MessagesResult)
getObeliskMessages (ProcessRegistry registryVar) projectId mCount = do
  registry <- readTVarIO registryVar
  case Map.lookup projectId registry of
    Nothing -> pure $ Left "ob watch not running for project"
    Just ObeliskHandle {..} -> do
      linesSeq <- readTVarIO ohBuffer
      now <- getCurrentTime
      let totalLines = toList linesSeq
          selected = maybe totalLines (\n -> reverse (take n (reverse totalLines))) mCount
      pure $ Right $ MessagesResult selected now

getLastLogLine :: ProcessRegistry -> ProjectId -> IO (Maybe Text)
getLastLogLine (ProcessRegistry registryVar) projectId = do
  registry <- readTVarIO registryVar
  case Map.lookup projectId registry of
    Nothing -> pure Nothing
    Just ObeliskHandle {..} -> readTVarIO ohLastMessage

listProjects :: ProcessRegistry -> IO [ProjectId]
listProjects (ProcessRegistry registryVar) = Map.keys <$> readTVarIO registryVar

pumpOutput :: TVar ObeliskStatus -> TVar (Seq Text) -> TVar (Maybe Text) -> Handle -> IO ()
pumpOutput statusVar bufferVar lastMessageVar handle = do
  result <- try @SomeException $ loop
  case result of
    Left ex -> atomically $ writeTVar statusVar (ObErrored $ T.pack (show ex))
    Right () -> pure ()
  where
    loop = do
      eof <- hIsEOF handle
      if eof
        then atomically $ writeTVar statusVar ObStopped
        else do
          line <- T.hGetLine handle
          let lowerLine = T.toLower line
          atomically $ do
            modifyTVar' bufferVar (appendLine line)
            writeTVar lastMessageVar (Just line)
            when ("all good" `T.isInfixOf` lowerLine) $ writeTVar statusVar ObRunning
          loop

pumpError :: TVar (Maybe Text) -> Handle -> IO ()
pumpError lastMessageVar handle = do
  result <- try @SomeException $ T.hGetContents handle
  case result of
    Left _ -> pure ()
    Right contents ->
      let linesText = T.lines contents
      in atomically $ writeTVar lastMessageVar (if null linesText then Nothing else Just (last linesText))

appendLine :: Text -> Seq Text -> Seq Text
appendLine line seqLines =
  let trimmed = if Seq.length seqLines >= bufferLimit
                then Seq.drop 1 seqLines
                else seqLines
  in trimmed Seq.|> line
