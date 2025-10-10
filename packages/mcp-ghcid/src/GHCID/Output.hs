{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHCID.Output
  ( -- * Output parsing
    parseGHCIDOutput
  , parseCompilerMessage
  , parseStatusUpdate
  
    -- * Message types
  , GHCIDMessage(..)
  , CompilerMessage(..)
  , StatusUpdate(..)
  , MessageSeverity(..)
  
    -- * Utilities  
  , extractSourceLocation
  , formatCompilerMessage
  , isErrorMessage
  , isWarningMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Text.Regex.TDFA ((=~))

-- Re-export from Client module
import GHCID.Client (CompilerMessage(..), MessageSeverity(..))

-- | GHCID output message types
data GHCIDMessage
  = StatusMsg StatusUpdate
  | CompilerMsg CompilerMessage  
  | UnknownMsg Text
  deriving (Show, Eq)

-- | GHCID status updates
data StatusUpdate
  = LoadingModules [FilePath]
  | CompilationComplete Int Int  -- (modules loaded, errors/warnings)
  | ReloadingFiles [FilePath]
  | TestsRunning
  | TestsComplete Bool           -- Success/failure
  deriving (Show, Eq)

-- | Parse GHCID output text into messages
parseGHCIDOutput :: Text -> IO [GHCIDMessage]
parseGHCIDOutput output = do
  let outputLines = T.lines output
  messages <- mapM parseGHCIDLine (filter (not . T.null) outputLines)
  return $ concat messages

-- | Parse a single line of GHCID output
parseGHCIDLine :: Text -> IO [GHCIDMessage]
parseGHCIDLine line
  | isStatusLine line = do
      case parseStatusUpdate line of
        Just status -> return [StatusMsg status]
        Nothing -> return [UnknownMsg line]
  | isCompilerMessageLine line = do
      maybeMsg <- parseCompilerMessage line
      case maybeMsg of
        Just msg -> return [CompilerMsg msg]
        Nothing -> return [UnknownMsg line]
  | otherwise = return [UnknownMsg line]

-- | Check if line contains status information  
isStatusLine :: Text -> Bool
isStatusLine line = 
  any (`T.isInfixOf` line) 
    [ "All good"
    , "modules loaded"
    , "Reloading"
    , "Test passed"
    , "Test failed"
    , "Loading"
    ]

-- | Check if line contains compiler message
isCompilerMessageLine :: Text -> Bool
isCompilerMessageLine line =
  any (`T.isInfixOf` line)
    [ ": error:"
    , ": warning:"
    , ": note:"
    , "Error:"
    , "Warning:"
    ]

-- | Parse status update from GHCID output
parseStatusUpdate :: Text -> Maybe StatusUpdate
parseStatusUpdate line
  | "All good" `T.isInfixOf` line = 
      case extractModuleCount line of
        Just count -> Just $ CompilationComplete count 0
        Nothing -> Just $ CompilationComplete 0 0
  | "modules loaded" `T.isInfixOf` line =
      case extractModuleCount line of  
        Just count -> Just $ CompilationComplete count 0
        Nothing -> Nothing
  | "Reloading" `T.isInfixOf` line =
      let files = extractReloadFiles line
      in Just $ ReloadingFiles files
  | "Loading" `T.isInfixOf` line =
      let files = extractLoadFiles line
      in Just $ LoadingModules files
  | "Test passed" `T.isInfixOf` line = Just $ TestsComplete True
  | "Test failed" `T.isInfixOf` line = Just $ TestsComplete False
  | otherwise = Nothing

-- | Extract module count from status line
extractModuleCount :: Text -> Maybe Int
extractModuleCount line = 
  case line =~ ("([0-9]+) modules?" :: Text) :: (Text, Text, Text, [Text]) of
    (_,_,_, [countStr]) -> case reads (T.unpack countStr) of
      [(count, "")] -> Just count
      _ -> Nothing
    _ -> Nothing

-- | Extract files being reloaded
extractReloadFiles :: Text -> [FilePath]
extractReloadFiles line =
  -- Simple extraction - in practice would need more sophisticated parsing
  let parts = T.words line
      files = filter (T.any (== '.')) parts  -- Files likely contain dots
  in map T.unpack files

-- | Extract files being loaded  
extractLoadFiles :: Text -> [FilePath]
extractLoadFiles = extractReloadFiles  -- Same logic for now

-- | Parse compiler message from GHC output
parseCompilerMessage :: Text -> IO (Maybe CompilerMessage)
parseCompilerMessage line = do
  timestamp <- getCurrentTime
  return $ case parseGHCMessage line of
    Just (severity, file, lineNum, col, msg) ->
      Just CompilerMessage
        { msgSeverity = severity
        , msgFile = file  
        , msgLine = lineNum
        , msgColumn = col
        , msgText = msg
        , msgTimestamp = timestamp
        }
    Nothing -> Nothing

-- | Parse GHC compiler message format
parseGHCMessage :: Text -> Maybe (MessageSeverity, Maybe FilePath, Maybe Int, Maybe Int, Text)
parseGHCMessage line
  -- Format: "src/Main.hs:10:5: error: message"
  | line =~ ("^([^:]+):([0-9]+):([0-9]+): (error|warning|note): (.*)$" :: Text) = 
      case line =~ ("^([^:]+):([0-9]+):([0-9]+): (error|warning|note): (.*)$" :: Text) :: (Text, Text, Text, [Text]) of
        (_,_,_, [file, lineStr, colStr, severityStr, msg]) ->
          let severity = parseSeverity severityStr
              lineNum = case reads (T.unpack lineStr) of
                        [(n,"")] -> Just n
                        _ -> Nothing
              colNum = case reads (T.unpack colStr) of
                       [(n,"")] -> Just n
                       _ -> Nothing
          in Just (severity, Just (T.unpack file), lineNum, colNum, msg)
        _ -> Nothing
  
  -- Format: "src/Main.hs:10: error: message" (no column)
  | line =~ ("^([^:]+):([0-9]+): (error|warning|note): (.*)$" :: Text) =
      case line =~ ("^([^:]+):([0-9]+): (error|warning|note): (.*)$" :: Text) :: (Text, Text, Text, [Text]) of
        (_,_,_, [file, lineStr, severityStr, msg]) ->
          let severity = parseSeverity severityStr
              lineNum = case reads (T.unpack lineStr) of
                        [(n,"")] -> Just n
                        _ -> Nothing
          in Just (severity, Just (T.unpack file), lineNum, Nothing, msg)
        _ -> Nothing
        
  -- Format: "Error: message" (no location)
  | line =~ ("^(Error|Warning): (.*)$" :: Text) =
      case line =~ ("^(Error|Warning): (.*)$" :: Text) :: (Text, Text, Text, [Text]) of
        (_,_,_, [severityStr, msg]) ->
          let severity = parseSeverity severityStr
          in Just (severity, Nothing, Nothing, Nothing, msg)
        _ -> Nothing
        
  | otherwise = Nothing

-- | Parse severity from text
parseSeverity :: Text -> MessageSeverity  
parseSeverity s
  | s `elem` ["error", "Error"] = Error
  | s `elem` ["warning", "Warning"] = Warning  
  | s `elem` ["note", "Note"] = Info
  | otherwise = Hint

-- | Extract source location from compiler message
extractSourceLocation :: CompilerMessage -> Maybe (FilePath, Int, Int)
extractSourceLocation CompilerMessage{..} = do
  file <- msgFile
  line <- msgLine  
  let col = maybe 1 id msgColumn
  return (file, line, col)

-- | Format compiler message for display
formatCompilerMessage :: CompilerMessage -> Text
formatCompilerMessage msg@CompilerMessage{..} =
  let severity = T.pack $ show msgSeverity
  in case extractSourceLocation msg of
    Just (file, line, col) -> 
      "[" <> severity <> "] " <> T.pack file <> ":" <> T.pack (show line) <> ":" <> T.pack (show col) <> ": " <> msgText
    Nothing -> 
      "[" <> severity <> "] " <> msgText

-- | Check if message is an error  
isErrorMessage :: CompilerMessage -> Bool
isErrorMessage CompilerMessage{..} = msgSeverity == Error

-- | Check if message is a warning
isWarningMessage :: CompilerMessage -> Bool
isWarningMessage CompilerMessage{..} = msgSeverity == Warning
