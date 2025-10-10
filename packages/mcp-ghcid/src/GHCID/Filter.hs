{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.Filter
  ( -- * Filter Request ADT  
    FilterRequest(..)
  , defaultFilterRequest
  
    -- * Shell Command Filtering  
  , applyShellFilter
  , buildFilterCommand
  
    -- * Safe Parameter Validation
  , validateGrepPattern
  , validateLineCount
  , validateLineRange
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.=), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Process.Typed
import System.Timeout (timeout)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS

-- | Filter request ADT following the same pattern as other request types
data FilterRequest
  = GrepRequest Text
  | HeadRequest Int
  | TailRequest Int
  | LinesRequest Int Int
  | NoFilterRequest
  deriving (Show, Eq)

-- | Default filter request (no filtering)
defaultFilterRequest :: FilterRequest
defaultFilterRequest = NoFilterRequest

-- | FromJSON instance for FilterRequest following explicit ADT pattern
instance FromJSON FilterRequest where
  parseJSON = withObject "FilterRequest" $ \o -> do
    mGrep <- o .:? "grep"
    mHead <- o .:? "head" 
    mTail <- o .:? "tail"
    mLines <- o .:? "lines"
    
    case (mGrep, mHead, mTail, mLines) of
      (Just pattern, Nothing, Nothing, Nothing) -> do
        validPattern <- case validateGrepPattern pattern of
          Left err -> fail $ T.unpack err
          Right p -> return p
        return $ GrepRequest validPattern
      (Nothing, Just n, Nothing, Nothing) -> do
        validN <- case validateLineCount n of
          Left err -> fail $ T.unpack err
          Right count -> return count
        return $ HeadRequest validN
      (Nothing, Nothing, Just n, Nothing) -> do
        validN <- case validateLineCount n of
          Left err -> fail $ T.unpack err
          Right count -> return count
        return $ TailRequest validN
      (Nothing, Nothing, Nothing, Just range) -> do
        (start, end) <- case parseLineRange range of
          Just pair -> return pair
          Nothing -> fail "Invalid line range format (use 'start-end')"
        case validateLineRange (start, end) of
          Left err -> fail $ T.unpack err
          Right (validStart, validEnd) -> return $ LinesRequest validStart validEnd
      (Nothing, Nothing, Nothing, Nothing) -> return NoFilterRequest
      _ -> fail "Only one filter option allowed: grep, head, tail, or lines"
    where
      parseLineRange :: Text -> Maybe (Int, Int)
      parseLineRange range = 
        case T.splitOn "-" range of
          [startStr, endStr] -> 
            case (readMaybe (T.unpack startStr), readMaybe (T.unpack endStr)) of
              (Just start, Just end) -> Just (start, end)
              _ -> Nothing
          _ -> Nothing
      
      readMaybe :: Read a => String -> Maybe a
      readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-- | ToJSON instance for FilterRequest 
instance ToJSON FilterRequest where
  toJSON (GrepRequest pattern) = object ["grep" .= pattern]
  toJSON (HeadRequest n) = object ["head" .= n]
  toJSON (TailRequest n) = object ["tail" .= n]
  toJSON (LinesRequest start end) = object ["lines" .= (T.pack $ show start ++ "-" ++ show end)]
  toJSON NoFilterRequest = object []

-- | Apply single shell filter to text input
applyShellFilter :: Text -> FilterRequest -> IO (Either Text Text)
applyShellFilter input NoFilterRequest = return $ Right input
applyShellFilter input filterReq = do
  case buildFilterCommand filterReq of
    Left err -> return $ Left err
    Right command -> do
      result <- timeout 30000000 $ -- 30 second timeout
        try @SomeException $ runShellCommand input command
      case result of
        Nothing -> return $ Left "Filter command timed out"
        Just (Left ex) -> return $ Left $ "Filter command error: " <> T.pack (show ex)
        Just (Right output) -> return $ Right output

-- | Build validated shell command for single filter
buildFilterCommand :: FilterRequest -> Either Text [String]
buildFilterCommand NoFilterRequest = Left "No filter specified"
buildFilterCommand (GrepRequest pattern) = do
  -- Pattern was already validated in FromJSON instance
  return ["grep", "--", T.unpack pattern]
buildFilterCommand (HeadRequest n) = do
  -- Count was already validated in FromJSON instance
  return ["head", "-n", show n]
buildFilterCommand (TailRequest n) = do
  -- Count was already validated in FromJSON instance
  return ["tail", "-n", show n]
buildFilterCommand (LinesRequest start end) = do
  -- Range was already validated in FromJSON instance
  return ["sed", "-n", show start ++ "," ++ show end ++ "p"]

-- | Run single shell command with input
runShellCommand :: Text -> [String] -> IO Text
runShellCommand input (cmd:args) = do
  let inputBytes = LBS.fromStrict $ T.encodeUtf8 input
      processConfig = setStderr closed $ 
                     setStdout byteStringOutput $ 
                     setStdin (byteStringInput inputBytes) $ 
                     proc cmd args
  (exitCode, output, _) <- readProcess processConfig
  case exitCode of
    ExitSuccess -> return $ T.decodeUtf8 $ toStrict output
    ExitFailure 1 | cmd == "grep" -> 
      -- grep returns 1 when no matches found, which is valid for empty results
      return ""
    ExitFailure code -> 
      error $ "Shell command failed: " ++ cmd ++ " (exit code: " ++ show code ++ ")"
runShellCommand input [] = return input

-- | Validate grep pattern for shell safety
validateGrepPattern :: Text -> Either Text Text
validateGrepPattern pattern
  | T.null pattern = Left "Empty grep pattern"
  | T.length pattern > 1000 = Left "Grep pattern too long (max 1000 chars)"
  | hasUnsafeChars pattern = Left "Grep pattern contains unsafe characters"  
  | otherwise = Right pattern
  where
    hasUnsafeChars :: Text -> Bool
    hasUnsafeChars p = any (`T.isInfixOf` p) ["`", "$(", "${", ";", "|", "&", ">", "<"]

-- | Validate line count parameters
validateLineCount :: Int -> Either Text Int
validateLineCount n
  | n <= 0 = Left "Line count must be positive"
  | n > 100000 = Left "Line count too large (max 100000)"
  | otherwise = Right n

-- | Validate line range parameters  
validateLineRange :: (Int, Int) -> Either Text (Int, Int)
validateLineRange (start, end) = do
  validStart <- validateLineCount start
  validEnd <- validateLineCount end
  if validStart <= validEnd
    then Right (validStart, validEnd)
    else Left "Invalid range: start line must be <= end line"

-- Helper function - better to use built-in fromList
-- catMaybes and foldM are from standard library imports
