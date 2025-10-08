{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Obelisk.Filter
  ( FilterRequest (..)
  , defaultFilterRequest
  , applyShellFilter
  , buildFilterCommand
  , validateGrepPattern
  , validateLineCount
  , validateLineRange
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.=))
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Process.Typed
import System.Timeout (timeout)

-- | Filter request mirroring the ghcid tooling so clients can reuse prompts.
data FilterRequest
  = GrepRequest Text
  | HeadRequest Int
  | TailRequest Int
  | LinesRequest Int Int
  | NoFilterRequest
  deriving (Show, Eq)

defaultFilterRequest :: FilterRequest
defaultFilterRequest = NoFilterRequest

instance FromJSON FilterRequest where
  parseJSON = withObject "FilterRequest" $ \o -> do
    mGrep <- o .:? "grep"
    mHead <- o .:? "head"
    mTail <- o .:? "tail"
    mLines <- o .:? "lines"
    let liftValidation = either (fail . T.unpack) pure
    case (mGrep, mHead, mTail, mLines) of
      (Just pattern, Nothing, Nothing, Nothing) -> GrepRequest <$> liftValidation (validateGrepPattern pattern)
      (Nothing, Just n, Nothing, Nothing) -> HeadRequest <$> liftValidation (validateLineCount n)
      (Nothing, Nothing, Just n, Nothing) -> TailRequest <$> liftValidation (validateLineCount n)
      (Nothing, Nothing, Nothing, Just rangeText) ->
        case parseLineRange rangeText of
          Nothing -> fail "Invalid line range format (expected 'start-end')"
          Just range -> do
            (s, e) <- liftValidation (validateLineRange range)
            pure $ LinesRequest s e
      (Nothing, Nothing, Nothing, Nothing) -> pure NoFilterRequest
      _ -> fail "Only one filter option may be specified"
    where
      parseLineRange :: Text -> Maybe (Int, Int)
      parseLineRange range =
        case T.splitOn "-" range of
          [startStr, endStr] -> do
            start <- readMaybe startStr
            end <- readMaybe endStr
            pure (start, end)
          _ -> Nothing

      readMaybe :: Read a => Text -> Maybe a
      readMaybe s = case reads (T.unpack s) of
        [(x, "")] -> Just x
        _ -> Nothing

instance ToJSON FilterRequest where
  toJSON (GrepRequest pattern) = object ["grep" .= pattern]
  toJSON (HeadRequest n) = object ["head" .= n]
  toJSON (TailRequest n) = object ["tail" .= n]
  toJSON (LinesRequest start end) = object ["lines" .= T.pack (show start ++ "-" ++ show end)]
  toJSON NoFilterRequest = object []

applyShellFilter :: Text -> FilterRequest -> IO (Either Text Text)
applyShellFilter input NoFilterRequest = pure $ Right input
applyShellFilter input filterReq =
  case buildFilterCommand filterReq of
    Left err -> pure $ Left err
    Right command -> do
      result <- timeout 30000000 $ try @SomeException $ runShellCommand input command
      case result of
        Nothing -> pure $ Left "Filter command timed out"
        Just (Left ex) -> pure $ Left $ "Filter command error: " <> T.pack (show ex)
        Just (Right output) -> pure $ Right output

buildFilterCommand :: FilterRequest -> Either Text [String]
buildFilterCommand NoFilterRequest = Left "No filter specified"
buildFilterCommand (GrepRequest pattern) = Right ["grep", "--", T.unpack pattern]
buildFilterCommand (HeadRequest n) = Right ["head", "-n", show n]
buildFilterCommand (TailRequest n) = Right ["tail", "-n", show n]
buildFilterCommand (LinesRequest start end) = Right ["sed", "-n", show start ++ "," ++ show end ++ "p"]

runShellCommand :: Text -> [String] -> IO Text
runShellCommand input (cmd:args) = do
  let inputBytes = LBS.fromStrict $ T.encodeUtf8 input
      processConfig = setStderr closed
                    $ setStdout byteStringOutput
                    $ setStdin (byteStringInput inputBytes)
                    $ proc cmd args
  (exitCode, output, _) <- readProcess processConfig
  case exitCode of
    ExitSuccess -> pure $ T.decodeUtf8 $ toStrict output
    ExitFailure 1 | cmd == "grep" -> pure ""
    ExitFailure code -> error $ "Shell command failed: " ++ cmd ++ " (exit code: " ++ show code ++ ")"
runShellCommand input [] = pure input

validateGrepPattern :: Text -> Either Text Text
validateGrepPattern pattern
  | T.null pattern = Left "Empty grep pattern"
  | T.length pattern > 1000 = Left "Grep pattern too long"
  | hasUnsafe pattern = Left "Grep pattern contains unsafe characters"
  | otherwise = Right pattern
  where
    hasUnsafe :: Text -> Bool
    hasUnsafe p = any (`T.isInfixOf` p) ["`", "$(", "${", ";", "|", "&", ">", "<"]

validateLineCount :: Int -> Either Text Int
validateLineCount n
  | n <= 0 = Left "Line count must be positive"
  | n > 100000 = Left "Line count too large"
  | otherwise = Right n

validateLineRange :: (Int, Int) -> Either Text (Int, Int)
validateLineRange (start, end) = do
  validStart <- validateLineCount start
  validEnd <- validateLineCount end
  if validStart <= validEnd
    then Right (validStart, validEnd)
    else Left "Invalid line range: start must be <= end"
