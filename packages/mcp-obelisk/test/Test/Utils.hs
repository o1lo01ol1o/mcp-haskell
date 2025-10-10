{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Utils
  ( findMCPObeliskExecutable
  , withMCPObeliskServer
  , sendRequest
  , readResponse
  , pollForMessage
  , pollForStatus
  , validateToolResponse
  , extractToolText
  , decodeMessagePayload
  , assertAllGood
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, forM)
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf, maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (BufferMode (LineBuffering), Handle, hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr)
import System.Process.Typed
import System.Timeout (timeout)
import System.Info (compilerVersion)
import Test.Hspec (expectationFailure)

findMCPObeliskExecutable :: IO (Maybe FilePath)
findMCPObeliskExecutable = do
  envOverride <- firstExistingEnv ["MCP_OBELISK_EXECUTABLE", "MCP_OBELISK_BIN"]
  case envOverride of
    Just path -> pure (Just path)
    Nothing -> fallback

 where
    fallback = do
      cwd <- getCurrentDirectory
      repoRoot <- locateRepoRoot cwd
      let directCandidates =
            [ repoRoot </> "dist/build/mcp-obelisk/mcp-obelisk"
            , repoRoot </> "dist/build/mcp-obelisk/mcp-obelisk.exe"
            ]
      directExisting <- selectLatest directCandidates
      case directExisting of
        Just path -> pure (Just path)
        Nothing -> do
          let searchRoots =
                [ repoRoot </> "dist/build"
                , repoRoot </> "dist-newstyle"
                ]
          distPaths <- fmap concat $ forM searchRoots findBinaries
          selectLatest distPaths

    firstExistingEnv [] = pure Nothing
    firstExistingEnv (name:names) = do
      mVal <- lookupEnv name
      case mVal of
        Just path | not (null path) -> do
          exists <- doesFileExist path
          if exists then pure (Just path) else firstExistingEnv names
        _ -> firstExistingEnv names

    findBinaries dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entries <- listDirectory dir
          fmap concat . forM entries $ \entry -> do
            let full = dir </> entry
            isDir <- doesDirectoryExist full
            if isDir
              then findBinaries full
              else pure [full | takeFileName full == "mcp-obelisk"]


    selectLatest paths = do
      existing <- filterM doesFileExist paths
      case existing of
        [] -> pure Nothing
        xs -> do
          stamped <- mapM stamp xs
          pure . Just . snd $ maximumBy (comparing fst) stamped

    stamp fp = do
      time <- getModificationTime fp
      pure (time, fp)

    locateRepoRoot path = do
      let cabalFile = path </> "cabal.project"
      exists <- doesFileExist cabalFile
      if exists
        then pure path
        else do
          let parent = takeDirectory path
          if parent == path
            then pure path
            else locateRepoRoot parent

withMCPObeliskServer :: FilePath -> ((Handle, Handle) -> IO a) -> IO a
withMCPObeliskServer execPath action =
  bracket start stop $ \(_, handles) -> action handles
  where
    start = do
      let cfg = setStdin createPipe
              $ setStdout createPipe
              $ setStderr inherit
              $ proc execPath []
      process <- startProcess cfg
      let stdinHandle = getStdin process
          stdoutHandle = getStdout process
      hSetBuffering stdinHandle LineBuffering
      hSetBuffering stdoutHandle LineBuffering
      pure (process, (stdinHandle, stdoutHandle))
    stop (process, _) = do
      _ <- try @SomeException $ stopProcess process
      pure ()

sendRequest :: Handle -> Value -> IO ()
sendRequest h value = do
  L8.hPutStrLn h (encode value)
  hFlush h

readResponse :: Handle -> Int -> IO (Either String Value)
readResponse h micros = do
  let readJsonLine = do
        line <- hGetLine h
        let trimmed = dropWhile isSpace line
        if not (null trimmed) && head trimmed == '{'
          then pure $ eitherDecode (L8.pack trimmed)
          else do
            hPutStrLn stderr $ "Ignoring non-JSON line from server: " <> line
            readJsonLine

  res <- timeout micros readJsonLine
  case res of
    Nothing -> pure $ Left "Timed out waiting for MCP response"
    Just decoded -> pure decoded

pollForMessage :: (Handle, Handle) -> FilePath -> Text -> Int -> IO (Either String Text)
pollForMessage (hin, hout) projectPath needle attempts = loop attempts Nothing
  where
    needleLower = T.toLower needle

    loop :: Int -> Maybe Text -> IO (Either String Text)
    loop 0 lastMsg =
      pure $ Left ("Exceeded polling attempts. Last message: " <> maybe "<none>" T.unpack lastMsg)
    loop n lastMsg = do
      sendRequest hin $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (fromIntegral (attempts - n + 1) :: Int)
        , "method" .= ("tools/call" :: Text)
        , "params" .= object
            [ "name" .= ("obelisk-messages" :: Text)
            , "arguments" .= object
                [ "projectPath" .= T.pack projectPath
                , "limit" .= (80 :: Int)
                ]
            ]
        ]
      resp <- readResponse hout 10000000
      case resp of
        Left err ->
          if "Timed out" `T.isInfixOf` T.pack err
            then do
              threadDelay 500000
              loop (n - 1) lastMsg
            else pure $ Left err
        Right val -> case extractToolText val of
          Nothing -> pure $ Left "Malformed tool response"
          Just txt ->
            case decodeMessagePayload txt of
              Left parseErr -> pure $ Left ("Failed to parse message payload: " <> parseErr)
              Right (outputTxt, linesList) ->
                if any (\lineTxt -> needleLower `T.isInfixOf` T.toLower lineTxt) linesList
                    || needleLower `T.isInfixOf` T.toLower outputTxt
                  then pure $ Right outputTxt
                  else do
                    threadDelay 500000
                    loop (n - 1) (Just outputTxt)


pollForStatus :: (Handle, Handle) -> FilePath -> Int -> IO (Either String (Text, Maybe Text, Maybe Text))
pollForStatus (hin, hout) projectPath attempts = loop attempts Nothing
  where
    loop :: Int -> Maybe (Text, Maybe Text, Maybe Text) -> IO (Either String (Text, Maybe Text, Maybe Text))
    loop 0 lastSnapshot =
      pure $ Left ("Exceeded status polling attempts. Last status: " <> maybe "<none>" formatSnapshot lastSnapshot)
    loop n lastSnapshot = do
      sendRequest hin $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (fromIntegral n :: Int)
        , "method" .= ("tools/call" :: Text)
        , "params" .= object
            [ "name" .= ("obelisk-status" :: Text)
            , "arguments" .= object
                [ "projectPath" .= T.pack projectPath
                ]
            ]
        ]
      resp <- readResponse hout 10000000
      case resp of
        Left err ->
          if "Timed out" `T.isInfixOf` T.pack err
            then do
              threadDelay 500000
              loop (n - 1) lastSnapshot
            else pure $ Left err
        Right val -> case extractToolText val of
          Nothing -> pure $ Left "Malformed status response"
          Just txt ->
            case decodeStatusPayload txt of
              Left parseErr -> pure $ Left ("Failed to parse status payload: " <> parseErr)
              Right snapshot@(stateTxt, lastMsg, errMsg) ->
                case T.toLower stateTxt of
                  "running" -> pure $ Right snapshot
                  "errored" -> pure $ Left ("Obelisk watch errored: " <> maybe (T.unpack stateTxt) T.unpack errMsg)
                  _ -> do
                    threadDelay 1000000
                    loop (n - 1) (Just snapshot)

    formatSnapshot :: (Text, Maybe Text, Maybe Text) -> String
    formatSnapshot (stateTxt, lastMsg, errMsg) =
      T.unpack stateTxt
        ++ maybe "" (\msg -> " (last: " ++ T.unpack msg ++ ")") lastMsg
        ++ maybe "" (\err -> " (error: " ++ T.unpack err ++ ")") errMsg


decodeStatusPayload :: Text -> Either String (Text, Maybe Text, Maybe Text)
decodeStatusPayload txt =
  case eitherDecode (L8.pack (T.unpack txt)) of
    Left err -> Left err
    Right val ->
      parseEither
        (withObject "statusPayload" $ \o -> do
          statusTxt <- o .: "status"
          errTxt <- o .:? "error"
          lastMsg <- o .:? "lastMessage"
          pure (statusTxt, lastMsg, errTxt)
        )
        val

decodeMessagePayload :: Text -> Either String (Text, [Text])
decodeMessagePayload txt =
  case eitherDecode (L8.pack (T.unpack txt)) of
    Left err -> Left err
    Right val ->
      parseEither
        (withObject "messagesPayload" $ \o -> do
          outputTxt <- fromMaybe "" <$> o .:? "output"
          linesList <- fromMaybe [] <$> o .:? "lines"
          pure (outputTxt, linesList)
        )
        val

validateToolResponse :: Text -> Value -> IO ()
validateToolResponse name value = do
  let resultCheck = parseMaybe (withObject "resp" $ \o -> do
        resultVal <- o .:? "result"
        case resultVal of
          Just (Object resObj) -> do
            contentVal <- resObj .:? "content"
            errFlag <- resObj .:? "isError"
            case errFlag of
              Just True -> fail "Tool response reported error"
              _ -> case contentVal of
                Just (Array arr) | not (null arr) -> pure ()
                _ -> fail "Tool response missing content"
          _ -> fail "Missing result object"
        ) value
  case resultCheck of
    Nothing -> expectationFailure $ "Malformed response for " <> T.unpack name <> ": " <> show value
    Just _ -> pure ()

assertAllGood :: Text -> Bool
assertAllGood msg = "all good" `T.isInfixOf` T.toLower msg


extractToolText :: Value -> Maybe Text
extractToolText = parseMaybe $ withObject "response" $ \o -> do
  resultVal <- o .:? "result"
  case resultVal of
    Nothing -> fail "Missing result"
    Just resObj -> withObject "result" (\r -> do
      content <- r .: "content"
      case content of
        Array arr -> case V.toList arr of
          (Object obj : _) -> obj .: "text"
          _ -> fail "Unexpected content structure"
        _ -> fail "Unexpected content type"
      ) resObj
