{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.EndToEndSpec (spec) where

import Test.Hspec
import Test.Fixtures (testWorkspaceFiles)
import Test.Utils (withTestWorkspace)
import Control.Exception (try, SomeException, bracket)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)
import Data.Function ((&))
import System.Process.Typed
import System.FilePath ((</>))
import System.IO (Handle, hGetLine, hFlush, hClose)
import qualified System.IO as IO (hPutStrLn)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

-- MCP Server Handle for end-to-end testing
data MCPServerHandle = MCPServerHandle
  { mcpProcess :: Process Handle Handle Handle
  , mcpStdin :: Handle
  , mcpStdout :: Handle
  , mcpStderr :: Handle
  }

-- Start MCP server process
startMCPServer :: FilePath -> IO MCPServerHandle
startMCPServer _workDir = do
  -- Use the MCP executable from the current project build
  let mcpExecutable = "./dist-newstyle/build/aarch64-osx/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
  let processConfig = proc mcpExecutable []
                    & setStdin createPipe
                    & setStdout createPipe  
                    & setStderr createPipe
  
  process <- startProcess processConfig
  let stdin_h = getStdin process
  let stdout_h = getStdout process  
  let stderr_h = getStderr process
  
  -- Give the server a moment to start
  threadDelay 500000  -- 0.5 seconds
  
  return MCPServerHandle
    { mcpProcess = process
    , mcpStdin = stdin_h
    , mcpStdout = stdout_h
    , mcpStderr = stderr_h
    }

-- Stop MCP server process
stopMCPServer :: MCPServerHandle -> IO ()
stopMCPServer handle = do
  hClose (mcpStdin handle)
  hClose (mcpStdout handle)
  hClose (mcpStderr handle)
  stopProcess (mcpProcess handle)

-- Send JSON-RPC request to MCP server
sendMCPRequest :: MCPServerHandle -> Value -> IO ()
sendMCPRequest handle request = do
  let jsonStr = L8.unpack (encode request)
  IO.hPutStrLn (mcpStdin handle) jsonStr
  hFlush (mcpStdin handle)

-- Read JSON-RPC response from MCP server
readMCPResponse :: MCPServerHandle -> IO (Either String Value)
readMCPResponse handle = do
  maybeResponse <- timeout 10000000 $ hGetLine (mcpStdout handle)  -- 10 second timeout
  case maybeResponse of
    Nothing -> return $ Left "Response timeout"
    Just responseStr -> case eitherDecode (L8.pack responseStr) of
      Left err -> return $ Left $ "JSON decode error: " ++ err
      Right response -> return $ Right response

-- Helper to run MCP server with cleanup
withMCPServer :: FilePath -> (MCPServerHandle -> IO a) -> IO a
withMCPServer projectRoot action = 
  bracket (startMCPServer projectRoot) stopMCPServer action

-- Test specs
spec :: Spec
spec = describe "MCP End-to-End Integration Tests" $ do
  
  describe "MCP Server Process Management" $ do
    it "can start and communicate with MCP server process" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          -- Send initialize request
          let initRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("initialize" :: T.Text)
                , "params" .= object
                    [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                    , "capabilities" .= object []
                    , "clientInfo" .= object
                        [ "name" .= ("test-client" :: T.Text)
                        , "version" .= ("1.0.0" :: T.Text)
                        ]
                    ]
                , "id" .= (1 :: Int)
                ]
          
          sendMCPRequest mcpHandle initRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get initialize response: " ++ err
            Right responseValue -> do
              -- Verify it's a valid JSON-RPC response
              case responseValue of
                Object obj -> do
                  case KM.lookup "jsonrpc" obj of
                    Just (String "2.0") -> return ()
                    _ -> expectationFailure "Invalid JSON-RPC version in response"
                  
                  -- Verify we got server info
                  case KM.lookup "result" obj of
                    Just (Object result) -> do
                      case KM.lookup "serverInfo" result of
                        Just (Object serverInfo) -> do
                          case KM.lookup "name" serverInfo of
                            Just (String name) -> name `shouldSatisfy` (\n -> "mcp-hls" `T.isInfixOf` n)
                            _ -> expectationFailure "Missing server name in response"
                        _ -> expectationFailure "Missing server info in response"
                    _ -> expectationFailure "Missing result in initialize response"
                _ -> expectationFailure "Response is not a JSON object"

    it "can list all available tools through MCP server" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          -- Send tools/list request
          let toolsRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/list" :: T.Text)
                , "id" .= (2 :: Int)
                ]
          
          sendMCPRequest mcpHandle toolsRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get tools list: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "tools" result of
                    Just (Array tools) -> do
                      -- Should have many tools
                      Prelude.length tools `shouldSatisfy` (> 20)
                      
                      -- Check for some expected tools
                      let toolNames = [name | Object tool <- toList tools,
                                            Just (String name) <- [KM.lookup "name" tool]]
                      toolNames `shouldSatisfy` ("hover_info" `Prelude.elem`)
                      toolNames `shouldSatisfy` ("add_type_signature" `Prelude.elem`)
                      toolNames `shouldSatisfy` ("get_hls_status" `Prelude.elem`)
                    _ -> expectationFailure "Missing tools array in response"
                _ -> expectationFailure "Missing result in tools/list response"

  describe "MCP-HLS Integration Tests" $ do
    it "can check HLS status through MCP server" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          -- Send tools/call request for get_hls_status
          let statusRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/call" :: T.Text)
                , "params" .= object
                    [ "name" .= ("get_hls_status" :: T.Text)
                    , "arguments" .= object []
                    ]
                , "id" .= (3 :: Int)
                ]
          
          sendMCPRequest mcpHandle statusRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get HLS status: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "content" result of
                    Just (Array content) -> do
                      content `shouldSatisfy` (not . Prelude.null)
                      -- Should contain status information
                      let textContents = [text | Object item <- toList content,
                                               String "text" <- maybeToList $ KM.lookup "type" item,
                                               Just (String text) <- [KM.lookup "text" item]]
                      case textContents of
                        (statusText:_) -> 
                          statusText `shouldSatisfy` (\t -> "stopped" `T.isInfixOf` T.toLower t ||
                                                           "running" `T.isInfixOf` T.toLower t ||
                                                           "error" `T.isInfixOf` T.toLower t)
                        [] -> expectationFailure "No text content in HLS status response"
                    _ -> expectationFailure "Missing content array in status response"
                _ -> expectationFailure "Missing result in status response"

    it "can get version information through MCP server" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          let versionRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/call" :: T.Text)
                , "params" .= object
                    [ "name" .= ("show_versions" :: T.Text)
                    , "arguments" .= object []
                    ]
                , "id" .= (4 :: Int)
                ]
          
          sendMCPRequest mcpHandle versionRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get version info: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "content" result of
                    Just (Array content) -> do
                      let textContents = [text | Object item <- toList content,
                                               String "text" <- maybeToList $ KM.lookup "type" item,
                                               Just (String text) <- [KM.lookup "text" item]]
                      case textContents of
                        (versionText:_) -> do
                          versionText `shouldSatisfy` (\t -> "version" `T.isInfixOf` T.toLower t)
                          versionText `shouldSatisfy` (\t -> "mcp" `T.isInfixOf` T.toLower t)
                        [] -> expectationFailure "No version text in response"
                    _ -> expectationFailure "Missing content in version response"
                _ -> expectationFailure "Missing result in version response"

  describe "MCP-HLS LSP Operations" $ do
    it "can call hover_info tool with file parameters" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          let testFile = workDir </> "ValidModule.hs"
          let hoverRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/call" :: T.Text)
                , "params" .= object
                    [ "name" .= ("hover_info" :: T.Text)
                    , "arguments" .= object
                        [ "filePath" .= testFile
                        , "line" .= (5 :: Int)
                        , "character" .= (10 :: Int)
                        ]
                    ]
                , "id" .= (5 :: Int)
                ]
          
          sendMCPRequest mcpHandle hoverRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get hover info: " ++ err
            Right responseValue -> do
              -- Should get a response (even if it's placeholder)
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "content" result of
                    Just (Array content) -> do
                      content `shouldSatisfy` (not . Prelude.null)
                      -- Check that response contains file path
                      let textContents = [text | Object item <- toList content,
                                               String "text" <- maybeToList $ KM.lookup "type" item,
                                               Just (String text) <- [KM.lookup "text" item]]
                      case textContents of
                        (hoverText:_) -> 
                          hoverText `shouldSatisfy` (\t -> T.pack testFile `T.isInfixOf` t)
                        [] -> expectationFailure "No hover text in response"
                    _ -> expectationFailure "Missing content in hover response"
                _ -> expectationFailure "Missing result in hover response"

    it "can call add_type_signature tool with range parameters" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          let testFile = workDir </> "ValidModule.hs"
          let typeSignatureRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/call" :: T.Text)
                , "params" .= object
                    [ "name" .= ("add_type_signature" :: T.Text)
                    , "arguments" .= object
                        [ "filePath" .= testFile
                        , "startLine" .= (3 :: Int)
                        , "startChar" .= (0 :: Int)
                        , "endLine" .= (3 :: Int)
                        , "endChar" .= (20 :: Int)
                        ]
                    ]
                , "id" .= (6 :: Int)
                ]
          
          sendMCPRequest mcpHandle typeSignatureRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to add type signature: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "content" result of
                    Just (Array content) -> do
                      content `shouldSatisfy` (not . Prelude.null)
                      -- Should mention the command execution
                      let textContents = [text | Object item <- toList content,
                                               String "text" <- maybeToList $ KM.lookup "type" item,
                                               Just (String text) <- [KM.lookup "text" item]]
                      case textContents of
                        (resultText:_) -> 
                          resultText `shouldSatisfy` (\t -> "ghcide-type-lenses:typesignature.add" `T.isInfixOf` t)
                        [] -> expectationFailure "No result text in type signature response"
                    _ -> expectationFailure "Missing content in type signature response"
                _ -> expectationFailure "Missing result in type signature response"

    it "handles invalid tool calls gracefully" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          let invalidRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("tools/call" :: T.Text)
                , "params" .= object
                    [ "name" .= ("nonexistent_tool" :: T.Text)
                    , "arguments" .= object []
                    ]
                , "id" .= (7 :: Int)
                ]
          
          sendMCPRequest mcpHandle invalidRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get error response: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "result") responseValue of
                Just (Object result) -> do
                  case KM.lookup "isError" result of
                    Just (Bool True) -> return () -- Expected error
                    _ -> do
                      -- Check content for error message
                      case KM.lookup "content" result of
                        Just (Array content) -> do
                          let textContents = [text | Object item <- toList content,
                                                   String "text" <- maybeToList $ KM.lookup "type" item,
                                                   Just (String text) <- [KM.lookup "text" item]]
                          case textContents of
                            (errorText:_) -> 
                              errorText `shouldSatisfy` (\t -> "unknown" `T.isInfixOf` T.toLower t ||
                                                             "nonexistent" `T.isInfixOf` T.toLower t)
                            [] -> expectationFailure "No error message in invalid tool response"
                        _ -> expectationFailure "Missing content in invalid tool response"
                _ -> expectationFailure "Missing result in invalid tool response"

  describe "MCP Server Error Handling" $ do
    it "handles malformed JSON gracefully" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          -- Send malformed JSON
          IO.hPutStrLn (mcpStdin mcpHandle) "{invalid json"
          hFlush (mcpStdin mcpHandle)
          
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> return () -- Expected - malformed JSON should cause timeout or error
            Right responseValue -> do
              -- Should get JSON-RPC error response
              case parseMaybe (.: "error") responseValue of
                Just (Object errorObj) -> do
                  case KM.lookup "code" errorObj of
                    Just (Number code) -> code `shouldSatisfy` (< 0) -- JSON-RPC error codes are negative
                    _ -> expectationFailure "Missing error code in error response"
                _ -> expectationFailure "Expected error response for malformed JSON"

    it "handles missing method gracefully" $ do
      withSystemTempDirectory "mcp-e2e-test" $ \workDir -> do
        withMCPServer workDir $ \mcpHandle -> do
          let invalidMethodRequest = object
                [ "jsonrpc" .= ("2.0" :: T.Text)
                , "method" .= ("nonexistent/method" :: T.Text)
                , "id" .= (8 :: Int)
                ]
          
          sendMCPRequest mcpHandle invalidMethodRequest
          response <- readMCPResponse mcpHandle
          
          case response of
            Left err -> expectationFailure $ "Failed to get error response: " ++ err
            Right responseValue -> do
              case parseMaybe (.: "error") responseValue of
                Just (Object errorObj) -> do
                  case KM.lookup "code" errorObj of
                    Just (Number (-32601)) -> return () -- Method not found error
                    Just (Number code) -> expectationFailure $ "Wrong error code: " ++ show code
                    _ -> expectationFailure "Missing error code"
                _ -> expectationFailure "Expected error response for unknown method"

-- Helper functions
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]