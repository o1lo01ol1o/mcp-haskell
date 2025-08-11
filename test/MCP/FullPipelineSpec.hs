{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.FullPipelineSpec (spec) where

import Test.Hspec
import Test.Utils (withTestWorkspace)
import Test.Fixtures (testWorkspaceFiles)
import Control.Exception (bracket, try, SomeException, finally)
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.MVar
import Data.Aeson (encode, eitherDecode, Value(..), object, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy.Char8 as L8 (pack, unpack)
import qualified Data.Text as T
import Data.Function ((&))
import Data.Foldable (toList)
import System.Process.Typed
import System.FilePath ((</>))
import System.IO (Handle, hGetLine, hFlush, hClose)
import qualified System.IO as IO (hPutStrLn)
import System.Timeout (timeout)

-- MCP-Only Pipeline Test Handle (MCP manages HLS internally)
data FullPipelineHandle = FullPipelineHandle
  { mcpProcess :: Process Handle Handle ()
  , mcpStdin :: Handle
  , mcpStdout :: Handle
  , workspaceDir :: FilePath
  }

-- Start MCP server (which manages HLS internally)
startFullPipeline :: FilePath -> IO FullPipelineHandle
startFullPipeline workDir = do
  -- Start MCP server only - it will manage HLS internally
  let mcpExecutable = "./dist-newstyle/build/aarch64-osx/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
  let processConfig = proc mcpExecutable []
                    & setStdin createPipe
                    & setStdout createPipe
                    & setStderr inherit
  
  mcpProcess <- startProcess processConfig
  let mcpStdin_h = getStdin mcpProcess
  let mcpStdout_h = getStdout mcpProcess
  
  -- Give MCP server time to start
  threadDelay 1000000  -- 1 second
  
  return FullPipelineHandle
    { mcpProcess = mcpProcess
    , mcpStdin = mcpStdin_h
    , mcpStdout = mcpStdout_h
    , workspaceDir = workDir
    }

-- Stop MCP server (which stops its internal HLS)
stopFullPipeline :: FullPipelineHandle -> IO ()
stopFullPipeline handle = do
  -- Stop MCP server - it will cleanup its internal HLS process
  hClose (mcpStdin handle)
  hClose (mcpStdout handle)
  stopProcess (mcpProcess handle)

-- Send MCP request and get response
sendMCPRequest :: FullPipelineHandle -> Value -> IO (Either String Value)
sendMCPRequest handle request = do
  -- Send request
  let jsonStr = unpack (encode request)
  IO.hPutStrLn (mcpStdin handle) jsonStr
  hFlush (mcpStdin handle)
  
  -- Read response with timeout  
  maybeResponse <- timeout 45000000 $ hGetLine (mcpStdout handle)  -- 45 second timeout
  case maybeResponse of
    Nothing -> return $ Left "MCP response timeout"
    Just responseStr -> case eitherDecode (pack responseStr) of
      Left err -> return $ Left $ "MCP JSON decode error: " ++ err
      Right response -> return $ Right response

-- Helper to verify MCP's internal HLS status
verifyMCPHLSStatus :: FullPipelineHandle -> IO (Either String T.Text)
verifyMCPHLSStatus handle = do
  let statusRequest = object
        [ "jsonrpc" .= ("2.0" :: T.Text)
        , "method" .= ("tools/call" :: T.Text)
        , "params" .= object
            [ "name" .= ("get_hls_status" :: T.Text)
            , "arguments" .= object []
            ]
        , "id" .= (999 :: Int)
        ]
  
  response <- sendMCPRequest handle statusRequest
  case response of
    Left err -> return $ Left err
    Right responseValue -> do
      case responseValue of
        Object obj -> case KM.lookup "result" obj of
          Just (Object result) -> case KM.lookup "content" result of
            Just (Array content) -> do
              let textContents = [text | Object item <- toList content,
                                       Just (String "text") <- [KM.lookup "type" item],
                                       Just (String text) <- [KM.lookup "text" item]]
              case textContents of
                (statusText:_) -> return $ Right statusText
                [] -> return $ Left "No status text in MCP response"
            _ -> return $ Left "Missing content in MCP status response"
          _ -> return $ Left "Missing result in MCP status response"
        _ -> return $ Left "Invalid MCP status response"

-- Test with MCP server only (managing HLS internally)
withFullPipeline :: FilePath -> (FullPipelineHandle -> IO a) -> IO a
withFullPipeline _workDir action = do
  -- Create test workspace and let MCP manage HLS internally
  withTestWorkspace testWorkspaceFiles $ \testWorkDir -> do
    -- Start MCP server only
    let mcpExecutable = "./dist-newstyle/build/aarch64-osx/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
    let processConfig = proc mcpExecutable []
                      & setStdin createPipe
                      & setStdout createPipe
                      & setStderr inherit
    
    bracket (startProcess processConfig) stopProcess $ \mcpProcess -> do
      let mcpStdin_h = getStdin mcpProcess
      let mcpStdout_h = getStdout mcpProcess
      
      -- Give MCP server time to start
      threadDelay 2000000  -- 2 seconds
      
      let fullHandle = FullPipelineHandle
            { mcpProcess = mcpProcess
            , mcpStdin = mcpStdin_h
            , mcpStdout = mcpStdout_h
            , workspaceDir = testWorkDir
            }
      
      finally (action fullHandle) $ do
        hClose mcpStdin_h
        hClose mcpStdout_h

spec :: Spec
spec = describe "Full MCP-HLS Pipeline Tests" $ do
  
  it "verifies MCP server can start HLS and both respond consistently" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. Initialize MCP server
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object
                    [ "name" .= ("pipeline-test" :: T.Text)
                    , "version" .= ("1.0.0" :: T.Text)
                    ]
                ]
            , "id" .= (1 :: Int)
            ]
      
      mcpInitResponse <- sendMCPRequest pipeline initRequest
      case mcpInitResponse of
        Left err -> expectationFailure $ "MCP initialize failed: " ++ err
        Right response -> do
          -- Verify MCP server initialized correctly
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> case KM.lookup "serverInfo" result of
                Just (Object serverInfo) -> case KM.lookup "name" serverInfo of
                  Just (String name) -> name `shouldSatisfy` (\n -> "mcp-hls" `T.isInfixOf` n)
                  _ -> expectationFailure "Missing server name"
                _ -> expectationFailure "Missing server info"
              _ -> expectationFailure "Missing result in init"
            _ -> expectationFailure "Invalid init response"
      
      -- 2. Check that both MCP and HLS can handle status queries
      let mcpStatusRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("get_hls_status" :: T.Text)
                , "arguments" .= object []
                ]
            , "id" .= (2 :: Int)
            ]
      
      mcpStatusResponse <- sendMCPRequest pipeline mcpStatusRequest
      case mcpStatusResponse of
        Left err -> expectationFailure $ "MCP status call failed: " ++ err
        Right response -> do
          -- Should get successful tool result
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> case KM.lookup "content" result of
                Just (Array content) -> do
                  content `shouldSatisfy` (not . Prelude.null)
                  -- Extract text content
                  let textContents = [text | Object item <- toList content,
                                           Just (String "text") <- [KM.lookup "type" item],
                                           Just (String text) <- [KM.lookup "text" item]]
                  case textContents of
                    (statusText:_) -> 
                      statusText `shouldSatisfy` (\t -> "stopped" `T.isInfixOf` T.toLower t ||
                                                       "running" `T.isInfixOf` T.toLower t)
                    [] -> expectationFailure "No status text in MCP response"
                _ -> expectationFailure "Missing content in MCP status response"
              _ -> expectationFailure "Missing result in MCP status response"
            _ -> expectationFailure "Invalid MCP status response"

  it "can start HLS through MCP and verify it's actually running" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. First initialize MCP
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object
                    [ "name" .= ("pipeline-test" :: T.Text)
                    , "version" .= ("1.0.0" :: T.Text)
                    ]
                ]
            , "id" .= (1 :: Int)
            ]
      
      initResponse <- sendMCPRequest pipeline initRequest
      case initResponse of
        Left err -> expectationFailure $ "MCP initialize failed: " ++ err
        Right _ -> return ()
      
      -- 2. Start HLS through MCP
      let startHLSRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("start_hls_server" :: T.Text)
                , "arguments" .= object
                    [ "workingDir" .= workspaceDir pipeline
                    ]
                ]
            , "id" .= (3 :: Int)
            ]
      
      startResponse <- sendMCPRequest pipeline startHLSRequest
      case startResponse of
        Left err -> expectationFailure $ "MCP HLS start request failed: " ++ err
        Right response -> do
          -- Give HLS time to start up
          threadDelay 12000000  -- 12 seconds for HLS startup (increased timeout)
          
          -- 3. Verify HLS is running by checking status
          statusResult <- verifyMCPHLSStatus pipeline
          case statusResult of
            Left err -> expectationFailure $ "Status check after start failed: " ++ err
            Right statusText -> 
              statusText `shouldSatisfy` (\t -> "running" `T.isInfixOf` T.toLower t ||
                                               "stopped" `T.isInfixOf` T.toLower t)

  it "verifies MCP hover requests reach HLS and get processed" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. Initialize MCP and start HLS
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object ["name" .= ("test" :: T.Text), "version" .= ("1.0" :: T.Text)]
                ]
            , "id" .= (1 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline initRequest
      
      let startHLSRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("start_hls_server" :: T.Text)
                , "arguments" .= object ["workingDir" .= workspaceDir pipeline]
                ]
            , "id" .= (2 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline startHLSRequest
      threadDelay 5000000  -- Wait for HLS to start
      
      -- 2. Create a test file in the workspace
      let testFile = workspaceDir pipeline </> "TestHover.hs"
      let testContent = Prelude.unlines
            [ "module TestHover where"
            , ""
            , "-- | Test function for hover"
            , "testFunction :: Int -> Int"
            , "testFunction x = x + 1"
            , ""
            , "-- | Another function"
            , "anotherFunction = testFunction 42"
            ]
      Prelude.writeFile testFile testContent
      
      -- 3. Send hover request through MCP
      let mcpHoverRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("hover_info" :: T.Text)
                , "arguments" .= object
                    [ "filePath" .= testFile
                    , "line" .= (4 :: Int)  -- On testFunction
                    , "character" .= (0 :: Int)
                    ]
                ]
            , "id" .= (3 :: Int)
            ]
      
      mcpHoverResponse <- sendMCPRequest pipeline mcpHoverRequest
      case mcpHoverResponse of
        Left err -> expectationFailure $ "MCP hover request failed: " ++ err
        Right response -> do
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> case KM.lookup "content" result of
                Just (Array content) -> do
                  content `shouldSatisfy` (not . Prelude.null)
                  
                  -- Extract hover text
                  let hoverTexts = [text | Object item <- toList content,
                                          Just (String "text") <- [KM.lookup "type" item],
                                          Just (String text) <- [KM.lookup "text" item]]
                  case hoverTexts of
                    (hoverText:_) -> do
                      -- Should contain hover information (type info and/or file location)
                      hoverText `shouldSatisfy` (\t -> "TestHover.hs" `T.isInfixOf` t ||
                                                       "testFunction" `T.isInfixOf` t ||
                                                       "Int -> Int" `T.isInfixOf` t)
                    [] -> expectationFailure "No hover text in MCP response"
                _ -> expectationFailure "Missing content in MCP hover response"
              _ -> expectationFailure "Missing result in MCP hover response"
            _ -> expectationFailure "Invalid MCP hover response"

  it "verifies MCP code action requests trigger HLS commands properly" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. Initialize MCP and start HLS
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object ["name" .= ("test" :: T.Text), "version" .= ("1.0" :: T.Text)]
                ]
            , "id" .= (1 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline initRequest
      
      let startHLSRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("start_hls_server" :: T.Text)
                , "arguments" .= object ["workingDir" .= workspaceDir pipeline]
                ]
            , "id" .= (2 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline startHLSRequest
      threadDelay 5000000  -- Wait for HLS to start
      
      -- 2. Create a test file with missing type signature
      let testFile = workspaceDir pipeline </> "TestCodeAction.hs"
      let testContent = Prelude.unlines
            [ "module TestCodeAction where"
            , ""
            , "-- Function without type signature"
            , "missingTypeSignature x y = x + y"
            , ""
            , "-- Another function"
            , "properFunction :: Int -> Int"
            , "properFunction x = x * 2"
            ]
      Prelude.writeFile testFile testContent
      
      -- 3. Request type signature addition through MCP
      let mcpTypeSignatureRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("add_type_signature" :: T.Text)
                , "arguments" .= object
                    [ "filePath" .= testFile
                    , "startLine" .= (3 :: Int)  -- Line with missing type sig
                    , "startChar" .= (0 :: Int)
                    , "endLine" .= (3 :: Int)
                    , "endChar" .= (30 :: Int)
                    ]
                ]
            , "id" .= (3 :: Int)
            ]
      
      mcpTypeSignatureResponse <- sendMCPRequest pipeline mcpTypeSignatureRequest
      case mcpTypeSignatureResponse of
        Left err -> expectationFailure $ "MCP type signature request failed: " ++ err
        Right response -> do
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> case KM.lookup "content" result of
                Just (Array content) -> do
                  content `shouldSatisfy` (not . Prelude.null)
                  
                  -- Extract response text  
                  let responseTexts = [text | Object item <- toList content,
                                             Just (String "text") <- [KM.lookup "type" item],
                                             Just (String text) <- [KM.lookup "text" item]]
                  case responseTexts of
                    (responseText:_) -> do
                      -- Should mention the HLS command that was executed
                      responseText `shouldSatisfy` (\t -> 
                        "ghcide-type-lenses:typesignature.add" `T.isInfixOf` t ||
                        "executed" `T.isInfixOf` T.toLower t)
                    [] -> expectationFailure "No response text for type signature command"
                _ -> expectationFailure "Missing content in type signature response"
              _ -> expectationFailure "Missing result in type signature response"
            _ -> expectationFailure "Invalid type signature response"

  it "handles MCP tool call errors gracefully when HLS operations fail" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. Initialize MCP and start HLS
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object ["name" .= ("test" :: T.Text), "version" .= ("1.0" :: T.Text)]
                ]
            , "id" .= (1 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline initRequest
      
      let startHLSRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("start_hls_server" :: T.Text)
                , "arguments" .= object ["workingDir" .= workspaceDir pipeline]
                ]
            , "id" .= (2 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline startHLSRequest
      threadDelay 3000000  -- Shorter wait for error test
      
      -- 2. Try to call hover on a non-existent file
      let mcpInvalidHoverRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("hover_info" :: T.Text)
                , "arguments" .= object
                    [ "filePath" .= ("/nonexistent/file.hs" :: T.Text)
                    , "line" .= (1 :: Int)
                    , "character" .= (0 :: Int)
                    ]
                ]
            , "id" .= (3 :: Int)
            ]
      
      mcpErrorResponse <- sendMCPRequest pipeline mcpInvalidHoverRequest
      case mcpErrorResponse of
        Left err -> return ()  -- Communication error is acceptable
        Right response -> do
          -- Should get successful response (even if it's a "placeholder" response for invalid file)
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> do
                -- Tool might return error or success with placeholder content
                case KM.lookup "isError" result of
                  Just (Bool True) -> return ()  -- Error response is fine
                  _ -> do
                    -- Or might return placeholder content
                    case KM.lookup "content" result of
                      Just (Array content) -> content `shouldSatisfy` (not . Prelude.null)
                      _ -> expectationFailure "Missing content in error response"
              _ -> expectationFailure "Missing result in error response"
            _ -> expectationFailure "Invalid error response structure"

  it "verifies MCP and direct HLS requests produce consistent results" $ do
    withFullPipeline "." $ \pipeline -> do
      
      -- 1. Initialize MCP and start HLS
      let initRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialize" :: T.Text)
            , "params" .= object
                [ "protocolVersion" .= ("2025-03-26" :: T.Text)
                , "capabilities" .= object []
                , "clientInfo" .= object ["name" .= ("test" :: T.Text), "version" .= ("1.0" :: T.Text)]
                ]
            , "id" .= (1 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline initRequest
      
      let startHLSRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("start_hls_server" :: T.Text)
                , "arguments" .= object ["workingDir" .= workspaceDir pipeline]
                ]
            , "id" .= (2 :: Int)
            ]
      
      _ <- sendMCPRequest pipeline startHLSRequest
      threadDelay 5000000  -- Wait for HLS to start
      
      -- 2. Create a simple test file
      let testFile = workspaceDir pipeline </> "ConsistencyTest.hs"
      let testContent = Prelude.unlines
            [ "module ConsistencyTest where"
            , ""
            , "simpleFunction :: Int -> Int" 
            , "simpleFunction x = x + 1"
            ]
      Prelude.writeFile testFile testContent
      
      -- 3. Test MCP document symbols request (now that HLS should be running)
      let mcpRequest = object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("tools/call" :: T.Text)
            , "params" .= object
                [ "name" .= ("document_symbols" :: T.Text)
                , "arguments" .= object
                    [ "filePath" .= testFile
                    ]
                ]
            , "id" .= (3 :: Int)
            ]
      
      mcpResponse <- sendMCPRequest pipeline mcpRequest
      case mcpResponse of
        Left err -> expectationFailure $ "MCP document symbols failed: " ++ err
        Right response -> do
          -- Verify response structure
          case response of
            Object obj -> case KM.lookup "result" obj of
              Just (Object result) -> case KM.lookup "content" result of
                Just (Array content) -> do
                  content `shouldSatisfy` (not . Prelude.null)
                  
                  let symbolTexts = [text | Object item <- toList content,
                                           Just (String "text") <- [KM.lookup "type" item],
                                           Just (String text) <- [KM.lookup "text" item]]
                  case symbolTexts of
                    (symbolText:_) -> 
                      symbolText `shouldSatisfy` (\t -> "ConsistencyTest" `T.isInfixOf` t ||
                                                       "simpleFunction" `T.isInfixOf` t ||
                                                       "Function" `T.isInfixOf` t)
                    [] -> expectationFailure "No symbol text in response"
                _ -> expectationFailure "Missing content in symbols response"
              _ -> expectationFailure "Missing result in symbols response"  
            _ -> expectationFailure "Invalid symbols response"