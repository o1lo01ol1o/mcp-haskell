{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MCP.Server.Obelisk as Server
import qualified Obelisk.Config as Config
import System.Environment (getArgs)
import System.Exit (die)

-- | Simple CLI that currently only accepts optional --help.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    ["-h"] -> showHelp
    [] -> startServer
    _ -> die "Unknown arguments. Use --help for usage."

showHelp :: IO ()
showHelp = putStrLn $ unlines
  [ "mcp-obelisk - Model Context Protocol server for Obelisk"
  , ""
  , "Usage:"
  , "  mcp-obelisk          Start the MCP server"
  , "  mcp-obelisk --help   Show this help message"
  , ""
  , "The server exposes tools for managing 'ob watch' via MCP."
  , "Changes to .cabal files require restarting ob watch through the"
  , "obelisk-start tool. Changes to the Nix environment require restarting"
  , "the mcp-obelisk server itself."
  ]

startServer :: IO ()
startServer = do
  config <- Config.defaultServerConfig
  Server.runObeliskServer config
