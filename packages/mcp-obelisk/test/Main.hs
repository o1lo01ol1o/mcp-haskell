module Main (main) where

import Test.Hspec (hspec)
import qualified MCP.ObeliskIntegrationSpec

main :: IO ()
main = hspec MCP.ObeliskIntegrationSpec.spec
