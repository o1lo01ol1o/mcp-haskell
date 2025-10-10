module MCP.SDK.Server.Middleware
  ( AuthMiddleware
  ) where

import MCP.SDK.Error (MCPError)
import MCP.SDK.Protocol (JSONRPCMessage)
import MCP.SDK.Types.Auth (AuthInfo)

-- | A middleware function that can be used to protect the server with
-- authentication. The middleware is given the incoming JSON-RPC message
-- and should return either an error or the authentication information.
--
-- If the middleware returns an error, the request processing is halted
-- and an error response is sent to the client.
-- If it returns an `AuthInfo`, the information is passed down to the
-- request handlers.
type AuthMiddleware m = JSONRPCMessage -> m (Either MCPError AuthInfo)
