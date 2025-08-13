# Claude Code MCP Client Bug Workaround

## Problem Description

The `claude-code` CLI client has a bug in its MCP (Model Context Protocol) implementation that prevents proper handshake completion with compliant MCP servers.

### Expected MCP Handshake Flow (per specification)

1. **Client** sends `initialize` request with `id`
2. **Server** responds with `initialize` response  
3. **Client** sends `initialized` notification (no `id` field) ← **MISSING IN CLAUDE-CODE**
4. Normal MCP operation begins

### Actual Claude Code Behavior

1. ✅ **Client** sends `initialize` request correctly
2. ✅ **Server** responds with correct `initialize` response
3. ❌ **Client** never sends required `initialized` notification
4. ❌ **Client** times out after 30 seconds and considers connection failed

## Root Cause Analysis

### Evidence from Logs

**Server logs** (`~/.mcp-ghcid/messages.log`):
```
2025-08-13 08:35:15 [RECV] {"id":0,"method":"initialize",...}
2025-08-13 08:35:15 [SEND] {"id":0,"result":{...},"protocolVersion":"2025-06-18"}
2025-08-13 08:36:15 [RECV] {"id":null,"method":"notifications/cancelled",...}
```

**Client logs** (`claude-cli-nodejs/.../mcp-logs-ghcid/...txt`):
```json
{
  "debug": "Connection failed: Error: Connection to MCP server \"ghcid\" timed out after 30000ms",
  "timestamp": "2025-08-13T08:35:45.794Z"
}
```

### Protocol Version Compatibility

- ✅ **Client protocol version**: `2025-06-18`  
- ✅ **Server protocol version**: `2025-06-18`
- ✅ **Versions match correctly**

The issue is **NOT** protocol version mismatch but missing `initialized` notification.

## Workaround Implementation

Since the claude-code client has this bug, we need to modify our MCP server to work without requiring the `initialized` notification. The workaround involves:

### Strategy

1. **Consider connection "ready" immediately after `initialize` response**
2. **Don't wait for `initialized` notification** 
3. **Start accepting normal MCP requests right away**

### Implementation Changes

The server needs to be modified to:

1. Send the `initialize` response as normal
2. Immediately consider the handshake complete
3. Begin processing tool calls and other requests without waiting for `initialized`

### Code Changes Required

In `packages/mcp-ghcid/src/MCP/Server/GHCID.hs`:

1. **Remove dependency on `initialized` notification**
2. **Consider server "ready" after `initialize` response sent**
3. **Accept tool calls immediately after initialization**

## Impact Assessment

### Pros
- ✅ Works with buggy claude-code client
- ✅ Still maintains protocol compliance for other clients
- ✅ Minimal code changes required

### Cons  
- ⚠️  Violates strict MCP specification
- ⚠️  May not work with other compliant clients that rely on proper handshake
- ⚠️  Masks the underlying client bug

## Recommended Actions

### Short Term
1. **Implement workaround** to unblock claude-code usage
2. **Document the deviation** from MCP specification
3. **Test thoroughly** with claude-code client

### Long Term  
1. **Report bug to claude-code team** at https://github.com/anthropics/claude-code/issues
2. **Revert workaround** once claude-code is fixed
3. **Add feature flag** to enable/disable workaround mode

## Testing Strategy

1. **Verify workaround works** with claude-code client
2. **Ensure backward compatibility** with compliant MCP clients  
3. **Test tool execution** after handshake completion
4. **Validate graceful shutdown** still works properly

## Bug Report Template

When reporting to claude-code team:

```markdown
## Bug: Missing `initialized` notification in MCP handshake

**Expected behavior**: After receiving `initialize` response, client should send `initialized` notification
**Actual behavior**: Client waits indefinitely and times out after 30 seconds
**MCP Spec reference**: https://spec.modelcontextprotocol.io/specification/basic/lifecycle/
**Impact**: Cannot use compliant MCP servers with claude-code
**Workaround**: Server must skip waiting for `initialized` notification
```

---

**Date**: 2025-08-13  
**Claude Code Version**: 1.0.77  
**MCP Protocol Version**: 2025-06-18