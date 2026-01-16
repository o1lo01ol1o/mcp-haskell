#!/bin/bash
set -euo pipefail

# Always resolve paths relative to the repo, not the caller's CWD.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Cache the built output in temp by default to avoid touching the repo.
# Override with MCP_CACHE_DIR (absolute paths recommended).
TMP_BASE="${TMPDIR:-/tmp}"
CACHE_ROOT="${MCP_CACHE_DIR:-$TMP_BASE/mcp-cache}"
if [[ "$CACHE_ROOT" != /* ]]; then
  CACHE_ROOT="$TMP_BASE/$CACHE_ROOT"
fi
CACHE_LINK="$CACHE_ROOT/mcp-hls"

if [[ -z "${MCP_LOG_FILE:-}" ]]; then
  LOG_DIR="$CACHE_ROOT/log"
  if /bin/mkdir -p "$LOG_DIR" 2>/dev/null; then
    export MCP_LOG_FILE="$LOG_DIR/mcp-hls-$$.log"
  else
    export MCP_LOG_FILE="$TMP_BASE/mcp-hls-$$.log"
  fi
fi
[[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-hls launcher: REPO_ROOT=$REPO_ROOT CACHE_LINK=$CACHE_LINK MCP_LOG_FILE=$MCP_LOG_FILE" >&2

BIN="${MCP_HLS_BIN:-}"
if [[ -n "$BIN" && -x "$BIN" ]]; then
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-hls launcher: exec MCP_HLS_BIN=$BIN" >&2
  exec "$BIN" "$@"
fi

candidate="$CACHE_LINK/bin/mcp-hls"
if [[ -x "$candidate" ]]; then
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-hls launcher: exec $candidate" >&2
  exec "$candidate" "$@"
fi

if [[ "${MCP_AUTO_BUILD:-0}" == "1" ]]; then
  nix build --offline --no-write-lock-file "$REPO_ROOT#mcp-hls" -o "$CACHE_LINK" 1>&2
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-hls launcher: built; exec $CACHE_LINK/bin/mcp-hls" >&2
  exec "$CACHE_LINK/bin/mcp-hls" "$@"
fi

echo "mcp-hls launcher: no executable found at $candidate; run: nix build .#mcp-hls -o $CACHE_LINK" >&2
exit 1
