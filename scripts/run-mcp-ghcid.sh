#!/bin/bash
set -euo pipefail

# Always resolve paths relative to the repo, not the caller's CWD.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Cache the built output in a project-local directory to avoid rebuilding on every launch.
# Avoid `.codex/` because Codex may mount it read-only for spawned tools.
CACHE_ROOT="${MCP_CACHE_DIR:-.mcp-cache}"
if [[ "$CACHE_ROOT" != /* ]]; then
  CACHE_ROOT="$REPO_ROOT/$CACHE_ROOT"
fi
CACHE_LINK="$CACHE_ROOT/mcp-ghcid"

TMP_BASE="${TMPDIR:-/tmp}"
if [[ -z "${MCP_LOG_FILE:-}" ]]; then
  LOG_DIR="$CACHE_ROOT/log"
  if /bin/mkdir -p "$LOG_DIR" 2>/dev/null; then
    export MCP_LOG_FILE="$LOG_DIR/mcp-ghcid-$$.log"
  else
    export MCP_LOG_FILE="$TMP_BASE/mcp-ghcid-$$.log"
  fi
fi
[[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-ghcid launcher: REPO_ROOT=$REPO_ROOT CACHE_LINK=$CACHE_LINK MCP_LOG_FILE=$MCP_LOG_FILE" >&2
[[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-ghcid launcher: PWD=$PWD" >&2

BIN="${MCP_GHCID_BIN:-}"
if [[ -n "$BIN" && -x "$BIN" ]]; then
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-ghcid launcher: exec MCP_GHCID_BIN=$BIN" >&2
  exec "$BIN" "$@"
fi

candidate="$CACHE_LINK/bin/mcp-ghcid"
if [[ -x "$candidate" ]]; then
  if [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]]; then
    echo "mcp-ghcid launcher: candidate=$candidate" >&2
    /bin/ls -l "$candidate" >&2 || true
  fi
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-ghcid launcher: exec $candidate" >&2
  exec "$candidate" "$@"
fi

if [[ "${MCP_AUTO_BUILD:-0}" == "1" ]]; then
  nix build --offline --no-write-lock-file "$REPO_ROOT#mcp-ghcid" -o "$CACHE_LINK" 1>&2
  [[ "${MCP_LAUNCHER_DEBUG:-0}" == "1" ]] && echo "mcp-ghcid launcher: built; exec $CACHE_LINK/bin/mcp-ghcid" >&2
  exec "$CACHE_LINK/bin/mcp-ghcid" "$@"
fi

echo "mcp-ghcid launcher: no executable found at $candidate; run: nix build .#mcp-ghcid -o $CACHE_LINK" >&2
exit 1
