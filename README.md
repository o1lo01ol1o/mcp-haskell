# MCP-HLS: Model Context Protocol for Haskell Development

A production-ready implementation of Model Context Protocol (MCP) servers for Haskell development tools, providing seamless integration between AI assistants and Haskell development workflows.

## Overview

This project provides two main MCP servers:

- **mcp-hls**: Integration with Haskell Language Server (HLS)
- **mcp-ghcid**: Integration with GHCID for continuous compilation

## Quick Start

### Building with Nix

**Important**: In downstream projects, prefer building `mcp-ghcid` with the same Haskell toolchain as your project (i.e. provide the `ghcid` derivation from your project’s package set). This ensures ghcid sees the same package database and GHC version as your workspace.

#### Basic Usage

```nix
# In your project's flake.nix
{
  inputs = {
    mcp-hls.url = "github:your-org/mcp-hls";  # or path to this repo
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, mcp-hls }: 
    let
      system = "x86_64-linux";  # or your system
      pkgs = import nixpkgs { inherit system; };
      
      # Use the SAME haskell package set as your project
      haskellPackages = pkgs.haskell.packages.ghc948;  # match your GHC version
      
      # Create ghcid from the same package set
      my-ghcid = haskellPackages.ghcid;
      
    in {
      packages.${system} = {
        # If you launch via `nix develop ... --command mcp-ghcid`, prefer the binary-only output.
        mcp-ghcid-bin = mcp-hls.lib.mkMcpGhcidBinary {
          inherit system;
          inherit pkgs;
          ghcPackageSet = haskellPackages;
        };

        # Or: create a wrapper that injects your project's `ghcid` onto PATH
        mcp-ghcid = mcp-hls.lib.mkMcpGhcid {
          inherit system;
          inherit pkgs;
          ghcPackageSet = haskellPackages;
          ghcid = my-ghcid;
        };
      };
    };
}
```

#### Advanced Example with Custom GHC

```nix
# For projects using a specific GHC version
{
  inputs = {
    mcp-hls.url = "github:your-org/mcp-hls";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, mcp-hls }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      
      # Match your project's exact Haskell setup
      myHaskellPackages = pkgs.haskell.packages.ghc966;  # or ghc948, etc.
      my-ghcid = myHaskellPackages.ghcid;
      
    in {
      packages.${system} = {
        mcp-ghcid = mcp-hls.lib.mkMcpGhcid {
          inherit system;
          inherit pkgs;
          ghcPackageSet = myHaskellPackages;
          ghcid = my-ghcid;
        };
        
        # Make it your default package
        default = self.packages.${system}.mcp-ghcid;
      };
      
      # Development shell with mcp-ghcid available
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ self.packages.${system}.mcp-ghcid ];
      };
    };
}
```

#### Running

```bash
# Build mcp-ghcid (this repo)
CACHE_ROOT="${MCP_CACHE_DIR:-${TMPDIR:-/tmp}/mcp-cache}"
nix build .#mcp-ghcid -o "$CACHE_ROOT/mcp-ghcid"

# Run it
"$CACHE_ROOT/mcp-ghcid/bin/mcp-ghcid" --help

# Or install to profile
nix profile install .#mcp-ghcid
```

### Codex / MCP Clients (stdio)

`mcp-ghcid` and `mcp-hls` support both common stdio framings:

- `Content-Length: ...\r\n\r\n{...}` (the MCP spec framing)
- newline-delimited JSON `{...}\n` (used by recent Codex CLI versions for MCP stdio)

If an MCP client reports a “handshake timed out” but the server starts instantly, double-check the framing expectations on both sides.

### Logging (configurable)

Logging is controlled by a combination of CLI flags and environment variables, and is intended to be set by the consuming project:

- `--log-level debug|info|warn|error` (recommended default: `warn`)
- `MCP_LOG_FILE=/path/to/log` (set to `/dev/null` to effectively disable file logging)
- `MCP_LOG_STDERR=1` to mirror logs to stderr (useful when you want logs surfaced in a parent process like Codex)

This repository’s launch scripts (`scripts/run-mcp-ghcid.sh`, `scripts/run-mcp-hls.sh`) only set `MCP_LOG_FILE` if it is not already set, so downstream wrappers can override it.

When running over MCP stdio, logs must not be written to stdout (stdout is reserved for JSON-RPC); use file logging and/or stderr mirroring instead.

## Packages

### mcp-ghcid

A static executable that provides MCP integration for GHCID (GHCi daemon).

**Features:**
- Start, stop, and monitor GHCID processes
- Real-time compilation feedback
- Compiler error and warning reporting
- Process management with graceful shutdown
- Comprehensive filtering and message formatting (`output` + `lines` are returned)
- Default `cabal repl` invocations use a per-project `--builddir` under `$TMPDIR/mcp-cache/cabal-build/` (or `MCP_CACHE_DIR`) to avoid `dist-newstyle` races when multiple sessions run concurrently in a monorepo

**Available MCP Tools:**
- `ghcid-start` – Start a new ghcid process for a project (automatically runs `cabal repl <package>` based on the supplied `.cabal` file or directory; override via `options.command` if you need something custom, or pass `component` to target a specific Cabal component)
- `ghcid-stop` – Stop a running ghcid process  
- `ghcid-restart` – Restart a ghcid process
- `ghcid-status` – Get status of a ghcid process
- `ghcid-messages` – Get compiler messages from ghcid (supports filtering)
- `ghcid-clear` – Clear messages from a ghcid process
- `ghcid-list` – List all active ghcid processes

**Cache location and `workDir`**

`ghcid-start`/`ghcid-restart` create Cabal build directories under a deterministic temp path by default.

- For the default `cabal repl` command (i.e. when you do *not* set `options.command`), `mcp-ghcid` injects `--builddir $TMPDIR/mcp-cache/cabal-build/...` (or `MCP_CACHE_DIR`) and creates that directory.
- This cache location is independent of `workDir`, which still controls the ghcid process working directory.
- To prevent `mcp-ghcid` from creating the default temp build directory, set `options.command` to include an explicit `--builddir` (or use a non-`cabal repl` command entirely).

**Message filtering**

`ghcid-messages` accepts an optional `filter` object allowing servers or clients to focus on relevant diagnostics.

When starting, include an optional `component` field to target a specific Cabal component (for example `"component": "lib:mcp-common"`). If omitted, `mcp-ghcid` will automatically load the primary library component of the `.cabal` file. Output now includes both stdout and stderr (stderr lines are prefixed with `[stderr]`).

```jsonc
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "ghcid-messages",
    "arguments": {
      "cabalURI": "file:///path/to/project",
      "count": 80,
      "filter": { "grep": "All good" }
    }
  }
}
```

Supported filter keys: `grep`, `head`, `tail`, `lines` (exactly one per request). Responses include `"output"` (full text) and `"lines"` (array of individual lines) so downstream tools can display or post-process easily.

#### Using mcp-ghcid in downstream projects

- See the [Obelisk-focused integration guide](docs/mcp-ghcid.md) for a full example of wiring `mcp-ghcid` into an existing project that reuses its own toolchain.

- **Expose a package**: add `mcp-hls` as a flake input and call `lib.mkMcpGhcid` with the `ghcid` derivation your project already uses. This keeps ghcid and your build on the identical GHC toolchain while always building the server with the nixpkgs pin from `mcp-hls`.

  ```nix
  {
    inputs = {
      mcp-hls.url = "github:your-org/mcp-hls";
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    outputs = { self, nixpkgs, mcp-hls }:
      let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc948; # match your toolchain
      in {
        packages.${system}.mcp-ghcid =
          mcp-hls.lib.mkMcpGhcid {
            inherit system;
            ghcid = haskellPackages.ghcid;
            # Optional: extend the runtime environment for the server
            shell = mcp-hls.lib.shellSpec.simple {
              packages = [
                haskellPackages.ghc
                pkgs.cabal-install
                pkgs.nix
              ];
              env = {
                GHCUP_USE_XDG = "1";
              };
            };
          };
      };
  }
  ```
  The helper always imports the `nixpkgs` pin from `mcp-hls`. When you use `shellSpec.simple`, the wrapper prepends the listed packages to `PATH`, exports any environment variables, runs optional `commands`, and finally starts the binary.
  Prefer `shellSpec.fromFlake` or `shellSpec.fromNixExpression` when your project already exposes a development shell—`mkMcpGhcid` will launch that shell with `nix develop`/`nix-shell` before invoking `mcp-ghcid-real`, so tools like Cabal or GHC see the same package DB they would inside your normal dev environment.

- **Choose a shell spec**: Pick the constructor that matches how your project defines its dev environment.
  - `lib.shellSpec.fromFlake` runs `nix develop` (or devenv, etc.) with the provided flake URI/attr/extra args.
  - `lib.shellSpec.fromNixExpression` runs `nix-shell` for legacy `default.nix` / `shell.nix` setups.
  - `lib.shellSpec.simple` keeps the original lightweight behaviour—extend `PATH`, set env vars, optionally run shell snippets.
  See the [shell specification guide](docs/mcp-ghcid.md#shell-specifications) for detailed walkthroughs and additional options.

- **Run the server**: build or run the package with `nix build .#mcp-ghcid` or `nix run .#mcp-ghcid -- --help`. The wrapper script (`mkMcpGhcid`) injects the provided `ghcid` onto `PATH` and, depending on the `shell` spec, either layers in simple PATH tweaks or spawns your project’s development shell before launching `mcp-ghcid-real`.
- **Connect an MCP client**: configure your MCP tooling to launch the flake attribute. For example, a `.codex/config.toml` entry can mirror the one used in this repository:

  ```toml
  [mcp_servers.ghcid]
  command = "nix"
  args = ["run", "/path/to/your/project#mcp-ghcid", "--", "--log-level", "debug"]
  ```

- **Codex via `nix develop` (common pattern)**: if your project already has a Nix shell that provides `mcp-ghcid` on `PATH`, Codex can start it via `nix develop --command`:

  ```toml
  [mcp_servers.ghcid]
  startup_timeout_sec = 60
  command = "nix"
  args = [
    "develop",
    "-f",
    "./nix/shell-codex.nix",
    "--command",
    "mcp-ghcid",
    "--log-level",
    "warn",
  ]

  [mcp_servers.ghcid.env]
  # Optional: override logging destination (or set to /dev/null to disable)
  MCP_LOG_FILE = "/tmp/mcp-ghcid.log"
  # Optional: mirror logs into Codex's own log (debugging only)
  # MCP_LOG_STDERR = "1"
  ```

  This pattern works with recent Codex CLI versions because the servers support both newline-delimited JSON and `Content-Length` stdio framing.

- **Codex via `direnv exec` (recommended when Codex is sandboxed)**: some Codex sandbox policies prevent writes to `$HOME`, which can break `cabal`/`ghcid` (and can also make `nix develop` fail if Nix needs to write caches). If your repo already uses `direnv` + `use flake`, prefer launching via `direnv exec` so the server inherits the fully-materialized dev environment; if you need caches inside the workspace, set `MCP_CACHE_DIR` to a workspace path.

  ```toml
  [mcp_servers.ghcid]
  command = "direnv"
  args = ["exec", ".", "mcp-ghcid", "--log-level", "warn"]

  [mcp_servers.ghcid.env]
  CABAL_DIR = ".cabal"
  # Optional: keep ghcid's build cache inside the workspace instead of $TMPDIR
  # MCP_CACHE_DIR = ".codex/cache"
  ```

- **Customize behaviour**: supply a JSON config via `--config path/to/config.json` to override defaults (e.g. `instructionsMessage`, `retentionPolicy`, `maxConcurrentProcesses`). The built-in auto-discovery falls back to the current working directory when no config file is provided.

### mcp-hls

MCP integration for Haskell Language Server.

**Features:**
- Language server management
- Diagnostic reporting
- Code actions and refactoring
- Documentation and hover information

## Architecture

The project uses a multi-package Cabal structure:

```
packages/
├── mcp-common/     # Shared MCP protocol types and utilities
├── mcp-ghcid/      # GHCID integration
└── mcp-hls/        # HLS integration
```

### Key Features

- **Production Ready**: Comprehensive error handling, logging, and graceful shutdown
- **Type Safe**: GADT-based request/response matching for compile-time safety
- **Concurrent**: STM-based process registries for safe concurrent operations
- **Resource Management**: Bracket patterns for exception-safe resource cleanup
- **Signal Handling**: Proper SIGTERM/SIGINT handling for graceful shutdown
- **Comprehensive Testing**: Full test suite with 32+ tests covering all components

## Nix Flake Outputs

### Library Functions

- `lib.mkMcpGhcid`: Function to create mcp-ghcid with provided ghcid
  ```nix
  mkMcpGhcid :: {
    system :: String,
    ghcid :: Derivation,
    shell ? {
      packages ? [ Derivation ],
      env ? AttrSet,
      commands ? [ String ]
    }
  } -> Derivation
  ```

This flake also exports packages for convenience (e.g. `.#mcp-ghcid`), but those builds are tied to this repository’s pinned `nixpkgs` and chosen GHC set. For downstream projects, prefer using the library functions and supply your project’s `ghcid` derivation.

### Why No Default Packages?

The mcp-ghcid server needs to use the exact same GHC toolchain as your Haskell project. If we provided a default package, it might use a different GHC version, leading to:

- Incompatible package databases
- Version mismatches between project dependencies and ghcid dependencies  
- Runtime errors when ghcid tries to load your project

By requiring you to provide the `ghcid` derivation, we ensure perfect compatibility.

### Example Integration

Create a `flake.nix` in your Haskell project:

```nix
{
  inputs.mcp-hls.url = "github:your-org/mcp-hls";
  
  outputs = { self, nixpkgs, mcp-hls }: {
    packages.x86_64-linux.mcp-ghcid = 
      mcp-hls.lib.mkMcpGhcid {
        system = "x86_64-linux";
        ghcid = nixpkgs.legacyPackages.x86_64-linux.haskell.packages.ghc948.ghcid;
      };
  };
}
```

## Development

### Building from Source

```bash
# Enter development shell
nix develop

# Build all packages
cabal build all

# Run tests
cabal test all

# Run specific package tests
cabal test mcp-ghcid
```

### Testing

The project includes comprehensive tests:

- Unit tests for all major components
- Integration tests for MCP protocol handling
- Concurrency tests for process management
- Resource leak tests
- End-to-end pipeline tests

All tests pass with 32/32 success rate.

## Configuration

### mcp-ghcid Configuration

Configuration files should be in JSON format:

```json
{
  "ghcid": {
    "command": "ghcid",
    "defaultArgs": ["--command", "cabal repl"],
    "timeout": 30
  },
  "server": {
    "logLevel": "info",
    "maxProcesses": 10
  }
}
```

## License

MIT License. See LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `cabal test all`
5. Submit a pull request

## Architecture Notes

### MCP Protocol Version

Uses MCP Protocol v2025-06-18 with full support for:
- Tools capability with list and execute operations
- Resources capability with list and read operations  
- Proper JSON-RPC 2.0 message handling
- Error handling with appropriate error codes

### Type Safety

The implementation uses GADTs for request/response type safety:

```haskell
data GHCIDRequest a where
  StartGHCID :: StartGHCIDData -> GHCIDRequest StartGHCIDResult
  StopGHCID :: StopGHCIDData -> GHCIDRequest StopGHCIDResult
  -- ... other requests
```

This ensures that responses are correctly typed at compile time.

### Concurrency Model

- STM-based shared state management
- Process registries with atomic operations
- Resource-safe concurrent operations
- Graceful shutdown coordination

### Error Handling

- Comprehensive error types for all failure modes
- Graceful degradation when external tools unavailable
- Proper resource cleanup on errors
- Detailed error reporting through MCP protocol
