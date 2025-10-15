# MCP-HLS: Model Context Protocol for Haskell Development

A production-ready implementation of Model Context Protocol (MCP) servers for Haskell development tools, providing seamless integration between AI assistants and Haskell development workflows.

## Overview

This project provides two main MCP servers:

- **mcp-hls**: Integration with Haskell Language Server (HLS)
- **mcp-ghcid**: Integration with GHCID for continuous compilation

## Quick Start

### Building with Nix

**Important**: This flake requires you to provide a `ghcid` derivation that uses the same GHC version as your project. This ensures compatibility between the MCP server and the Haskell project it's monitoring.

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
        # Create mcp-ghcid with your project's ghcid
        mcp-ghcid = mcp-hls.lib.mkMcpGhcid {
          inherit system;
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
# Build your mcp-ghcid
nix build .#mcp-ghcid

# Run it
./result/bin/mcp-ghcid --help

# Or install to profile
nix profile install .#mcp-ghcid
```

## Packages

### mcp-ghcid

A static executable that provides MCP integration for GHCID (GHCi daemon).

**Features:**
- Start, stop, and monitor GHCID processes
- Real-time compilation feedback
- Compiler error and warning reporting
- Process management with graceful shutdown
- Comprehensive filtering and message formatting (`output` + `lines` are returned)

**Available MCP Tools:**
- `ghcid-start` – Start a new ghcid process for a project (automatically runs `cabal repl <package>` based on the supplied `.cabal` file or directory; override via `options.command` if you need something custom, or pass `component` to target a specific Cabal component)
- `ghcid-stop` – Stop a running ghcid process  
- `ghcid-restart` – Restart a ghcid process
- `ghcid-status` – Get status of a ghcid process
- `ghcid-messages` – Get compiler messages from ghcid (supports filtering)
- `ghcid-clear` – Clear messages from a ghcid process
- `ghcid-list` – List all active ghcid processes

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

**Important**: This flake does NOT provide pre-built packages. You must use the library function with your own `ghcid` derivation to ensure GHC version compatibility.

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

Uses MCP Protocol v2025-03-26 with full support for:
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
