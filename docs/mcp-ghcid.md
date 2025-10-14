# mcp-ghcid Obelisk Integration

This document explains how the Plast Obelisk project layers the upstream [`mcp-haskell`](https://github.com/o1lo01ol1o/mcp-haskell) flake into its workflow so that the `mcp-ghcid` MCP server is available to local tooling and `.codex`. The integration is designed to reuse the existing Obelisk toolchain, keeping the server aligned with the same GHC, `ghcid`, and supporting packages that the project already relies on.

At a high level the setup is powered by three parts:

1. A flake shim in `nix/flake.nix` that exposes `mcp-ghcid` based on the legacy `default.nix`.
2. Additional logic in `default.nix` that imports the upstream flake, pulls in Obelisk’s packages, and adapts to the latest `mkMcpGhcid` API.
3. A `.codex` entry that launches the flake output so editors can start the MCP server automatically.

## Flake Shim (`nix/flake.nix`)

The repository advertises `mcp-ghcid` through a lightweight shim at `nix/flake.nix`. The shim evaluates the existing `default.nix` for each supported system and mirrors those results into the flake’s packages, apps, and dev shells:

```nix
{
  description = "Plast flake shim exposing mcp-ghcid from default.nix";

  inputs = {
    systems.url = "github:nix-systems/default";
    mcp-haskell.url = "github:o1lo01ol1o/mcp-haskell";
  };

  outputs = { self, systems, mcp-haskell, ... }:
    let
      evaluatedSystems = map (system: {
        inherit system;
        result = builtins.tryEval (import ../default.nix {
          inherit system;
          mcpFlake = mcp-haskell;
        });
      }) (import systems);
      supportedSystems = builtins.filter (entry: entry.result.success) evaluatedSystems;
      forEachSystem = f: builtins.listToAttrs (map (entry: {
        name = entry.system;
        value = f entry.system entry.result.value;
      }) supportedSystems);
    in {
      packages = forEachSystem (_: project: {
        mcpGhcid = project.mcpGhcid;
        "mcp-ghcid" = project.mcpGhcid;
        default = project.mcpGhcid;
      });
      apps = forEachSystem (_: project: let drv = project.mcpGhcid; in {
        mcpGhcid = { type = "app"; program = "${drv}/bin/mcp-ghcid"; };
        "mcp-ghcid" = { type = "app"; program = "${drv}/bin/mcp-ghcid"; };
        default = { type = "app"; program = "${drv}/bin/mcp-ghcid"; };
      });
      devShells = forEachSystem (_: project: {
        ghc = project.shells.ghc;
        default = project.shells.ghc;
      });
    };
}
```

Key details:

- The shim delegates back to `default.nix`, which already understands how to build the Obelisk shells. The shim never evaluates the upstream flake directly.
- Passing `mcpFlake = mcp-haskell` ensures the upstream flake is reused without fetching it twice.
- The `apps` outputs mirror the `packages` so `nix run ./nix#mcp-ghcid` works immediately, without additional wrappers.

## Upstream Pinning (`default.nix`)

`default.nix` now handles importing the upstream flake, pinning its revision, and adapting Obelisk’s packages to the latest `mkMcpGhcid` API. The relevant definitions live near `default.nix:89-127`.

### Pinning the Flake

```nix
resolvedMcpFlake = if mcpFlake != null then
  mcpFlake
else
  builtins.getFlake
  "github:o1lo01ol1o/mcp-haskell/78ed451cdb265844c79a237729cb92b24b4695ce";
```

The function accepts an `mcpFlake` override (used by `nix/flake.nix`) but otherwise falls back to a locked commit hash. The same revision is mirrored in `nix/flake.lock`, keeping the project’s lock files aligned when upstream is bumped.

### Matching the Obelisk Toolchain

The latest `mkMcpGhcid` expects a `shell.packages` field. Because Obelisk already defines the authoritative toolchain, the integration reuses those packages so `mcp-ghcid` runs with the same GHC (`ghc8107` today), `ghcid`, and `cabal-install` that developers already use:

```nix
cabalInstallPkg =
  if obelisk.nixpkgs ? cabal-install then
    obelisk.nixpkgs.cabal-install
  else if obelisk.nixpkgs.haskellPackages ? cabal-install then
    obelisk.nixpkgs.haskellPackages.cabal-install
  else
    throw "mcpGhcid shim: expected cabal-install in obelisk nixpkgs";

ghcCompilerPkg =
  if obelisk.nixpkgs.haskellPackages ? ghc then
    obelisk.nixpkgs.haskellPackages.ghc
  else if obelisk.nixpkgs.haskell.compiler ? ghc8107 then
    obelisk.nixpkgs.haskell.compiler.ghc8107
  else if obelisk.nixpkgs ? ghc then
    obelisk.nixpkgs.ghc
  else
    throw "mcpGhcid shim: expected ghc compiler in obelisk nixpkgs";
```

These helpers are defensive: they look through the common attribute paths used by different Obelisk snapshots and produce a clear error if either `cabal-install` or `ghc` cannot be located.

### Constructing `mcpGhcid`

```nix
mcpGhcid = resolvedMcpFlake.lib.mkMcpGhcid {
  inherit system;
  ghcid = if obelisk.nixpkgs ? ghcid then
    obelisk.nixpkgs.ghcid
  else if obelisk.nixpkgs.haskellPackages ? ghcid then
    obelisk.nixpkgs.haskellPackages.ghcid
  else
    throw "mcpGhcid shim: expected ghcid in obelisk nixpkgs";
  shell = {
    packages = [
      ghcCompilerPkg
      cabalInstallPkg
    ];
  };
};
```

Passing the Obelisk-managed `ghcid`, `ghc`, and `cabal-install` guarantees that the server launches with the exact toolchain the project already uses. The derivation is re-exported under both `mcpGhcid` and `"mcp-ghcid"`, matching the attribute names that downstream consumers (including the flake shim) expect.

## `.codex` Integration

With the flake outputs in place, the `.codex` configuration at `.codex/config.toml` delegates server startup to `nix run`, letting Codex reuse the same entry point as manual invocations:

```toml
model = "gpt-5-codex"
model_provider = "openai"

[mcp_servers.ghcid]
command = "nix"
args = ["run", "./nix#mcp-ghcid"]
```

The server definition stays minimal on purpose—Codex appends `--` and any additional flags automatically. Because the app resolves to the same derivation exposed by `default.nix`, Codex shares the exact dependency stack used elsewhere in the project.

Set `CODEX_HOME=.codex` when running the CLI (for example `export CODEX_HOME=.codex`) so Codex stores its state alongside the repository instead of in the global user directory.

Because `.codex/` accumulates session history, logs, and credentials, ensure everything except `config.toml` is listed in `.gitignore` if it is not already:

```
.codex/*
!.codex/config.toml
```

This keeps sensitive data out of version control while preserving the shared configuration file.

## Using the Integration

1. **Update the lock file** to pull the latest upstream changes when needed:
   ```bash
   nix flake update ./nix --update-input mcp-haskell
   ```
   Running the command inside `nix/` avoids rewriting unrelated top-level flakes.
2. **Verify the build** by running the server manually:
   ```bash
   nix run ./nix#mcp-ghcid -- --log-level debug
   ```
3. **Configure local editors** to point at the `.codex` entry. Codex-aware tooling will automatically launch the MCP server via the flake app.

## Updating Upstream Revisions

When `mcp-haskell` publishes a new commit:

1. Run `nix flake lock --update-input mcp-haskell` inside `nix/`.
2. Update the hard-coded revision in `default.nix`.
3. Launch the server once to confirm the API still matches. Adjust the call to `mkMcpGhcid` if upstream changes its interface (e.g., the switch from `shell.uri`/`shell.attr` to `shell.packages`).
4. Commit the lock file, `default.nix`, and any required changes together so future updates stay synchronized.
5. After upgrading Obelisk itself, verify that `ghcCompilerPkg` and `cabalInstallPkg` still resolve; add new fallbacks if the attribute paths move.

## Summary

- `nix/flake.nix` exposes `mcp-ghcid` as packages and apps by reusing the outputs from `default.nix`, enabling `nix run ./nix#mcp-ghcid` without additional boilerplate.
- `default.nix` imports the pinned upstream flake, reuses Obelisk’s toolchain, and satisfies the current `mkMcpGhcid` contract through `shell.packages`.
- `.codex/config.toml` invokes the flake app so Codex and manual workflows rely on the same derivation, guaranteeing a consistent build and debugging experience.

With these pieces in place, all consumers—CLI, editors, or automation—launch `mcp-ghcid` through a single path that stays aligned with the project’s existing Obelisk environment.
