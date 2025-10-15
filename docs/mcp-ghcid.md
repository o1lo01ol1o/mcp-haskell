# mcp-ghcid Obelisk Integration

This document explains how the `mcp-hls` repository exposes the `mcp-ghcid` MCP server after the split between `default.nix` and `flake.nix`. The goal is to keep the Obelisk project self-contained while still publishing convenient flake outputs for editors and tooling.

## Integration Overview

- `default.nix` is once again the canonical Obelisk entry point. It imports `.obelisk/impl`, defines the project packages and shells, and exports those results—nothing more.
- `flake.nix` mirrors the legacy behaviour. It imports `default.nix` for each supported system, wires in the upstream `mcp-haskell` flake, and calls `mkMcpGhcid` to build the MCP server and related tools.
- The flake redistributes those derivations as packages, apps, and dev shells so commands like `nix run .#mcp-ghcid` work out of the box.

> **Important:** Downstream Obelisk projects should import `default.nix`, not the flake output that exposes `mcp-ghcid`. The flake is a convenience layer around this repository; other projects should reuse the project definition exported by `default.nix` and provide their own MCP wiring if needed.

## `default.nix` responsibilities

`default.nix` now focuses exclusively on the Obelisk project:

- Imports `.obelisk/impl` and constructs the `project` attribute with the usual overrides.
- Exposes the project’s `packages`, `shells`, and other Obelisk-specific entry points.
- Does **not** fetch or build `mcp-ghcid`, nor does it call `builtins.getFlake`. The file stays pure so downstream projects can include it without pulling additional infrastructure into their evaluation.

This separation keeps the Obelisk build graph isolated, avoids implicit network fetches, and lets downstream users depend on the project definition without inheriting the MCP server by default.

## `flake.nix` responsibilities

`flake.nix` reintroduces the MCP integration on top of the clean `default.nix` interface.

### Importing the project once

For each supported system the flake:

1. Imports its pinned `nixpkgs` to get the local toolchain.
2. Imports `./default.nix` (passing the current system) to obtain the `project` attribute and its shells.
3. Pulls in the upstream `mcp-haskell` flake via the `mcp-haskell` input.

This ensures all project-specific knowledge lives in one place—`default.nix`—while the flake simply borrows those results.

### Constructing `mcp-ghcid`

The flake defines helpers such as `lib.mkMcpGhcid` that wrap the upstream `mcp-haskell` API. A typical call looks like:

```nix
self.lib.mkMcpGhcid {
  inherit system;
  ghcid = pkgs.ghcid;
  shell = self.lib.shellSpec.fromFlake {
    # Resolve to a store path so Codex sandboxes can still enter the shell.
    uri = self.outPath;
    attr = "devShells.${system}.ob";
    extraArgs = [ "--impure" "--accept-flake-config" ];
  };
}
```

Key points:

- The project’s dev shell provides the authoritative GHC and supporting tools, and the wrapper now enters that shell with `nix develop` before invoking `mcp-ghcid-real`.
- Because the flake encodes the shell entrypoint as a store path, remote runners (e.g. Codex sandboxes) can still discover and enter the correct environment.
- Adjust the `attr` and `extraArgs` to match the shells your Obelisk project exports; the important part is reusing the same toolchain.
- Outputs are re-exported under `packages`, `apps`, and `devShells`, with `mcp-ghcid` usually set as the default package/app for convenience.

### Shell specifications

`mkMcpGhcid` and `mkMcpHls` accept a structured `shell` value that tells the wrapper how to prepare its environment before launching the real binary. The helper constructors under `lib.shellSpec` build those values for the three supported shell types:

| Constructor | When to use | Keys |
|-------------|-------------|------|
| `lib.shellSpec.fromFlake` | Your project exposes a flake-based dev shell (e.g. `nix develop`, `devenv`, `flake-parts`). | `uri` (flake or path), optional `attr`, optional `extraArgs`, optional `env`. |
| `lib.shellSpec.fromNixExpression` | You rely on `shell.nix`, `default.nix`, or any legacy `nix-shell` expression. | `expression` (path or inline string), optional `attr`, optional `extraArgs`, optional `env`. |
| `lib.shellSpec.simple` | You only need to extend `PATH`, export environment variables, or run a few shell commands without entering a nix shell. | `packages`, `env`, `commands`. |

Each constructor returns an attribute set tagged with `type = "flake" | "nix-shell" | "simple"`. If you need more control, you can assemble that set manually using the same keys—`mkMcpGhcid` normalises both `type` and the legacy `kind` spelling.

#### Flake-based shells (`nix develop`, `devenv`, flake-parts)

1. Point `uri` at the flake that defines the development shell. You can pass `self` or `self.outPath` when the shell lives in the same repository; `lib.mkMcpGhcid` resolves the path into the store so remote runners stay hermetic.
2. Provide the attribute selector via `attr`. For devenv/flake-parts the attribute often looks like `devShells.${system}.default`.
3. Add any flags you normally pass to `nix develop` (e.g. `--impure`, `--accept-flake-config`) using `extraArgs`.
4. Optionally set `env` to export additional variables once the shell is active.

```nix
mcp-hls.lib.mkMcpGhcid {
  inherit system;
  ghcid = pkgs.ghcid;
  shell = mcp-hls.lib.shellSpec.fromFlake {
    uri = self.outPath;
    attr = "devShells.${system}.default";
    extraArgs = [ "--impure" "--accept-flake-config" ];
    env = { CODEX_HOME = ".codex"; };
  };
}
```

The generated wrapper adds `nix` to `PATH`, runs `nix develop` with the supplied arguments, and executes `mcp-ghcid-real` inside that shell. Any inline `env` values are exported before the `nix develop` invocation.

#### Legacy `nix-shell` (`shell.nix`, `default.nix`, inline expressions)

1. Set `expression` to the path of your `shell.nix` / `default.nix`, or to a string if you use `nix-shell -E`.
2. Provide `attr` when the expression exposes multiple shells and you need a specific attribute.
3. Forward extra flags (e.g. `--argstr`, `--arg`) with `extraArgs`.
4. Optional `env` entries export variables directly before `nix-shell` executes.

```nix
mcp-hls.lib.mkMcpGhcid {
  inherit system;
  ghcid = pkgs.ghcid;
  shell = mcp-hls.lib.shellSpec.fromNixExpression {
    expression = ./shell.nix;
    extraArgs = [ "--argstr" "profile" "dev" ];
    env = { NIX_PATH = "nixpkgs=/path/to/pin"; };
  };
}
```

The wrapper adds `nix` to `PATH`, runs `nix-shell <expression> ... --command mcp-ghcid-real "$@"`, and ensures the command works even when executed from remote sandboxes.

#### Simple PATH tweaks

Use `shellSpec.simple` when the project doesn’t expose a nix shell and you only need to prepend derivations to `PATH`, export environment variables, or run pre-flight shell snippets.

```nix
mcp-hls.lib.mkMcpGhcid {
  inherit system;
  ghcid = pkgs.ghcid;
  shell = mcp-hls.lib.shellSpec.simple {
    packages = [
      pkgs.haskell.packages.ghc948.ghc
      pkgs.cabal-install
    ];
    env = {
      GHCUP_USE_XDG = "1";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    };
    commands = [
      "test -f .envrc && source .envrc"
    ];
  };
}
```

In this mode the wrapper skips `nix develop`/`nix-shell`, exports the environment variables, runs each `commands` entry, and finally calls `mcp-ghcid-real` with the adjusted `PATH`.

### Dev shell wiring

The flake also sets the default `devShell` to the plain Obelisk shell from `default.nix`. This keeps local development focused on the same environment whether it is entered through `nix develop`, Obelisk tooling, or the MCP integrations.

### Constructing `mcp-hls`

`lib.mkMcpHls` mirrors the `mkMcpGhcid` helper but accepts a `haskell-language-server` (or wrapper) derivation instead of `ghcid`. This keeps the MCP launch script, runtime inputs, and shell customisation logic identical across both servers:

```nix
self.lib.mkMcpHls {
  inherit system;
  hls = pkgs.haskell-language-server;
  shell = self.lib.shellSpec.simple {
    packages = [
      pkgs.cabal-install
      pkgs.nix
      pkgs.haskell.packages.ghc948.ghc
    ];
    env = {
      HLS_LOG_LEVEL = "info";
    };
    commands = [
      "echo \"Preparing HLS runtime\""
    ];
  };
}
```

The helper injects the project’s `mcp-hls` executable, adds the supplied HLS derivation to `PATH`, and honours the optional shell `env`/`commands` entries when using `shellSpec.simple`. Swap in `shellSpec.fromFlake` or `shellSpec.fromNixExpression` if HLS should run inside the project’s development shell.

> **Note:** Until the HLS end-to-end suite is green, `mkMcpHls` disables Cabal checks for the `mcp-hls` package to avoid failing builds. Re-enable the checks by calling `lib.mkMcpServer` directly once the tests are stable.

## Guidance for downstream Obelisk projects

- Import `default.nix` to obtain the Obelisk project definition. This file is intentionally free of MCP-specific logic so it can be vendored or overridden without pulling in extra dependencies.
- Avoid importing this repository’s `flake.nix` just to obtain the `mcp-ghcid` attribute. Doing so would reintroduce the tight coupling we removed and would make your project depend on the MCP wiring decisions made here.
- If you need `mcp-ghcid` in your own flake, copy the pattern: import your project’s `default.nix`, bring in `mcp-haskell`, and call `mkMcpGhcid` with the packages from your Obelisk shell.

## `.codex` integration

`.codex/config.toml` continues to launch the MCP servers through the flake apps:

```toml
[mcp_servers.ghcid]
command = "nix"
args = ["run", ".#mcp-ghcid"]

[mcp_servers.hls]
command = "nix"
args = ["run", ".#mcp-hls"]
```

Because the flake already includes the Obelisk toolchain, Codex shares the exact dependency stack used by manual invocations (`nix run .#mcp-ghcid`). Remember to set `CODEX_HOME=.codex` in your shell so Codex stores its state locally, and ensure `.codex/*` (except `config.toml`) is ignored by Git.

## Updating upstream revisions

When `mcp-haskell` publishes a new revision:

1. Run `nix flake update --update-input mcp-haskell` (or update the lock file for the relevant input).
2. Verify that `lib.mkMcpGhcid` still matches the upstream API; adjust helper arguments if the contract changes.
3. Confirm the Obelisk project shell still exports the expected `GHC`, `cabal-install`, and `ghcid` packages.
4. Run `nix run .#mcp-ghcid -- --version` (or similar) to verify the server builds and launches.

## Summary

- `default.nix` exports the pure Obelisk project without any MCP hooks.
- `flake.nix` imports that project, wires in the upstream `mcp-haskell` flake, and publishes both `mcp-ghcid` and `mcp-hls` as packages, apps, and dev shells.
- Downstream Obelisk projects should depend on `default.nix` and reproduce the MCP wiring themselves if required, rather than importing this repository’s flake.
