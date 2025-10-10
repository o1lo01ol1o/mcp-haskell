{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv/d1388a093a7225c2abe8c244109c5a4490de4077";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    # mcp-haskell.url = "github:o1lo01ol1o/mcp-haskell/a8ad736efe9e4780d0aa119f0d3c2168ba77b597";

  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      systems,
      ...
    }@inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = forEachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

        in
        {
          devenv-up = self.devShells.${system}.default.config.procfileScript;
          devenv-test = self.devShells.${system}.default.config.test;

          # Provide mcp-ghcid as a convenient package using GHC 9.8.4
          mcp-ghcid = self.lib.mkMcpGhcid {
            inherit system;
            ghcid = pkgs.ghcid;
            shell = {
              uri = ".";
              attr = "runtime";
              extraArgs = [ "--accept-flake-config" ];
            };
          };

          mcp-obelisk = self.lib.mkMcpObelisk {
            inherit system;
            shell = {
              uri = ".";
              attr = "runtime";
              extraArgs = [ "--accept-flake-config" ];
            };
            obBinaryPath = "/Users/timpierson/.nix-profile/bin/ob";
          };

          # Make it the default package
          default = self.packages.${system}.mcp-ghcid;
        }
      );

      # Library functions for creating mcp-ghcid with custom ghcid
      lib = {
        # Main function to create mcp-ghcid with a provided ghcid derivation
        # Usage: mkMcpGhcid { system = "x86_64-linux"; ghcid = myGhcidDerivation; shell = { uri = self.outPath; attr = "default"; }; }
        mkMcpGhcid =
          { system
          , ghcid
          , shell ? null
          }:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };

            mcpOverlay = self: super: {
              mcp-sdk-hs = self.callCabal2nix "mcp-sdk-hs" (./mcp-sdk-hs) { };
              mcp-common = self.callCabal2nix "mcp-common" (./packages/mcp-common) { };
              mcp-ghcid = self.callCabal2nix "mcp-ghcid" (./packages/mcp-ghcid) { };
              mcp-obelisk = self.callCabal2nix "mcp-obelisk" (./packages/mcp-obelisk) { };
            };

            haskellPkgs = pkgs.haskell.packages.ghc948.override {
              overrides = mcpOverlay;
            };

            haskellPkg = haskellPkgs.mcp-ghcid;

            staticPkg = pkgs.haskell.lib.justStaticExecutables haskellPkg;

            realBinary = pkgs.writeShellScriptBin "mcp-ghcid-real" ''
              export PATH=${pkgs.lib.makeBinPath [ ghcid ]}:$PATH
              exec ${staticPkg}/bin/mcp-ghcid "$@"
            '';

            shellTarget = if shell == null then null else (
              let
                uri = if shell ? uri then toString shell.uri else throw "mkMcpGhcid: shell.uri required when shell is set";
                attrSuffix = if shell ? attr then "#${shell.attr}" else "";
              in "${uri}${attrSuffix}"
            );

            nixExtraArgs = if shell == null then [] else pkgs.lib.optionals (shell ? extraArgs) shell.extraArgs;

            escapedExtraArgs = if shell == null then "" else pkgs.lib.concatStringsSep " " (map pkgs.lib.escapeShellArg nixExtraArgs);
            extraArgsSegment = if shell == null || nixExtraArgs == [] then "" else "${escapedExtraArgs} ";

            launchScript = if shellTarget == null then ''
              exec ${realBinary}/bin/mcp-ghcid-real "$@"
            '' else ''
              exec ${pkgs.nix}/bin/nix ${extraArgsSegment}develop ${pkgs.lib.escapeShellArg shellTarget} --command ${realBinary}/bin/mcp-ghcid-real "$@"
            '';

          in
          pkgs.writeShellScriptBin "mcp-ghcid" launchScript;

        mkMcpObelisk =
          { system
          , shell ? null
          , obeliskCommand ? null
          , obBinaryPath ? null
          , extraRuntimePackages ? [ ]
          }:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };

            mcpOverlay = self: super: {
              mcp-sdk-hs = self.callCabal2nix "mcp-sdk-hs" (./mcp-sdk-hs) { };
              mcp-common = self.callCabal2nix "mcp-common" (./packages/mcp-common) { };
              mcp-ghcid = self.callCabal2nix "mcp-ghcid" (./packages/mcp-ghcid) { };
              mcp-obelisk = self.callCabal2nix "mcp-obelisk" (./packages/mcp-obelisk) { };
            };

            haskellPkgs = pkgs.haskell.packages.ghc948.override {
              overrides = mcpOverlay;
            };

            basePkg = haskellPkgs.mcp-obelisk;

            haskellPkg = pkgs.haskell.lib.overrideCabal basePkg (old: {
              preCheck = (old.preCheck or "") + ''
                MCP_BIN=""
                if [ -x "$PWD/dist/build/mcp-obelisk/mcp-obelisk" ]; then
                  MCP_BIN="$PWD/dist/build/mcp-obelisk/mcp-obelisk"
                elif [ -x "$PWD/dist/build/mcp-obelisk/mcp-obelisk.exe" ]; then
                  MCP_BIN="$PWD/dist/build/mcp-obelisk/mcp-obelisk.exe"
                elif [ -d dist-newstyle ]; then
                  MCP_BIN=$(find dist-newstyle -type f -name mcp-obelisk -perm -111 2>/dev/null | head -n 1 || true)
                  if [ -n "$MCP_BIN" ]; then
                    case "$MCP_BIN" in
                      /*) : ;;
                      *) MCP_BIN="$PWD/$MCP_BIN" ;;
                    esac
                  fi
                fi
                if [ -n "$MCP_BIN" ]; then
                  export MCP_OBELISK_EXECUTABLE="$MCP_BIN"
                  export MCP_OBELISK_BIN="$MCP_BIN"
                fi
              '';
            });

            staticPkg = pkgs.haskell.lib.justStaticExecutables haskellPkg;

            obPkg = if obeliskCommand != null then obeliskCommand else
              if pkgs ? "obelisk-command" then pkgs."obelisk-command"
              else if pkgs ? obelisk then pkgs.obelisk
              else throw "mkMcpObelisk: unable to locate obelisk-command in nixpkgs";

            runtimeBins =
              let baseBins = [ pkgs.nix pkgs.git ] ++ extraRuntimePackages;
              in if obBinaryPath == null then baseBins ++ [ obPkg ] else baseBins;

            obPathExport = pkgs.lib.optionalString (obBinaryPath != null) ''
              OB_BIN=${pkgs.lib.escapeShellArg obBinaryPath}
              export PATH="$(dirname "$OB_BIN"):$PATH"
            '';

            realBinary = pkgs.writeShellScriptBin "mcp-obelisk-real" ''
              export OBELISK_SKIP_UPDATE_CHECK=1
              export PATH=${pkgs.lib.makeBinPath runtimeBins}:$PATH
              ${obPathExport}
              exec ${staticPkg}/bin/mcp-obelisk "$@"
            '';

            shellTarget = if shell == null then null else (
              let
                uri = if shell ? uri then toString shell.uri else throw "mkMcpObelisk: shell.uri required when shell is set";
                attrSuffix = if shell ? attr then "#${shell.attr}" else "";
              in "${uri}${attrSuffix}"
            );

            nixExtraArgs = if shell == null then [] else pkgs.lib.optionals (shell ? extraArgs) shell.extraArgs;

            escapedExtraArgs = if shell == null then "" else pkgs.lib.concatStringsSep " " (map pkgs.lib.escapeShellArg nixExtraArgs);
            extraArgsSegment = if shell == null || nixExtraArgs == [] then "" else "${escapedExtraArgs} ";

            launchScript = if shellTarget == null then ''
              exec ${realBinary}/bin/mcp-obelisk-real "$@"
            '' else ''
              exec ${pkgs.nix}/bin/nix ${extraArgsSegment}develop ${pkgs.lib.escapeShellArg shellTarget} --command ${realBinary}/bin/mcp-obelisk-real "$@"
            '';

          in
          pkgs.writeShellScriptBin "mcp-obelisk" launchScript;
      };

      devShells = forEachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              {
                # Per https://devenv.sh/guides/using-with-flakes/
                devenv.root = toString ./.;

                # https://devenv.sh/reference/options/
                languages.haskell = {
                  enable = true;
                  package = pkgs.haskell.packages.ghc948.ghc;
                };
                languages.nix.enable = true;

                packages = [
                  pkgs.hello
                  pkgs.zlib
                  pkgs.ghcid
                  # Add mcp-ghcid using the same GHC version as our Haskell development
                  (self.lib.mkMcpGhcid {
                    inherit system;
                    ghcid = pkgs.ghcid;
                    shell = {
                      uri = self.outPath;
                      attr = "devShell";
                      extraArgs = [ "--accept-flake-config" ];
                    };
                  })
                ];

                claude.code = {
                  enable = true;
                  model = "claude-sonnet-4-20250514";

                  agents.haskell-expert = {
                    description = "Expert Haskeller who fixes issues upstream and prefers correct-by-construction representations";
                    proactive = true;
                    tools = [ "*" ]; # All tools allowed
                    prompt = ''
                      You are an expert Haskell developer with deep knowledge of functional programming principles, type systems, and best practices.

                      ## Core Principles:
                      - **Fix issues as far upstream as possible**: Address root causes rather than symptoms
                      - **Prefer Grep for searching**: Use Grep tool over other search methods when looking for code patterns
                      - **Correct-by-construction**: Design types and data structures that make invalid states unrepresentable
                      - **Parse, don't validate**: Transform data at API boundaries into well-typed representations rather than validating existing structures

                      ## Technical Approach:
                      - Use precise types (newtypes, GADTs, phantom types) to encode invariants
                      - Leverage the type system to prevent bugs at compile time
                      - Design APIs that make misuse difficult or impossible
                      - Prefer total functions and explicit error handling
                      - Use smart constructors to maintain data integrity
                      - Apply domain-driven design with strong typing

                      ## Problem-Solving Strategy:
                      1. **Root cause analysis**: Trace problems to their source
                      2. **Type-driven development**: Let types guide implementation
                      3. **Systematic refactoring**: Improve code structure while maintaining correctness
                      4. **Performance through correctness**: Well-typed code is often more efficient

                      ## Reflective Development Process:
                      - **CRITICAL**: Use the `mcp__sequentialthinking__sequentialthinking` tool liberally for ANY code edit
                      - Before implementing any changes, reflect systematically on:
                        - **Code Quality**: Type safety, readability, and maintainability
                        - **Design Patterns**: Appropriate use of functional patterns and abstractions
                        - **Generalizability**: How well the solution extends to related problems
                        - **Performance**: Time/space complexity and lazy evaluation strategies
                        - **Laziness**: Proper use of Haskell's lazy evaluation for efficiency
                        - **Robustness**: Error handling, edge cases, and failure modes
                        - **Maintainability**: Long-term code evolution and debugging ease
                        - **Denotational Correctness**: Mathematical soundness and semantic clarity
                      - Use sequential thinking to explore trade-offs and validate design decisions
                      - Question initial approaches and consider alternative implementations

                      ## Code Quality:
                      - Write self-documenting code through expressive types
                      - Minimize partial functions and runtime errors
                      - Use functional patterns (monads, applicatives, etc.) appropriately
                      - Maintain clear separation of pure and impure code

                      Always explain your reasoning and show how the proposed changes improve correctness, maintainability, or performance.
                    '';
                  };

                };

                enterShell = ''
                  hello
                '';

                processes.hello.exec = "hello";
              }
            ];
          };

          runtime = pkgs.mkShell {
            packages =
              let
                obeliskPkg =
                  if pkgs ? "obelisk-command" then pkgs."obelisk-command"
                  else if pkgs ? obelisk then pkgs.obelisk
                  else throw "runtime shell: obelisk-command not found";
              in [
                pkgs.ghcid
                pkgs.cabal-install
                pkgs.nix
                pkgs.haskell.packages.ghc948.ghc
                obeliskPkg
              ];
          };
        }
      );
    };
}
