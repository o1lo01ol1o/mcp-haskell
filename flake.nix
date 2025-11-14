{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv/d1388a093a7225c2abe8c244109c5a4490de4077";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    codex-nix = {
      url = "github:sadjow/codex-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
      codex-nix,
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
          codex = codex-nix.packages.${system}.codex;

          # Provide mcp-ghcid as a convenient package using GHC 9.8.4
          mcp-ghcid = self.lib.mkMcpGhcid {
            inherit system;
            ghcid = pkgs.ghcid;
            shell = {
              packages = [
                pkgs.cabal-install
                pkgs.nix
                pkgs.haskell.packages.ghc948.ghc
              ];
            };
          };

          # Provide mcp-hls using the same helper pattern
          mcp-hls = self.lib.mkMcpHls {
            inherit system;
            hls = pkgs.haskell-language-server;
            shell = {
              packages = [
                pkgs.cabal-install
                pkgs.nix
                pkgs.haskell.packages.ghc948.ghc
              ];
            };
          };

          # Make it the default package
          default = self.packages.${system}.mcp-ghcid;
        }
      );

      # Library functions for creating mcp-ghcid with custom ghcid
      lib =
        let
          nixpkgsLib = inputs.nixpkgs.lib;
          mkMcpServer =
            { system
            , packageName
            , packageSrc
            , runtimeTool
            , shell ? null
            , binaryName ? packageName
            , disableChecks ? false
            }:
            let
              pkgs = import inputs.nixpkgs {
                inherit system;
                config.allowUnfree = true;
              };

              mcpOverlay = self: super: {
                mcp-sdk-hs = self.callCabal2nix "mcp-sdk-hs" (./mcp-sdk-hs) { };
                mcp-common = self.callCabal2nix "mcp-common" (./packages/mcp-common) { };
                ${packageName} =
                  let
                    base = self.callCabal2nix packageName packageSrc { };
                  in
                  if disableChecks then pkgs.haskell.lib.dontCheck base else base;
              };

              haskellPkgs = pkgs.haskell.packages.ghc948.override {
                overrides = mcpOverlay;
              };

              haskellPkg =
                if builtins.hasAttr packageName haskellPkgs then haskellPkgs.${packageName}
                else throw "mkMcpServer: expected package not found in Haskell overlay";

              staticPkg = pkgs.haskell.lib.justStaticExecutables haskellPkg;

              realBinary =
                pkgs.writeShellScriptBin "${binaryName}-real" ''
                  exec ${staticPkg}/bin/${binaryName} "$@"
                '';

              shellConfigRaw =
                if shell == null then { type = "none"; }
                else if builtins.isAttrs shell then shell
                else throw "mkMcpServer: shell must be an attribute set when provided";

              shellType =
                if shellConfigRaw ? type then shellConfigRaw.type
                else if shellConfigRaw ? kind then shellConfigRaw.kind
                else "simple";

              shellConfig = builtins.removeAttrs shellConfigRaw [ "type" "kind" ];

              toStringList = what: err:
                let
                  asList =
                    if builtins.isNull what then [ ]
                    else if builtins.isList what then what
                    else if builtins.isString what then [ what ]
                    else throw err;
                in
                builtins.map (value: toString value) asList;

              toPackageList = value:
                if builtins.isNull value then [ ]
                else if builtins.isList value then value
                else [ value ];

              shellEnv = shellConfig.env or { };

              shellPackages =
                if shellType == "simple" then toPackageList (shellConfig.packages or [ ])
                else [ ];

              shellCommands =
                if shellType == "simple" then
                  toStringList (shellConfig.commands or [ ]) "mkMcpServer: shell.commands must be a string or list of strings"
                else [ ];

              resolveFlakeUri = uri:
                let
                  uriString =
                    if builtins.isPath uri then builtins.toString uri
                    else if builtins.isAttrs uri && uri ? outPath then toString uri.outPath
                    else if builtins.isString uri then uri
                    else throw "mkMcpServer: shell.uri must be a path, flake, or string";
                  treatAsPath =
                    builtins.isPath uri
                    || nixpkgsLib.hasPrefix "/" uriString
                    || nixpkgsLib.hasPrefix "." uriString;
                in
                if treatAsPath then
                  builtins.toString (builtins.path { path = uriString; })
                else
                  uriString;

              resolveNixExpression = expression:
                let
                  rawString =
                    if builtins.isPath expression then builtins.toString expression
                    else if builtins.isAttrs expression && expression ? outPath then toString expression.outPath
                    else if builtins.isString expression then expression
                    else throw "mkMcpServer: shell.expression must be a path or string";
                  treatAsPath =
                    builtins.isPath expression
                    || nixpkgsLib.hasPrefix "/" rawString
                    || nixpkgsLib.hasPrefix "." rawString;
                in
                if treatAsPath then
                  builtins.toString (builtins.path { path = rawString; })
                else
                  rawString;

              shellExtraArgs =
                toStringList (shellConfig.extraArgs or [ ]) "mkMcpServer: shell.extraArgs must be a string or list of strings";

              runtimeToolChecked =
                if builtins.isNull runtimeTool then
                  throw "mkMcpServer: runtimeTool must be provided"
                else runtimeTool;

              needsNix = shellType == "flake" || shellType == "nix-shell";

              runtimeInputs =
                pkgs.lib.unique (
                  shellPackages
                  ++ [ runtimeToolChecked ]
                  ++ pkgs.lib.optionals needsNix [ pkgs.nix ]
                );

              envLines =
                pkgs.lib.mapAttrsToList
                  (name: value:
                    ''export ${name}=${pkgs.lib.escapeShellArg (toString value)}'')
                  shellEnv;

              shellExecLine =
                let
                  escape = pkgs.lib.escapeShellArg;
                  commandParts =
                    if shellType == "flake" then
                      let
                        baseUri =
                          if shellConfig ? uri then resolveFlakeUri shellConfig.uri
                          else throw "mkMcpServer: shell.uri required for flake shell";
                        attrValueRaw =
                          if shellConfig ? attr then
                            let
                              attr = shellConfig.attr;
                            in
                            if builtins.isString attr then attr
                            else throw "mkMcpServer: shell.attr must be a string"
                          else null;
                        attrValue =
                          if builtins.isNull attrValueRaw then null
                          else if nixpkgsLib.hasPrefix "#" attrValueRaw then builtins.substring 1 (builtins.stringLength attrValueRaw - 1) attrValueRaw
                          else attrValueRaw;
                        flakeRef =
                          if ! builtins.isNull attrValue then
                            if builtins.match ".*#.*" baseUri != null then
                              throw "mkMcpServer: shell.uri already includes a flake attribute; remove shell.attr or drop the inline #"
                            else "${baseUri}#${attrValue}"
                          else baseUri;
                      in
                      [
                        (escape "${pkgs.nix}/bin/nix")
                        (escape "develop")
                        (escape flakeRef)
                      ]
                      ++ builtins.map escape shellExtraArgs
                      ++ [ (escape "--command") (escape "${realBinary}/bin/${binaryName}-real") ]
                    else if shellType == "nix-shell" then
                      let
                        expression =
                          if shellConfig ? expression then shellConfig.expression
                          else throw "mkMcpServer: shell.expression required for nix-shell shells";
                        attrArgs =
                          if shellConfig ? attr then [ "--attr" shellConfig.attr ] else [ ];
                        expressionResolved = resolveNixExpression expression;
                      in
                      [
                        (escape "${pkgs.nix}/bin/nix-shell")
                      ]
                      ++ [ (escape expressionResolved) ]
                      ++ builtins.map escape attrArgs
                      ++ builtins.map escape shellExtraArgs
                      ++ [ (escape "--command") (escape "${realBinary}/bin/${binaryName}-real") ]
                    else
                      [ (escape "${realBinary}/bin/${binaryName}-real") ];
                in
                ''exec ${pkgs.lib.concatStringsSep " " commandParts} "$@"'';

              scriptLines =
                pkgs.lib.filter (line: line != "") (
                  envLines
                  ++ shellCommands
                  ++ [ shellExecLine ]
                );

              scriptBody = pkgs.lib.concatStringsSep "\n" scriptLines;

            in
            pkgs.symlinkJoin {
              name = binaryName;
              paths = [
                (pkgs.writeShellApplication {
                  name = binaryName;
                  runtimeInputs = runtimeInputs;
                  text = scriptBody + "\n";
                })
                realBinary
              ];
            };
        in
        rec {
          inherit mkMcpServer;

          # Main function to create mcp-ghcid with a provided ghcid derivation
          mkMcpGhcid =
            { system
            , ghcid
            , shell ? null
            }:
            mkMcpServer {
              inherit system shell;
              packageName = "mcp-ghcid";
              packageSrc = ./packages/mcp-ghcid;
              runtimeTool = ghcid;
              binaryName = "mcp-ghcid";
            };

          # Similar helper for mcp-hls wrapping a provided haskell-language-server derivation
          mkMcpHls =
            { system
            , hls
            , shell ? null
            }:
            mkMcpServer {
              inherit system shell;
              packageName = "mcp-hls";
              packageSrc = ./packages/mcp-hls;
              runtimeTool = hls;
              binaryName = "mcp-hls";
              disableChecks = true;
            };

          shellSpec = {
            fromFlake =
              { uri
              , attr ? null
              , extraArgs ? [ ]
              , env ? { }
              }:
              {
                type = "flake";
                inherit uri;
              }
              // nixpkgsLib.optionalAttrs (attr != null) { inherit attr; }
              // nixpkgsLib.optionalAttrs (extraArgs != [ ]) { extraArgs = extraArgs; }
              // nixpkgsLib.optionalAttrs (env != { }) { inherit env; };

            fromNixExpression =
              { expression
              , attr ? null
              , extraArgs ? [ ]
              , env ? { }
              }:
              {
                type = "nix-shell";
                inherit expression;
              }
              // nixpkgsLib.optionalAttrs (attr != null) { inherit attr; }
              // nixpkgsLib.optionalAttrs (extraArgs != [ ]) { extraArgs = extraArgs; }
              // nixpkgsLib.optionalAttrs (env != { }) { inherit env; };

            simple =
              { packages ? [ ]
              , env ? { }
              , commands ? [ ]
              }:
              {
                type = "simple";
              }
              // nixpkgsLib.optionalAttrs (packages != [ ]) { inherit packages; }
              // nixpkgsLib.optionalAttrs (env != { }) { inherit env; }
              // nixpkgsLib.optionalAttrs (commands != [ ]) { inherit commands; };
          };
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
                  self.packages.${system}.codex
                  # Add mcp-ghcid using the same GHC version as our Haskell development
                  (self.lib.mkMcpGhcid {
                    inherit system;
                    ghcid = pkgs.ghcid;
                    shell = {
                      packages = [
                        pkgs.cabal-install
                        pkgs.nix
                        pkgs.haskell.packages.ghc948.ghc
                      ];
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
