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
          };

          # Make it the default package
          default = self.packages.${system}.mcp-ghcid;
        }
      );

      # Library functions for creating mcp-ghcid with custom ghcid
      lib = {
        # Main function to create mcp-ghcid with a provided ghcid derivation
        # Usage: mkMcpGhcid { system = "x86_64-linux"; ghcid = myGhcidDerivation; }
        mkMcpGhcid =
          { system, ghcid }:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };

            mcpOverlay = self: super: {
              mcp-sdk-hs = self.callCabal2nix "mcp-sdk-hs" (./mcp-sdk-hs) { };
              mcp-common = self.callCabal2nix "mcp-common" (./packages/mcp-common) { };
              mcp-ghcid = self.callCabal2nix "mcp-ghcid" (./packages/mcp-ghcid) { };
            };

            haskellPkgs = pkgs.haskell.packages.ghc948.override {
              overrides = mcpOverlay;
            };

            haskellPkg = haskellPkgs.mcp-ghcid;

            # Create static executable
            staticPkg = pkgs.haskell.lib.justStaticExecutables haskellPkg;

          in
          pkgs.runCommand "mcp-ghcid-with-ghcid"
            {
              buildInputs = [ pkgs.makeWrapper ];
            }
            ''
              mkdir -p $out/bin
              makeWrapper ${staticPkg}/bin/mcp-ghcid $out/bin/mcp-ghcid \
                --prefix PATH : ${ghcid}/bin
            '';
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
                  # Add mcp-ghcid using the same GHC version as our Haskell development
                  # (self.lib.mkMcpGhcid {
                  #   inherit system;
                  #   ghcid = pkgs.ghcid;
                  # })
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
        }
      );
    };
}
