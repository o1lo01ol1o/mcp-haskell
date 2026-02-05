{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    codex-nix = {
      url = "github:sadjow/codex-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, codex-nix, systems, ... } @ inputs:
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
          ghcPackageSet = pkgs.haskell.packages.ghc912;
          mcpGhcidPkg = self.lib.mkMcpGhcid {
            inherit system;
              inherit pkgs;
            inherit ghcPackageSet;
            ghcid = ghcPackageSet.ghcid;
            shell = {
              packages = [
                pkgs.cabal-install
                pkgs.nix
                ghcPackageSet.ghc
              ];
            };
          };
          mcpCabalPkg = self.lib.mkMcpCabal {
            inherit system;
              inherit pkgs;
            inherit ghcPackageSet;
            cabal = pkgs.cabal-install;
            shell = {
              packages = [
                pkgs.cabal-install
                pkgs.nix
                ghcPackageSet.ghc
              ];
            };
          };
          mcpHlsPkg = self.lib.mkMcpHls {
            inherit system;
              inherit pkgs;
            inherit ghcPackageSet;
            shell = {
              packages = [
	                pkgs.cabal-install
	                pkgs.nix
	                ghcPackageSet.ghc
	              ];
	            };
	          };
	        in
	        {
          codex = codex-nix.packages.${system}.codex;
          "mcp-ghcid" = mcpGhcidPkg;
          "mcp-cabal" = mcpCabalPkg;
          "mcp-hls" = mcpHlsPkg;
          default = mcpGhcidPkg;
        }
      );

      lib =
        let
          nixpkgsLib = inputs.nixpkgs.lib;
          shellSpec =
            let
              normalizePackages = packages:
                if builtins.isList packages then packages else [ packages ];
            in
            {
              simple = { packages ? [ ], env ? { }, commands ? [ ] }:
                {
                  type = "simple";
                  packages = normalizePackages packages;
                  inherit env commands;
                };

              fromFlake = { uri, attr ? null, extraArgs ? [ ], env ? { } }:
                {
                  type = "flake";
                  inherit uri attr extraArgs env;
                };

              fromNixExpression = { expression, attr ? null, extraArgs ? [ ], env ? { } }:
                {
                  type = "nix-expression";
                  inherit expression attr extraArgs env;
                };
            };

	          mkMcpServer =
	            { system
              , pkgs ? null
	            , packageName
	            , packageSrc
	            , shell ? null
              , pathPackages ? [ ]
              , wrap ? true
	            , binaryName ? packageName
	            , disableChecks ? false
	            , ghcPackageSet ? null
	            }:
	            let
	              pkgs' =
                  if pkgs == null then
                    import inputs.nixpkgs {
                      inherit system;
                      config.allowUnfree = true;
                    }
                  else
                    pkgs;
	
	              ghcSet =
	                if ghcPackageSet == null then
	                  pkgs'.haskell.packages.ghc912
	                else
	                  ghcPackageSet;

                # Normalise legacy `shell` arguments into a single shape consumed by the wrapper.
                shellConfig =
                  let
                    raw = shell;
                    shellType =
                      if raw == null then "simple"
                      else if builtins.isAttrs raw && (raw.type or raw.kind or null) != null then (raw.type or raw.kind)
                      else if builtins.isAttrs raw && (raw ? packages || raw ? env || raw ? commands) then "simple"
                      else "simple";

                    toList = value: if builtins.isList value then value else [ value ];
                    env = if builtins.isAttrs raw && (raw ? env) then raw.env else { };
                  in
                  if shellType == "simple" then
                    shellSpec.simple {
                      packages = if builtins.isAttrs raw && (raw ? packages) then toList raw.packages else [ ];
                      env = env;
                      commands = if builtins.isAttrs raw && (raw ? commands) then toList raw.commands else [ ];
                    }
                  else if shellType == "flake" then
                    shellSpec.fromFlake {
                      uri = raw.uri;
                      attr = raw.attr or null;
                      extraArgs = toList (raw.extraArgs or [ ]);
                      env = env;
                    }
                  else if shellType == "nix-shell" || shellType == "nix-expression" then
                    shellSpec.fromNixExpression {
                      expression = raw.expression;
                      attr = raw.attr or null;
                      extraArgs = toList (raw.extraArgs or [ ]);
                      env = env;
                    }
                  else
                    shellSpec.simple { };

              mcpOverlay = self: super: {
                mcp-sdk-hs = self.callCabal2nix "mcp-sdk-hs" (./mcp-sdk-hs) { };
                mcp-common = self.callCabal2nix "mcp-common" (./packages/mcp-common) { };
                ${packageName} =
                  let
                    base = self.callCabal2nix packageName packageSrc { };
                  in
                  if disableChecks then pkgs'.haskell.lib.dontCheck base else base;
              };

	              haskellPkgs = ghcSet.override {
	                overrides = mcpOverlay;
	              };

              haskellPkg =
                if builtins.hasAttr packageName haskellPkgs then haskellPkgs.${packageName}
                else throw "mkMcpServer: expected package not found in Haskell overlay";

              staticPkg = pkgs'.haskell.lib.justStaticExecutables haskellPkg;

              # `wrap = false` exposes only the built executable(s), with no shell-wrapping logic.
              _maybeWrapped =
                if wrap == false then
                  staticPkg
                else
                  let
                    needsNix = shellConfig.type != "simple";
                    wrapperTooling =
                      if needsNix then [ pkgs'.bash pkgs'.coreutils pkgs'.nix ] else [ pkgs'.bash pkgs'.coreutils ];
                    extraPathPackages = if builtins.isList pathPackages then pathPackages else [ pathPackages ];

                    shellPackages =
                      if shellConfig.type == "simple" then shellConfig.packages else [ ];
                    pathPrefix = pkgs'.lib.makeBinPath (wrapperTooling ++ extraPathPackages ++ shellPackages);

                    shellEnv = shellConfig.env or { };
                    shellCommands =
                      if shellConfig.type == "simple" then (shellConfig.commands or [ ]) else [ ];

                    exports = pkgs'.lib.concatMapStringsSep "\n" (entry: entry)
                      (pkgs'.lib.mapAttrsToList (name: value: "export ${name}=${value}") shellEnv);
                    commandScript =
                      if shellCommands == [ ] then ""
                      else builtins.concatStringsSep "\n" shellCommands;

                    realBinary = pkgs'.writeShellScriptBin "${binaryName}-real" ''
                      exec ${staticPkg}/bin/${binaryName} "$@"
                    '';

                    developInstallable =
                      if shellConfig.type == "flake" then
                        let
                          uri = toString shellConfig.uri;
                          attrSuffix = if shellConfig.attr == null then "" else "#${shellConfig.attr}";
                        in
                        "${uri}${attrSuffix}"
                      else
                        null;

                    developArgs =
                      if shellConfig.type == "nix-expression" then
                        let
                          expr = toString shellConfig.expression;
                          attrArgs = if shellConfig.attr == null then [ ] else [ shellConfig.attr ];
                        in
                        [ "develop" "-f" expr ] ++ (shellConfig.extraArgs or [ ]) ++ attrArgs ++ [ "--command" "${realBinary}/bin/${binaryName}-real" ]
                      else if shellConfig.type == "flake" then
                        [ "develop" ] ++ (shellConfig.extraArgs or [ ]) ++ [ developInstallable "--command" "${realBinary}/bin/${binaryName}-real" ]
                      else
                        [ ];

                    wrapper =
                      pkgs'.writeShellScriptBin binaryName ''
                        ${if pathPrefix == "" then "" else "export PATH=${pathPrefix}:$PATH"}
                        ${exports}
                        ${commandScript}
                        if [ "${shellConfig.type}" = "simple" ]; then
                          exec ${realBinary}/bin/${binaryName}-real "$@"
                        else
                          exec ${pkgs'.nix}/bin/nix ${pkgs'.lib.concatStringsSep " " (builtins.map (arg: pkgs'.lib.escapeShellArg arg) developArgs)} "$@"
                        fi
                      '';
                  in
                  wrapper;
            in
            _maybeWrapped;
        in
        {
          inherit mkMcpServer shellSpec;

          mkMcpGhcid = args:
            let
              ghcidPkg = args.ghcid;
            in
            mkMcpServer (
              (builtins.removeAttrs args [ "ghcid" ])
              // {
                packageName = "mcp-ghcid";
                packageSrc = ./packages/mcp-ghcid;
                pathPackages = [ ghcidPkg ];
              }
            );

          mkMcpCabal = args:
            let
              cabalPkg =
                if builtins.hasAttr "cabal" args then
                  args.cabal
                else
                  null;
              pathPackages =
                if cabalPkg == null then
                  [ ]
                else
                  [ cabalPkg ];
            in
            mkMcpServer (
              (builtins.removeAttrs args [ "cabal" ])
              // {
                packageName = "mcp-cabal";
                packageSrc = ./packages/mcp-cabal;
                pathPackages = pathPackages;
              }
            );

          # Build only the `mcp-ghcid` executable (no wrapper, no shell logic).
          mkMcpGhcidBinary = args:
            mkMcpServer (
              (builtins.removeAttrs args [ "ghcid" "shell" "pathPackages" ])
              // {
                packageName = "mcp-ghcid";
                packageSrc = ./packages/mcp-ghcid;
                wrap = false;
              }
            );

          # Build only the `mcp-cabal` executable (no wrapper, no shell logic).
          mkMcpCabalBinary = args:
            mkMcpServer (
              (builtins.removeAttrs args [ "cabal" "shell" "pathPackages" ])
              // {
                packageName = "mcp-cabal";
                packageSrc = ./packages/mcp-cabal;
                wrap = false;
              }
            );

          mkMcpHls = args:
            let
              pkgs =
                  if builtins.hasAttr "pkgs" args && args.pkgs != null then
                    args.pkgs
                  else
                    import inputs.nixpkgs {
                      inherit (args) system;
                      config.allowUnfree = true;
                    };
	              ghcSet = args.ghcPackageSet or pkgs.haskell.packages.ghc912;
	              hlsPkg =
	                if builtins.hasAttr "haskell-language-server" ghcSet then
	                  ghcSet.haskell-language-server
	                else
	                  pkgs.haskell-language-server;
	              shellConfig = args.shell or { };
	              existingPackages = shellConfig.packages or [ ];
	              packageList =
	                if builtins.isList existingPackages then existingPackages else [ existingPackages ];
	              updatedShell =
	                shellConfig // {
	                  packages = packageList ++ [ hlsPkg ];
	                };
	            in
	            mkMcpServer (
	              (builtins.removeAttrs args [ "hls" ])
	              // {
	                packageName = "mcp-hls";
	                packageSrc = ./packages/mcp-hls;
	                shell = updatedShell;
                  pathPackages = [ hlsPkg ];
	              }
	            );

          # Build only the `mcp-hls` executable (no wrapper, no shell logic).
          mkMcpHlsBinary = args:
            mkMcpServer (
              (builtins.removeAttrs args [ "hls" "shell" "pathPackages" ])
              // {
                packageName = "mcp-hls";
                packageSrc = ./packages/mcp-hls;
                wrap = false;
              }
            );
	        };

      devShells = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              {
                packages = [
                  pkgs.hello
                  codex-nix.packages.${system}.codex
                  self.packages.${system}."mcp-ghcid"
                  self.packages.${system}."mcp-cabal"
                ];
                languages = {
                  haskell = {
                    enable = true;
                    package = pkgs.haskell.packages.ghc912.ghc;
                  };
                  nix.enable = true;
                };
                scripts."rebuild-mcp-ghcid".exec = ''
                  set -euo pipefail
                  CACHE_ROOT="''${MCP_CACHE_DIR:-''${TMPDIR:-/tmp}/mcp-cache}"
                  mkdir -p "$CACHE_ROOT"
                  rm -rf "$CACHE_ROOT/mcp-ghcid"
                  nix build .#mcp-ghcid -o "$CACHE_ROOT/mcp-ghcid"
                '';
                scripts."rebuild-mcp-cabal".exec = ''
                  set -euo pipefail
                  CACHE_ROOT="''${MCP_CACHE_DIR:-''${TMPDIR:-/tmp}/mcp-cache}"
                  mkdir -p "$CACHE_ROOT"
                  rm -rf "$CACHE_ROOT/mcp-cabal"
                  nix build .#mcp-cabal -o "$CACHE_ROOT/mcp-cabal"
                '';
                scripts."rebuild-mcp-hls".exec = ''
                  set -euo pipefail
                  CACHE_ROOT="''${MCP_CACHE_DIR:-''${TMPDIR:-/tmp}/mcp-cache}"
                  mkdir -p "$CACHE_ROOT"
                  rm -rf "$CACHE_ROOT/mcp-hls"
                  nix build .#mcp-hls -o "$CACHE_ROOT/mcp-hls"
                '';
                scripts."rebuild-mcp-all".exec = ''
                  set -euo pipefail
                  CACHE_ROOT="''${MCP_CACHE_DIR:-''${TMPDIR:-/tmp}/mcp-cache}"
                  mkdir -p "$CACHE_ROOT"
                  rm -rf "$CACHE_ROOT/mcp-ghcid" "$CACHE_ROOT/mcp-cabal" "$CACHE_ROOT/mcp-hls"
                  nix build .#mcp-ghcid -o "$CACHE_ROOT/mcp-ghcid"
                  nix build .#mcp-cabal -o "$CACHE_ROOT/mcp-cabal"
                  nix build .#mcp-hls -o "$CACHE_ROOT/mcp-hls"
                '';
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
