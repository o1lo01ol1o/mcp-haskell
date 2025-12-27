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
          "mcp-hls" = mcpHlsPkg;
          default = mcpGhcidPkg;
        }
      );

      lib =
        let
          nixpkgsLib = inputs.nixpkgs.lib;
	          mkMcpServer =
	            { system
              , pkgs ? null
	            , packageName
	            , packageSrc
	            , shell ? { }
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

              shellPackages =
                let
                  pkgList = shell.packages or [ ];
                in
                if builtins.isList pkgList then pkgList else [ pkgList ];

              shellEnv = shell.env or { };
              shellCommands = shell.commands or [ ];

              pathPrefix = pkgs'.lib.makeBinPath shellPackages;
              exports = pkgs'.lib.concatMapStringsSep "\n" (entry: entry)
                (pkgs'.lib.mapAttrsToList (name: value: "export ${name}=${value}") shellEnv);
              commandScript =
                if shellCommands == [ ] then ""
                else builtins.concatStringsSep "\n" shellCommands;

              wrapper =
                pkgs'.writeShellScriptBin binaryName ''
                  ${if pathPrefix == "" then "" else "export PATH=${pathPrefix}:$PATH"}
                  ${exports}
                  ${commandScript}
                  exec ${staticPkg}/bin/${binaryName} "$@"
                '';
            in
            wrapper;
        in
        {
          inherit mkMcpServer;

          mkMcpGhcid = args:
            let
              ghcidPkg = args.ghcid;
              shellConfig = args.shell or { };
              existingPackages = shellConfig.packages or [ ];
              packageList =
                if builtins.isList existingPackages then existingPackages else [ existingPackages ];
              updatedShell =
                shellConfig // {
                  packages = packageList ++ [ ghcidPkg ];
                };
            in
            mkMcpServer (
              (builtins.removeAttrs args [ "ghcid" ])
              // {
                packageName = "mcp-ghcid";
                packageSrc = ./packages/mcp-ghcid;
                shell = updatedShell;
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
                  mkdir -p .mcp-cache
                  rm -rf .mcp-cache/mcp-ghcid
                  nix build .#mcp-ghcid -o .mcp-cache/mcp-ghcid
                '';
                scripts."rebuild-mcp-hls".exec = ''
                  set -euo pipefail
                  mkdir -p .mcp-cache
                  rm -rf .mcp-cache/mcp-hls
                  nix build .#mcp-hls -o .mcp-cache/mcp-hls
                '';
                scripts."rebuild-mcp-all".exec = ''
                  set -euo pipefail
                  mkdir -p .mcp-cache
                  rm -rf .mcp-cache/mcp-ghcid .mcp-cache/mcp-hls
                  nix build .#mcp-ghcid -o .mcp-cache/mcp-ghcid
                  nix build .#mcp-hls -o .mcp-cache/mcp-hls
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
