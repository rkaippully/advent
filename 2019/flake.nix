{
  description = "Advent of code - 2019";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        };
        ghcVersion = "9101";
        hsPkgs = pkgs.haskell.packages."ghc${ghcVersion}";

        pkgName = "advent2019";

        haskellOverlay = final: prev: {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              "ghc${ghcVersion}" = prev.haskell.packages."ghc${ghcVersion}".override {
                overrides = hfinal: hprev: {
                  ${pkgName} = hfinal.callCabal2nix pkgName (gitignore.lib.gitignoreSource ./.) {};
                };
              };
            };
          };
        };

        haskell-language-server = pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };
      in {
        packages.default = hsPkgs.${pkgName};
        devShells.default = hsPkgs.shellFor {
          name = pkgName;
          packages = pkgs: [ pkgs.${pkgName} ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            hsPkgs.fourmolu
            hsPkgs.ghc
            pkgs.hlint
            haskell-language-server
          ];
        };
      });
}
