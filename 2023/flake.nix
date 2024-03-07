{
  description = "Advent of code - 2023";

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
        ghcVersion = "ghc981";
        hsPkgs = pkgs.haskell.packages.${ghcVersion};

        pkgName = "advent2023";

        haskellOverlay = final: prev: {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${ghcVersion} = prev.haskell.packages.${ghcVersion}.override {
                overrides = hfinal: hprev: {
                  ${pkgName} = hfinal.callCabal2nix pkgName (gitignore.lib.gitignoreSource ./.) {};
                };
              };
            };
          };
        };
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
            hsPkgs.haskell-language-server
          ];
          src = null;
        };
      });
}