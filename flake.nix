{
  description = "Reactivegas - social tool for group of consumers";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, mkdocs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
          inherit (haskellNix) config;
        };
        project = import ./nix/project.nix {
          inherit pkgs;
          inherit (self) rev;
          mkdocs = mkdocs.packages.${system};
        };
        flake = project.flake { };
        # The simulator gates (`just simulator`) run node and, on Linux, a
        # headless chromium inside the dev shell.
        devShells = flake.devShells // {
          default = flake.devShells.default.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.nodejs ]
              ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.chromium;
          });
        };
      in {
        packages = flake.packages // {
          default = flake.packages."reactivegas:exe:server";
        };
        inherit devShells;
        devShell = devShells.default;
      });
}
