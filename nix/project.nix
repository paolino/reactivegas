{ pkgs, rev ? null }:
let
  compiler-nix-name = "ghc984";
  indexState = "2025-01-01T00:00:00Z";
in pkgs.haskell-nix.cabalProject' {
  src = ../.;
  inherit compiler-nix-name;
  index-state = indexState;

  shell.tools = {
    cabal = { index-state = indexState; };
    hlint = { index-state = indexState; };
  };
  shell.buildInputs = with pkgs; [
    just
    nixfmt-classic
    shellcheck
    sqlite
    haskellPackages.fourmolu
  ];
}
