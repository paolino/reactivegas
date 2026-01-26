{ pkgs, rev ? null, mkdocs }:
let
  compiler-nix-name = "ghc984";
  indexState = "2026-01-01T00:00:00Z";
in pkgs.haskell-nix.cabalProject' {
  src = ../.;
  inherit compiler-nix-name;
  index-state = indexState;

  shell.tools = {
    cabal = { index-state = indexState; };
    hlint = { index-state = indexState; };
    haskell-language-server = { index-state = indexState; };
  };
  shell.buildInputs = with pkgs; [
    just
    nixfmt-classic
    shellcheck
    sqlite
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
    pkgs.mkdocs
    mkdocs.from-nixpkgs
    mkdocs.static-i18n
  ];
}
