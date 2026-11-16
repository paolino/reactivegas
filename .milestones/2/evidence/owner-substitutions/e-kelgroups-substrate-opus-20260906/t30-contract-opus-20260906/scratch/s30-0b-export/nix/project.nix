{ pkgs, keri-hs }:
let
  indexState = "2026-02-01T00:00:00Z";
  project = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = {
      tools = {
        cabal = { index-state = indexState; };
        cabal-fmt = { index-state = indexState; };
        haskell-language-server = {
          index-state = indexState;
        };
        hoogle = { index-state = indexState; };
        fourmolu = { index-state = indexState; };
        hlint = { index-state = indexState; };
      };
      buildInputs = with pkgs; [
        just
        lean4
        nixfmt-classic
        python3
        python3Packages.mkdocs-material
        uv
        purs
        spago-unstable
        purs-tidy-bin.purs-tidy-0_10_0
        esbuild
        nodejs_20
        (writeShellScriptBin "haskell-language-server-wrapper" ''
          exec haskell-language-server "$@"
        '')
      ];
      shellHook = ''
        echo "packages: ${keri-hs}" > cabal.project.local
      '';
    };
    cabalProjectLocal = ''
      packages: ${keri-hs}
    '';
  };
  flake = project.flake { };
  ciShell = (pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = {
      withHoogle = false;
      tools = {
        cabal = { index-state = indexState; };
        cabal-fmt = { index-state = indexState; };
        fourmolu = { index-state = indexState; };
        hlint = { index-state = indexState; };
      };
      buildInputs = with pkgs; [
        just
        lean4
        purs
        spago-unstable
        purs-tidy-bin.purs-tidy-0_10_0
        esbuild
        nodejs_20
      ];
      shellHook = ''
        echo "packages: ${keri-hs}" > cabal.project.local
      '';
    };
    cabalProjectLocal = ''
      packages: ${keri-hs}
    '';
  }).shell;
in {
  packages = flake.packages;
  devShells.default = project.shell;
  devShells.ci = ciShell;
}
