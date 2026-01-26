# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i Applicazioni Core Events Lib Server System UI Voci app Console
    done
    cabal-fmt -i *.cabal
    nixfmt *.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint Applicazioni Core Events Lib Server System UI Voci app

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all

# Clean build artifacts
clean:
    #!/usr/bin/env bash
    cabal clean

# Run the server
run:
    #!/usr/bin/env bash
    cabal run server

# Full CI pipeline
CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    fourmolu -m check Applicazioni Core Events Lib Server System UI Voci app Console
    hlint Applicazioni Core Events Lib Server System UI Voci app

# Generate module dependency graph
modules:
    #!/usr/bin/env bash
    graphmod -q -p -r Main Applicazioni Core Events Lib Server System UI Voci \
        | dot -T png > modules.png
