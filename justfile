# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    hs_files=$(find . -name '*.hs' \
        -not -path './dist-newstyle/*' \
        -not -path './.direnv/*' \
        -not -name 'FileSystem.hs' \
        -not -name 'Valuta.hs' \
        -not -path './Core/Aggiornamento.hs')
    for i in {1..3}; do
        fourmolu -i $hs_files
    done
    find . -name '*.cabal' -not -path './dist-newstyle/*' | xargs cabal-fmt -i
    find . -name '*.nix' -not -path './dist-newstyle/*' | xargs nixfmt

# Check formatting without modifying files
format-check:
    #!/usr/bin/env bash
    set -euo pipefail
    hs_files=$(find . -name '*.hs' \
        -not -path './dist-newstyle/*' \
        -not -path './.direnv/*' \
        -not -name 'FileSystem.hs' \
        -not -name 'Valuta.hs' \
        -not -path './Core/Aggiornamento.hs')
    fourmolu -m check $hs_files
    find . -name '*.cabal' -not -path './dist-newstyle/*' | xargs cabal-fmt -c

# Run hlint
hlint:
    #!/usr/bin/env bash
    set -euo pipefail
    find . -name '*.hs' \
        -not -path './dist-newstyle/*' \
        -not -path './.direnv/*' \
        -not -name 'FileSystem.hs' \
        -not -name 'Valuta.hs' \
        -not -path './Core/Aggiornamento.hs' \
        | xargs hlint

# Build all components
build:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal build all

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just format-check
    just hlint

# Clean build artifacts
clean:
    #!/usr/bin/env bash
    cabal clean
    rm -rf result

# Run the server
run *args:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal run server -- {{ args }}

# Generate haddock documentation
haddock:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal haddock all

# Watch for changes and rebuild
watch:
    #!/usr/bin/env bash
    ghcid --command="cabal repl lib:reactivegas"

# Generate module dependency graph
modules:
    #!/usr/bin/env bash
    set -euo pipefail
    graphmod -q -p Applicazioni Core Eventi Lib Server UI Voci \
        | dot -T png > modules.png
    echo "Generated modules.png"
