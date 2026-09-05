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

# Build the lean state-machine specification
lean:
    #!/usr/bin/env bash
    set -euo pipefail
    ./nix/lean-dependency-direction.sh
    scripts/check-reactivegas-inversion-coverage
    scripts/check-reactivegas-inversion-coverage --negative-control
    scripts/check-trace-coverage-agreement
    cd lean && lake build

# Execute the shipped integrated-corpus evaluator and require exact `true`
lean-corpus-gate:
    #!/usr/bin/env bash
    set -euo pipefail
    result=$(cd lean && lake env lean Reactivegas/CorpusGate.lean)
    [[ "$result" == "true" ]]

# Emit both frozen corpus files via the CorpusExport exe (sole writer of the JSON)
lean-corpus-export:
    #!/usr/bin/env bash
    set -euo pipefail
    cd lean
    mkdir -p corpus
    lake build corpusExport
    ./.lake/build/bin/corpusExport corpus/economic.json corpus/integrated.json
    sha256sum corpus/economic.json corpus/integrated.json > corpus/corpus.sha256

# Re-emit to temp and byte-compare against checked-in files + manifest; fail closed
lean-corpus-verify:
    #!/usr/bin/env bash
    set -euo pipefail
    cd lean
    lake build corpusExport
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    ./.lake/build/bin/corpusExport "$tmp/economic.json" "$tmp/integrated.json"
    cmp "$tmp/economic.json" corpus/economic.json
    cmp "$tmp/integrated.json" corpus/integrated.json
    sha256sum -c corpus/corpus.sha256

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just lean-toolchain-contract
    just build
    just format-check
    just hlint
    just lean
    just lean-corpus-gate
    just lean-corpus-verify

# Assert the declared Lean pin matches the toolchain that actually runs
lean-toolchain-contract:
    #!/usr/bin/env bash
    set -euo pipefail
    scripts/check-lean-toolchain

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

# Serve mkdocs documentation locally
serve-docs:
    #!/usr/bin/env bash
    mkdocs serve

# Build mkdocs documentation
build-docs:
    #!/usr/bin/env bash
    mkdocs build
