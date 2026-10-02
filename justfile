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

# Execute the permanent money custody economic suite (#90 S90-CUSTODY)
economic-test:
    #!/usr/bin/env bash
    set -euo pipefail
    start=$(date +%s.%N)
    echo "[stage] economic-test START: cabal test money-custody-tests"
    rc=0
    cabal test money-custody-tests || rc=$?
    end=$(date +%s.%N)
    elapsed=$(awk -v a="$start" -v b="$end" 'BEGIN { printf "%.3f", b - a }')
    echo "[stage] economic-test EXIT=${rc} ELAPSED=${elapsed}s"
    exit "$rc"

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
    scripts/check-lean-axioms
    scripts/check-trace-coverage-agreement
    cd lean && lake build
    cd "{{ justfile_directory() }}"
    date +%s%N > lean/.lake/s4b-mirror-nonce
    scripts/check-lean-mirrors
    grep -q "nonce=$(cat lean/.lake/s4b-mirror-nonce)" lean/.lake/s4b-mirror-receipt && grep -q '^MIRROR-CHECK-OK' lean/.lake/s4b-mirror-receipt || (echo 'MIRROR-RECEIPT-ABSENT: checker did not operate' >&2; exit 1)
    scripts/check-lean-mirrors-control

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
    # Repair 1: live-value binding of traces/steps (element-wise, nonzero extent)
    ./.lake/build/bin/corpusExport check corpus/economic.json corpus/integrated.json
    # Repair 2: exact key sets on the bytes, top level and one level in
    jq -e '
      (keys == ["auth","traces","view"]) and
      ((.traces | length) > 0) and
      ([.traces[] | keys] | all(. == ["initial","schema","steps","version"]))
    ' corpus/economic.json > /dev/null
    jq -e '
      (keys == ["auth","initial","steps"]) and
      ((.steps | length) > 0) and
      ([.steps[] | keys] | all(. == ["accepted","change","event","signer","state"]))
    ' corpus/integrated.json > /dev/null

# Full CI pipeline
# Every stage reports its invocation, exit and elapsed cost; nothing is
# skipped or hidden. The money custody suite runs additively (S90).
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    stage() {
        local name start end elapsed rc
        name="$1"; shift
        start=$(date +%s.%N)
        echo "[ci-stage] ${name} START"
        if "$@"; then rc=0; else rc=$?; fi
        end=$(date +%s.%N)
        elapsed=$(awk -v a="$start" -v b="$end" 'BEGIN { printf "%.3f", b - a }')
        echo "[ci-stage] ${name} EXIT=${rc} ELAPSED=${elapsed}s"
        return "$rc"
    }
    stage lean-toolchain-contract just lean-toolchain-contract
    stage build just build
    stage format-check just format-check
    stage hlint just hlint
    stage economic-test just economic-test
    stage lean just lean
    stage lean-corpus-gate just lean-corpus-gate
    stage lean-corpus-verify just lean-corpus-verify

# Theorem-keyed Lean mutant ledger (#66 S3): re-run every mutant, check every
# kill claim, ledger row and rendering against what Lean reports, then show
# every runner check failing on its own negative control
lean-mutants:
    #!/usr/bin/env bash
    set -euo pipefail
    scripts/lean-mutants/run
    scripts/lean-mutants/run --negative-control

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

# Verify S4-B Prop/Bool mirror correspondence (mandatory; S4-B lane owns these lines)
lean-mirrors:
    #!/usr/bin/env bash
    set -euo pipefail
    scripts/check-lean-mirrors

# Give the checkout what the claim gate resolves: its pins are commits that
# must be ancestors of HEAD (in CI the PR merge commit), and its selftest
# walks the history before them. A shallow checkout (CI) is unshallowed; a
# history still truncated afterwards fails here, since no pin could then be
# judged. A full checkout is left untouched. The gate's own pin and
# reachability checks still decide.
simulator-history:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "$(git rev-parse --is-shallow-repository)" = true ]; then
        echo "[simulator] shallow checkout: git fetch --unshallow origin"
        git fetch --quiet --no-tags --unshallow origin
    fi
    if [ "$(git rev-parse --is-shallow-repository)" = true ]; then
        echo "simulator: history of HEAD is still truncated after unshallowing" >&2
        exit 1
    fi
    echo "[simulator] history: shallow=false HEAD=$(git rev-parse --short HEAD)"

# Run the simulator gates: the build --check, then every
# economics-simulator-*-gate.mjs gate and its --selftest, failing on the first
# error. The gate set is discovered, never listed; an empty set is red. The
# trace gates replay the two committed Lean drivers, built first.
simulator: simulator-history
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    gates=(economics-simulator-*-gate.mjs)
    if [ "${#gates[@]}" -eq 0 ]; then
        echo 'simulator: no economics-simulator-*-gate.mjs discovered' >&2
        exit 1
    fi
    echo "[simulator] lake build TraceDriverV1 KelTraceDriverV1"
    (cd lean && lake build TraceDriverV1 KelTraceDriverV1)
    echo "[simulator] economics-simulator-build.mjs --check"
    node economics-simulator-build.mjs --check
    for gate in "${gates[@]}"; do
        echo "[simulator] ${gate}"
        node "${gate}"
        echo "[simulator] ${gate} --selftest"
        node "${gate}" --selftest
    done
    echo "[simulator] OK gates=${#gates[@]}"
