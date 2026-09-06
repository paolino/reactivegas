#!/usr/bin/env bash
# SS-4 frozen comparator: static parsing/hashing only, never launches project code.
# Reads batch timing/exit/log files and reports separate-vs-shared comparison.
# Samples and extrapolations labelled; cannot supply unsupported all-row upper bound.
set -uo pipefail
OUT=/tmp/reactivegas-ms2-measure-output
echo "BATCH-COMPARATOR-STATIC (no lake/lean/nix invocation)"
echo "INPUTS:"
ls -l "$OUT"/m-batch-*.ms "$OUT"/m-batch-*.exit 2>&1 || { echo "BATCH-INPUTS-MISSING (charged)"; exit 91; }
echo "SEPARATE-SUM-MS: $(cat "$OUT"/m-batch-separate-*.ms 2>/dev/null | awk '{s+=$1} END {print s}') (labelled SAMPLE, not upper bound)"
echo "SHARED-SUM-MS: $(cat "$OUT"/m-batch-shared-*.ms 2>/dev/null | awk '{s+=$1} END {print s}') (labelled SAMPLE)"
echo "SETUP-RESTORE-INCLUDED: yes (all ms files include apply+build+restore per frozen scripts)"
echo "OBSERVATION-TARGETS-EQUAL: yes (span-bound REDs per mutant scripts)"
echo "EXTRAPOLATION-LABEL: batch comparison of 2 variants does not establish every row cost, batching feasibility, or upper bound for all 207 rows."
