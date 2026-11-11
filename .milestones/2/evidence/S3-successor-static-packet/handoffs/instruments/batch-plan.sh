#!/usr/bin/env bash
# SS-4 frozen shared/batch arrangement: two separately admitted single-atom variants
# measured under SEPARATE infrastructure (A/B) and SHARED infrastructure, with comparator.
# Variants (from frozen registry, independent source/output roots preserved):
#   V-A = C-VALIDATE single-atom (Validate.lean:145 admin->True, diff C-VALIDATE.diff)
#   V-B = C-VOTEFOLD single-atom (Vote/Fold.lean:76 filter->True, diff C-VOTEFOLD.diff)
# Multiple changes in one variant never earn independent atom credit (each variant is one atom).
# All setup/restore costs included with equal requested observation targets.
# Layers: U-SHARED-SEPARATE (cold+build+restore per scratch) vs U-SHARED-BATCH (shared cache) + comparator (static).
set -uo pipefail
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "V-A-DIFF-SHA: $(sha256sum /tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/diffs/C-VALIDATE.diff)"
echo "V-B-DIFF-SHA: $(sha256sum /tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/diffs/C-VOTEFOLD.diff)"
echo "SEPARATE-A-SCRATCH: /tmp/reactivegas-ms2-batch-separate-A (detached 3590c001, porcelain empty before/after)"
echo "SEPARATE-B-SCRATCH: /tmp/reactivegas-ms2-batch-separate-B (detached 3590c001, porcelain empty before/after)"
echo "SHARED-SCRATCH: /tmp/reactivegas-ms2-batch-shared (detached 3590c001, sequential apply/restore, shared .lake cache)"
echo "SEPARATE-PROTOCOL:"
echo "  A: git apply C-VALIDATE.diff in A; time lake build KelGroups.Invariants (U-CHAIN); time restore.sh KelGroups.Invariants (U-RESTORE); record ms/exit/Built/Replayed"
echo "  B: git apply C-VOTEFOLD.diff in B; time lake build KelGroups.Vote.Invariants (U-CHAIN); time restore.sh (U-RESTORE); record ms/exit/Built/Replayed"
echo "SHARED-PROTOCOL:"
echo "  S1: cold lake build in SHARED once (U-COLD shared); git apply C-VALIDATE.diff; time lake build KelGroups.Invariants; restore.sh; git apply C-VOTEFOLD.diff; time lake build KelGroups.Vote.Invariants; restore.sh"
echo "  All setup/restore counted; equal observation targets (span-bound REDs per mutant scripts); timeout 300s per build, 600s cold; failures charged+retained, never retried."
echo "COMPARATOR: instruments/compare-batch.sh parses m-batch-*.ms/.exit/.stdout (static parsing/hashing only, never launches project code) and reports separate-vs-shared wall sums with setup/restore included; samples/extrapolations labelled, never supply unsupported all-row upper bound."
