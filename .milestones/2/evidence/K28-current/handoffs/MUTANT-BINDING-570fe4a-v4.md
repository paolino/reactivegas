# MUTANT-BINDING-570fe4a-v4 — M5 rebind to actual GREEN bytes (addendum record)

Ticket owner `t28-app-api`. Lineage: v1 (RED bindings) + v2 (header
correction) + v3 (Q-004 reconciliation) preserved unmodified; this v4 binds
the M5 fix (Q-005/NOTE-008) to actual GREEN bytes, versioned/hashed BEFORE
any execution. All other anchors stand as previously bound (v1–v3); their
leg-5 preconditions re-verify mechanically at runtime (fail closed).

## M5 rebind (dirty candidate bytes, read-only verification)

- Equation head: `lib/KelGroups/Fold.hs:463`
  (`foldIntegrated integration initial =`).
- In-extraction arm: `:468` (`Left _ -> gs`, 16-space indent inside case).
- From-counterexample (present in file, provably uncounted): `:477` bare
  `foldIntegratedFrom`, `:482` equation head, `:487` arm `Left _ -> gs`;
  extraction output contains 0 occurrences of `foldIntegratedFrom`.
- Extraction actuals (v6 awk, run pre-freeze): 10 lines, `m5a=1`, `m5b=0`
  → H2 branch taken; H2b absent (`either (const gs)` 0 hits file-wide —
  reconfirmed).
- Dirty `Fold.hs` hash `d76219e5…` (transfer: committed candidate bytes must
  hash equal at submission review or re-bind before leg-5).
- M5 mutation program UNCHANGED from v5 (traced correct on these exact
  bytes: `in_f` survives col-0 heads, arm 468 reached, `!done5` contains
  487, splice-count==1). H2/H2b selection UNCHANGED. Kill UNCHANGED
  (agreement witness + `MUTANT-M5` in log).

## Standing TBB (bind at GREEN submission via ANCHOR-ATTEST + leg-5 preconditions)

All production spellings (H1–H5 incl. the M5 arm above, H4', H7, M6
success-write) re-verify mechanically at leg-5 (fail closed) and by
owner attestation; BINDING-GREEN review compares committed hashes against
this record before any mutation run.

## Freeze refs

Gate v6 normalized + full hashes + `gate-v6.sh.backup`: STATUS NOTE
GATE-FROZEN-v6. FROZEN_BASE RED `570fe4a…` unchanged.
