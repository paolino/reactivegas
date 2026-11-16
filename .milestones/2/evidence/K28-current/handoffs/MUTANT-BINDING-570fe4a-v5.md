# MUTANT-BINDING-570fe4a-v5 — Q-006 rebinds to committed bytes (addendum record)

Ticket owner `t28-app-api`. Lineage: v1 (RED bindings) + v2 (header
correction) + v3 (Q-004 reconciliation) + v4 (Q-005 M5 rebind) preserved
unmodified; this v5 binds F1–F4 to actual committed `84a2dae` bytes,
versioned/hashed BEFORE any execution. Prior anchors re-confirmed below
(M4/M5 programs + all other preconditions hold on committed bytes as at
their proofs).

## F1 — command parse proof (charge-0 dry-run, no compile)

`cabal build all --enable-tests -O0 --dry-run -v1` exit 0 with
`kelgroups-0.1.0.0 (test:invariants)` in plan (quoted in freeze notes) —
the exact frozen M1 leg command compiles the suite carrying the M1 splice.
M1 splice text, preconditions (A6 + boundary imports, re-confirmed on GREEN
demo bytes at v4 freeze), and kill UNCHANGED.

## F2 — synthetic mid-line proof + committed H1 count

H1 count==1 on committed `Fold.hs` (re-confirmed). New splice on synthetic
`in  if isMemberInView signer view` + `then`-next-line bytes: result
`in  if True` + intact `then` line, zero comment text emitted, diff exactly
1 changed line (1c1). No line-start assumption anywhere in the new splice.

## F3 — real-region copy proof (committed `Fold.hs:315-352`)

New skip awk on the exact region copy: equation block replaced by
marker+stub (count==1); pre-region doc comment, split signature, blank
line, `{- | Enact…` block (opener/continuations/closer), and `tryEnactBase`
head ALL byte-identical; no stray fragment. Single equation re-confirmed
on committed bytes (no multi-equation overlap hazard).

## F4 — construction review + committed names (recorded judgment)

`unsafeSetAppStateSTM :: TVar (GroupState s) -> GroupState s -> STM ()` /
`unsafeSetAppStateSTM var newGs = writeTVar var newGs`, rewired call
`unsafeSetAppStateSTM (stateVar store) gs`: TVar/GroupState/writeTVar
present in committed `Store.hs` imports; STM via the (unchanged) import
splice; `gs` bound at `Store.hs:604` enclosing the 627 STM write; zero
`appFold` mentions in splice+rewire (both NOTE-011 defects closed by
construction). Export/import splices, count==4, kill UNCHANGED. Kill class:
persisted row vs stuck live state vs replay → authority witness fails; a
compile failure at re-proof is a FAILED row, never a pass.

## Re-confirmed standing anchors (committed bytes)

M4: `^data BaseMutation` ×1, `ChangeRolesVoted` arm ×1, `AdmitMemberVoted`
absent; M5: extraction m5a=1/m5b=0 on committed bytes, From-arm excluded,
dirty-hash transfer holds vs committed hash (recorded at freeze); M1 demo
shapes (name lines, flattened signature, boundary mentions, freshness);
M2/M3/M6 preconditions per above; mutant-residue markers absent
(`_m1_boundarySeparates`, `AdmitMemberVoted`, `unsafeSetAppStateSTM`,
`MUTANT-M[1-6]` all 0 hits); Trivial/cabal/Main untouched (diff file list
== the authorized 13 implementation files).

## Leg-4 REGSELF note

Owner self-count `3-3-3-6-3-4eq23` stands by for mechanical adjudication:
leg-4's registered==file cross-check decides (fail closed); any mismatch is
diagnosed, never waived.

## Freeze refs

Gate v7 normalized + full hashes + `gate-v7.sh.backup`: STATUS NOTE
GATE-FROZEN-v7. FROZEN_BASE RED `570fe4a…` unchanged (ancestry covers
`84a2dae`).
