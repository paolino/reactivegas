# MUTANT-BINDING-570fe4a-v2 — header correction (gate bytes refrozen, anchors stand)

Ticket owner `t28-app-api`. Supersedes NOTHING technically: v1
(`MUTANT-BINDING-570fe4a.md`, preserved unmodified) stands in full; this v2
rebinds the corrected gate freeze (NOTE-005 §1, child Q-002). No anchor,
extraction, dry-run, or RED-byte fact changes — proven below by diff.

## Corrected gate freeze (quoted, eye-verified 2026-09-05)

- Defective v4 (preserved in `handoffs/gate-v4.sh.backup`, full
  `1c19f172…`, DO NOT TOUCH): header `e358cc38…` (v3 value carried into the
  v4 draft; placeholder-replace no-op'd — embed post-condition missing, now
  a standing procedural rule), blanked-normalized `ad7826a5…`.
- Corrected file (worktree `gate.sh` + `handoffs/gate-v4b.sh.backup`, full
  `831138ba788e52f39e7e9761aa25dfeaa239990c2409edf53e844d1ed19cda9e`,
  hashes match): header `ad7826a5…`.
- Coherence triple (the demonstration, not self-reference — blanked bytes
  are invariant under the header write): blanked-BEFORE `ad7826a5…` ==
  blanked-AFTER `ad7826a5…` == header `ad7826a5…`.
- Nothing-else-changed proof: `diff gate-v4.sh.backup gate.sh` == EXACTLY
  line 31 (the header), quoted in freeze notes. All v1 anchor programs,
  preconditions, kill checks, and extraction logic are byte-identical.
- Leg-2 by eye: HEAD `570fe4a…` == FROZEN_BASE `570fe4a…`; header ==
  blanked; `merge-base --is-ancestor` DESCENDS-OK. PASSES.
- FROZEN_BASE stays RED `570fe4a…` (pinned origin/main `368b596…`
  alongside). Pinned instruments unchanged (v4 header pins stand).

## Standing confirmations (carried from v1, unaffected by a header-line change)

M1 flattened preconditions + boundary-use splice; M4 renamed-arm program +
freshness; M6 export/import/backdoor/rewire + arity-correct signature;
extraction actuals 3/3/3/3/3/4 = 19 with zero warnings; freshness baselines;
D5 H7 shape; known-vacuous R-a/b/c directed to GREEN. TBB production anchors
(H1–H5) bind at GREEN submission via ANCHOR-ATTEST + leg-5 preconditions
(fail closed). D5 founding-aggregate RATIFIED per NOTE-005 §2 (r5
provisional qualifier superseded; report line: founding param + table +
equality refusal under Validate.lean founding rule + NOTE-004 item 4).
