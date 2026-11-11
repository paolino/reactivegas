# MUTANT-BINDING-570fe4a-v6 — M2 rebind to committed bytes (addendum record)

Ticket owner `t28-app-api`. Lineage: v1 (RED bindings) + v2 (header
correction) + v3 (Q-004 reconciliation) + v4 (Q-005 M5 rebind) + v5 (Q-006
rebinds) preserved unmodified; this v6 binds the M2 import-step repair
(Q-007/NOTE-012) to actual committed `84a2dae` bytes, versioned/hashed
BEFORE any execution. All other anchors stand as previously bound and
proven (M1/M3/M4/M5 programs IDENTICAL v7→v8 — proven by gate diff showing
ONLY M2-block + version hunks; their GREEN leg-5 executions + kills stand
as evidence, re-established by the final full gate per NOTE-010 §2).

## M2 rebind (committed bytes)

- Guard: `Fold.hs:443` (`in  if isMemberInView signer view`, `then` split
  next line); H1 count==1 file-wide (re-confirmed).
- Import line: `Fold.hs:62` (`    , isMemberInView`, LAST entry of the
  `KelGroups.Types` import block — verified block context; deletion leaves
  `( GroupView … , Role` + `)` valid, no dangling comma); count==1
  (NOTE-012 static-uniqueness rule satisfied).
- No other use sites file-wide (post-splice full-excision target state).
- Dry-run on `/tmp` copy with EXACT frozen splice bytes: `isMemberInView`
  count 0 after; diff exactly 1 insertion / 2 deletions (`62d61` +
  `443c442`); `in  if True` + intact `then`; fourmolu-parse zero errors.
- Kill UNCHANGED (rejecting-step witness quote; compile failure at re-proof
  = FAILED row, never a pass). Freshness: guard unmutated + import present
  pre-splice (leg-5 preconditions); v8 emits no comment markers by design
  (diff-shape checks instead).

## Standing anchors (re-confirmed, unchanged programs)

M1 (A6 + boundary imports on GREEN demo; `--enable-tests` plan proof);
M3 (flattened conjuncts + single equation + comment-tolerant skip);
M4 (block + `ChangeRolesVoted` arm + freshness); M5 (extraction m5a=1/m5b=0
on committed bytes, From-arm excluded, dirty-hash transfer holds); M6
(export/import/success-write anchors + TVar/GroupState imports + gs
binding + whole-state variant review). Mutant-residue markers absent
repo-wide. Diff RED..HEAD == exactly the authorized 13 files
(Trivial/cabal/Main untouched).

## Leg-4 REGSELF note

Owner self-count vs extractor per-row variance noted previously; leg-4's
registered==file cross-check adjudicates mechanically at runtime
(fail closed). No pre-freeze change.

## Freeze refs

Gate v8 normalized + full hashes + `gate-v8.sh.backup`: STATUS NOTE
GATE-FROZEN-v8. FROZEN_BASE RED `570fe4a…` unchanged (ancestry covers
`84a2dae`).
