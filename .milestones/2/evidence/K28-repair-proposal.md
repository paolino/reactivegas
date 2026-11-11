# RECUT-PROPOSAL-S28-R1 — successor repair campaign (PROPOSAL ONLY, nothing granted)

Ticket owner `t28-app-api`. Authority: NOTE-023 (desk terminal-findings via
epic — author this; epic accepts; desk rules). Status: PROPOSED (no ticket
created, no branch cut, no budget consumed, no work dispatched by this
document). One-submission mandate + owner 34/34 spent = NO implementation
dispatch under the current campaign. Residual auditor 3/12+17/24 is NOT a
repair grant and is NOT carried over.

## 1. Successor identity (proposed)

- Campaign id: `S28-R1` (substrate repair 1), child of `paolino/kelgroups#29`
  (epic assigns the real ticket number).
- Branch (proposed): `fix/28-r1-conservation-effect` FROM `84a2dae…`
  (keeps all S28-1 work byte-identical; reviewable RED..new diff; no rebase
  risk, no re-derivation). Alternative considered and NOT recommended:
  branching from origin/main + cherry-picking (cleaner history, but
  re-derivation risk + evidence invalidation).
- Base for the new envelope: the repaired candidate's own parent chain
  (frozen at dispatch per lifecycle); carried inputs below are pinned to
  current SHAs for traceability, not as mutable defaults.

## 2. Carried inputs (exact, pinned)

- Accepted base: `368b596fef0b6d393c2ac7afc631d236c55d86d1`.
- Current candidate (UNACCEPTED — starting point, not approved base):
  `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2`, tree
  `6f24bb30e3b20e27a71ec4388d36c155cfa8614c`, worktree clean.
- Frozen gate family G28-1 v8 (`f5796d1e…`/`7a7a99e3…`) to be re-cut to the
  new base per lifecycle (new version + hash + falsification — part of the
  proposed budget, not free).

## 3. FULL unaccepted subject (retained entire — never narrowed)

- R1/R3/R5 OPEN with stated limits (value coverage of canonical views,
  success-side hook payload, whole-KEL agreement incl. proposal/approval
  events; kills are evidence, not verdicts).
- R2/R4/R6 BLOCKED: F1 (lost update under overlapping accepted appends)
  inside R2+R6; F2 (vacuous effect correlate) inside R4.
- Reliances with unassessed limits: INV-HISTORICAL-FOLD (unjudged beyond
  suites), INV-CESR-KEY-VALIDITY (unjudged beyond key tests; new route
  correctly key-format-free per Lean), INV-STORE-STM-DISCIPLINE (F1
  inside), INV-MAJORITY-FRANCHISE (unjudged beyond partial evidence),
  INV-HISTORICAL-APPFOLD-SHAPE (pass-as-scoped only).
- NO narrowing to two isolated tests on an assumed-good base (`84a2dae`
  was never accepted; every unassessed row stays required).

## 4. Supported repairs (fences, not patches — mechanism is the AUTHOR role's
choice when commissioned)

- F1 acceptance binds, coherently: successful concurrent calls + durable
  event/order/replay + live state + event-count (the 4-pair witness is
  input, not exhaustive scheduling/crash proof), PRESERVING refusal and
  SQL-error behavior (no masking by weakening success returns or by
  stale-snapshot comparison).
- F2 acceptance observes resulting membership over present/absent targets
  (constructor enumeration + compile kills never substitute for effect
  evidence; raw instruments are inputs, never a shipped fix by copying).
- Fence: `Store.hs` success-path atomicity rework (F1) + effect-observing
  test properties incl. reopen/replay (F2) + gate/contract re-freeze to the
  new base; E1/E2-class test-helper adaptations if mechanically forced
  (same class, disclosed). EXCLUDED: client/Lean/Trivial/historical-beyond-
  suites/publication. Existing E1×4+E2 bounds carry unless re-ruled.

## 5. EXPLICIT new budget + submission plan (PROPOSED for ruling)

Mirroring measured single-pass actuals: owner 16 builds / 24 targeted (RED 4 + GREEN-envelope 9 + SLIM-final 3; dev via narrowed probes ≤10 within 24) +
auditor 12 builds / 24 targeted (cold 1 + envelope 9 + ≤2 discretionary;
probes narrowed-stated) + ONE submission. Total proposed envelope: 28
builds + ≤48 probes across BOTH seats (each cap separate, no mixing).
Acceptance gate (proposed, desk/epic finalize): re-cut frozen gate exit 0
with six kills quoted + INDEPENDENT auditor verdict PASS with zero blocking
findings + identical-envelope SLIM + merge authority at desk. Independent
final acceptance defined AT the actual resulting SHA (to be recorded then —
never pre-declared).

## 6. What this proposal is not

No implementation dispatch, no auditor dispatch, no issue filing, no merge,
no provider workaround, no narrowing, no verdict language about any future
candidate. Epic accepts/disposes; desk rules budget/merge.
