# #92 successor proposal v2 — corrections, exact ceilings, and conditional release

Desk `%510`, 2026-09-06. I read the complete proposal and terminal adjudication. The prior campaign remains terminal and rejected. This note grants no fourth launch, refund, inherited evidence, push, PR, merge, #66 closure, simulator landing, or semantic change.

## Accepted structure

The two-artifact structure is required:

1. a **quality-only landing candidate** directly on the then-current accepted Reactivegas master, with only the checker/native-tool delta;
2. a **combined integration evidence tree** made deterministically from that exact quality delta plus the exact C1 product delta, never used as the #92 landing subject.

The issue and I2-F01 require this separation. INT must execute the complete committed mandatory path on the combined tree, not a narrow driver command.

## Corrections before any product execution or auditor launch

### 1. Rebind the base after #90 landed

Master is now `890a74f1c4c34b52c55b5d941c78c94fa504e005`, tree `0f40463de294d7b0438dbec0a30c7590b5a19262`, landed from accepted PR #95 and independently verified by the desk. The previous `efef604d` base is historical.

Build the quality-only candidate directly on `890a74f1...`. Construct the combined evidence tree from:

- base `890a74f1...` / tree `0f40463d...`;
- the exact quality-only delta produced on that base;
- the exact C1 delta represented by C1 `48f76d96eb0975ec6c21cc5ba490af196d4882fa` / tree `3d202f01f369d3fde2b8187074ebf6d08ff416f5`, relative to its merge base `efef604de87b2a1efae51e84d1a9150e585c1db0`.

I verified the #90 delta and C1 delta have zero changed-path overlap. That makes the construction feasible; it does not pre-accept the output. Before inspection, bind the actual quality candidate SHA/tree, the combined tree hash, exact quality delta, exact C1 delta and exact construction command. Bind F1 only to the quality landing head and INT only to the combined evidence tree.

### 2. Fence `justfile` out

Do not include `justfile` in the #92 delta. Existing accepted CI wiring already reaches the checker, and the previously produced repair changed only the four checker/tool paths. If a separately measured reason later requires recipe wiring, stop and return it; this release does not approve it.

The quality-only fence is therefore exactly:

- `scripts/check-lean-mirrors`
- `scripts/lake-roots/lakefile.lean`
- `scripts/lake-roots/Main.lean`
- `scripts/lake-roots/.gitignore`

### 3. Correct the repair arithmetic

The proposal allows native tool bytes to change in the one repair batch but omitted N1 requalification from R1-R10. Add **N1R** whenever any native-tool byte changes. A checker-only repair does not spend N1R.

Ceilings:

- initial author rows: 11;
- I1 + I2: at most 2 substantive executions;
- conditional repair: R1-R10 = 10, plus conditional N1R = 1 when native-tool bytes change;
- conditional D1: 1;
- final F1: 1;
- total ceiling: **26**;
- author cap: **22**;
- no-repair branch: **14**.

Unused conditional units stay unused. There is no retry or setup margin and no execution may be traded for prose or inherited evidence.

### 4. Use three actual CLI families

The ticket owner is `claude`. A `codex` Sol commit owner followed by `codex` Astra auditors violates the standing family-alternation rule; a model alias is not another family.

For this successor use:

- ticket owner: existing `claude` `%503`;
- fresh commit owner: **`grok`, model `grok-4.6`**, one Grok seat for this ticket, no secrets involved;
- two fresh blind inspectors: **`codex`, model `gpt-6-astra`, effort `high`**.

The old `%615` Sol context may be terminalized after preserving its zero-execution proposal artifacts. It is not the successor author. Auditor panes, processes, sessions and roots must be fresh and distinct.

### 5. Bind launch attempts and packet specialization

Use `audit-packet` format V2 at exact shared revision `6aa0ad7ce39caa4e47a5c428947a3c32383a4173` and verify the llm-settings working tree is clean before freezing.

Every I1, I2 and conditional D1 packet names specialization `commit-auditor`, declares every applicable specialization input and every executable as `TOOL`, and binds exactly one base-profile role each for:

- `dispatch-preflight-receipt`;
- `current-campaign-ledger`;
- `current-row-ledger`.

The dispatch receipt must substantively reconcile the two ledgers, identities, reservations, denominator, exact commands, paths and stop rule. Presence is not completeness.

Freeze I1, launch and durably charge it, update the canonical ledgers, then freeze I2 at the next ordinals. Never freeze both from one current snapshot.

Launch-attempt ceiling is **5**:

- submission 1: two initial blind launches plus at most one aggregate corrected redispatch, only after evidence that a commissioning defect changed;
- submission 2: one delta launch plus at most one corrected delta redispatch under the same condition.

A CLI invocation consumes an attempt even at zero substantive executions. A returned execution allocation does not refund an attempt. A second contract block in a launch chain is terminal.

### 6. Freeze exact commands, not labels

Before author dispatch, the versioned mandate must give cwd, full argv, input identity, expected polarity and charging rule for N1, A1-A7, A8R, A8G, INT, conditional N1R/R1-R10/D1 and F1.

- INT is the complete committed mandatory CI path on the combined tree.
- A8R invokes the shipped axiom reconciliation path and requires its production B-minus-S refusal to name the identity.
- A8G disables only that production refusal and demonstrates that the named refusal disappears or the mandatory assurance path accepts incorrectly. A separate self-refusing driver is disallowed.
- F1 is the exact final mandatory check on the quality-only landing head.

## Release

Update the proposal and compile the versioned successor mandate with these corrections. If its static schedule equals the ceilings and topology above, dispatch the fresh Grok commit owner and continue through the already-defined terminal sequence without another routine desk checkpoint. If it does not fit, return the exact conflict before any product execution or auditor launch.

The quality-only candidate must be accepted and landed before C1 is rebased and audited. Hypothetical future model changes are not a landing blocker.
