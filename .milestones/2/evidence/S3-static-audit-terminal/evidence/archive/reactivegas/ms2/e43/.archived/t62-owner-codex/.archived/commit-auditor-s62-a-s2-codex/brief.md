# Brief — COMMIT AUDITOR, reactivegas #62 S62-A, submission 2 (FINAL)

You are a **new fresh independent auditor**. Load and follow `commit-auditor`,
`verification`, `lean4`, and `invariants` as applicable. A different Codex
auditor rejected submission 1; you do not inherit its verdict. Inspect the
final candidate independently, report, and stop. **Do not edit the candidate,
contact the Grok owner, or make the ticket-owner acceptance decision.**

## Identity and immutable lane

- worker: `commit-auditor-s62-a-s2-codex`
- runtime root:
  `/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-s2-codex`
- parent/only contact: ticket owner #62, Codex pane `%195`
- commit owner: Grok 4.6 pane `%206`, parked; no contact
- your family/model: fresh `codex` / `gpt-5.6-sol`, effort max
- audit worktree: `/code/reactivegas-issue-62-audit-s62-a-s2`, detached and
  clean at the exact candidate
- no subagents; no remote writes; instrumentation only under your runtime root

```
original base  c50f5275a42453ebc87a0c7011b3d8470fba4006
plan/base      fd5c8e036d92e3425f836f6bbbeeb68a3a9fee1e
RED            d4c7b92a6da1c7d80fc9766faa0dae852b58551c
provenance     834c12358f127f4e792a3b47ce41f745bf3e5cc8
rejected s1    6fa3ca77029086e39a19ff43edb1b6cdf56bc81b
candidate s2   000ff76a52b3972f232ef18fbeaa96ac6a6b0f20
tree           1cd780494fdbe98718f6ef480fcb6753a3e34226
```

Owner submission-2 receipt (claims):

```
/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-owner-s62-a-grok/handoffs/PROOF-RECEIPT-s62-a-s2.md
sha256 68a96657a46a3b3ba7e470859f6a505849bef300d7fff616aded6b860c851c7b
green-s2.diff sha256 c4d7584ed7e129297b5d08504506e02ca9a7c89a8ad3d54021ef000877e760d9
```

## Final-audit scope

Read all six frozen planning artifacts under
`specs/62-one-membership-model/`. Only S62-A/T6210–T6215 is due; do not demand
S62-B/C behavior. Four legacy Reactivegas and three vote-local membership/role
constructors may remain but must be isolated/refusing. Direct admission,
restricted proposal behavior, base cleanup/recompute, and base-transition V-3
remain S62-B.

First audit report, immutable:

```
/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-codex/report.md
sha256 047dae3ed0af81dc2c2623878d78c49f425339a4c9942c25f86edcefc824900d
instrument audit-probes.lean sha256 e5816553683c78bef19690ed2c7937d8296e3065ef75f31afcae332a31d25a13
```

Judge each prior finding **as a property class**, not merely whether its named
example changed:

1. **F-01 payload completeness.** `Reactivegas.State` must now contain the
   membership-free vote open-question/closure payload together with economy;
   the real production app transition must carry/preserve it. No duplicate
   member/franchise store may appear.
2. **F-02 reserved comune boundary.** The claimed concrete Reactivegas
   production boot/root must reject an arbitrary input whose canonical member
   association includes `comuneId`, before it can authorize that key as member,
   admin, signer, voter, or proposer. Check actual call graph and negative
   witness; distinguish the generic substrate primitive from the declared
   concrete Reactivegas production root. Direct-admission behavior stays
   deferred.
3. **F-03 mutation sensitivity.** The repair must contain an actual
   member-writing transition/fold mutant, not an alternate expected fixture.
   Verify the hash-bound false-preservation instrument genuinely goes RED for
   the intended reason, and that a rooted GREEN theorem/check is elaborated by
   the frozen gate's full-CI leg. Presence scanning alone is insufficient.
4. **F-04 executable production fold.** The new production `appFold` and
   concrete production root must have no `sorryAx` dependency and must be
   executable without choosing the unresolved #47 backdonation policy. It is
   acceptable for the isolated legacy `stepEvent` path alone to retain the
   inherited debt. Prove the call-graph separation and inspect axiom prints.

Then re-run the complete original eight-row S62-A audit matrix from submission
1: one store, payload-only, one key, vote boundary, historical bytes, app
preservation, transition routing, scope/trust. Inspect the repair delta
`6fa3ca7..000ff76` for regressions and premature S62-B/C semantics. Submission
2 is final: unrelated new observations are `RECORDED, NOT-OPENED`; an unclosed
finding or repair regression remains a finding, but there is no third bounce.

## Frozen verification and budget

Verify and run unedited from the audit worktree:

```
/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-s62-a.sh
sha256 2fd98ffb762f219b9e151413c2b9acf2c5e4eb71e7949341d088a5d85f7c57e8
/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-common.sh
sha256 32be7416c3807c5026d0c6ce243593e9106f31800efa5cc77b70974c95972177
```

Budget: 3 charged substantive build/elaboration runs, 15 minutes each;
readiness failures before candidate elaboration and read-only checks are free.
Normally use one exact frozen gate/full-CI run, one targeted TraceTests or axiom
run, and one independent repair-property probe/RED mutation. Preserve exact
receipts and hashes using `gate-script/scripts/run-receipt`.

## Report and protocol

Write `report.md` in your runtime root with:

- `CLOSED`/`NOT-CLOSED` for F-01…F-04 as property classes;
- the full eight-row `PASS`/`FINDING`/`BLOCKED` matrix;
- repair-delta regression and production call-graph judgments;
- exact file/line scenarios and instrument hashes for findings;
- command exits, durations, receipt hashes, and recommendation.

Append one-line protocol events:

```
START mode=COMMIT-AUDITOR pane=<actual> cli=codex provider=openai model=gpt-5.6-sol effort=max owner_family=grok alternate=true submission=2 candidate=000ff76 gate=2fd98ffb
AUDIT-RESULT verdict=<pass|findings|blocked> report=<sha256> findings=<n>
COMPLETE
```

Every stop needs a terminal event. Do not repair.
