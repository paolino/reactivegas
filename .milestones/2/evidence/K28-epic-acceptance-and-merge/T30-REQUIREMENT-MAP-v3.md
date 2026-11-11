# #30 requirement → accepted-evidence / deliverable / dependency map v3

Epic owner, 2026-09-06. SUPERSEDES v2 map (retained unmodified as history;
v1 draft likewise retained). Corrections in this version: (1) accepted
source rebound to Reactivegas `3590c001` with zero-diff comparison below;
(2) ruled proposer lifecycle SPLIT from unruled refusal policy (R30-5 /
R30-10 / R30-13 corrected — the v2 conflation is withdrawn, history kept);
(3) cross-repository links fully qualified everywhere. Frozen inputs:
accepted Lean @ `3590c001` + later rulings (V-2 settled-unlanded,
paolino/reactivegas#68 OPEN; pledge agency; NOTE-016/A-Q001 composition),
S28 LANDED (`933e385d`), D1/EPIC43/ASSENSO packets, R3.1 contract.

## Accepted-source binding (verified 2026-09-06, not asserted)

`git diff 4a6cd87..3590c001 -- lean/KelGroups/Vote/ lean/KelGroups/
Integration.lean lean/KelGroups/State.lean lean/KelGroups/Validate.lean`
= EMPTY (the `3590c001` delta is #66 S2R axiom gate + #87 corpora only).
All Vote-subtree snippets quoted from `4a6cd87` remain CURRENT for the
relevant extent. Re-verify at #30 freeze; later landings rebind explicitly.

## Vocabulary triad (binding — unchanged from v2)

Event-validation refusals EXIST; negative verdicts EXIST and stay
deliverable; EXPIRY does not exist and is unruled (R-54). Threshold is a
parameter; exhibits are not defaults.

## Rows

| id | requirement (reconciled) | accepted evidence | deliverable / owner | depends on |
|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event`, `validateVoteEvent` openQuestion arm | kelgroups Haskell #30 | none |
| R30-2 | cast assent/dissent: one-position placement, idempotent re-cast, switch moves voter | `placeBallot`, `effectedState` | kelgroups Haskell #30 | none |
| R30-3 | sweepClosures same-step close + appended ClosureRecord (tally + franchiseChange); retention, never silent drop; NO expiry | `sweepClosures`/`sweepStep`, append-only `closed`, R-51/R-61/R-54 | kelgroups Haskell #30 | none |
| R30-4 | verdictOf: collective threshold @ current franchise (legacy order) + permission designee arm (never tally) | `verdictOf`, R-46/R-49/R-50/R-64 | kelgroups Haskell #30 | none |
| R30-5 | refusal identities: `notResponsabile` + `questionNotFound` PRODUCED now. `notDesignee` + `notProposer` are DECLARED with zero Slice-A construction sites — an unruled INTENTION, not a scheduled dependency, not a Slice-B promise by this lane (CORRECTED v3: v2's 'future Lean-owned Slice-B work' label withdrawn — paolino/reactivegas#81 explicitly excludes both as unruled) | `VoteError` + 3-arm `validateVoteEvent`; declaration-site note; paolino/reactivegas#81 out-of-scope section | Haskell carries vocabulary now; NO producing semantics scheduled anywhere | NONE (unruled; current record-but-don't-decide behavior is the preserved boundary) |
| R30-6 | franchise from canonical GroupView every evaluation (no local copy) | `franchise`/`franchiseSize`, R62-11 | kelgroups Haskell #30 | none |
| R30-7 | negative-verdict delivery observable at the boundary | ClosureRecord verdict+cause; S28 hook surface | interface #30; consumption → paolino/reactivegas#76 | #76 for effect |
| R30-8 | vote routing separated from base admission | T6222 removal; non-admitting `pendingBase`; S28 direct-only | boundary definition #30 | none |
| R30-9 | approve path matches LANDED base rule (majority + proposer selfbar) | V-2 ruling (settled, paolino/reactivegas#68 UNLANDED) | REBIND after #68; freeze #30 on current base meanwhile | #68 (boundary: `tryEnactBase` majority + proposer rules) |
| R30-10 | RULED proposer lifecycle ONLY: proposer renounce ⇒ negative `renounced` closure; proposer departure ⇒ negative `proposerDeparted` closure atomically via post-base hook; causes retained + distinguished; continuation/refund downstream (CORRECTED v3: v2's 'renounce-by-nonproposer refusal → #81' WITHDRAWN) | V-5 ruling; `renounce` event + carried `renounced`/`proposerDeparted` causes; paolino/reactivegas#81 scope §1–§3 | kelgroups exposes the voted-side mechanism; closure CONTENT → paolino/reactivegas#81 (which depends on #76 for continuation/refund) | #81 for content; #76 for refund |
| R30-10U | UNRULED refusal policy (explicitly NOT scheduled): non-proposer `renounce` reading; non-designee ballot refusal. Current record-but-don't-decide behavior PRESERVED as the boundary; open operator questions in both directions | paolino/reactivegas#81 out-of-scope section (quotes V-5 trigger naming only the proposer; permission text deciding only the designee) | NONE — no ticket, no promise, no dependency edge | — |
| R30-11 | verdict → economic effect (grant/deny/backdonate, target/polarity/provenance/one-use) | NOTE-016/A-Q001 (ruled, wire missing) | paolino/reactivegas#76 (evidence exposed only) | #76 |
| R30-12 | PureScript client proposing/voting app questions (API + minimal views, adapt-only) | `kelgroups-client` package (no Reactivegas UI / wasm there) | kelgroups #30 additions | none in team |
| R30-13 | Lean proof obligations | existing Vote proofs = evidence; Slice-B PRODUCERS only where RULED (R30-10a content); unruled identities (R30-10U) have NO proof obligation anywhere | LEAN-OWNED (Reactivegas lanes); zero unilateral kelgroups Lean edits | Lean lanes where ruled; none where unruled |
| R30-14 | denial/dissent observable for L2-style consumers | negative closure delivery | interface #30; consumption #76 | #76 |
| R30-X | NON-GOALS: expiry; theta default; votable admission; second store/fold; Reactivegas browser UI + wasm (paolino/reactivegas#84, portable-core lane); Reactivegas economics in kelgroups | R-54; threshold-param; S28; R9c/R11; ASSENSO gap | none (guarded) | — |

## #29 owned remainder (unchanged dispositions, explicit)

Runnable demo (follow-on scoped ticket post-#30; implementation then,
publication separately gated). v2 major release (flow post-demo; publication
separately gated). Downstream notes (doc handoff with #73 lane on S30
landing). #29 + paolino/reactivegas#73 OPEN. Delivery tickets filed
separately per desk filing disposition (see receipt).
