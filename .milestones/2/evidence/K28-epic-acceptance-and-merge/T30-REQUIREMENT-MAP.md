# #30 requirement → accepted-evidence / deliverable / dependency map

Epic owner, 2026-09-06. Frozen inputs: Reactivegas accepted Lean
(`master 4a6cd87` + later rulings: V-2 settled-unlanded #68 OPEN, pledge
agency, NOTE-016/A-Q001 composition), S28 LANDED (`933e385d`), D1/EPIC43/
ASSENSO packets, R3.1 contract. Current kelgroups #30 body (2026-08-25) is
STALE — corrected per-row below, then in the issue body itself.

## Vocabulary disambiguation (binding for this map)

- EVENT-validation refusal: a signed event rejected at the boundary
  (`NotAMember`, `NotAnAdmin`, `MemberAlreadyExists`, `ProposalNotFound`,
  `AlreadyApproved`, `ReservedKey`, `notResponsabile`, `questionNotFound`,
  …). EXISTS on every route. Never removed.
- Negative VERDICT: dissent reaching threshold → `.negative` closure record
  delivered to observers. EXISTS (vote machine) and must stay deliverable.
- EXPIRY: time-based removal/closure of open questions. DOES NOT EXIST
  anywhere (R-54: no clock, no time-like field — expiry unprovable and
  unruled). Any ticket text saying otherwise is stale.
- THETA/threshold: a PARAMETER everywhere (`Threshold := Nat → Nat`);
  `legacyThreshold`/`zeroThreshold` are exhibits, never defaults. No shipped
  default exists; none is selected here.

## Rows

| id | requirement (reconciled) | accepted evidence | deliverable / owner | depends on |
|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event`, `validateVoteEvent` openQuestion arm, `isResponsabile`-via-view | kelgroups Haskell #30 | none (accepted) |
| R30-2 | cast assent/dissent: one-position placement, idempotent re-cast, switch moves voter | `placeBallot`, `effectedState` cast arm | kelgroups Haskell #30 | none |
| R30-3 | sweepClosures: same-step close of every decided question + appended ClosureRecord (tally + franchiseChange causes); retention, never silent drop; NO expiry | `sweepClosures`/`sweepStep`, `VoteState.closed` append-only, R-51/R-61/R-54 | kelgroups Haskell #30 | none |
| R30-4 | verdictOf: collective arm (threshold @ current franchise, legacy order/symmetry) + permission arm (designee ballot only, never tally) | `verdictOf`, R-46/R-49/R-50/R-64 | kelgroups Haskell #30 | none |
| R30-5 | refusal identities: `notResponsabile` + `questionNotFound` produced now; `notDesignee`/`notProposer` DECLARED, zero Slice-A construction sites | `VoteError` + `validateVoteEvent` (3 arms); declaration-site note (Slice-B forward) | Haskell carries vocabulary now; PRODUCING semantics = Lean-owned Slice-B work (Reactivegas Lean lanes), kelgroups makes NO unilateral Lean change | Lean lanes (production); kelgroups implements frozen spec |
| R30-6 | franchise read from canonical GroupView every evaluation (no local/stale copy) | `franchise`/`franchiseSize`/`isResponsabile`, R62-11 | kelgroups Haskell #30 | none |
| R30-7 | negative-verdict delivery observable by the app (dissent → `.negative` record through the boundary) | ClosureRecord verdict+cause; S28 `commitBaseChange`/hook composition surface | interface #30; economic CONSUMPTION → #76 | #76 for effect |
| R30-8 | vote routing separated from base admission (no votable admission; vote machine holds no membership) | T6222 removal, `pendingBase` non-admitting shape, S28 direct-only | boundary definition #30 | none |
| R30-9 | approve path matches LANDED base approval rule (majority + proposer selfbar) | V-2 ruling (settled, #68 UNLANDED) | REBIND after #68 lands; freeze #30 against current accepted base meanwhile | #68 landing (concrete boundary: `tryEnactBase` majority + proposer rules) |
| R30-10 | renounce-by-nonproposer refusal + proposerDeparted/renounced closing + retention/refund continuation | carried constructors (`renounce` no-op in Slice A; `closeProposerQuestions` arrives Slice B) | #81 (kelgroups exposes vocabulary + closure records only) | #81; refund continuation → #76 |
| R30-11 | verdict → economic effect (grant/deny/backdonate, target/polarity/provenance/one-use) | NOTE-016/A-Q001 rulings (requirement ruled; wire missing) | #76 Reactivegas-side (kelgroups exposes ClosureRecord evidence only) | #76 |
| R30-12 | PureScript client: propose/vote app questions (API + minimal views) | `kelgroups-client` package exists (keys/transport/shell/bundle); NO Reactivegas UI, NO core.wasm integration there | kelgroups #30 ADDITIONS to client package (adapt-only discipline as #28) | none inside team |
| R30-13 | Lean proof obligations for the above | Vote subtree proofs exist = evidence; Slice-B producers = missing semantics | LEAN-OWNED (Reactivegas lanes); kelgroups converges to frozen Lean, zero unilateral Lean edits | Lean lanes |
| R30-14 | denial/dissent observable for L2-style consumers (refunds on dissent) | negative closure delivery (R30-7) | interface #30; consumption #76 | #76 |
| R30-X | NON-GOALS (frozen): expiry mechanism; shipped theta default; votable admission; second membership/fold; Reactivegas browser UI + wasm core (#84/#82); Reactivegas economics in kelgroups | R-54; threshold-param; S28 direct-only; R9c/R11 corrections; ASSENSO gap | none (guarded) | — |

## #29 owned remainder (delivery containers, authority disposition)

- Runnable demo (v2 API + app-scoped proposal for a user): container =
  follow-on SCOPED ticket AFTER #30 lands (bounded contract: runnable demo
  gating an app action on a majority verdict, both outcomes, replay
  equality). Authority: local implementation authorized then; publication
  separately gated. NOT FILED now (unscoped until #30 interface frozen;
  #29 epic body already tracks the row — no duplicate needed).
- v2 major release (`feat!:` via release-please): container = release flow
  after demo; authority: publication/release separately gated (desk). NOT
  FILED now (same reason).
- Downstream consumption notes: container = doc handoff with the #73 lane
  on S30 landing; no publication. NOT FILED now.
- #29 + Reactivegas#73 stay OPEN (verified 2026-09-06).
