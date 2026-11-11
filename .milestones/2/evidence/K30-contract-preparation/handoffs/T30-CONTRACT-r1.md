# T30-CONTRACT-r1 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout).

- Brief: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/brief.md`
  sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`
  (quoted per brief; START journaled in `STATUS.md`).
- Operative base: kelgroups `main` @
  `933e385df2f2a251bb54a08bb7663f0d41fafb64` (verified by read-only
  `rev-parse`; S28 LANDED via PR#32 guarded squash; post-merge CI+Release
  SUCCESS; #28 CLOSED post-scope-verdict, zero comments).
- Accepted Lean: Reactivegas @
  `3590c0015b84fd58004bf6fb44dd18b107304c48`; zero-diff RE-VERIFIED at this
  freeze by read-only command
  `git diff 4a6cd87..3590c001 -- lean/KelGroups/Vote/
  lean/KelGroups/Integration.lean lean/KelGroups/State.lean
  lean/KelGroups/Validate.lean` = EMPTY (no output). The `3590c001` delta is
  #66-S2R axiom gate + #87 corpora only (per v3 map; consistent with empty
  extent diff). Later landings rebind explicitly; nothing here anticipates
  them.
- Supersedes: epic `handoffs/T30-MANDATE-v3.md` + `handoffs/T30-REQUIREMENT-
  MAP-v3.md` as the implementation-authorization contract. v1/v2 retained as
  history; where this contract differs from v3 it says so explicitly (§0).
  This contract ≠ the ticket owner's final immutable gate: the ticket owner
  authors that instrument at commissioning from this packet.
- Companion frozen map: `handoffs/T30-COMMAND-MAP-r1.md` (hash-bound below;
  the §7 table here and that file are the same frozen content).

## 0. Honesty corrections to v3 (binding, stated plainly)

1. **Mutant count / owner arithmetic.** v3 §2 lists TWELVE semicolon-separated
   mutant entries while fitting 8–10 (`16–18 → PROPOSED owner 18/24`). Under
   the written one-build-per-mutant rule twelve mutants make
   RED 2 + GREEN (cold 1 + full-test 1 + 12 + final-CI 1 = 15) + SLIM 3 = **20**,
   not 18. The `minus shared-step dedup` pairing is unproved (no proved
   pairing, no actual command), so it is WITHDRAWN as a fit basis. This
   contract freezes **12 mutants M1–M12** (§7, each paired to observed Lean
   equation sites + row witnesses) and proposes **owner 20/24** (§9). The
   18/24 figure was an estimate, never an allowance, and is superseded.
2. **Bare cross-repo links.** v3 map/mandate still contain bare `#76`/`#81`.
   Every reference in this contract is fully qualified
   (`paolino/reactivegas#76`, `paolino/reactivegas#81`,
   `paolino/reactivegas#68`, `paolino/reactivegas#84`,
   `paolino/reactivegas#82`, `paolino/reactivegas#73`,
   `paolino/reactivegas#75`); the ticket owner's compiled packet inherits
   this rule.
3. **Team fence.** v3 §3's "Muse owner pair" shorthand is superseded by the
   standing fence in this contract's brief: commit-owner seat Muse,
   auditor seat Codex-or-Grok (never Muse/GLM/Claude), `draft=NONE`. This
   contract commissions NEITHER seat (preparation-only); the fence binds at
   implementation authorization.
4. **v2 §0 corrections stand** (triad, client adapt-only inclusion, V-2
   concrete boundary, bottom-up-only envelope) and are not re-argued here.

## 1. Objective (one observable)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with its explicit cause) plus its closure record through the
**integrated** boundary — with refusals before durable effects and replay
equality. Test-only proving instance (as S28); the runnable user demo is the
#29 follow-on `paolino/kelgroups#33`, not this ticket. Threshold stays a
parameter in every verdict evaluation; exhibits are never defaults.

## 2. Scope — rows (FULL inherited #30 scope preserved; Terms compliance per row)

Accepted behaviours stay INTACT: this contract converges to the LANDED S28
interface (`Integration` surface, direct-only admission, sealed
`commitBaseChange`, `foldIntegrated`/`foldIntegratedFrom` shared step,
`openIntegratedKEL`/`appendIntegratedEvent` validate-then-append,
`GroupView` sole projection) and never redesigns it. `Trivial` stays
degenerate presence-only. No unilateral Lean edits. No shipped threshold
default. No expiry. No votable admission. No second store/fold.

| id | requirement (reconciled) | accepted evidence (Lean @3590c001) | deliverable / owner | depends on | Terms statement |
|---|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event.lean`: `openQuestion(questionId)(kind)`; `Vote/Validate.lean:57`: openQuestion arm (`isResponsabile signer view` else `notResponsabile`) | kelgroups Haskell #30: vote vocabulary + validation mirror | none | intact: converges to S28 `Integration` surface; no base-rule change |
| R30-2 | cast assent/dissent: one-position placement, idempotent re-cast, switch moves voter | `Vote/Fold.lean:53-56` `placeBallot` (insert-one-erase-other both arms); `effectedState` cast arm (`Fold.lean:95-100`); `Validate.lean:59-64` cast arm | kelgroups Haskell #30 | none | intact: placement semantics transcribed, not reinvented |
| R30-3 | sweepClosures same-step close + appended ClosureRecord (tally + franchiseChange); retention, never silent drop; NO expiry | `Vote/Fold.lean:65-76` `sweepStep`/`sweepClosures`; `Vote/State.lean` `closed` append-only + R-51/R-61; `State.lean` header: no clock/no time-like field (R-54 provable absence) | kelgroups Haskell #30 | none | later lifecycle content (§R30-10) neither implemented ahead of Lean nor marked permanently out — stated per row here and in §7 kills |
| R30-4 | verdictOf: collective threshold @ current franchise (legacy order) + permission designee arm (never tally) | `Vote/State.lean:82-96` `verdictOf` (collective: assents≥required then dissents≥required else open; permission: designee assent→positive, dissent→negative, else open); R-46/R-49/R-50/R-64 | kelgroups Haskell #30 | none | threshold is a parameter everywhere; `legacyThreshold`/`zeroThreshold` are exhibits, never defaults; no theta shipped |
| R30-5 | refusal identities: `notResponsabile` + `questionNotFound` PRODUCED now. `notDesignee` + `notProposer` DECLARED (`Vote/Validate.lean:41-42`) with zero Slice-A construction sites — an unruled INTENTION, not a scheduled dependency, not a Slice-B promise | `VoteError` 4 ctors + 3-arm `validateVoteEvent` (`Validate.lean:56-70`); `paolino/reactivegas#81` out-of-scope § (quotes V-5 trigger naming only the proposer; permission text deciding only the designee) | Haskell carries the 4-ctor vocabulary now; NO producing semantics for `notDesignee`/`notProposer` scheduled anywhere | NONE (unruled; current don't-produce boundary preserved: no promise, no dependency edge, no new refusals) | UNSCHEDULED preserved boundary, explicitly: no ticket, no promise, no edge |
| R30-6 | franchise from canonical GroupView every evaluation (no local copy) | `Vote/State.lean` `franchise`/`franchiseSize`/`isResponsabile` over `GroupView.admins`; R62-11; S28 `groupView` sole projection (`lib/KelGroups/State.hs`) | kelgroups Haskell #30 | none | intact: reads S28 `GroupView`, never a payload-local copy |
| R30-7 | negative-verdict delivery observable at the boundary | `ClosureRecord{verdict,cause}`; `sweepStep` negative arm; S28 hook surface (`commitBaseChange` post-base hook) | interface #30; consumption → `paolino/reactivegas#76` | `paolino/reactivegas#76` for effect only | #76 content neither implemented here ahead of Lean nor marked out of the eventual substrate contract |
| R30-8 | vote routing separated from base admission | T6222 removal (no membership event in `VoteEvent`); non-admitting `pendingBase` typed by `BaseMutation`; S28 direct-only (`DirectCommand` sole admission) | boundary definition #30 | none | intact: `BaseMutation` gains no admission constructor (M10 guards) |
| R30-9 | approve path matches LANDED base rule; V-2 rebind after landing | V-2 ruling settled (proposer opens at zero assents; proposer supplies none of the required assents; arithmetic `(n+1)/2` unchanged); `paolino/reactivegas#68` OPEN (guard change + proofs + proposer-credit mutant row) | REBIND after `paolino/reactivegas#68` lands; freeze #30 on current base meanwhile | `paolino/reactivegas#68`, concrete boundary ONLY: `tryEnactBase` majority + proposer rules (`lib/KelGroups/Fold.hs:347-377`, `majority`/`adminCount` in `lib/KelGroups/State.hs`) — no blanket-block language | separate dependency on the actual approval path; named boundary above |
| R30-10 | RULED proposer lifecycle ONLY: proposer renounce ⇒ negative `renounced` closure; proposer departure ⇒ negative `proposerDeparted` closure atomically via post-base hook; causes retained + distinguished; continuation/refund downstream | V-5 ruling; `renounce` event + carried `renounced`/`proposerDeparted` causes (`Vote/Types.lean:76-77`); `paolino/reactivegas#81` scope §1–§3 (L-1–L-6b closable without #76; L-7 gated on #76) | kelgroups exposes the voted-side mechanism (close + record + cause + retention + atomicity via existing post-base hook); closure CONTENT → `paolino/reactivegas#81` (which depends on `paolino/reactivegas#76` for continuation/refund) | `paolino/reactivegas#81` for content; `paolino/reactivegas#76` for refund | neither silently implemented ahead of Lean (only the ruled Slice-A-carried mechanism surface: close/record/cause/retention/atomicity hooks shaped so Slice-B extends rather than redesigns) nor marked permanently out (explicit downstream rows L-1–L-7 referenced, L-7 gated) |
| R30-10U | UNRULED refusal policy, explicitly NOT scheduled: non-proposer `renounce` reading; non-designee ballot refusal. Current don't-produce behaviour PRESERVED as the boundary; open operator questions both directions | `paolino/reactivegas#81` out-of-scope § (both exclusions quoted as unruled intentions, not rulings) | NONE — no ticket, no promise, no dependency edge | — | UNSCHEDULED preserved boundary, explicitly |
| R30-11 | verdict → economic effect (grant/deny/backdonate, target/polarity/provenance/one-use) | NOTE-016/A-Q001 (ruled, wire missing); ASSENSO-ORACLE-GAP rev2 | `paolino/reactivegas#76` (this ticket exposes evidence only) | `paolino/reactivegas#76` | neither implemented ahead of Lean nor marked out; evidence-exposure only |
| R30-12 | PureScript client proposing/voting app questions (API + minimal views, adapt-only) | `client/kelgroups-client` package (`Api.purs` postEvent/getEvents/getInfo; `Fold.purs` second base-fold tension noted in EPIC43 — adapt-only, no wholesale redesign) | kelgroups #30 client additions (propose/vote app questions; minimal views) | none in team | intact: necessary existing client type/API adaptations to keep `just ci` green are INCLUDED (adapt-only); Reactivegas browser UI + wasm (`paolino/reactivegas#84` / `paolino/reactivegas#82`) and wholesale `Client/Fold.purs` deletion/redesign are NOT |
| R30-13 | Lean proof obligations | existing Vote proofs = evidence; Slice-B PRODUCERS only where RULED (R30-10 content per `paolino/reactivegas#81`); unruled identities (R30-10U, R30-5-producing) have NO proof obligation anywhere | LEAN-OWNED (Reactivegas lanes); zero unilateral kelgroups Lean edits | Lean lanes where ruled; none where unruled | Lean never edited from this lane; no obligation invented for unruled identities |
| R30-14 | denial/dissent observable for L2-style consumers | negative closure delivery (`ClosureRecord` negative + cause through the boundary) | interface #30; consumption `paolino/reactivegas#76` | `paolino/reactivegas#76` for effect | same as R30-7 |
| R30-X | NON-GOALS (guarded, all preserved): expiry; theta default; votable admission; second store/fold; Reactivegas browser UI + wasm; Reactivegas economics in kelgroups | R-54; threshold-param (R-46/R-47); S28 direct-only/INV-62; R9c/R11; ASSENSO gap | none | — | guards enforced by fence (§8) + M9/M10/M11 kills + leg 7 |

#29 owned remainder (unchanged dispositions): runnable demo
(`paolino/kelgroups#33`, blocked by #30; implementation then, publication
separately gated); v2 major release (`paolino/kelgroups#34`, blocked by #33 +
#30; publication authority with desk only); downstream notes (doc handoff with
the `paolino/reactivegas#73` lane on S30 landing). `paolino/kelgroups#29` +
`paolino/reactivegas#73` stay OPEN.

## 3. Compact spec

Stories: (S1) as a nondegenerate app operator I open a collective assent
question over an application payload through the integrated boundary and every
franchise member sees it open; (S2) as a responsabile I cast assent/dissent,
change my mind (switch moves), re-cast (idempotent), and observe others'
ballots exactly once each; (S3) as the app I observe positive AND negative
verdicts with explicit causes (`tally`/`franchiseChange`; ruled lifecycle
causes arrive via `paolino/reactivegas#81` scope, not here) plus retained
closure records — never a silent disappearance, never an expiry; (S4) as a
non-responsabile I am refused before anything durable happens (state AND log
unchanged); (S5) as an operator I replay an accepted KEL and get identical
state; (S6) as a client user I propose/vote app questions through
`kelgroups-client` additions.

Requirements: implement the §2 rows R30-1–R30-8 + R30-12 + R30-14 now; carry
R30-9/R30-10/R30-11/R30-13 as explicit dependencies (never scope); hold
R30-5-producing + R30-10U unscheduled with the preserved boundary; hold all
R30-X fences. Threshold parameterized at every evaluation; franchise read
from the canonical `GroupView` at every evaluation; refusals precede effects;
replay reproduces identical state; closures retained with causes.

Rejection behaviour (exact identities, part of the contract): vote-path
refusals are `notResponsabile` / `questionNotFound` (vote error type);
substrate-path refusals keep S28 identities (`NotAMember`/`NotAnAdmin`/…);
refusal advances nothing (aggregate AND log unchanged). `notDesignee` /
`notProposer` are vocabulary, never produced by this slice. No expiry refusal
exists anywhere.

Observable success: S1–S6 executable through the integrated boundary on the
candidate SHA (§10 acceptance); `Trivial` intact degenerate-only; full
`just ci` green; fresh independent audit PASS; no shipped defaults; no silent
drops.

## 4. Plan

Strategy: single coherent slice S30-1 carrying the FULL
vote/integration/replay/closure/client boundary (Haskell mirrors of
`Vote/{Types,State,Event,Validate,Fold}` + `Integration` wiring +
persistence path + client Api additions + test-only proving instance). No
S30-2 deferral slice: one boundary delivered once, never double-counted as
delivered-now and deferred-hardening (S28 precedent). RED-equivalence first
(failing-first properties vs absent Haskell Vote API, zero-extent control
with positive control on the LANDED S28 surface, exact absent names quoted);
then GREEN implementation inside the frozen envelope; then SLIM
identical-envelope; then fresh FULL audit; then draft PR + remote CI; then
acceptance. The ticket owner freezes the final immutable gate from this
packet; this contract is the authorization basis, not the instrument.

Constraints: preparation fence already honoured (this packet cost 0 builds);
implementation respects the §8 fence (writable set minimal; S28 production
files read-only except named integration points versioned in the mandate;
`lean/**` untouched; historical `Proposal`/`BaseEvent`/`GroupEvent`/
`validateEvent` bodies untouched; `Trivial.hs` presence-only; no Reactivegas
UI/wasm/economics); every whole-project invocation is BUILD-class (§9);
concurrency discipline inherits S28 (serialized `appendIntegratedEvent`,
refusal-before-codec F3 order — vote appends use the same path, never a
second writer); no parallel heavy builds.

Live boundaries exercised (all with can-fail controls, §7–§8): pure
step/validate agreement; integrated Store/KEL append + replay
(`appendIntegratedEvent` refusal-persists-nothing; `foldIntegratedFrom`
replay == live); client Api roundtrip (`kelgroups-client` propose/vote
against the test boundary); founding-mismatch guard
(`openIntegratedKEL` founding equality). No source-token search, no
shrinkable fixture inventory, no absent-API import failure cited as
behavioural evidence anywhere.

Ordered slices: S30-1 (this contract; all rows now) → audit → PR → #33 demo
→ #34 release/notes. `paolino/reactivegas#68` rebind interrupts only the
R30-9 boundary (`tryEnactBase` majority + proposer rules) on landing, with
revalidation; nothing else waits on it.

## 5. Models (compact; no implementation content)

Modules model (responsibility + direction only): new `KelGroups.Vote.*`
Haskell mirror modules own vote vocabulary/state/validation/placement/
sweep (depend on `KelGroups.Types.GroupView` + S28 `State`, never duplicate
them); `KelGroups.Fold` owns the `Integration`-wiring composition (vote
payload as the `AppState` of a proving integration OR a dedicated vote-aware
app fold — ticket owner decides and versions; either way `BaseProposal`
payload reading, `proposalMutation → BaseMutation`, `digest`, and post-base
hook composition live here and nowhere else); `KelGroups.Store` owns the
vote persistence path (vote events travel the existing integrated
validate-then-append + replay path; no second writer, no second table
family); `kelgroups-client` owns propose/vote Api additions (depend on
existing `Api.purs`/`Codec.purs` transport, adapt-only); test proving
instance (`S30DemoApp`-analogous + `S30VoteSpec`-analogous) owns the
nondegenerate proving app (test-only, never shipped as the #33 demo).

Data model: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}`; `VoteState{openQuestions,closed-append-only}`;
`Verdict{positive,negative,open}`; `Threshold = Nat→Nat` (parameter);
`QuestionKind{collective,permission(designee)}`;
`ClosureCause{tally,franchiseChange,proposerDeparted,renounced}` (last two
carried, Slice-A produces first two); `VoteEvent{openQuestion,cast,renounce}`
(no membership event — T6222); `VoteError{notResponsabile,questionNotFound,
notDesignee,notProposer}` (last two vocabulary-only). Franchise never
stored; verdict single site; closure = remove-from-open + append-record as
one operation.

Functions model (new/changed signatures only; ticket owner binds exact
Haskell spellings at freeze): vote vocabulary constructors; `verdictOf :
Threshold → GroupView → Question → Verdict`; `franchise/franchiseSize/
isResponsabile` over `GroupView`; `lookupQuestion`; `closureCause`;
`validateVoteEvent : Threshold → GroupView → VoteState → signer → event →
Except VoteError ()` (exhaustive, no wildcard); `placeBallot`;
`sweepStep`/`sweepClosures`; `effectedState` (authorization-free by
architecture: assumes admitted input); `applyVoteEventChecked` (single
checked step; integrated production path uses it and never revalidates) +
historical state-returning fold documented as non-production-reporting;
`foldVote`/`foldFrom`; `Integration`-wiring (`digest`,
`proposalMutation → BaseMutation`, vote-aware `appFold`/`baseHook`
composition). Every signature takes `GroupView`/`Threshold` explicitly.

## 6. Tasks (stable IDs, grouped by slice S30-1)

- T30-1 RED-equivalence: failing-first properties vs absent Haskell Vote API
  (zero-extent + S28 positive control; exact absent names quoted).
- T30-2 vocabulary + state mirror (R30-1/4/6: kinds, verdicts, threshold-param,
  franchise reads, no time-like field).
- T30-3 validation mirror (R30-1/2/5: exhaustive 3-arm validation; 4-ctor
  error vocabulary with two vocabulary-only ctors, zero construction sites).
- T30-4 placement + effects (R30-2: one-position placement, switch-moves,
  idempotent-recast; open-never-overwrites; renounce Slice-A no-op effect).
- T30-5 sweep + closure + retention (R30-3/4/7/14: same-step close, record
  with cause, append-only retention, no expiry).
- T30-6 `Integration` wiring (R30-8/6/9-freeze: payload reading,
  `proposalMutation`, `digest`, hook composition; freeze on current base,
  R30-9 rebind pending `paolino/reactivegas#68`).
- T30-7 persistence path (Store/KEL append + replay equality + founding guard;
  refusal-persists-nothing; no second writer).
- T30-8 client additions (R30-12 adapt-only propose/vote; client CI green).
- T30-9 ruled-lifecycle shaping (R30-10 mechanism surface only: close/record/
  cause/retention/atomicity extension points; no unruled producing semantics;
  content defers to `paolino/reactivegas#81`).
- T30-10 GREEN envelope + SLIM + full CI + hygiene (legs 1–7, 12 kills,
  `Trivial` intact, tracked-clean).
- T30-11 audit handback + PR (fresh FULL audit; draft PR post-GREEN only;
  exact-SHA merge at desk; no merge/push by workers beyond the authorized
  draft-PR push).

## 7. Frozen requirement-to-command/control map (binding)

Conventions (frozen with the map): every whole-project invocation = 1 BUILD
(expected-RED, warm reruns, and per-mutant runs all count); targeted probes
counted per row against the 24-probe cap; charge-0 recon enumerated below and
never cited as evidence; per-mutant cycle = apply + build/test + revert with
hash-verified restore (restoration failure aborts exit 3, no fallthrough);
each kill must quote a registered example of its row in the `Failures:`
section (empty sections, crashes, parse errors NEVER count); compiler-vs-test
attribution kept (build-RED vs test-RED); the `pendingBase` compile-RED
establishes ONLY the narrow interface-existence claim "no admission
constructor is encodable" (type-level absence), never behavioural refusal.

Actual toolchain (observed read-only; versions re-pinned exactly at ticket
freeze from live `*-version` output — S28 pins quoted as precedent shape,
never carried over blindly): `nix develop .#ci --quiet -c` wrapping
`just build` (= `cabal build all -O0`), `cabal test all -O0
--test-show-details=direct`, `just ci` (= format + cabal-fmt + lint + build
+ test + `cd lean && lake build` + `cd client && spago build` + `spago test
-p kelgroups-client`), plus `ghc/cabal/lake/node/spago/just --version` pins.
`test/Main.hs` registers suites (S30 spec modules added at freeze);
`kelgroups.cabal` `test-suite invariants` is the suite vehicle.

| row(s) | concrete commands / controls (class) | shares with | kill / control |
|---|---|---|---|
| RED-equiv (T30-1) | C1 `cabal build all --enable-tests -O0` expecting failure naming the exact absent Vote API (BUILD, 1); C2 `cabal test all -O0` expecting the proving spec to fail for absence with zero historical breakage (BUILD, 1). Zero-extent control + positive control: same commands on LANDED S28 surface names succeed. | nothing (first cost) | absence RED, not behaviour; quoted absent names; historical suites green |
| R30-1 open/admit | full-suite `cabal test … --match` proving open collective + permission-with-designee + non-responsabile refusal-before-effects (BUILD via leg-4 run, shared); targeted `--match` reruns per witness (PROBE, charged) | leg-4 run shared by all behavioural rows | M1: openQuestion-nonresponsabile bypass mutant → test-RED quoting refusal witness |
| R30-2 cast/place | same shared leg-4 run; probes per witness | shared | M2: cast-nonresponsabile bypass → test-RED; M3: unknown-question accept → test-RED quoting `questionNotFound` witness; M4: placement mutant (drop erase-other-list) → test-RED quoting BOTH switch-moves AND idempotent-recast witnesses (two witnesses, one run) |
| R30-3 sweep/retain | same shared leg-4 run; Store/KEL append + `foldIntegratedFrom` replay equality at the persistence boundary (BUILD, in leg-4/6) | shared | M5: tally-positive suppress → test-RED; M6: dissent-negative suppress → test-RED quoting negative-delivery witness; M7: franchiseChange→tally collapse → test-RED; M9: close-and-discard (drop append) → test-RED quoting retention witness |
| R30-4 verdict | same shared run; permission-ignores-tally property (tally changing under a permission question never flips its verdict) | shared | M8: permission-tally consultation mutant → test-RED quoting permission witness |
| R30-5 refusals | vocabulary presence via exhaustive-match compile (BUILD, shared with cold build: 4-ctor type + 3-arm validation compiles; adding a 4th producing arm is a mandate change, not a mutant) | cold build | NO producing-semantics mutant scheduled (unruled); guard: any construction site for `notDesignee`/`notProposer` outside vocabulary declaration fails review + gate (grep-anchored precondition, charge-0 recon, never cited as kill) |
| R30-6 franchise | canonical-view property: verdicts recomputed against changed `GroupView` (member added/removed) with no payload-local copy (shared run) | shared | covered by M7 (stale-tally cause) + reopen/recompute witness; dedicated mutant: franchise-snapshot (stash membership in payload) → test-RED quoting recompute witness |
| R30-7/14 negative delivery | integrated-boundary observation: negative verdict + cause delivered through `applyIntegratedEvent`/`appendIntegratedEvent` to the proving app (shared run; persistence boundary) | shared | M6 (above) quotes the negative-delivery witness at the integrated boundary |
| R30-8 route separation | type-level: `BaseMutation` exhaustive enactment compiles with 2 arms (shared cold build); voted proposal cannot encode admission | cold build | M10: added admission constructor → build-RED (exhaustiveness failure quoting the new ctor; narrow interface-existence claim ONLY) |
| R30-9 approve/V-2 | freeze on current base (shared runs); rebind check (conditional, audit-time): if `paolino/reactivegas#68` landed, `tryEnactBase` majority + proposer-rule rerun + revalidation (AUDIT-BUILD, from auditor cap) | shared now; conditional later | no anticipation mutant now; post-#68: proposer-credit mutant must RED (defined in `paolino/reactivegas#68` scope) |
| R30-10 ruled lifecycle | mechanism-surface properties: renounce-close + departure-close + cause distinction + retention + atomicity via post-base hook (shared run; hook atomicity at the integrated boundary) | shared | M11: hook-refusal-ignored (always-commit) → test-RED quoting restoration witness; cause-collapse mutant (renounced→tally) → test-RED quoting cause witness. L-7 refund gated on `paolino/reactivegas#76`, never mocked here |
| R30-10U/R30-5-prod | NO commands, NO mutants, NO dependency edge (preserved boundary; recon-only guard as in R30-5) | — | — |
| R30-11 economics | evidence-exposure only; no wire built; no mock wire (would be silent ahead-of-Lean implementation) | — | — |
| R30-12 client | `spago build` + `spago test -p kelgroups-client` (BUILD, in full CI leg-6; client boundary) + Api propose/vote roundtrip against the test boundary | leg-6 | M12: client-propose-roundtrip break (drop propose path) → client-test-RED quoting roundtrip witness at the actual client boundary (never a fixture inventory) |
| R30-13 Lean | no kelgroups Lean commands beyond inherited `lake build` green (BUILD, in leg-6; Lean-owned content stays in Reactivegas lanes) | leg-6 | — |
| Cold/final | cold `just build` 1B (first compile = entire cost; COLD/WARM logged) + final `just ci` 1B + tracked-clean before/after + `Trivial` presence-only + founding-mismatch guard | serve all rows | final CI is acceptance, never shared away |
| SLIM | identical-envelope 3B (legs 1,2,2b,3,4,6,7 analog on the slim envelope) | — | — |
| Audit | exact command table pre-dispatch (§9 auditor table: mandate rows + reliances + conditional R30-9 rebind check); fresh FULL Codex-or-Grok audit in a clean detached worktree; reruns named gates; hash-bound report | separate seat/cap | auditor kills are verification, never counted toward owner fit |

Discovery bounds (extent-quantified, not enumerated-from-memory): the mutant
set quantifies over the observed Lean equation-site extent — `placeBallot` 2
arms (`Vote/Fold.lean:53-56`), `sweepStep` 2 arms (`:65-66`), `effectedState`
3 arms + 2 sub-arms (`:89-101`), `validateVoteEvent` 3 arms + 2 sub-arms
(`Vote/Validate.lean:57-70`), `verdictOf` 2 kind-arms (`Vote/State.lean:87-96`)
+ `closureCause` 3 arms (`:111-113`), `sweepClosures` filter+filterMap shared
step (`Vote/Fold.lean:72-76`). Twelve mutants cover the twelve distinct
behavioural sites with the pairings in the table above (M4 covers two
placement witnesses in one run — stated, not hidden). Guard against vacuity:
any Lean constructor added later breaks the exhaustive Haskell matches at
compile time (new site ⇒ new mutant required, never silent pass); the gate's
inventory + registered + execution legs (adapted S28 pattern: ≥K groups,
registered-total == file examples, every registered example executed, none
`# PENDING`) fail closed on an empty or truncated set — and that guard is
itself falsified by the RED-equivalence absence run.

Charge-0 recon (enumerated, never evidence): reads, `grep` extent queries,
`git status/diff/rev-parse/log`, `gh issue view`, `--version` pin reads
without build, `find test -name '*.hs'`. Anything whose result is cited for a
row is BUILD or PROBE, never charge-0.

## 8. Candidate-independent initial gate design (ticket owner freezes the final instrument from this)

Fence (proposed writable set; ticket owner versions at freeze): WRITABLE —
new `KelGroups.Vote.*` Haskell mirror modules, `Integration`-wiring
integration points (`Fold.hs` composition, `State.hs`/`Event.hs` only where
the wiring requires, versioned), `Store.hs` vote persistence path (existing
integrated path only, no second writer/tables), test proving instance (new
`S30*Spec` + proving-app modules, analogous to `S28DemoApp`/`S28AppApiSpec`),
client Api additions (adapt-only under existing transport), `kelgroups.cabal`
+ `test/Main.hs` registration. FENCED — `lean/**` (semantics; `lake build`
green only), historical `Proposal`/`BaseEvent`/`GroupEvent`/`validateEvent`
bodies, `Trivial.hs` (presence-only, uncounted), S28 landed production files
outside the named integration points, `client/` beyond adapt-only additions,
Reactivegas UI/wasm/economics, all release/publication metadata.

Legs (adapted from the S28 v10.2 shape; thresholds K/M frozen by the ticket
owner from the actual proving spec): 1 tracked-hygiene before/after; 2
identity + self-hash (blank-normalized header-equals-bytes) + ancestry (HEAD
descends from the frozen slice base); 2b instrument pins exact, fail-closed
(re-pinned at freeze; S28 pin set is the shape precedent); 3 build cold/warm;
4 inventory (≥K vote groups, presence only) + registered (row-group
extraction, total == file examples, no orphans) + execution (every registered
example executed, none pending; historical suites green) + exit 0; 5 mutants
M1–M12 per §7 (entry requires tracked-clean committed candidate at recorded
HEAD; per-mutant apply+run+revert; hash-verified restore every exit path;
restoration failure aborts exit 3, no fallthrough); 6 full `just ci`
(Haskell + Lean + client suites); 7 `Trivial` degenerate presence (exports
present, zero slice references — not counted) + client CI presence. `set +e`
with diagnosable capture throughout; full log + per-leg evidence files with
sha256; meta file binding gate version/HEAD/evidence.

Kill-attribution rules: a kill counts iff (a) the run exits non-zero AND
(b) its `Failures:` section names ≥1 registered example of the mutant's row
(M4 requires both placement witnesses) AND (c) the RED class matches
(build-RED: unification/exhaustiveness failure quoting the ctor/site, no
parse error; test-RED: witness-quoted failure). Empty `Failures:`, crashes,
timeouts, infra errors, and parse errors NEVER count. Compiler-vs-test
attribution recorded per mutant (M10 build-RED; all behavioural test-RED).

Evidence bindings: `run-receipt`-style capture (command hash, exit,
duration, evidence hash, bytes, lines, path) for every cited run; evidence
dir per campaign; gate self-hash verified in leg 2; ancestry verified in leg
2; mutant diff hashes logged pre-run; restore hashes verified post-run.

Spend classes (frozen with this map): BUILD (any whole-project invocation:
build, test-all, ci, per-mutant run, expected-RED, warm rerun — charged 1);
PROBE (narrow execution: single-suite `--match`, single-component build,
isolated runner — charged 1 against the 24-probe cap); CHARGE-0 (enumerated
recon above — free, never evidence); AUDIT-BUILD/AUDIT-PROBE (same classes
inside the auditor seat against the auditor cap). No parallel heavy builds;
every failed setup/attempt journaled; no automatic ceiling raises (exact
workload/cost gap returned before exceeding).

## 9. Operational classifications + proposed ceilings (FROZEN with the §7 map above)

Bottom-up fit (honest, one-build-per-mutant): RED 2 (C1 build-absence +
C2 test-absence) + GREEN 15 (cold build 1 + full test 1 + M1–M12 12 +
final CI 1) + SLIM 3 = **20 builds**. Probes: ≤24 counted targeted probes
(per-row `--match` reruns, dispute narrowing, reconfirmations; charge-0
recon enumerated and free). **PROPOSED owner ceiling: 20/24**
(builds/probes). This supersedes the 18/24 estimate (which undercounted two
mutant builds). Caps remain PROPOSALS pending fit-proof at freeze +
explicit authorization; fit failure returns the exact gap, never trimmed
scope.

Auditor command table (pre-dispatch, exact; fits 12/24): A1 pins+identity
recon (0B, charge-0); A2 cold build 1B; A3 full test 1B; A4 full CI 1B;
A5–A9 rerun of up to 5 disputed mutant kills 5B; A10–A11 independent
boundary reruns (Store replay equality + client roundtrip) 2B; A12
conditional R30-9 rebind check (runs iff `paolino/reactivegas#68` landed;
else returned unspent with reason) 1B; reserve 1B for one repair-verification
rerun. Probes ≤24 (finding narrowing, `--match` confirmations).
**PROPOSED auditor ceiling: 12/24.** Auditor seat: fresh Codex-or-Grok
(never Muse/GLM/Claude), clean detached worktree at the exact candidate SHA,
`argv`-pinned model+effort with post-cursor START, hash-bound invariant
report; auditor recommends, ticket owner decides; every repair gets a fresh
auditor.

Team (standing fence, commissioned at authorization — NOT by this packet):
Muse ticket owner → Muse commit owner (`draft=NONE`, alternate-seat rule per
fence) → fresh Codex-or-Grok auditor per submission (max two audited
submissions, one findings bounce, repair re-audited); signed commits; draft
PR post-GREEN only; exact-SHA merge at desk.

## 10. Acceptance (binding when commissioned)

Executable controls on the candidate SHA: threshold-parameterized verdicts
(exhibits never defaults; permission never consults tally); explicit closure
records retained with causes (never silent drop; no expiry anywhere);
refusal before durable effects (vote refusal leaves aggregate AND log
unchanged; replay of accepted KEL reproduces identical state);
validate/fold agreement (accept/replay never disagree on the same
aggregate/event/signer; never historical `validateEvent`, never
same-wrapper-twice); negative delivery observable at the integrated boundary
(positive AND negative verdicts with causes through `applyIntegratedEvent`/
`appendIntegratedEvent`); no dormant-constructor refusals (zero
`notDesignee`/`notProposer` construction sites outside vocabulary
declaration); client additions covered by client CI (`spago build` +
`spago test` green); `Trivial` intact (exports present, zero slice
references); full `just ci` green; tracked-clean before/after;
founding-mismatch guard held; fresh independent audit PASS with complete
invariant matrix; bounded claims only (finite scope, named SHA).

## 11. Open questions / dependencies (enumerated, not invented)

- `paolino/reactivegas#68` (V-2 landing) → explicit rebind of the R30-9
  boundary (`tryEnactBase` majority + proposer rules) + revalidation. Nothing
  else waits on it.
- `paolino/reactivegas#81` (V-5 lifecycle content, scope §1–§3; L-7 gated on
  #76) → R30-10 content dependency; Slice-A-carried causes already in
  vocabulary. Unruled exclusions (non-proposer renounce, non-designee ballot)
  are NOT dependencies of anything.
- `paolino/reactivegas#76` (composition wire: reachability/target/polarity/
  one-use; verdict→grant/deny/backdonate) → Reactivegas side; kelgroups
  exposes the interface + closure evidence only.
- `paolino/reactivegas#75` (R3.1 replay context: table authority, pre-replay
  refusal vs runtime abort vs mismatch) → test input for the persistence
  boundary (threshold = test input), not a kelgroups shipped default.
- Upstream Lean gaps for #30 → enumerated in this contract, never invented;
  later Lean landings rebind explicitly.

## 12. Provenance + freeze record

Sources read (all read-only; newest governs on conflict, conflicts listed —
none blocking): epic `handoffs/T30-MANDATE-v3.md` + `handoffs/T30-
REQUIREMENT-MAP-v3.md` (v1/v2 retained as history); current bodies
`paolino/kelgroups#30` (2026-09-06 correction operative), `#29` (2026-09-06
Lean correction operative), `#33`, `#34`; accepted Lean
`lean/KelGroups/Vote/{Types,State,Event,Validate,Fold}.lean` +
`lean/KelGroups/{Integration,State,Validate}.lean` @ `3590c001` (zero-diff
re-verified EMPTY vs `4a6cd87` on this extent); V-2 ruling (settled,
`paolino/reactivegas#68` OPEN); `paolino/reactivegas#81` body (proposer scope
+ explicit unruled exclusions); R3.1 replay contract (threshold = test
input); LANDED S28 interface on `main` @ `933e385d` (converge, never
redesign); S28 gate v10.2 shape (legs/attribution/restoration precedent);
commissioning note `KELGROUPS-T30-TICKET-CONTRACT-PREPARATION-20260906`
(fit gap + preparation boundary — both honoured: honest 20-count; instrument
ownership left to the ticket owner). Inbox: empty at intake and before
writing; NOTE-001 (clock correction, epic owner) received, read in full,
acked in STATUS with correction + fresh acknowledgement (supervision
baseline); re-checked before COMPLETE. Spend: 0 builds, 0 probes, 0 mutations, 0
gate runs. Skills: orchestrator-contract, ticket-orchestrator,
resolve-ticket (planning phases only), context-compiler, worker-protocol,
tmux-orchestrator, verification, invariants, gate-script, haskell, nix,
lean4 (read-only).

Frozen content hashes (sha256, at write time):
- base HEAD `933e385df2f2a251bb54a08bb7663f0d41fafb64`
- Lean `3590c0015b84fd58004bf6fb44dd18b107304c48`
- brief `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`
