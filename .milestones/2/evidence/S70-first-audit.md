# Commit audit — FINDINGS

Candidate `af9c1e5091014702c88df89e4b591819aad57979` has **8 blocking findings** despite two independently green full v12 runs. The economic functions matched the bounded Lean-generated campaign; the failures concern integration, identity handling, and assurance coverage.

- Submission **1/2**, full `dbd1ed859c6eb9510fedc1139c20916db83db572..af9c1e5091014702c88df89e4b591819aad57979`; one repair submission remains allowed.
- Mandate: `../commit-owner-one-membership-glm/brief.md`, SHA256 `c013e595e7af8175a01b431b2891aec024ffa2006a59b412bf586b948cab8f58`.
- Auditor `%517`, codex/gpt-6-astra/high, alternate to glm owner; NOTE-054 authorization. Fresh detached audit checkout; candidate tracked files unchanged. Reviewed all 13 changed files, including the generated page's changes outside its 13 shared core slices.
- Campaign **CLOSED, stopped=SET-POINT**: 6 KILLED, 5 BLOCKED, 0 OPEN, 0 RESIDUAL. BLOCKED means an observed failure requires a candidate/gate repair outside this read-only seat; it does not waive the requirement.
- Builds **4/30** ticket total, **1** this audit, warm cache, **26 remaining**; ceiling raises **0/2**. Ledger: `../campaign-ledger-S62-SIM.md`. No submission-cap or budget overrun.
- All paths below are relative to this report's runtime root unless a repository path is named. Full evidence and instrument SHA256 values are in `receipts.md` and `evidence/manifest.sha256`. Reproduce instruments in a fresh writable runtime copy against the exact candidate; the retained originals and outputs are frozen.

## Invariant matrix

Every row has the mandate's BLOCKING severity. PASS is bounded by the cited checks, not a proof of unrestricted JavaScript behavior.

| Row | Verdict | Campaign state | Evidence / exact blocker |
|---|---|---|---|
| INV-1 one membership | PASS | KILLED | No second live membership payload/cache found: group membership owns authority; vote payload has no members. `payload-members` mutant rejected by the frozen oracle. Historical view snapshots are retained; their faulty provenance binding is F3. |
| INV-2 substrate Key | FAIL | BLOCKED | F4: the live chip handler converts string member Key `"01"` into number `1`. Core string-key checks pass but cannot close this browser failure. |
| INV-3 canonical authority | PASS | KILLED | View changes alter economic authority; `all-authorized` mutant rejected by the oracle, and generated Lean comparison kills no-auth. |
| INV-4 fourteen only | FAIL | BLOCKED | Current inventory is 14 = 11 direct + 3 appDecided + 0 baseEnacted. F6 demonstrates the narrowed retired-event exclusion accepts a live retired branch. |
| INV-5 sealed consequences | FAIL | BLOCKED | F1–F3: duplicate cleanup during adoption, lost hook-vote continuity, and cleanup accepted without any base transition. |
| INV-6 zero read / departure row | PASS | KILLED | `admission-row` mutant rejected; admission/promotion preserve payload, actual Lean cleanup comparison preserves the departing zero row. |
| INV-7 one election deliberation | PASS | KILLED | Candidate's ordinary existing-member election works. Reintroducing admission before election makes the shipped browser selftest fail specifically `Bruno non eletto`. Key conversion remains F4. |
| INV-8 freshness | PASS | KILLED | All 22 source pins independently match origin/master blobs; event source remains unique. Production composition freshness assertion accepts current pin and rejects old `c8c4dd89` with both actual blob IDs. |
| INV-9 emitted fixtures / hook seeds | FAIL | BLOCKED | Fresh bytes do match both Lean producers. F7: producer seeds call cleanup with supplied views, bypassing the sealed integrated hook and its vote sweep. |
| INV-10 unruled threshold | PASS | KILLED | n=2 remains 1 in machine and exercised UI. `strict-majority` mutant fails for n=2; 96 generated majority comparisons agree with Lean. No anticipatory threshold found. |
| INV-11 existing guarantees | FAIL | BLOCKED | F1–F5 and F8: replay, identity/name input, sorry-detector sensitivity, and visible Italian surface fail while full gates pass. |

## Blocking findings

### F1 — Successful departure exports verify, but adoption turns state into null

Repository `economics-simulator.html:2457` (`adoptSession`, especially 2475 and 2487). Starting with Anna, admit Bruno, deposit 40 from Anna to Bruno, then propose Anna's departure. The live transition succeeds, leaves Bruno's conto 40 and comune −40, and the exported session verifies. Adoption first executes the hook on the `b` entry, then repeats `economicCleanup` on its paired `e` entry. The second cleanup sees the newly stalled comune and returns null; adoption silently installs null as the economic state. A normal deposit roundtrip succeeds and a tampered balance is rejected, so this is not a generally broken replay instrument.

Reproduce: `node browser-probes.mjs`, case `control-and-stalled-replay`. Evidence: `evidence/browser-probes-v2.log`, complete session in `evidence/browser-probes.json`.

**Property class:** for every reachable accepted integrated history, export/verify/adopt/reload must preserve the entire canonical aggregate, apply each sealed hook exactly once, and either return a valid aggregate or an explicit rejection. Include transitions that create a stalled state, not only transitions starting stalled.

### F2 — Hook sweep changes live votes without a replayable vote transition

Repository `economics-simulator.html:2373`, `economics-simulator-core.mjs:1270` and `:1573`. Make Anna, Bruno and Elena admins; open a collective question and cast Anna's assent. Demote Bruno with Anna proposing and Elena approving. The sealed sweep correctly closes the question at the new threshold 1. Export verifies at this point. Open another question: this succeeds live, but its export now fails `voto: passo 2: input discontinuo`. The recorded vote stream lacks the closure caused by the base event; the governance walk also evolves its vote state only on `k` entries.

Reproduce: `node browser-probes.mjs`, case `hook-vote-persistence`; same log and complete JSON evidence as F1.

**Property class:** replay one chronological aggregate across base, vote and economic events, including every hook-driven closure without a ballot. Every accepted extension of a valid live history must remain importable, and governance decisions must use those same closures.

### F3 — Import accepts economic departure consequences without departure

Repository `economics-simulator-core.mjs:1594`. With Bruno still a member and holding conto 40, append an `e` entry with `baseChange: {memberRemoved:'bruno'}`, unchanged pre/post membership, and the cleanup-computed result. Append no base proposal or approval. The production session verifier accepts it; Bruno remains a member while his conto becomes 0 and comune gains 40. The `baseChange` arm checks only the optional post-view against current membership, then continues; it never binds the cleanup to an actual preceding committed base change.

Reproduce: `node browser-probes.mjs`, case `unbound-cleanup-import`; same log and JSON evidence as F1. This is an accepted import witness, not a claim of a new ordinary task button.

**Property class:** require an exact one-to-one provenance relation between each committed base change and its full hook effect, bound to the actual change, pre-state, post-state, order and multiplicity. Omitted, forged, duplicated, reordered or independently appended effects must reject before adoption.

### F4 — User input corrupts substrate keys and names

Repository `economics-simulator.html:3118` converts every digit-shaped chip ID with `Number`, even when selecting a member rather than a numeric collection. Through the actual controls, admit `01`, choose that member for election, then choose Anna: the selected key becomes number 1 and the valid election is refused with `memberNotFound`. The same changed input boundary at `:3141` replaces `/s+/g` rather than whitespace: entering `Alessio` creates key `ale io`, displayed name `Ale io`.

Reproduce: `node ui-key-probes.mjs`; `evidence/ui-key-probes.log` and `.json` record DOM actions, selected type, group membership and rejection. Ordinary named members in the shipped selftest avoid both cases.

**Property class:** the complete UI-to-event path must preserve arbitrary supported substrate string keys exactly, independently of numeric collection IDs. Name normalization may perform only its documented whitespace/case operations and must preserve other characters. Test numeric-looking keys, leading zeroes, Unicode and names containing `s` through real controls.

### F5 — A broken sorry detector passes its own negative control

Repository `economics-simulator-claim-gate.mjs:1011`–1027. Replace the single production classification at `:275` with `const sorried = false`. A known `sorryAx` report is then incorrectly classified `provato`, yet the candidate's full 17-control selftest returns 0. Its sorry control explicitly accepts `condizione del rilevatore non raggiungibile: on="provato" off="provato"` as its expected RED. Both the intended detection and failure to construct the detection condition are treated as success.

Reproduce: `node checker-probes.mjs`; `evidence/checker-probes.log`; frozen subject `claim-detector-mutant.mjs` SHA256 `de4787bdaebd6d5e358d8f0ea0186f8639521d51687fd555bcd20121659fe693`. Only transport paths are rebound to the pristine candidate; production detector classification is the semantic mutation.

**Property class:** exercise the production proof-state pipeline with independently known sorry-backed and sorry-free declarations, including displayed proof state. Disabling or corrupting the detector must make the shipped control fail; unreachable fixtures/setup failures must never count as successful negative controls. A fabricated report's unconditional `ok:false` is not detector sensitivity.

### F6 — The ticket owner's retired-vocabulary exemption admits a real branch

Frozen `../handoffs/gate-v12-one-membership.sh`, step 3. In the runtime copy, replace the claim's `removeMember` word with `departure`, add live `case 'removeMember': return {ok:true,state:s}; // comp-base-threshold`, and regenerate the identical HTML core slices. There are still exactly two occurrences, both on exempted lines. The **exact step-3 scanner** returns 0; the frozen oracle and its selftests return 0; calling the retired event returns `ok:true`.

Reproduce: `node mutation-probes.mjs`; `evidence/mutation-probes.log`, `evidence/retired-survivor.log`, subject `mutant-retired/economics-simulator-core.mjs` SHA256 `a70e24ec612d9ec4aa7cc7329f8170e10ea489ad43683819cf7239c003a2a140`. This is scanner-plus-oracle evidence, **not** a claim that the complete gate passes a modified candidate. The ordinary added-route mutant is separately killed.

**Property class:** derive and compare the executable accepted-event surface to the Lean constructors; constrain the prose exception to the actual claim field and its value. Moving an allowed token out of prose into live dispatch must fail regardless of substring comments or an unchanged total count. Keeping truthful claim prose is reasonable; the owner's substring/count narrowing does not establish its claimed boundary.

### F7 — Fresh Lean fixtures bypass the sealed integration they must exercise

Repository `lean/TraceDriverV1.lean:53`–80 accepts handcrafted `Seed.base pre post change` and directly calls `economicCleanup`. It never executes `Reactivegas.baseHook` or a signed integrated base transition. `lean/KelTraceDriverV1.lean` separately folds votes under a fixed view. Thus the freshly reproduced 28 economic and 15 vote steps cannot observe the combination of accepted base enactment, cleanup and vote sweep. F2 is a reachable failure in precisely that omitted combination. This is a seed-coverage finding; there is no evidence that the embedded bytes were hand-edited.

Evidence: candidate-bound driver source, both full gate receipts, `evidence/provenance-probes.json` import graph, and F2's session. The independent 1,728-row campaign additionally compares actual Lean `baseHook` results but is auditor evidence, not the required shipped producer coverage.

**Property class:** generate reachable integrated Lean histories whose signed base transitions exercise each relevant cleanup branch and hook-driven vote changes, including closure without a cast. Derive every pre/post view and payload from the integrated transition; byte-compare and replay the whole aggregate. Fixture freshness alone cannot establish this.

### F8 — Ordinary Italian surface exposes internal vocabulary and stale proof status

Repository `economics-simulator.html:3849` adds visible `KelGroups — una sola membership (GroupState.members)`. The adjacent visible status still says proofs are `enunciate, non dimostrate`, despite the newly derived receipt's 56 proved citations and zero stated-only citations; it also exposes pin/constructor jargon. The new heading directly violates NOTE-034, and the retained status became false against the new pin. The teaching text controls inspect the strips, leaving this ordinary group card outside their protection.

Reproduce: `node browser-probes.mjs`, case `visible-vocabulary`; complete visible DOM strings in `evidence/browser-probes.json`.

**Property class:** validate all ordinary reachable UI states and visible text against the approved Italian vocabulary, and derive factual proof status from the same receipt used by proof glyphs. Cover cards, dialogs, refusals and teaching text, rather than only selected strips.

## Failure modes altered and remaining limits

| Boundary | Before / now / observability |
|---|---|
| Economic authorization | Payload-local membership became supplied-view authorization. Bounded valid-type cases exercise every constructor with both acceptance and refusal; no accidental exception appeared in that corpus. Arbitrarily malformed JS views and integers outside exact Number range are not certified. |
| Base mutation | Separate economic role events became hook execution. Live validation returns explicit `validate`/`rejected` failures; the adoption boundary loses this guarantee (F1), and stream binding is incomplete (F2/F3). |
| Key selection | Nat IDs became string Keys in the model, but the DOM retains numeric coercion (F4), moving valid choices to observable `memberNotFound` rejection. |
| Name acquisition | Changed normalization silently alters letters before creating membership (F4). |
| Proof derivation | All current citations now derive proved. The old real middle-state situation was replaced by a fabricated sorry report; the detector can fail silently inside selftest (F5). |
| Gate teardown | Five `finally` cleanup paths now suppress removal errors. Independently forced teardown exceptions together with actual claim, trace, vote, scenario and teaching defects each preserve semantic exit 1 and their domain reason (`boundary-gate-probes.log`). Non-finally scratch operations remain throwing. No semantic weakening from `rmQuiet` was found. |

The desk's silent-cleanup limitation is retained as non-blocking **for C1 only**, conditional checks satisfied; no candidate logging change was made. It does not terminate any blocking row as RESIDUAL.

S1 dependency was independently checked transitively, not inferred from one import: neither driver's local import closure contains `Reactivegas.Trace`; the 22 cited sources omit it; the 43 corpus rows contain no `guard` or `declaration` keys (41 applied, 2 vote refusals). See `provenance-probes.json`. This conclusion changes if a future producer adds that dependency or those fields. No sibling source was changed.

Both independent full runs returned exit 0. This establishes two repeatable verdicts under the observed environment, not a proof of determinism under changing Git refs, toolchains or process scheduling. No same-SHA opposite-verdict witness was found.

No additional unratified candidate invariant is proposed. Onward discoveries: `onward-discoveries.md` (none). Packet bookkeeping note: the final binding and ledger say three prior builds, while one narrative sentence still says “two spent”; this audit consistently used the authoritative 3/30 and now carries 4/30.

## Verification receipts and coverage

- Exact command, run twice from this runtime root: `bash ../handoffs/gate-v12-one-membership.sh /code/reactivegas-sim-fable-audit-s62sim-b`. Logs: `evidence/gate-v12-independent.log`, `evidence/gate-v12-repeat.log`. Both independently emitted fresh claim/trace results, scenario checks and actual Chromium selftests at 1280 and 390 pixels.
- Exact budgeted build: `nix develop -c bash -c 'cd lean && lake build'`; successful 27 jobs, warm replay (`evidence/lake-build.log`). `/code` free space before 220836220928 bytes, after 220727459840; concurrent host activity prevents attributing that delta to this audit.
- Lean-generated comparison: `cd lean && lake env lean <runtime>/PropertyProbe.lean`, then `node property-compare.mjs`. 1,728 rows = 1,344 event cases + 288 cleanup/hook cases + 96 majority cases. Every one of the 14 constructors has nonzero accepted and refused counts; all compare equal. Four source mutants (no-auth, wrong-cassa-sign, no-vote-sweep, no-absorb) are killed by this comparison. Deterministic small-integer/state enumeration, not unrestricted proof or a claim that every generated state is reachable.
- Frozen-oracle source mutants: 7 executed, 7 rejected for their intended semantic reasons. Added election-flow and composition-freshness controls also reject specifically. Two checker-boundary survivors are separately retained in F5/F6. Five forced-cleanup/semantic-failure combinations retain exit 1.
- `negative-controls.md` enumerates all 60 named outer gate controls and their observed reasons. Seven oracle selfcontrols compare fabricated constants instead of mutating production; they are not accepted as production mutation evidence. The independent real mutants supply the row evidence. The sorry selfcontrol is a demonstrated survivor, so the answer to “can every check still fail?” is **no**.
- Instrument setup failures retained but excluded from candidate findings: initial browser probe shared localStorage between cases; initial Lean probe had a syntax error. Corrected instruments produced the named receipts above.

Instruments, input bindings, raw evidence and this report are retained; reproducible runtime build/profile scratch is retired only after report hashing. The detached candidate worktree remains the ticket owner's to retire. This seat stops at this submission-1 FINDINGS report.
