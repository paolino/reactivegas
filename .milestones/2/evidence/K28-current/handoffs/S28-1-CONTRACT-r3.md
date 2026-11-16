# S28-1 frozen ticket contract r3 — kelgroups #28 (G28-1 v2)

Ticket owner `t28-app-api` (Muse, `%534`), parent epic `paolino/kelgroups#29`
(owner `%532`), runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Brief sha256 `df98d09932edfcabb3465a29c891cfb51386d49ce6ad7cd6aa60748f42396466`.
Pinned origin/main = frozen base `368b596fef0b6d393c2ac7afc631d236c55d86d1`
(no setup commit: `/gate.sh` already ignored via shared `info/exclude`;
HEAD == pinned at freeze). Worktree `/code/kelgroups-issue-28`, branch
`feat/28-generalize-app-api`. Frozen 2026-09-05 per NOTE-003 re-freeze
checklist, before ANY campaign run. This r3 SUPERSEDES r2 for execution;
r1 (`handoffs/S28-1-CONTRACT.md`) and r2 (`handoffs/S28-1-CONTRACT-r2.md`,
gate backup `e164a121`) are PRESERVED unmodified as evidence. Later
#68/#76/#81 landings need explicit rebind + revalidation. V-2 settled but
UNLANDED: freeze against current accepted base, rebind after #68, never
anticipate.

## NOTE-003 dispositions (F1–F5)

| finding | disposition in r3 |
|---|---|
| F1 budget counting | §3 rule rewritten: EVERY whole-project invocation (`all`, full suite, `just ci`) is BUILD-class incl. expected-RED and warm runs — warmth changes minutes, never charge. Only genuinely narrowed targets (named `--match`, single-module) with stated scope are probes. Imported 23× timing claim DROPPED (no such measurement on this repo; counts are invocations). Worst case no longer fits: exact itemized gap returned (§3, Q-001) — BLOCKED for ruling, no waiver, no scope trimmed. |
| F2 mutant semantics | §6/M1–M6 + gate leg 5 rewritten: (a) all splices use `-- MUTANT-Mn` full-line markers or true-EOL trailing `--` (never `/* */`; M5 uses a string literal); (b) M4 uses fresh `AdmitMemberVoted` (no collision with `DirectCommand.AdmitMember`) with exhaustiveness-failure output quoted; (c) anchors carry status DISCOVERED vs PROVISIONAL-UNTIL-RED (M1 test-file anchors) vs PROVISIONAL-UNTIL-GREEN (M2–M6 production anchors) + named re-verification (ANCHOR-ATTEST in commit-owner handoff, leg-5 preconditions; precondition failure = gate FAIL, never skip); no runs until this freeze; (d) M1 bound to the production path (ill-typed assertion in `Integration`-boundary context + `demoIntegration` instantiation precondition); (e) M6 restored as second-decision-path mutant (appended exported `unsafeSetAppState` + rewired success write; authority RED = full-log replay equality broken by an outside verdict). Standing rule: kills fail FOR the intended semantic reason with the witness named; parse/setup/dependency errors never count (explicit exclusion greps). |
| F3 slug proof | Leg 4b/leg 5 rewritten: RED claims must quote the ACTUAL failing example from hspec's `Failures:` section; a nonzero exit whose failure section names none of the row's REGISTERED examples = mutant failure (fix mutant, row not passed). Pending scan replaced by per-example accounting: REGISTERED names extracted from spec source (frozen one-example-per-line format), GREEN requires every registered name executed + no `# PENDING` + `0 failures`; any row with zero executed examples FAILs. Appendix R reconciles discovery ↔ registered ↔ executed ↔ required ↔ killer with omission/empty controls per layer. |
| F4 candidate safety | Leg 5 mechanics = option (B): committed-candidate + clean-admission (tracked-clean + HEAD-unchanged + all five target files TRACKED required at leg-5 entry, else refuse closed) + abort-on-first-restoration-failure (immediate `exit 3`, no fallthrough) + byte-exact restore verified by `git hash-object` before/after on EVERY exit path. (A) rejected: extra cold builds + copy drift. Every leg/mutant log teed to `handoffs/evidence/` with sha256 in output; `mktemp` only for transient pipes (with `PIPESTATUS` exactness). |
| F5 identity | Leg 2/2b rewritten: (a) EXACT full-string comparisons (symmetric trailing-whitespace fold documented; any other difference fails — a wrong version merely containing the pin never passes); (b) every instrument query's exit checked FIRST — failed tool = gate FAIL with tool error, error text never matched; (c) legs 1/2/2b failure STOPS the gate before any build/mutation (immediate nonzero exit); (d) "cold" replaced by measured `dist-newstyle` object count (cold iff absent/zero). |

## Lane-state honesty (per NOTE-003)

The `NOTE GATE-FROZEN r2 ... next=dispatch-...` line was an ANNOUNCEMENT, not
a dispatch: only two panes exist (`%532`,`%534`), no commit-owner START exists
anywhere, worktree still clean at `368b596`. The lane has been stopped-intake
idle, not live implementation.

## 1. Reconciliation (frozen inputs read 2026-09-05; unchanged from r2)

Accepted Lean = Reactivegas `master 4a6cd87` + later rulings (§4 names the
exact `lean/KelGroups/` modules + Vote subtree as REQUIRED-OF-SUBSTRATE;
`#28 appOnBase`-total, `#30` immediate-enactment, `#29` legacy-authoritative
all STALE per dated corrections; no smuggled notDesignee/notProposer; no
theta default; V-2 settled/unlanded). Zero-extent control (11:10Z): new-API
grep over `lib/`+`test/` = ZERO hits with positive control (11 files hit for
`AppFold|pendingProposals`) = true absence. Remote CI green on base (CI
success 2026-08-26 PR#31 = 368b596). No unresolved authority conflict; new
conflict → BLOCK with Q-file + competing evidence. Proceed without waiting
for #66/#71.

## 2. Objective (S28-1: the ONE coherent slice; C4)

Test-only demo instance (`test/`, no executable) with distinct state/event
types, signer + sole GroupView, domain refusal BEFORE durable append, sealed
atomic hook — proven by G28-1 v2 on the exact candidate commit, exercising
the real `Store`/KEL boundary (durable accept, byte-identical refusal,
tentative-base + failing-hook restoring pre-state AND pre-log, replay
equality). No promised proof deferred; S28-2 contingent/unscoped (opens only
on review-named remainder). Candidate-commit discipline (F4): the commit
owner submits the candidate as committed local history; leg 5 mutates only a
tracked-clean tree at a recorded HEAD and restores it hash-verified.

## 3. Surface, fences, budgets, expenditure (F1)

Owned/fences/forbidden per r2 §3 (unchanged: six lib files + test additions +
cabal/Main wiring + minimal forced Bootstrap/Server fixes; Trivial UNCHANGED
and uncounted; client adapt-only; no Fold.purs redesign/UI/economics/
vendoring/publication/merge; `draft=NONE`; auditors never Muse/GLM/Claude;
≤2 submissions; fresh FULL audit per candidate, first Codex gpt-6-astra/high,
auditor envelope 5+20 separate).

Counting rule (F1, frozen): every whole-project invocation — `cabal build
all`, `cabal test all`, `just build`, `just ci` — is BUILD-class, ALWAYS,
including expected-RED runs and warm-tree reruns. Only genuinely narrowed
targets (named test `--match` slug, single-module build) with stated scope
are probes. Instrument interrogations (`--version`) and recon reads
(grep/git/sha256/`bash -n`) are enumerated with charge 0 (no compilation, no
test execution). No timing claims anywhere; the unit is invocations.

Per-envelope charges under the frozen v2 gate: leg 2b = 2 interrogations
(charge 0, enumerated); leg 3 = 1B; leg 4 = 1B; leg 5 = 6B (M1 build-all, M2
test-all, M3 test-all, M4 build-all, M5 test-all, M6 test-all — each
whole-project by F1(b)); leg 6 = 1B. One GREEN envelope = 9B. One RED
envelope on base (legs 1–7 run; leg 5 refuses at entry before spending —
zero mutant builds; leg 6 runs: base is green) = 3B (legs 3, 4, 6). Slim
final (legs 1–4+6–7, no leg-5 re-mutation — a RULING OPTION, not decided) =
3B.

| plan | builds | probes | vs 8/24 |
|---|---|---|---|
| frozen-v2 worst: RED + GREEN×2 (one repair bounce) + FULL final | 3+18+9 = 30 | recon 4 + 0 = 4 | OVER by 22 builds; probes fit |
| frozen-v2 best: RED + GREEN×1 + FULL final (first-pass audit) | 3+9+9 = 21 | 4 | OVER by 13 builds |
| frozen-v2 minimal: RED + GREEN×1 + SLIM final | 3+9+3 = 15 | 4 | OVER by 7 builds |
| v3-narrowed worst (reffreeze+refalsify required): mutants as `--match` probes; RED 3B, GREEN 3B+6p, FULL final 3B+6p | 12 | 4+18 = 22 | OVER by 4 builds; probes fit |
| v3-narrowed best: RED + GREEN + FULL final | 9 | 4+12 = 16 | OVER by 1 build |

Gap (binding): EVERY acceptance-capable plan exceeds the 8-build ceiling —
minimum conceivable overrun is +1 (v3-narrowed best), frozen-gate worst is
+22. Probes fit in all variants. Returned itemized above; BLOCKED Q-001 asks
the epic/desk to rule: (1) raise the build ceiling to N (≥30 keeps frozen v2
worst case; state N for any other shape); (2) gate shape — v2 as-is vs
v3-narrowed (re-freeze + re-falsification cost acknowledged) vs slim-final
dispensation (rely on submission receipts + auditor kills for leg 5 at
final); (3) confirm charge-0 enumeration for interrogations/recon; (4)
single-submission leash option (second findings → new mandate, bounding worst
to best). No waiver taken, no scope trimmed to fit.

## 4. Frozen Haskell API + anchor status

API per r2 §4 (unchanged): GroupView sole projection; DirectCommand sole
admission; BaseMutation non-admitting exhaustive; BaseChange evidence;
IntegratedEvent distinct `IE-` params; PendingBase non-admitting-typed +
historical PendingProposal marked; GroupState +`pendingBase`, `appFold`
holds AppState; `ReservedKey` + three exact validators, no bootstrap arm;
IntegratedAppFold/BaseHook/IntegratedError/IntegratedResult/Integration/
commitBaseChange/tryEnactBase/applyIntegratedEvent/foldIntegrated;
openIntegratedKEL/appendIntegratedEvent validate-then-append; Trivial
unchanged.

Anchor table (F2(c)): status DISCOVERED = verified in current tree;
PROVISIONAL-UNTIL-RED = lives in RED-bundle test files (re-verify when RED
lands: splice-lands check or gate FAILs); PROVISIONAL-UNTIL-GREEN = lives in
not-yet-written production code (commit-owner ANCHOR-ATTEST greps required in
PROOF-COMPLETE handoff; leg-5 preconditions re-verify mechanically; any
splice that cannot land as frozen → gate v2 FAILs closed → gate v3 re-freeze,
no silent change). NO campaign runs until this r3 freeze (this line); RED is
the first run.

| anchor | text | status |
|---|---|---|
| A1 | `if isMemberInView signer view` (Fold.hs, applyIntegratedEvent app route; count==1) | PROVISIONAL-UNTIL-GREEN |
| A2 | `=> gs` refusal-keep in foldIntegrated block (accumulator named `gs`, count==1) | PROVISIONAL-UNTIL-GREEN |
| A3 | `INSERT INTO events` in appendIntegratedEvent persist statement | PROVISIONAL-UNTIL-GREEN |
| A4 | `writeTVar (stateVar store) (irState result)` success write (M6 rewire site) | PROVISIONAL-UNTIL-GREEN |
| A5 | `^data BaseMutation` block containing `ChangeRoles Text (Set Role)` (M4 insert site) | PROVISIONAL-UNTIL-GREEN |
| A6 | `^data DemoState` + `^data DemoEvent` + `demoIntegration :: Integration DemoState DemoEvent DemoProposal DemoError` (M1 preconditions) | PROVISIONAL-UNTIL-RED |
| A7 | `    , closeKEL` in Store.hs export list (M6 export site; line 26 today) | DISCOVERED (re-verify at RED: file exists on base) |
| A8 | `readTVar`/`writeTVar`/`atomically` imported in Store.hs; `appFold` field in State.hs (M6 backdoor needs) | DISCOVERED (Store.hs:36-40, State.hs:39 today) |

## 5. Test-only demo + spec formatting rule (F3 countability)

Demo per r2 §5 (DemoState/DemoEvent/DemoError/DemoProposal/demoIntegration +
demoReserved/protectedKey; JSON; no executable/library exposure). Agreement
traces MUST include non-member and domain-invalid events (M5 observability).
Spec formatting rule (frozen, makes registration countable): in
`test/S28AppApiSpec.hs` each group header is one line `describe "<SLUG>"`
(4-space indent) and each example is one line `it "<name>"` / `prop "<name>"`
(8-space indent, name on the same line). The commit-owner brief enforces it
as mechanism. Example names must describe the witness behavior (they are
quoted in kill evidence).

## 6. Requirements → witnesses + killers (F2/F3 revisions)

Standing kill rule (F2/F3): each kill fails FOR the intended semantic reason
with a REGISTERED example of its own row named in hspec's `Failures:`
section. Nonzero exit with an empty/missing failure section (crash, timeout,
infra) or with only other rows' examples = MUTANT-FAILURE (fix the mutant;
row not passed). Parse errors never count: M1 requires a unification error
(`Couldn't match`|type `mismatch`, DemoEvent+DemoState named); M4 requires
the exhaustiveness error (`non-exhaustive`|incomplete-patterns,
AdmitMemberVoted/BaseMutation named). Rows reference appendix R for the
registered/executed/killed reconciliation.

| # | requirement (applicability) | witnesses (new S28-1; discovered ancestors in R) | killer (must RED) |
|---|---|---|---|
| 1 | distinct types + signer + sole view (P+R; A→#5 by reference, no duplicate) | P1/R1 per r2 | M1 (build): preconditions A6; append `-- MUTANT-M1` + `_m1_conflateAtBoundary :: Integration DemoState DemoEvent DemoProposal DemoError -> DemoEvent -> DemoState` / `_m1_conflateAtBoundary _ = id` — GHC unification failure AT the boundary-typed signature (proves the production API is instantiated at distinct params AND they differ). Kill = nonzero build + unification error naming DemoEvent/DemoState. |
| 2 | reject-before-append (P+R+A2 persistence; shares membership gate with #1-R1, recorded) | P2/R2/A2 per r2 | M2 (test): precondition A1 count==1; replace guard with marker line `-- MUTANT-M2: membership gate bypassed` + `if True` (tail preserved via capture). Kill = test nonzero + `Failures:` naming a registered `S28-1 rejecting step before append` example. |
| 3 | sealed atomic hook (P+R+A3; THE persistence-atomicity proof) | P3/R3/A3 per r2 | M3 (test): stub `commitBaseChange` equations (awk, signature kept) to `commitBaseChange _ _ post change = Right (IntegratedResult post (Just change))` + full-line `-- MUTANT-M3` marker. Kill = test nonzero + `Failures:` naming a registered `S28-1 atomic hook` example. |
| 4 | direct-only admission (P + TYPE-LEVEL R4 + A4-correlate; no runtime refusal exists for the unrepresentable) | P4/R4/A4 per r2 | M4 (build): insert `\| AdmitMemberVoted Text Text (Set Role)` into the BaseMutation block + full-line `-- MUTANT-M4` marker. Kill = nonzero build + exhaustiveness error quoted (`Pattern match(es) are non-exhaustive`, `-Werror`, naming `enactMutation`/`validateBaseMutation`/`BaseMutation`/`AdmitMemberVoted`). |
| 5 | validate/fold agreement (P+A5; R5 N/A — refusals belong to #2/#3) | P5/A5 per r2 | M5 (test): precondition A2 count==1; replace refusal-keep `=> gs` with `=> error "MUTANT-M5"` (string literal marker, parse-safe; replay crashes where accept keeps). Kill = test nonzero + `Failures:` naming a registered `S28-1 validate/fold agreement` example. |
| 6 | no second authority (P+R6; A→#2-A2/#3-A3 + log-explains-state property below) | P6: verdicts ONLY via the integrated boundary. R6: full-log replay equality — `readState == foldIntegrated demoIntegration initial (decode all rows from readEventsFrom)` — plus historical-path uninstantiability for the demo (ill-typed `AppFold DemoState` over `DemoEvent`). Same group. | M6 (test): (i) export-list insert `, unsafeSetAppState` + `-- MUTANT-M6-EXPORT` marker (anchor A7); (ii) append the backdoor (frozen text: `-- MUTANT-M6` + `unsafeSetAppState :: KELStore s -> s -> IO ()` / `unsafeSetAppState store newApp = atomically (readTVar (stateVar store) >>= \gs -> writeTVar (stateVar store) (gs { appFold = newApp }))`); (iii) rewire the success write (anchor A4, count==1) to `unsafeSetAppState store (appFold gs)` + `-- MUTANT-M6-REWIRE` marker — one verdict routed outside the boundary. Kill = test nonzero + `Failures:` naming a registered `S28-1 no client-decided authority` example (replay/state mismatch quoting the outside verdict). |

## 7. Frozen gate G28-1 v2 (immutable; `GATE_VERSION="G28-1 v2 (r3)"`)

Location/ignore/backup per r2 §7 (v2 backup `handoffs/gate-v2.sh.backup`;
`set +e` + `PIPESTATUS[0]` exactness; evidence dir default
`handoffs/evidence/`, override `G28_EVIDENCE_DIR` echoed+recorded; every
leg/mutant log teed + sha256 printed; `mktemp` transient-only). FROZEN_BASE =
`368b596…`. Frozen mechanism quotes (backup authoritative):

- exact pins: `[ "$(strip_ws "$got")" = "$(strip_ws "$want")" ]` on
  STDOUT-first-lines only, after exit-first refusal (`[ $exit -ne 0 ] → FAIL
  with tool error; error text never matched`). Pins: nix `nix (Nix) 2.31.3+2`;
  ghc `The Glorious Glasgow Haskell Compilation System, version 9.8.4`;
  cabal `cabal-install version 3.16.1.0`; lake `Lake version
  5.0.0-src+v4.25.0 (Lean version 4.25.0)` (joints lake+lean); node
  `v20.19.6`; spago `1.0.3`; just `just 1.43.1` (stdout `2>/dev/null`-separate
  from stderr; node-spawned warnings cannot pollute pins).
- stop-before-spend: `if [ "$OVERALL_FAIL" -ne 0 ]; then echo
  "IDENTITY-FAIL: stopping before any build/mutation"; exit $OVERALL_FAIL; fi`
  after leg 2b (covers legs 1/2/2b).
- cold measurement: `o_count=$(find dist-newstyle -name '*.o' 2>/dev/null |
  wc -l)`; reported; cold iff absent/zero (no bare labels).
- leg 4b: REGISTERED per slug extracted from spec source (frozen format §5;
  zero registered = FAIL); GREEN requires every registered name in the test
  log + no `# PENDING` within 3 lines after any registered name + `0
  failures` + exit 0. Base RED expectation: inventory 0 (absence) with
  historical suites exit-0 inside the same failing run (absence-not-breakage;
  historical-nonzero = breakage, investigate, no absence claim).
- leg 5 entry (option B, refuse-closed): HEAD==leg-2 HEAD + tracked-clean +
  all five target files `git ls-files --error-unmatch`; on FROZEN_BASE the
  refusal is the EXPECTED base RED (messaged as such), elsewhere a defect.
  Per-file `git hash-object` before each mutant; revert via `git checkout
  --`; post-hash must equal pre-hash else immediate `exit 3` (no fallthrough;
  recovery printed; legs 6–7 and 1-after skipped by the abort).
- kill_check (per mutant): failure section = `awk '/^Failures:/,0'`; empty =
  INCONCLUSIVE (never a kill); must contain ≥1 REGISTERED name of the row's
  own group (build-mutants: compiler-error-class match instead, with
  parse-error exclusion). Otherwise MUTANT-FAILURE (row not passed).

RED bundle (first run, on base): legs 3,6,7 green + leg-4 inventory 0 with
historical exit-0 + leg-5 entry refusal, gate hash quoted. GREEN: exit 0 with
all REGISTERED executed and all six kills quoting their witnesses. No push
without epic authorization; draft PR only after GREEN + fresh FULL audit.

## 8. S30 surface (unchanged from r2 §8; NOT built here)

## 9. Residual risks + rebind (r2 §9 carried; plus: any splice that cannot
land as frozen → v3 re-freeze per F2(c); hspec rendering drift → parsing-only
v3 with identical frozen semantics + re-falsification)

## 10. Supervision + reporting (r2 §10 carried; plus: commit-owner brief will
require ANCHOR-ATTEST greps in PROOF-COMPLETE; auditors never Muse/GLM/Claude;
upward local-files-only)

## Appendix R — reconciliation map (F3 columns)

`registered`/`executed`/`killed` fill at RED (registered + absence) and GREEN
(executed + killed); at freeze they state exact expected evidence. Discovered
column = r2 appendix R (ValidateSpec:147/158 historical membership;
FoldSpec:45-46 payload discipline; StoreSpec:134/187/340 durability;
StoreInvariants:59; ValidateSpec:175ff/239ff proposal discipline;
Invariants:77ff majority; TransitionInvariants:122 nil-fold; ServerSpec+E2E
single-path lifecycle; remote CI green 368b596) — statuses as r2.

| req | registered (expected: named examples in the row's group, ≥1) | executed (expected: all registered in GREEN log, no #PENDING, 0 failures) | killing example (expected: Failures: names a registered example of the row) |
|---|---|---|---|
| 1 | TBD-RED: P1+R1 example names in `S28-1 distinct types + signer + GroupView` | TBD-GREEN | M1: build unification error (no example; compiler names DemoEvent/DemoState at `_m1_conflateAtBoundary`) |
| 2 | TBD-RED: P2+R2+A2 names in `S28-1 rejecting step before append` | TBD-GREEN | M2: TBD-GREEN failing example quoted |
| 3 | TBD-RED: P3+R3+A3 names in `S28-1 atomic hook` | TBD-GREEN | M3: TBD-GREEN failing example quoted |
| 4 | TBD-RED: P4 + exhaustiveness-correlate names in `S28-1 direct-only admission` | TBD-GREEN | M4: exhaustiveness error quoted (no example; compiler names AdmitMemberVoted) |
| 5 | TBD-RED: P5+A5 names in `S28-1 validate/fold agreement` | TBD-GREEN | M5: TBD-GREEN failing example quoted |
| 6 | TBD-RED: P6 + replay-equality names in `S28-1 no client-decided authority` | TBD-GREEN | M6: TBD-GREEN failing example quoted |
| regression | N/A (historical suites, names unchanged) | TBD-GREEN: full suite exit 0 | N/A (any RED = regression finding) |
