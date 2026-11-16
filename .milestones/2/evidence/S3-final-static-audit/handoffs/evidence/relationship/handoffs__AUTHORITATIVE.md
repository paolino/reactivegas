# AUTHORITATIVE ASSESSMENT — S3 Phase 1 (v2; folds CORRECTIONS-013 §3H/§3G per NOTE-014)

Base `3590c0015b84fd58004bf6fb44dd18b107304c48` (#66 OPEN). Seat PID/PGID
`2401092`. Static correction only — no builds/queries/probes/mutations/Phase-2/
code change. Spend: 5 substantive (4 historical overrun + 1 granted OP-10) / 3
targeted (2 + 1); 0 remaining in any grant. No coverage claimed. Old versions
(R-original `dbc2cb68…`, R2 `32c06530…`, R3 `9b5bf6c2…`, R4 `5216adc3…`, R5
`3e51f229…`, CORRECTIONS-008/009/010/013, OP10-RESULT, FINAL-RECEIPT, INDEX,
AUTHORITATIVE-v1 `393e8ec7…`) are preserved separately as history; where this
document contradicts them, THIS document governs and says so per item below.

## 0. Correction ledger (each correction + the evidence that required it)

| # | correction | required by |
|---|---|---|
| C-239 | 224 short names → 239 fully-qualified identities (7 intra-file alias pairs + 8 inter-file mirrors; `:881` call-through verified) | NOTE-003 §1 |
| C-COUNT | 4v3 build overrun recorded (repo-root `lake build` charged per ruling) | NOTE-003 §5 |
| C-QUAL | roster phantoms removed (3 declaration sites, all `KelGroups/Invariants.lean:79,87,109`); groups H-none 40 / H-prop 3 / H-some 23 / H-mem-cons 4 / H-neq 5 / H-bool 6 | NOTE-005 §1 |
| C-FIXT | t54 s2 PARTITION/FRANCHISE reclassified FIXTURE (own `mutantFold`, production untouched — sources read) | NOTE-005 §2 |
| C-PREM | comune row = premise-transport; runtime-establishment half corrected to producers below | NOTE-008 §1 + NOTE-009 |
| C-ALIAS | aliases keep statements; kills counted once through the call | NOTE-008 §2 + NOTE-009 |
| C-LOAD | M-elab** = closure rebuilds + RED-must-quote-mutant witness; costs estimate-marked | NOTE-008 §3 + NOTE-009 |
| C-COST | Validate-class 4→3 (convention); envelope 129 targeted + 1 build, unfunded | NOTE-010 §1 + NOTE-011 §1 |
| C-WIT | verbatim-quote sufficient, never necessary | NOTE-010 §2 |
| C-1213 | exclusive partition 163+76+1+12+961 with per-identity artifact | NOTE-010 §3 + NOTE-011 §2 |
| C-TRANS | F-01 four transcriptions fixed (below); grouped markings replaced by per-receipt seven-field rows | NOTE-012 F-01 |
| C-PROV | `3a7b355a` = report sha (not Git object); candidate `000ff76a…` in DB; t57 six mutants + NC + RED logs located; assessments reopened, nothing auto-upgraded | NOTE-012 F-02 |
| C-ATOM | role-guard + per-hook atoms added (below); `no_expiry` scope corrected to accepted statement | NOTE-012 F-03/F-04 |
| C-HYP | vacuous instantiations replaced (below) or row marked OPEN (none needed — all 81 exhibited) | NOTE-012 F-05 |
| C-MAP | finite requirement→operation map with historical/prospective cost separation (below) | NOTE-012 F-06 |
| C-ISO | restoration vs isolation claims separated with own evidence; cold-log status per measurement | NOTE-012 F-07 |
| C-FOLD-3H | §3H folded to corrected file-42/file-43 positions (v1 joint OPEN/NONE withdrawn as false for file 42); evidence: corrected-TSV archive read + NOTE-014's own verification | NOTE-014 |
| C-FOLD-3G | §3G blanket dismissal replaced by per-file row readings incl. `commit-auditor-s62-c-a011-s1-codex-r1` by name; evidence: eight ledger tables read + NOTE-014 citation | NOTE-014 |

## 1. Extent (floor, not target)

- 239 qualified theorem identities (163 non-private + 76 private with
  `file:line` mapping; machine list `P1A-qualified-inventory.txt`; classes in
  `P1A-qualified-classified.txt`: 158 AUTHORED / 81 HELPER). 32 `example`s
  excluded (no identity). Compiler-generated excluded by rule; environment
  enumerated by OP-10 (1213; per-identity classes `OP10-identity-classes.txt`).
- 27 source modules (S roster retained). Entry points per R3/R5 (rev-3 table,
  zero disagreement) PLUS F-03 additions now in the atom ledger:
  - role-guard atoms: `checkRoleAddition`/`checkRoleRemoval`
    (`Validate.lean:43-60`) — per-role conjuncts `roleDef.canAdd gs.appFold` /
    `roleDef.canRemove gs.appFold` over `config.roleDefs` lookup (admin roles
    bypass). These guard atoms were absent from all prior maps.
  - per-hook effect atoms: `commitBaseChange` (`Integration.lean:139-145`)
    binds `integration.baseHook change (groupView pre) (groupView post)
    pre.appFold` per `BaseChange` constructor (memberAdmitted/memberRemoved/
    rolesChanged) — one hook-effect atom each, not one family row.
- Guard/effect/error axis per R3 + the two additions above.
- CORRECTED `no_expiry` (`KelGroups.Vote.no_expiry`): arbitrary
  `event : VoteEvent` + prefix decomposition `hevents` + open-at-prefix
  `hopen` + `hpres : PreservesQuestionSemantics …` ⇒ open-preserved +
  verdict-open. Scope: ANY event satisfying `PreservesQuestionSemantics` at a
  reachable prefix. Owned by V-arbitrary-event-under-hpres + V-fold-prefix
  atoms. Historical t54 single-question/cast-only scope does NOT stand in.

## 2. Ownership relation (per-row; shown evidence only)

Families A–E per R3 annex + R5 §2 as corrected: Family A (14 `hstep` literals);
conservation/authorized (14+14 counted arms); solvent per proof dependencies
with comune limb owning NO guard atom and comune membership owned by the
ESTABLISHED producers (`validateDirectAdmission` reserved-refusal,
`productionWellFormed` boot/apply gates, `reserved := comuneId` wiring —
bodies read, NOTE-009); vote rows vote-atoms-only; substrate rows base atoms;
aliases inherit counterpart atoms (single-count); witnesses per-check;
`no_expiry` per §1; role-guard rows own the canAdd/canRemove conjuncts;
hook rows own per-`BaseChange`-ctor effects. Helpers own no atoms.
Unresolved semantic relevance: solvent per-atom fund-sensitivity (kill work)
and t57-row→atom pins where instruments are prose-level — marked explicitly,
never filled.

Unresolved semantic relevance: solvent per-atom fund-sensitivity (kill work)
and t57-row→atom pins where instruments are prose-level — marked explicitly,
never filled.

## 3. Receipt admissibility — seven fields per receipt (subject | mutation | checker | fixture | footprint+evidence | toolchain | command → marking → re-key)

UNKNOWN = not recorded in the archive (explicit, not absent-by-omission).
"Same rig" = the ledger's own gate/instrument set at its recorded candidate.

**A. t48 s1 (file 32, candidate `4898e55e`; rig lacks tightness/declaredIn/quota vocabulary — whole-lineage difference):** all 10 rows (SURFACE/GUARDS/CANFAIL/AXIOMS/REGRESSION/FENCE/EVENT-SYNTAX/INV-HYP/STEP-ITE/STEPEVENT-DELEGATE, 6 OPEN + 4 KILLED per ledger) | subjects: inversion theorems + s1 rig per row text | mutations: as row text (commented-theorem/custom-axiom survivors; weakened-guard elaboration; direct-removal kills; Step.lean-path rejection; unparsed-marker rejection; unfolding-removal elaboration fails) | checker: s1 rig | fixture: s1 instruments | footprint: UNBOUND (no per-row footprint recorded) + lineage evidence (467-line rig diff) | toolchain: UNKNOWN (no pin line in ledger) | command: UNKNOWN → STALE-DEMONSTRATED on lineage replacement (mapping-inside-rig caveat stated) → re-key to 14 `step_*_inv` rows (REGRESSION: instrument retired, re-key none).

**B. t48 s2 (file 33, candidate `a408e09`; components per §1-corrected hunks):** SURFACE: s2 rig binding rule | commented/indented/private/non-theorem decoys + required-name swap survivor (`fba84ff0…`, truncated) | s2 rig | s2 instruments (manifest full hashes) | ownership+parser hunks (named) | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → 14 binding rows. GUARDS: same rig | `True`/dropped-conjunct/wrong-successor kills + swap survivor | s2 rig (`successOf/coveredBy/tightnessProved` UNCHANGED — no hunks) | s2 instruments | NO governing-diff found (retrieval record) | UNKNOWN | UNKNOWN → UNESTABLISHED-REUSE → 14 guard conjuncts. CANFAIL KILLED: live-inversion removal + unwiring (`273c8b65…`) | s2 rig (`projectConstants` hunks govern) | s2 instruments | named hunks | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → inversion rows + `stepDetailed_erases`. AXIOMS KILLED: custom-opaque + `admit` (`c30bd8a5…`); six `[propext]` reports | s2 rig; axiom-gate ABSENT at `a408e09` (verified) | s2 instruments | replacement demonstrated | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → axiom-gate scope. REGRESSION OPEN: 163/163 lexical count | s2 rig; quota ABSENT at base (verified) | s2 instruments | retired instrument | — | — → STALE-DEMONSTRATED (retired), re-key none. FENCE/EVENT-SYNTAX KILLED: path/marker rejections (`e408a627…`/`5a0d5bda…`) | s2 rig; NO fence hunks found (retrieval record) | s2 instruments | none demonstrated | UNKNOWN | UNKNOWN → UNESTABLISHED-REUSE → vocabulary/fence controls. STEP-ITE/STEPEVENT KILLED: unfolding-removal elaboration fails (`4ebb7324…`/`196738e4…`) | s2 rig | s2 instruments | subject proof file changed 28+/20− | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → unfolding discipline. INV-HYP OPEN: spelling/association survivors (`3ecb6b54…`) | s2 rig (parser hunks govern) | s2 instruments | named hunks | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → 14 hypotheses. File 34 (main, same candidate): same 10 rows as file 32-shape transcription (ledger text governs; states per that file). File 29: byte-identical copy of file 34 (verified) — cited, no double evidence.

**C. t48 recut1 (file 31): 10 rows, all killed** (SURFACE incl. duplicate-theorem controls; NAME-BINDING incl. deposit/withdraw swap + phantom ctor; GUARDS incl. dropped guard; CANFAIL incl. removed `step`/`stepEvent`; AXIOMS incl. proof escape; REGRESSION incl. unwired checker; FENCE; STEP-ITE incl. live-branch probes; STEPEVENT-DELEGATE; BOUNDARY roots/args) | subjects per row text | mutations per row text | recut1 rig | recut1 instruments | same checker lineage as s2 (D1) | UNKNOWN | UNKNOWN → STALE-DEMONSTRATED → same targets as B.

**D. t54 slice-a (lineage `757dac98`; D3 re-cut demonstrated):** s1 (file 26): PARTITION OPEN (C3 went red; `questions_partition` lacks opened-history) | — (no kill recorded) | slice gate | s1 harness | D3 hunks | 4.25.0 (harness script) | per-module `lean` (script-quoted) → STALE-DEMONSTRATED → `KelGroups.Vote.questions_partition`. DISJOINT KILLED: C2 opposite-tally erase omitted (`203c8a3e…`) | s1 per-module `lean` RED | slice gate | run.* module copies | placeBallot 13-line hunks | 4.25.0 | script-quoted argv → STALE-DEMONSTRATED → `ballots_nodup_disjoint`. NOSTALE KILLED: C4 ballot-only sweep (`73620710…`) | same | same | run.* copies | sweep 45/29-line hunks | 4.25.0 | script-quoted → STALE-DEMONSTRATED → sweep rows. FRANCHISE OPEN: premise-shape gap (recorded) | — | slice gate | s1 instruments | D3 + open premise text | 4.25.0 | script-quoted → STALE-DEMONSTRATED → `franchise_of_tallies`/`unfranchised_cast_noop`. NOEXPIRY OPEN (advisory): member-event gap v1 | — | slice gate | gap instrument | D3 + D5 | 4.25.0 | script-quoted → STALE-DEMONSTRATED (scoped) → `no_expiry` (corrected scope §1). POLICYFREE KILLED: hard-coded-threshold production mutation (`9e738cfb…`, RED `e0b3703a…`) | s1 per-module `lean` RED | slice gate | run.* copies | verdictOf 49/effectedState 47-line hunks (legacyThreshold def itself unchanged — stated) | 4.25.0 | script-quoted → STALE-DEMONSTRATED → threshold/verdict rows. s2 (file 27): PARTITION KILLED — reclassified FIXTURE (`partition-silent-deletion-mutant.lean` read: own `dropSweep`+`mutantFold`, production untouched; RED `partition-mutant-red.log` read, `lean` exit 1, 4.25.0; trace uses removed `.admitMember`) → check-sensitivity for `questions_partition`, kills NO row; STALE-DEMONSTRATED (D3+D5). FRANCHISE KILLED — same FIXTURE shape (`franchise-unfranchised-recast-mutant.lean` + RED log read) → sensitivity only; STALE-DEMONSTRATED (D3+D5). DISJOINT/NOSTALE/POLICYFREE KILLED: s1-production kills carried terminal (ledger states it; no s2 mutant) → inherit s1 rows above (no double count). NOEXPIRY OPEN: gap v2 instrument + GREEN survival logs (assurance-gap evidence, never a kill) → scoped row as s1. R-45: boundary record → preserved, disposition outside S3. File 30: byte-identical copy of file 27 (verified) — cited only.

**E. t57 reopened (file 12, gate base `bb3ac41`, toolchain 4.25.0 bound per receipt; six mutant sources + NC script + falsify RED logs LOCATED in `gate/instruments/`):** mutant↔row binding recovered per instrument body: `mutant-partition.lean` (own `dropSweep`+fold; RED `falsify-mutant-partition.log` read, `lean` exit path) → INV-57-BOUNDARY? No — partition instrument tests closed/open partition → re-key INV-54-PARTITION sensitivity + INV-57-BOUNDARY/EXHAUSTIVE as recorded; `mutant-disjoint.lean` (`badPlace`, no opposite erase) → INV-54-DISJOINT sensitivity + INV-57-AUTH; `mutant-nostale.lean` → INV-54-NOSTALE sensitivity + INV-57-NOOP; `mutant-franchise.lean` (`badEffect`) → INV-54-FRANCHISE sensitivity; `mutant-bypass.lean` (foldVote-prefix instrument) → INV-57-NOOP/AUTH; `mutant-policyfree.lean` → INV-54-POLICYFREE + INV-57-NOEXPIRY. ALL SIX are own-fold FIXTURE instruments (production untouched — bodies read), traces use removed `.admitMember`/`.setRoles` (counts verified per file) → check-sensitivity ONLY, kills no theorem row; STALE-DEMONSTRATED (D4 import-target re-cut + D5 per-instrument vocab). NOT upgraded (nothing here is a production kill). Remaining t57 rows (INV-57-EXHAUSTIVE sweep discriminator etc.) without pinned instruments: UNESTABLISHED-REUSE with this retrieval record.

**F. t54 gate-wiring auditors (files 24/25: R-1..R-29 + R-2b + E-PRED; file 24 has R-11 FAIL):** subjects are pipeline properties (root+imports existence, gate wiring, legal-direction counts, `Reactivegas.*`-in-KelGroups absence); mutants are gate/invocation controls (e.g. R-2 injected `import Reactivegas.Types` → `just lean` exit 1; R-4 type error in `Types.lean` → build exit 1); checker = ticket/frozen gates; toolchain 4.25.0/4.27 legs per ledger; commands = gate argv per logs. No theorem identity in any row → re-key target NONE for D2 (pipeline controls, retained as context). Marking: UNESTABLISHED-REUSE per row (footprints unbound; R-11 FAIL preserved as the ledger's own verdict, not re-labelled).

**G. t62 reopened (file 22; F-02 recovery):** `3a7b355a` verified as a REPORT sha (found in archived STATUS/ledger/acceptance texts, not the object DB); the named candidate `000ff76a52b3972f232ef18fbeaa96ac6a6b0f20` IS in the local object DB (verified `commit`); auditor evidence (`carried-boundary/provenance/repair/toolchain-preflight/fresh-*gate` logs) present in the archived seat. Ledger prefixes reproduce from retrieved outputs (report + gate/probe logs read). Per-row seven fields: ONE-STORE/PAYLOAD-ONLY/ONE-KEY KILLED — subjects: sole-members/payload-only/comune-identity properties; mutants: frozen duplicate-field seeds, member-writing transition, comune-authorization probes (per ledger text); checker: s62 gates + fresh probes; footprint: candidate `000ff76a` lineage (recovered) BUT per-row mutant↔source pins sought, found only at ledger-prose level → UNESTABLISHED-REUSE (reopened from recovery, NOT upgraded — no per-row footprint bound). HISTORICAL/DIRECT-ONLY/ATOMIC-HOOK/V3-BASE/CLOSED-SUMS/PROOF-TRUST OPEN: open obligations (blocker vocabularies now present for the four S62-B rows → re-key owed; nothing claimed). Files 15–21/23 — row-ledgers read row-by-row (the blanket dismissal in v1 is withdrawn): file 15 (`commit-auditor-s62-c-a011-s1-codex-r1`, named): G62-C-THEOREMS/ECONOMY/EXHAUSTIVE/TRUST-CI KILLED inherited (declaration blob `ab9b4aad…`, full-ticket receipt `d369103c…`), I57-01-BOUNDARY FAIL→BLOCKED (duplicate-state finding, `d53064ca…`), G62-C-INHERITED57 KILLED (franchise/caller-threshold controls, DISJOINT inherited). File 16 (a011-s2-glm): same six rows KILLED carried (`evidence/a013-gate.log`-class gate logs; I57-01 with fresh `evidence/probe-main2.log` duplicate/bypass instrument). File 17 (a013-grok): six rows KILLED carried (`evidence/a013-gate.log` `716afe5d…`, `Step.lean` blob `06b2d12e…`). File 18 (s1-codex): THEOREMS/ECONOMY/EXHAUSTIVE KILLED (receipts `968fc50f…`/`76c32a87…`/`28d6a8fb…`, frozen instrument `6dee1ad1…`); TRACE BLOCKED (F-TRACE: no integrated emitter); INHERITED57 BLOCKED (F-I57-ONE-DECISION, F-I57-INTEGRATED-LEGS — "DISJOINT, FRANCHISE, POLICYFREE checks do not reach the integrated transition and are insensitive to path mutations", recorded as scope evidence); TRUST-CI BLOCKED (F-I57-TOOLCHAIN: pin 4.27.0 vs executing 4.25.0). File 19 (s2-codex): THEOREMS/ECONOMY/EXHAUSTIVE KILLED carried-terminal; TRACE/INHERITED57/TRUST-CI BLOCKED with active scope. File 20 (owner-a011-a012): six rows KILLED inherited. File 21 (campaign-a013): six rows with A013 carry/re-run obligations. File 23 (ceiling-s62-c-a011): THEOREMS/ECONOMY/EXHAUSTIVE/TRUST-CI KILLED inherited (receipts `968fc50f…`/`76c32a87…`/`90c28b37…`/`b6117b60…`). Markings: inherited-KILLED → UNESTABLISHED-REUSE + the ledgers' own "do not reopen/rebuild" procedural bar (stated, not overridden); BLOCKED → open obligations with named blockers; I57-01 → UNESTABLISHED-REUSE + re-key to vote rows. Seven fields per row as recorded (subjects/mutants/checkers/fixtures per ledger text; footprints ledger-prose-level; toolchains where recorded; commands via evidence logs); UNKNOWN explicit where the ledger gives prose only.

**H. All other files (exact corrected transcriptions; D2-subject files none — dispositions with reason):** file 1: R2-CORE/SUBJECTS/CLICK-PARAMS KILLED with `receipts/mutants.log` (simulator subject → UNUSABLE for D2). File 2: prose, 0 rows → nothing to mark. File 3: 18 rows — 10 E-* (E-TOJSON **KILLED** per `evidence/survivor-campaign-v3.log#killed-tojson-instance`, NOT open; other E-* per-row states as TSV) + 8 INV-48-* (all OPEN, evidence NONE → open obligations, UNESTABLISHED-REUSE). Files 6/7: same 18-ID shape, per-file states as TSV (NOT assumed identical to file 3 — E-TOJSON differs; each file's own column governs). File 4: header + M1–M5 (5 doc-mutant receipts, full 64-hex hashes, observed RED legs read) — design-record controls for #71, UNUSABLE for D2. File 5: header + M1–M4 (4 rows, same kind). File 8: I10-FENCE/TYPES/COVERAGE/SEMANTICS/ADDITIVE/DEBT, all OPEN "pending … mutant" → open obligations, UNESTABLISHED-REUSE. File 9: A1–A4, all OPEN process/contract rows → UNUSABLE for D2. Files 10/11: EVENT-/VERDICT-EXHAUSTIVE, R-2/R-3-LAYERING, PRODUCTION-ENACTMENT/VERDICT, ROUTE-THRESHOLD (all KILLED per tables) — production-trace rows at pre-split bases → STALE-DEMONSTRATED (subjects renamed since), re-key to `Composition.*`/`voteApply` rows owed. Files 13/14: I-FENCE-*/I-R1-*/M-*/E-*/FENCE-* rows with per-row verdicts/states as TSV (fence/citation/debt subjects → UNUSABLE for D2). File 28: G74-CALLS-EXISTING/G74-ENVELOPE-CLOSED/G74-VERIFY-FAILS-CLOSED/G74-RECORD-HONEST/G74-ADDITIVE-ONLY — **five** OPEN rows (not two), Haskell exporter subject → UNUSABLE for D2. File 35: I001–I005, I007, I008 KILLED-S001 (7 release-control kills, evidence per row); I006 OPEN-PENDING-POST-MERGE; C001/C002 DEFERRED-S002, open (predecessor instrument hashes recorded; both stay deferred, visible, never dropped) — release controls → UNUSABLE for D2. File 36: 12 INV-* KILLED (simulator) → UNUSABLE for D2. Files 37–41: GEO/IT/LINK/E-/NOTE- rows per file → UNUSABLE for D2. Files 42/43 — CORRECTED (v1's joint OPEN/NONE withdrawn as false for file 42): file 42 (`t-toolchain-contract/.archived/auditor-s1-corrected/handoffs/campaign-ledger.tsv`): INV-TC-01..04, ALL KILLED — M01-historical-4.27.0-pin (`instruments/m01-pin-mismatch.sh` `6ac1bbc1…`, RED `evidence/m01-pin-mismatch.log` `7606b51e…`, exit 1, pinned 4.27.0 vs running 4.25.0), M02-exact-bytes (`70146ea5…`, RED `a0028319…`, candidate pin bytes `leanprover/lean4:v4.25.0`), M03-always-agree (`71e71d53…`, RED `a9ea57c1…`, mutated pin 4.25.1), M04-justfile-drop+yaml-swap (`5e950b2d…`, RED `f4a73526…`, `gate-v1.log` `ccfcb62a…`) — all four instruments/evidence present in-archive and read. Toolchain-contract subject (not Lean theorems): UNUSABLE for D2 kills, retained as toolchain-context evidence; demonstrated-move OPEN (base pin matches the auditor's running pin; subject-file diffs unestablished). File 43 (owner `handoffs/campaign-ledger.tsv`): INV-TC-01..04 OPEN/NONE — open obligations, UNESTABLISHED-REUSE. **REUSABLE-BOUNDED: 0, everywhere.**

## 4. Helper witnesses corrected (F-05 — non-vacuous instantiations)

Vacuous cases withdrawn and replaced (definition branches verified):
`stripCollections_sublist`: `cols=[c]`, `c=⟨7,"ref",false,[],[]⟩`, `r="other"` ∉ — `(stripCollections r [c]).1=[c]`, `y=c` satisfies `hy` (the `([],[])` nil-branch is NOT used). `stripCollections_amount_lemma`: `c.referente=r`, `accepted=[p0]`, `p=p0` (pledges surface in `.2` only on the referente-match branch — verified body). `option_bind_inv`: `o=some ()`, `f=fun _=>some 5`, `b=5` (all three unknowns exhibited). `assocLookup_insert_of_none`: `hne` exhibited (`"a"≠"b"` by decide) + `h` via `entries=[]` (nil-arm verified). Universal-hypothesis rows (∀ over lists with no membership premise) keep empty witnesses VALIDLY — satisfiability of `∀`-hyps by `[]` is sound (nothing to witness beyond the empty instance). All other H-some singletons/inequalities/literals stand as exhibited (R3 §4 roster, phantom-free). Result: all 81 helpers exhibited non-vacuously; **zero OPEN helper rows** (no compiler testing invoked; static argument as permitted).

## 5. Requirement→operation map with separated costs (F-06)

| requirement | operation(s) | historical cost (measured, logs kept/lost stated) | prospective cost (estimate, basis stated) | status |
|---|---|---|---|---|
| 239 identities inventoried/classified/mapped | OP-01..06 static (done) | 0 builds (reads/greps) | — | DONE |
| 1213 compiled enumerated + classified | OP-10 (done, granted) | 1 build (35 s wall, retained log) + 1 elaboration | — | DONE |
| 14 inversion kills | OP-11..24, M-elab** Step-closure | Build-2 10 s RED (retained; different layer — reference only) | 3 targeted/op (est.) | UNFUNDED |
| 7 solvent kills | OP-25..31, Step-closure | — | 3/op (est.) | UNFUNDED |
| 7 alias pairs | static classification (done, §2/C-ALIAS) | 0 | 0 | DONE (no execution) |
| STALE production re-verifications (DISJOINT/NOSTALE/POLICYFREE/t57-pinned/vote/substrate groups) | OP-39..65 per-atom, closure-counted (Fold 2 / Validate 3 / Integration 2 / Step 3) | s1 precedent shape (no wall times recorded there — stated) | per-class counts above (est.) | UNFUNDED |
| witness re-elaborations | OP-66..70 (5 files) | 11 s / 2 s measured (retained summaries) | 5 elaborations | UNFUNDED |
| final acceptance | OP-71 | — | 1 build + 2 elaborations | UNFUNDED |
| 81 helpers | §4 static witnesses | 0 | 0 | DONE |
| past-provenance permanents | none available | — | — | PERMANENT OPEN (no execution closes them) |

"131 targeted plus one build"-style totals are WITHDRAWN as budgets; the map above is the finite proposal. No grant implied.

## 6. Costing honesty (F-07 — restoration ≠ isolation; missing stays missing)

- RESTORATION claim (only): Build-3 GREEN single-file restore, 3 s wall, retained log — establishes that restoring one file re-greens this tree. Nothing about mutant isolation.
- ISOLATION claim (only): scratch diff (1 file, 1 line, retained) + RED specificity (exactly the owning theorem failed at `:407` with the mutated atom quoted) — establishes single-cause attribution for Build-2. Nothing about restore speed.
- COLD 19 s: observed-once, full log LOST — cited nowhere as a ceiling or bound; any cold-cost use is an explicit gap (withdrawn G-B2 stands withdrawn).
- Elaboration layers: 11 s (TraceTests), 2 s (CorpusGate module+eval), 35 s OP-10 (build+elaboration+gates) — each at its layer, never averaged, never transferred across layers.

## 7. Missing evidence (faithful OPEN list) + owners + reconciliation

OPEN: UNESTABLISHED-REUSE rows (GUARDS, FENCE, t57-unpinned, t54-auditor R-rows, evidence-NONE rows, t62 KILLED); past-provenance permanents; environment-side mechanism questions (projection `thmInfo` mechanism); per-atom solvent fund-sensitivity kills; t57 new instruments. Gaps: G-B1 CLOSED by OP-10; no other execution proposed (exact ops only after this map — none requested here). Owners: D6 clarity items → #71 via desk; S5 (comune connection question, ONWARD-68-INV-01, retention) → S5; V-5 lifecycle → #81.
Reconciliation: DISCOVERED 239+27+axis+additions; REQUIRED-INPUT same 239, zero exemptions, relevant pairs only; EXECUTED/KILLED/SURVIVED/BLOCKED unclaimed; every identity kill-pending (unfunded §5), helper-exhibited (§4), or explicitly OPEN above. 4v3 overrun explicit. Counters: 5 substantive / 3 targeted spent, 0 remaining.

*End of AUTHORITATIVE assessment. Prior versions preserved separately; this document governs where they conflict, per correction C-xxx above.*
