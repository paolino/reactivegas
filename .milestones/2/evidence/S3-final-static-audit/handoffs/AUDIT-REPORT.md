# Independent FULL STATIC audit — frozen submission 3

**Verdict: AUDIT-FINDINGS. Phase 1 remains incomplete.** This is my assessment of the full mandate, not acceptance, a repair, or an inherited verdict. The eight original findings are **three CLOSED, five PARTLY, zero wholly OPEN**. The PARTLY findings remain blocking for Phase-1 completeness; none is demoted to a residual. Seven concrete finding groups below describe the remaining work, including errors beyond the four supplied challenges.

The author remains terminal at submission cap **3/3**. This verdict does not authorize a fourth author repair. My execution account is **0 builds, 0 elaborations/queries, 0 probes, 0 mutant executions**. Historical owner spending and submission counts are separate.

## Identity, authority and evidence

Launch identity: own `/proc/3302516/cmdline` and targeted `ps` identify Codex 0.153.2, `-m gpt-6-astra -c model_reasoning_effort=high`, PID=PGID **3302516**, PPID **3302414**. Active model identity: the session's own `turn_context`, session **01a0752f-055a-7540-aeef-4787eb87f010**, records **gpt-6-astra / high**, including the context at **2026-09-06T05:46:48.651Z**. This is separate evidence from launch argv. Own pane `%575`, `reactivegas:rg-s3-audit-final`, was bound using targeted `tmux display-message`. No host-wide process listing was executed; the declined proposal was replaced by own PID and immediate-child queries. [Identity receipt](evidence/identity.md) preserves observation scope and commands.

My first journal line is START at **2026-09-06T05:54:21.879Z**, after intake and both model checks. Local UTC was available; the owner's supplied timestamp was not substituted. The reference is a clean detached checkout at **3590c0015b84fd58004bf6fb44dd18b107304c48**, `/code/reactivegas-66-s3-audit-final`, with zero oleans. All work is local, read-only against source and admitted inputs.

Verified personally:

- Brief SHA-256 **8a49e15a29b4bee9008927a24eeeb79ac71ac2dca907411d8e4129f60905c48e**.
- `sha256sum -c admitted/MANIFEST.sha256`, from its correct relative-path directory: **49/49**, no self-entry. Manifest hash **a88d8594a66de813b4740545919e857923ead5df9ef3dc62cb2a2e39a5315f64**. The first wrong-directory attempt was an auditor invocation error, not an input defect.
- Submission-3 manifest **a0dbe2c24236f2f5ef21d4c7a260e60bc93f926bea0a67737b4c2e5f1e290a36**, six candidate files. Submission-1 and submission-2 remain separate evidence sets, not governing versions.
- Original audit manifest **ac172dbfecad4447e105707fef8e3f6674712a3f5d735e9a1cf73497342a771e**: all **323/323** referenced files mechanically rehashed against the original immutable runtime. [Receipt](evidence/original-323-check.json).

I read all admitted inputs before START, including structured tables in lossless form and predecessor differences. I followed the original relationship through R3/R4/R5, the operative AUTHORITATIVE assessment, corrections 013–019, original raw receipts/instruments, and both owners' handback relationship. Original runtime text inspected outside the 49-file wrapper is explicitly captured in [relationship-index.json](evidence/relationship-index.json); those captures are not misrepresented as entries of the admitted manifest. Later corrections govern where they actually replace prior claims; merely omitting a topic from the six newer files does not retire it.

## Full extent and what is established

[ROW-REVIEW-207.md](ROW-REVIEW-207.md) gives an individual judgment, source identity and reason for **every map row**. [HELPER-REVIEW-81.md](HELPER-REVIEW-81.md) gives concrete antecedent instantiations for **every helper identity**. [REQUIREMENT-LEDGER.md](REQUIREMENT-LEDGER.md) covers the original 36 requirement rows, D1–D6 and this commission. These are inspection ledgers, not mutation coverage counts.

I independently parsed namespace/section boundaries after stripping nested comments and strings. All **239** declaration sites match the retained source map, with **no missing, extra, privacy or namespace mismatch**: **163 public / 76 private**, **158 authored / 81 helper**. The **27 project modules plus lakefile configuration** (28 source/config files) also match the retained import graph. See [independent-source-inventory.json](evidence/independent-source-inventory.json).

The seven root declarations after `end KelGroups` at `KelGroups/Invariants.lean:872` are real and distinct: sites **877, 883, 889, 899, 909, 914, 923**. The eight production/TraceTests pairs are also distinct identities. Only the two withdrawn `KelGroups.baseHook_votes` / `KelGroups.base_change_recomputes_votes` spellings are phantoms; the real declarations are `Reactivegas.*` at **1600/1616**. There is exactly **one** map row for `KelGroups.base_change_runs_hook` (207).

The retained OP-10 output has **1,213 distinct names**, **1,214 walk occurrences**, and a **1,213-row exclusive class partition**. All 239 source names match their retained public/private compiled names; all **974** remainder names are accounted for. This confirms retained name accounting, not a fresh compilation or semantic irrelevance of generated declarations. [Recheck](evidence/retained-identity-recheck.json).

The map mechanically contains **85 KILL, 31 OBSERVED, 60 ELAB-STATIC, 7 PREDICTED-SURVIVE, 9 RECOVERED, 10 OPEN-KILL, 2 STATIC, 2 WITHDRAWN-DUPLICATE, 1 ACCEPT** = 207, covering all 158 authored identities. Its evidence tags are **71 THEOREM-FAIL, 13 PROOF-FAIL, 1 MIXED, 31 CASCADE, 81 N-A, 10 NONE**. These are candidate labels, not validated outcomes. The audit's per-row dispositions explicitly distinguish supported static deductions, conditional claims, dependency-only information, baseline-only shapes, historical records and challenged claims.

## Original eight findings, independently dispositioned

| Original finding | My disposition | Evidence and remaining work |
|---|---|---|
| F-01 receipt transcription/inventory | **PARTLY — blocking** | Corrected docs, toolchain, recut and exporter entries are real improvements. Original relationship still misstates/omits archived identities and states; seven fields are not supplied per experiment. S-01 below. |
| F-02 recoverable provenance dismissed | **PARTLY — blocking** | Report SHA/candidate and six t57 instruments are recovered. Closure of recovery itself is supported. The required reassessment remains incomplete: available t62 instrument/command/hash bindings are collapsed back to prose, and t57 instruments acquire unsupported cross-property links. S-01. No current reuse inferred. |
| F-03 semantic denominator/relation | **PARTLY — blocking** | Economic conjunct enumeration and explicit role OPENs improve the account. Vote/base effects, refusal identities, hook bindings and semantic ownership remain incomplete or misassigned. S-02. |
| F-04 obsolete no_expiry scope | **CLOSED** | Actual theorem at Vote/Invariants:877 takes arbitrary event, prefix, hopen and hpres; AUTHORITATIVE §1 corrects the cast-only claim. One target across one preserving event remains the limit. No theorem repair or broader guarantee inferred. |
| F-05 helper antecedent witnesses | **CLOSED, static only** | I checked all 81 actual identities. The corrected retained-collection/refund witnesses satisfy the implicated memberships; the same retained-collection instance satisfies both sublist theorem and lemma. Option f/b and distinct insertion keys are supplied. My 81-row ledger makes remaining literal choices explicit, including w=0 for nonnegative fold increments. The three old helper phantoms were already removed by R4/C-QUAL. No compiled proof or helper exemption claimed. |
| F-06 complete executable phase plan | **PARTLY — blocking** | All 158 names appear, but mutation specifications, property classifications, all-current-atom searches, failure routing, mirror references and allocation remain defective. S-03 through S-06. Numeric sufficiency has not been established. |
| F-07 measured costs/isolation | **PARTLY — blocking** | Historical module timings/diff and restore log exist. Cold/check/replay/batching/full closure measurements do not; the new request cannot supply them as written. Historical candidate mutation is not cured by restoration or future scratch. S-05/S-07. |
| F-08 terminal journal tail | **CLOSED** | Actual phase1 STATUS ends with FINAL EVENT/STOPPING and final counters, superseding the stale OP-10 Next text. Snapshot retained. This confirms handback only, not its inherited five-CLOSED assertion or acceptance. |

## S-01 — original receipt assessment is still inaccurate and incomplete

**Blocks D3/P1-B and F-01/F-02.** The complete 43-file roster exists. I read the raw table contents, including all columns; [archive-tables.txt](evidence/archive-tables.txt) preserves them losslessly. Table lines, headers and multi-experiment rows are not summed as kills.

The operative AUTHORITATIVE §3F still declares that neither t54 wiring auditor has any theorem identity. The actual s2 **VI-3** row names a production `applyEventDetailed` mutation and `exact threshold` mismatch at `:338`. The VI/EP rows cannot be discarded by describing the entire file as pipeline controls.

AUTHORITATIVE §3G says six rows for several t62 ledgers, omitting **G62-C-TRACE**. The omissions hide materially different states: **BLOCKED** in a011-s1 and a011-s2, **KILLED** with a tracked-CI `false &&` mutant in a013-grok, **KILLED** in owner-a011-a012, and **OPEN** in campaign-a013 and the ceiling ledger. The s2-codex ledger has an initial and final table; final TRUST-CI is **KILLED**, whereas the assessment carries its earlier BLOCKED state. Both historical tables must be retained with order, not flattened into the wrong terminal result.

AUTHORITATIVE §3H still reports **18** rows, **10 E-*** and main-file E-TOJSON **KILLED**. The main t48 TSV has **17** data rows, **9 E-***, and E-TOJSON **OPEN**. The emitter auditor TSVs have KILLED E-TOJSON. It also omits **I10-REGRESSION** from file8 and **M1–M5** from file9. File1's three-name summary omits further simulator rows. Composition preflight/candidate tables also cannot be compressed into “all KILLED.” These errors persist despite correct repairs to other examples.

The recovered t62 report hashes to **3a7b355a260b018c70a004f4c9384d7e408d28737ebc240fef6de10a57853ae1** and names candidate **000ff76a52b3972f232ef18fbeaa96ac6a6b0f20**. It provides exact false-preservation and repair-properties instrument hashes, commands, gate bindings, outputs and measured durations. The current assessment's “pins only at ledger-prose level” is not an adequate inventory of these available records. Recovering these does not establish the entire transitive compiled footprint or current reusability.

The six t57 sources are readable, but they do not justify speculative links such as **policyfree → INV-57-NOEXPIRY**, **disjoint → AUTH**, or **nostale → NOOP**. `mutant-policyfree.lean` defines `badVerdictOf` and compares threshold outcomes; it is not an own-fold no-expiry instrument. The driver separately names a **no-expiry-member** baseline instrument. Preserve each actual subject, mutation, check, fixture, evidenced footprint, toolchain and command, with exact absent-field/search records. Classify alternate-model/fixture/checker evidence independently from actual production-definition mutations. Do not reconstruct missing history by rerunning it.

## S-02 — named identities are complete; the semantic relation is not

**Blocks D1b/D2 planning and F-03.** The atom file contains **131 table IDs**, while its footer says approximately140; the relation has **561 lines / 160 subject names**, including two explicit role OPEN names. Literal lines are not proof of semantic completeness.

Economic coverage is substantially improved. Nevertheless, aggregate effects such as E08 distribution/debit, E10 pending/accepted movement and E11 refund/removal combine independently mutable effects. Vote tags such as `Vtally-place`, `Vsweep-closures`, `Vfold` and `Vqid` still combine both ballot arms, insertion versus opposite-side erase, filter versus closure append, record fields, and distinct fold accumulators. Explicit `effectedState` open-id/closed-id guards and checked-versus-state-returning routing need their own accounting. Base validation must preserve actual refusal identities and order; “reserved FIRST” is false globally: signer-admin is checked first, reserved before duplicate.

The original hook omission is not closed by three BaseChange labels. `commitBaseChange` separately binds change, pre-view, post-view, input payload, returned payload, returned member state, reported change and rejection propagation. `economicCleanup` has pre/post-admin and stall branches plus separate balance/cassa/collection effects. Those bindings are not interchangeable per-constructor atoms. The two role predicates are named but their ownership remains explicitly unresolved.

Several relation assignments are not semantic ownership: `KelGroups.app_event_preserves_members`, `app_event_has_no_base_change`, and `enactMutation_preserves_absence` are attached to **Benact-met**, although those statements/bodies do not depend on the enactment threshold. `WITNESS` and `SHARED` tags preserve names but do not allocate the actual check/atom relation. References to `Vtally`/`V-event-hpres` also lack exact corresponding atom IDs in the ledger. Do not fill a Cartesian product to fix this; derive the relevant relation from statements and reached definitions.

## S-03 — semantic, script and dependency outcomes remain confused

**Blocks D2/F-06.** Correct distinctions exist: OP-25 only breaks `rw bal_bump_ne` for a theorem excluding comune; OP-39 tally inclusion remains true although its erase-subset term changes; OP-58's hook/absence proofs do not need the numerical comparison shape. Those corrected examples do not validate the whole taxonomy.

Additional source counterexamples:

- **Row68 / OP-51:** removing the sweep can falsify `VoteWellFormed`, not just its proof script. Take an empty well-formed state, an admin signer, threshold constantly0, and a fresh collective openQuestion. The admitted effect leaves an open question whose verdict is positive. `opensOpen` is false. This is a static mathematical counterinstance to the candidate classification, not an executed mutant.
- **Rows69–70 / OP-52:** for a fixed-initial accumulator mutant, the properties stay true, but the supplied cons proof uses the induction hypothesis with an **updated** seed. On a two-event list the target applies the last event to the original initial state; the term applies it to the first event's result. Those are not definitionally equal. The assertion “no induction-unification mismatch exists” is false. The literal `accum→current` is not an exact source diff either; the current source already names its accumulator `current`.
- **Row92 / OP-60:** `members_change_implies_enacted` does not have the claimed removeMember-case proof. It analyzes propose/approve and threshold metadata, retaining `finishEnact` symbolically; it never unfolds `enact`. Removal becoming identity preserves both the property and this proof structure. Row102's claimed upstream failure consequently lacks a source.
- **Row2 / conservation:** donate under OP-22 leaves money effects unchanged, and the actual donate proof directly unfolds the transition, ignores the guard value, and uses the same two `bump_sum` rewrites. Calling this arm PROOF-FAIL is unsupported. The grant and accept arms consume inversion lemmas; that alone does not establish a diagnostic at conservation. OP-13's “drop accepted-move” must distinguish dropping the whole transfer from dropping only accepted credit while still deleting pending escrow; those have different conservation outcomes.
- **Row3 / AUTH:** the source calls **eight** inversion lemmas, not fourteen. The other six arms unfold `step` directly. No loss of a non-author guard or effect establishes semantic AUTH failure.
- **Rows72–73 / OP-53:** nonempty is not synonymous with malformed. A single clean open permission question can satisfy both carriers. Freeze a concrete violating list before claiming failure. `wrong`, `some(admit)`, fixed-accumulator substitutions and several effect-drop descriptions also require exact typed replacements.

Consumers can have semantically false statements even if their unchanged proof reuses a failed earlier declaration. Conversely, script failure with a true statement is not semantic falsification. Record both axes; neither the word KILL nor an upstream dependency discharges the original production-semantic requirement.

## S-04 — eight of the ten remaining OPEN value-insensitivity claims are refuted

**Blocks claimed examined extents and F-06.** I read the 31 check extents, their cited callee bodies and relevant fixture/production paths. The table below challenges the **current-map** universal; these are finite deductions, not observed kills or an exhaustive search for all possible mutants.

| OPEN extent / map row | Existing map atom | Static false-value path |
|---|---|---|
| 1 /170 admissionPreservation | OP-58B or intended OP-69 | Successful donate30 returns wiped members; `checkAppMembersPreservation=false`; admission conjunction false. |
| 2 /171 app-members mutant caught | OP-69L | Reversed productionWellFormed rejects the normal input; preservationDonate and memberWritingApply return error; detector=false. |
| 12 /181 disjoint mutant caught | OP-58B | First openQuestion wipes members; later casts reject, so expected dissent is absent. OP-39 touching only the opposite cast direction does not establish insensitivity to all atoms. |
| 14 /183 franchise mutant caught | OP-50 | Cast effect becomes identity; alice's recorded assent and bypass bob's membership-in-assents check fail. |
| 15 /184 noexpiry | OP-69L | Normal v3Group input rejects; the check's error branch is false. |
| 23 /192 sweep mutant caught | OP-58 | With strict base majority, Eve is not removed at two approvals; v3PostView still has three admins. The one-assent q is below threshold2, so sweepDuplicating does not move it; first non-vacuity conjunct false. |
| 25 /194 app-members mirror | OP-69L | Same false Bool as extent2; distinct identity and import boundary retained. |
| 30 /199 sweep-mutant mirror | OP-58 | Same false Bool as extent23; distinct identity and import boundary retained. |

Extent7 (threshold/comune sanity) and extent21 (stranger cast refusal) remain bounded unresolved rows against the listed atoms. Neither is proved universally value-insensitive or exempt. Sanity does call the actual threshold policy alias, so “nothing production to mutate” is too strong.

The 21 converted rows generally contain genuine false-value paths, with the exact-diff and alternative-atom qualifications in the 207-row ledger. Their reachability labels require S-05. All seven OPEN-table TraceTests sites are stale: actual sites are **1090, 931, 928, 937, 934, 943, 940**, respectively. This contradicts the claimed site-verified examined extent even where the named mirror is real.

## S-05 — source order does not prove first-failure blocking or cascade

**Blocks F-06 and cost/isolation reasoning.** I challenged the supplied assertion against the raw log. `P1C-build2-incremental.log:159–166` records the failure at `Reactivegas/Invariants.lean:407:11`; physical log lines167–172 then record later **info** outputs at source **1639,1640,1641,2351,2352,2353**. The same module continued elaboration. This refutes COST-MODEL §6's universal stop claim. It does not prove every later check executed, every later theorem succeeded soundly, or any new mutant loaded.

A failed prerequisite module can prevent a dependent import/build target. An earlier diagnostic in the same module is a different mechanism: elaboration can recover and later declarations can be processed, potentially carrying failed-declaration placeholders. Inspect actual diagnostics/dependencies and trusted axiom results in a future execution; do not infer failure or survival from source order or a function-call edge.

Two concrete causal errors survive independently of that general rule:

- **Row35** cites `open_mem` under OP-39. The sweep lemma and its open/closed/sweepStep dependencies do not depend on the mutated `placeBallot`; there is no such failing upstream.
- **Row207**, exactly once, cites `tryEnactBase_runs_hook` under OP-58 while row85 predicts it survives. The source supports preservation of the hook-on-reported-change property. A survivor supplies no failure to cascade.

The 31 OBSERVED labels are not observations made here. Treat valid edges as static dependencies until actual per-declaration outcomes are available. Rows48/60/78 also lack an exact causing mutant or conflict with their named source classification.

## S-06 — cross-file totals and references still disagree

**Blocks F-06's concrete plan, not merely presentation.** The four supplied contradiction examples are supported, with these limits: a baseline ELAB-STATIC row and a mutant-sensitive row for the same identity can coexist legitimately; the false part is the accompanying universal claim of no current kill.

- ELAB-GREEN says no row is killable by a current mutant, although canonical_economy and productionWellFormed explicitly have current-map falsifiers. Its A section lists **26**, not22; A26+B11+C4+D3 enumerates **44**, despite the 43-row footer. The extra Step mutant-detector row does not disappear because it has a structural label.
- OPEN-EXTENTS says 21 converted/10 OPEN at the top and all31 OPEN in its introduction/end. COST-MODEL still allocates all31 to NO-EXECUTION.
- Map mirror rows195–198 inherit the wrong numbered producer rows: **175→172, 172→173, 173→174, 174→177** are the necessary reference corrections. As printed, a base-close mirror inherits canonical economy and a direct-admission mirror inherits recomputation.
- Cost allocation omits **OP-69/69L** from the Step list, and row-based multipliers such as OP40×4/OP49×3 do not define independent executions versus multiple obligations in one compilation. OP-OPEN alternatives need explicit single-atom operation identities and deduplication.
- `solvent_init` is a direct proof over `State.empty`; it has no boot premise. “Structural” is defensible; “boot elimination” remains inaccurate.

A mechanically generated single registry should govern identity, atom, evidence kind, observation path and cost allocation. It must not generate semantic judgments from names or a shallow call parser.

## S-07 — the measurement request is not a sufficient executable costing campaign

**Blocks D5/P1-C/F-07.** The retained Step costing interaction is narrow and real: Step **1.2s**, Predicates **445ms**, failing RI **7.6s**, one-line donate positivity mutation, plus a restore-success log. Historical top-level 19/10/3/11/2-second prose observations are not interchangeable with retained timer receipts. OP-10's35 seconds spans its own build/elaboration/gates. No average or ceiling follows.

The raw Step log stops its successful build closure at failed RI. It does not measure Trace/TraceTests completion. Plain `lake build` has default library roots; **TraceTests, CorpusGate and CorpusExport are not all reached by those defaults**. The requested chains therefore need exact target closures rather than `...` and a presumed full-tree command.

The proposed18 invocations have these unresolved defects:

1. No initial clean baseline precedes cycle1; cold build is listed after the eight mutation/restore cycles. Incremental classification of cycle1 is consequently not established.
2. U-CHECK imports RI then evaluates unqualified `checkSweepIdempotent`, actually `Reactivegas.checkSweepIdempotent`. It is a `#eval`, not an isolated `by decide` elaboration. Even qualified, the recorded kind would still be mixed module elaboration/evaluation.
3. U-REPLAY is absent. No frozen standalone runtime artifact, input, command or timing boundary is specified.
4. No per-batch/shared-compilation experiment exists. Measuring one mutation per import root is sampling within a class; it does not establish every row's cost, batching feasibility, or an upper bound.
5. Common mutation/restore commands have no explicit retained wall-time/exit wrapper, timeout, failure-continuation protocol or changed-module loading proof. A version print alone does not pin executable identity. Unexpected exits must consume budget and be retained, never silently retried.
6. C-RSTATE refundAll->m first falsifies the **State.refundAll_sum** theorem, before the advertised deny/fail consumer. C-VOTESTATE's generic congruence/WF lemmas need not fail merely because the threshold comparison changes; expected observables must name an actual value-dependent check.
7. Restoration/single-cause attribution are different from filesystem isolation. Original argv/LEAN_PATH and the owner's description place the mutation in the candidate checkout. Future scratch work can comply prospectively; it cannot cure that historical fence event or recover a lost cold log.

This is a request for additional commissioned work, not authorization supplied by this audit. It should not be executed as printed.

## Remaining required work and successor

Keep all five PARTLY original findings blocking. Preserve all239 identities and both semantic axes; do not convert these findings into residuals or alter original statements. Complete the receipt-level archive assessment, normalize exact atoms/property relationships and the 207-row operation registry, correct the eight false OPEN searches and other source classifications, and freeze complete measurement instruments with bounded accounting. New measurement must separately cover cold, incremental production, proof/check, runtime replay, restore and sharing/batch costs; retained history remains history.

[Successor recommendation](SUCCESSOR-RECOMMENDATION.md) gives a bounded commissioning sequence with concrete outputs and stopping rules. The immediate next seat is a fresh static specification/instrument owner, **one submission, zero project execution**, followed by an independently commissioned measurement seat only after exact operations and numeric ceilings are approved. It is not the exhausted author's fourth repair and not another self-review loop. An incomplete successor freezes its findings and returns control; it does not grant itself another round.

D6's reader experiment remains VOID. Known source/comment ambiguities are recorded in [ONWARD-DISCOVERIES.md](ONWARD-DISCOVERIES.md), with #71/#81/S5 routing through the commissioner. None of the blocking in-scope findings is moved there. No acceptance, #66 closure, S3 coverage, remote CI, author contact, source edit or execution is implied.
