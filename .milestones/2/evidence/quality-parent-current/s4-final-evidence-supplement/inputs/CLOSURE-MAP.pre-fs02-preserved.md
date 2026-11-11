# CURRENT STATE — authoritative, 2026-09-05

> **Read this section first. Where anything below it disagrees, this section
> wins.** Everything after the divider is the historical record, preserved in
> the order it was written, including wordings later corrected. It is kept so
> the corrections remain auditable — not as current guidance.

## Where the work actually stands

| | |
|---|---|
| **`master`** | **`4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`** — S1 landed, squash, verified tree-identical to accepted `fa01779` |
| **#66** | **OPEN**, and stays open until S2–S5 and the routed items are done |
| **S1** | **CLOSED.** Accepted at `fa01779`, merged as `4a6cd87`. Receipt `handoffs/S1-MERGE-RECEIPT.md`, acceptance `handoffs/S1-ACCEPTANCE.md` |
| **S2** | **IN FLIGHT — submission 3.** Branch `chore/66-s2-axiom-gate`, worktree `/code/reactivegas-66-s2`, PR #85 draft. **Mandate v3 `a8e18e478ca8d063`** (brief ∥ AMENDMENT-1 ∥ AMENDMENT-2); v1 `0a1db9887ccc9d8f` and v2 `7cfb7aec95a37448` preserved as evidence. Gate at `561347d` = `cd67ade9bc137f87`. **Submissions 2/2 exhausted, then explicit desk extension 2→3 (NOTE-024); no fourth.** Owner ceiling **11→14, second and final raise**; 10 spent, **4 attempts remain including failures**, plus ≤16 separately counted probes. **Campaign EXHAUSTED and preserved.** Three submissions incl. the exceptional third; owner **14/14**; final audit **6/6 builds + 24/24 probes**. `b0c2cdb` **NOT ACCEPTED**, terminal verdict **AUDIT-FINDINGS** (report sha256 `8e27e455…`). **TWO binding items.** **F-004**: equivalent relative project path yields B=0/T=0; the completed Std-alias probe finished exit 1 in 712 s at **S=26/26, B=288, T=15707**, misclassifying **all 262 dependency `Std` modules as project-built**, no panic, no import error. **G-001**: the empty/unset `LEAN_PATH` entry guard was **never fired** — an unexecuted required control under AMENDMENT-2 row 6, instrument prepared and hashed, cost **one elaboration, zero builds**; an **assurance gap, not proof the guard is broken**. Supported statements only: the audited default inventory agrees and CI passes, and the adverse-path controls are **loud false refusals**, which is **not** evidence of current silent poison acceptance. **No "fails safe" generalization.** **Desk NOTE-030: re-cut** — no fourth submission, no narrowed acceptance. Successor campaign proposed: `handoffs/S2-SUCCESSOR-CAMPAIGN-PROPOSAL.md`, **awaiting desk disposition; nothing dispatched**. Rejected: `5745a2c` (F-001, F-002) and `561347d` (F-003) — both preserved with their audits |
| **S3** | Contract at **revision 3**, `handoffs/S3-MANDATE.md` (v1/v2 preserved). **Phase 1 AUTHORIZED** (NOTE-021) but **not dispatched** — conditional on the **accepted, landed** S2 base. Brief bound at `77c9d6bf8425afdd`, **3-build cap**, no coverage claim. Phases 2..n unauthorized |
| **S4, S5** | Authorized, not started, **not discharged by S3**. Contract packets at **revision 2** (NOTE-027): `handoffs/S4-CONTRACT.md`, `handoffs/S5-CONTRACT.md`, v1 of each preserved. Auditors restricted to the inherited set `codex`/`grok`; numeric targeted-query ceilings set; blocked rows stay on the milestone completion map. S5 owns **both** retention completeness outside the ruled #81 lifecycle **and** `ONWARD-68-INV-01` |
| **#71** | Holds §5a, §5c, §8b content and C7. **#71 alone writes `docs/en/design/`** |
| **Desk** | Owns any semantic ticket arising from §10; #81 owns the V-5 lifecycle |

## The four divergences are DECIDED, not pending

All four were accepted by the desk in NOTE-006 and are rewritten in place below.
**Nothing is awaiting a divergence decision.**

| row | disposition |
|---|---|
| §1b | ACCEPTED as a historical **verification limit** only. "not adjudicable" removed |
| §8a | ACCEPTED. No separate `Goals.lean`; statement obligations, ruling traceability and total axiom gates **remain required** |
| §8b | Filename ACCEPTED; **verbatim authority content remains an open #71 deliverable** |
| §9 | ACCEPTED as build organization, **plus** a new obligation: executable controls proving a newly discovered module cannot evade the mandatory path |

## The three decoys are NOT residuals — that proposal is withdrawn

Any text below offering `checkI57Trust` / `checkI57Direction` /
`checkI57Toolchain` as **named residuals** is **superseded**. The desk rejected
it: a report calling them residuals does not repair them. S2 renames all three
and their wrappers to what they compute, deletes the three dead `TraceTests`
re-exports, and binds each real obligation to its actual enforcer — trust to the
new axiom gate, direction to `lean-dependency-direction.sh`, toolchain to
`check-lean-toolchain`.

Likewise superseded: any claim that import-direction or toolchain checks are
*inherently impossible in Lean*. They are not. The precise claim is that **those
Bools did not compute those properties**, and the obligations are enforced by the
existing external scripts in the mandatory path.

## `ONWARD-68-INV-01` — inversion binding is not inversion exactness

Inherited from the terminal #68 audit
(`/tmp/reactivegas/ms2/t68-proposer-assent/.archived/auditor-s1/handoffs/audit-report.md`,
sha256 `37f3f1b2017646b07c14c4d1859846b17454a32bfbc6b53a589cb559b04899dd`;
onward record `onward-discoveries.md`). **Owned by S5.** No expansion of the
frozen S2 candidate, no #68 model edit.

**The finding.** That audit independently reproduces **14/14 constructor
bindings** but only **11/14 unrestricted converse proofs**. `step_pledge_inv`,
`step_accept_inv` and `step_close_inv` **omit the live non-stalled guard**: a
compiled close-state value satisfies the exposed conjunction while `stepEvent`
refuses because `stalled`.

**What it is not.** Forward implications remain true — this is **not** a
false-forward-theorem finding, and **not** a reachable-economic violation claim.
No genesis reachability was established, and none is needed to refute an
*unrestricted* converse. **The accepted six inversion repairs remain green.**

**Why the six are unaffected, stated precisely.** The mandatory path proves
tightness only for `requiredInversions` — `step_open_inv`, `step_deposit_inv`,
`step_withdraw_inv`, `step_transferCassa_inv`, `step_donate_inv`,
`step_backdonate_inv`. The other eight carry **bindings without a
machine-checked converse**, and three of those eight are now shown not to be
exact.

**The distinction to carry, and it corrects a reading of our own S1 result:**
S1's `covered=14 missing=0` is a **binding** count. **Metadata 14/14 does not
establish exactness 14/14**, and an owned gap does not stop being a finding
because it has an owner.

**S5 owes, per NOTE-023:**

1. re-establish the **exact current signatures and guard comparison**, with the
   evidence limits stated;
2. state, **per actual consumer**, whether these inversions claim **exact
   premises** or only **necessary conditions** — the two are different
   contracts and the consumers may not agree;
3. the concrete **disposition or repair obligation** required for the
   completeness that is being claimed.

The independent evidence stays available to #71 **through the desk**.

## `CI-T-SHARED-FILTER` — named ADVISORY LIMIT (desk NOTE-024)

Accepted by the desk as a **named advisory limit**, and it must appear in this
map **and in the final S2 PR body**.

The axiom gate derives its theorem set **twice** — a per-built-module constant
walk, and a fold over environment constants attributed by `getModuleIdxFor?`.
Both derivations **share `thmInfo` and B membership**, so they are **two views of
one inventory, not two independent theorem sources.**

Demonstrated survivors, executed by the submission-2 audit:

| mutation | result |
|---|---|
| skip the same identity in **both** derivations | **exit 0**, `axiom-gate: ok`, `count=1212` |
| drop `KelGroups.Types` from the T-side `builtStrings` copy after S/B reconcile | **exit 0**, `count=1162` (was 1213), S/B still 26/26 |

One-sided skips **are** caught, in both directions — that required control works.

**What may not be said about it:** that the gate has two independent theorem
sources, or that it resists common-filter omissions. It does not. Current
agreement between the independent enumerator and the gate (occurrences 1214,
distinct 1213, duplicate `KelGroups.setInsert.eq_1`) is **evidence about the
current tree, not proof of future checker completeness.**

This advisory **waives nothing** — not F-003, not any required independent row.

## Superseded numbers

| withdrawn | why |
|---|---|
| "6 of 224 theorems (≈3%) carry a tracked mutant" | inconsistent counting rule; `checkCanonicalEconomyMutant` is a fixture comparison, not a killed transition mutant. **No corrected percentage is offered** |
| "19 Prop predicates, zero Bool mirrors" | false twice — two have executable decidability, and 19 was my filter's output, not the extent. Re-derived: **24 found, 23 authored, 2 decidable, 4 authored inductives, 17 instance-less** |
| "the only finding whose cost rises with delay" | unsupported; evidence loss also raises cost |
| `expectedDeclarations` "not yours to touch" | too broad. The prohibition is against another **quota**, not against **real discovery**. S2 removes it |

## Current owed list

- **S2** — submission 3 in flight for **F-003's discriminator only**; F-001 and
  F-002 are **closed at `561347d`** on executed evidence and not reopened. Then a
  **fresh FULL Codex audit** (`gpt-6-astra`, effort `high` **argv-pinned**, fresh
  context/root/START, `.lake` initially absent) over the **entire unaccepted
  candidate** `4a6cd87..final SHA` — ≤6 build/gate attempts, ≤24 probes, no
  automatic raise. **Both build ceilings are now exhausted as raises: owner 2/2
  used, and there is nothing left to grant.** Budgets itemized in
  `handoffs/S2-BUDGET-PLAN-SUBMISSION3.md`; both fit with zero slack.
- **S3** — `handoffs/S3-MANDATE.md`, not dispatched.
- **S4** — Prop/Bool correspondence, classify before writing, fresh audit.
- **S5** — two owned obligations now:
  - retention statement-completeness, bounded outside V-5;
  - **`ONWARD-68-INV-01`** — inversion **exactness**, inherited from the terminal
    #68 audit (NOTE-023). See the row below.
- **#71** — §5a, §5c, §8b content, C7.
- **Desk** — semantic ticket from §10; #81 owns V-5.

---

# HISTORICAL RECORD — preserved, superseded where it conflicts with the above

# Closure map for #66 — every report finding, with its disposition

Required by NOTE-004. **This is not a completion claim.** Nothing here proposes
#66 complete; it states, for every finding, whether it is closed by an exact
artifact, assigned to a named pending slice or to #71, or offered as a
specifically justified divergence for desk review.

Bound to the accepted findings of the choices audit
(`../auditor-choices-codex/handoffs/AUDIT-REPORT.md`, verdict AUDIT-FINDINGS)
and to extents **discovered in this build**, never to a count copied from a
note. Where the audit withdrew one of my measurements, the row carries the
withdrawal, not the withdrawn number.

Base `e6c5924`; S1 candidate `09f8230` under independent audit in `%513`.

## Legend

| mark | meaning |
|---|---|
| **CLOSED** | an exact artifact or executable check discharges it, named here |
| **SLICE Sn** | assigned to a named pending slice in this lane |
| **#71** | design-record content, already routed, not this lane's to close |
| **DIVERGENCE** | offered as a justified divergence, **requires explicit desk acceptance** — unfinished until then |
| **WITHDRAWN** | my own claim, retracted on audit evidence; nothing to close |

## The map

| # | Finding | Disposition | Discharged by / owed |
|---|---|---|---|
| §1a | Zero-`sorry` / axiom cleanliness of the current head | **CLOSED** | Fresh-`.lake` sweep over the discovered extent: 26 library modules from 27 tracked files, **1213 unique compiled theorem identities**, zero `sorryAx`, zero non-standard axioms. Independently re-derived by `%508` with matching axiom multiset and four self-falsification controls. |
| §1b | Historical axiom provenance | **ACCEPTED** (NOTE-006) | As a historical **verification limit** only: current candidate re-established; historical provenance not adjudicated. The stronger "not adjudicable" is **removed** — no evidentiary impossibility was proved, and old receipts need not be fabricated to establish today's trust. |
| §1c | No CI gate takes a total axiom receipt on a fresh `.lake`; the only axiom gate covers its declared six inversions | **SLICE S2** | Owed: a tracked gate quantifying over the discovered extent, with an executable negative control. The sweep instrument exists; wiring is the work. |
| §2a | No `lean/<MACHINE>-MUTANTS.md`; evidence untracked and keyed to `INV-` rows, not theorems | **SLICE S3** | Owed: theorem-keyed ledger. |
| §2b | "6 of 224 theorems (≈3%) carry a tracked mutant" | **WITHDRAWN** | Audit C1: `checkCanonicalEconomyMutant`'s compiled closure contains no `stepEvent` — a fixture comparison, not a killed transition mutant; and excluding TraceTests' controls while `all_checks_pass` is a compiled theorem over them is an inconsistent counting rule. **No corrected percentage is offered.** S3 must first separate shipped fixtures, executed same-property failures, and real production-definition campaigns. |
| §2c | Runtime evidence loss risk | **CLOSED** | Archived outside `/tmp`: `/home/paolino/reactivegas-ms2-runtime-archive/ms2-runtime-20260905-0833.tar.gz` — 6489 files, 43 campaign ledgers, 27MB. Preservation deliberately decoupled from mapping, per audit C6. |
| §3 | Manifest resolves only unqualified names; two instruments disagree 14/14 vs 8/14; six proved guards emit `UNPROVED` | **SLICE S1, candidate submitted** | Candidate `09f8230`. Independent audit running in `%513`. Not closed until that verdict returns. |
| §4 | Three checks green under names describing properties they do not compute | **SLICE S2** | Defect call **upheld** by audit: their compiled dependencies never query `collectAxioms` or the environment. Owed: implement honestly (the axiom sweep *is* what `i57TrustNoSorry` claims) or record named residuals. Audit warns: do not generalise the six-inversion axiom gate into a mitigation for all contractual statements. |
| §5a | 37 declarations / 29 short names are finite oracles (`check… = true`) whose names read as universal laws | **#71** | Confirmed by audit *for the stated syntactic category*, with the caveat that it is **not** a census of every finite witness — `majority_table` and others use different syntax. Law-vs-witness marking is design-record content. |
| §5b | "`open_questions_are_open` is the run-level companion, so the guarantee is not missing" | **WITHDRAWN** | Audit C3: it states only that a question *still present* has verdict open; it does not assert retention, and the post-state condition survives replacing every open question with an empty map. The intended no-expiry guarantee goes to statement/authority reconciliation. Not a production counterexample; no missing guarantee declared proved. |
| §5c | `canCloseGroup` orphan (`Predicates.lean:85`) | **#71** | Compiled-model observation confirmed by audit: no other compiled constant references it. That is the dependency fact, **not** authority to delete a design law. Disposition stays with #71. |
| §5d | `Predicates.lean` cites `docs/design/state-machine.md`; actual path is `docs/en/design/` | **SLICE S2** | Lean-side doc comment; trivial, but not to be done under an unrelated slice. |
| §6a | Refusal names reconcile to guard hypotheses | **CLOSED** | `TraceTests.permittedNames` (independent literal table), `checkCoveredDeclarationBound` (membership, not prefix), `checkGuardOfAgrees` over a total sample. Its one hole was §3, in flight. |
| §6b | "19 Prop predicates, zero Bool mirrors" | **WITHDRAWN and re-measured** | Audit C2 falsified both halves. Re-derived over the discovered extent in this build (all non-theorem, non-constructor project constants whose type telescopes to `Prop`, with `Decidable` synthesis attempted per constant): **24 found, 23 authored** (`Reach.below` is compiler-generated and excluded). **2 carry synthesizable `Decidable` instances** — `KelGroups.Vote.PreservesQuestionSemantics` and `stalled` — independently matching the two the audit named. **4 are authored inductives** (`KelGroups.WellFormed`, `KelGroups.Vote.SweepReady`, `KelGroups.Vote.VoteWellFormed`, `Reach`); `Reach` is a reachability predicate for which a decision procedure is not a reasonable expectation. **17 remaining `def`s carry no instance.** |
| §6c | The genuine correspondence question, on the corrected extent | **SLICE S4 (new)** | NOTE-004 is explicit that this is a distinct standard requirement, discharged neither by an axiom receipt nor by S3. Owed: classify the 17 into simulator-consumed, definitionally-corresponding, and genuinely missing; ship mirrors with correctness theorems only where actually missing. A missing *separately named* mirror theorem is an artifact convention, not proof of missing correspondence. |
| §7 | `LEAN-CLARITY.md` absent | **SLICE S3** | Void **upheld** by audit for the historical experiment. Owed: record the void, the known ambiguities, and prospective observations. "Void" forbids neither a future fresh experiment nor recording ambiguities already known. |
| §8a | `<Machine>Goals.lean` absent | **ACCEPTED** (NOTE-006) | No separate `Goals.lean` filename required. **Complete statement obligations, ruling traceability and total axiom gates remain required** — they sit in §1c, §5a and §5b, and the filename acceptance discharges none of them. |
| §8b | `decisions.md` absent | **ACCEPTED (filename) + OPEN #71 (content)** (NOTE-006) | Filename divergence accepted. **Verbatim and precise authority content remains an open #71 deliverable** — an issue title alone does not supply a missing ruling. Not a waiver. |
| §9 | `lake build` alone does not build `TraceTests`/`CorpusGate`; their CI coverage rests on the coverage script's file discovery, not the lakefile | **ACCEPTED as build organization** (NOTE-006) + **new obligation, SLICE S2** | Accepted: discover all required modules in the mandatory CI path. **New requirement:** retain executable controls proving a newly discovered module **cannot evade** that gate. "`lake build` alone" is not a full gate claim and must not be stated as one. |
| C5 | Two of my three stated reasons for the unqualified rendering were factually wrong | **CLOSED (correction only)** | Decision itself upheld by audit and not reopened. Corrections recorded in `COMPLIANCE-REPORT.md`; commit owner reported the true figures (six manifest bindings, one envelope occurrence) regardless, so nothing was accepted on the wrong rule. |
| C6 | "§3 is the only finding whose cost rises with delay" | **WITHDRAWN** | Evidence loss also raises cost; no imminent `/tmp` deletion was observed, so a deadline in either direction would be invented. S1 remains first *code* slice; #74 means S1 before final content freeze, not that exporter work is blocked. |
| C7 | `DESIGN-RECORD-FOR-71.md` writes `Reactivegas.State`/`Reactivegas.Event`; the compiled constants are root `State` and `Event` | **#71** | Harmless as prose, wrong if #71 makes those citations executable. Correction routed to the desk. |

## What is owed before #66 could be proposed complete

- **S1** verdict from `%513`, then acceptance or repair.
- **S2** — axiom gate over the discovered extent (§1c), decoy checks (§4), doc path (§5d).
- **S3** — theorem-keyed mutant ledger (§2a) with the counting rule the audit
  requires, and `LEAN-CLARITY.md` (§7).
- **S4** — Prop/Bool correspondence on the corrected extent (§6c).
- **Four divergences** — §1b, §8a, §8b(partial), §9 — each needs **explicit desk
  acceptance**. Until then they are unfinished, not waived.
- **#71** holds §5a, §5c, §8b content and C7.

No semantics change and no simulator rewrite is proposed under #66. If S2 or S4
turns out to need one, it is surfaced to the desk rather than commissioned here.

---

# Desk dispositions and new obligations — NOTE-006, 2026-09-05

The four divergence rows above are now **decided** and rewritten in place. Three
further items follow. Nothing here proposes #66 complete; #66 stays open until
its substantive closure conditions actually hold.

## §5b re-opened — a withdrawal is not a closure

**Corrected.** §5b was marked WITHDRAWN, which is right about *my claim* and
wrong about *the obligation*. Withdrawing a false reassurance does not discharge
the underlying requirement; it only removes the thing that was pretending to.

| | |
|---|---|
| **Row** | §5b-R — no-expiry / retention statement completeness |
| **Status** | **OPEN**, assigned |
| **Owner** | this lane (`e-lean-compliance`), as a **statement-completeness assessment**, not a proof task |
| **Action** | Determine whether a statement asserting *retention* of a previously open question exists in the model. `open_questions_are_open` does not: it constrains a question **still present**, and the audit showed the same post-state condition survives replacing every open question with an empty map. **A present question being open does not prove a previously open question was retained.** |
| **Slice** | **S5** (new), sequenced after S4 |
| **Bounds** | No unreviewed strengthening of antecedents. **No semantics change inside a quality repair.** If the assessment finds the model genuinely lacks a retention guarantee, that is a statement-vs-ruling reconciliation for the desk, not a repair to commission here. |
| **Feasible now?** | Yes. Past audit *ordering* cannot be reconstructed; a statement-completeness assessment against the current model can be done today and remains relevant. |

## §10 — Slice B vocabulary declared but unreachable, and a renounce that succeeds and does nothing

Verified by this lane on the candidate, at the desk's instruction. **Not a new
finding of mine — the code says both plainly**, which is the opposite of the §4
decoy pattern and should be recorded as such.

| observation | evidence |
|---|---|
| `VoteError.notDesignee` and `VoteError.notProposer` are declared but **never constructed** by any path | `KelGroups/Vote/Validate.lean:41-42` declares them; its doc comment at :19-22 states outright *"nothing in Slice A produces them yet"* |
| `effectedState` on `.renounce` is **identity** | `KelGroups/Vote/Fold.lean:101` `\| .renounce _ => gs`; three inversion proofs discharge it by `rfl` (`Vote/Invariants.lean:661, 768, 1067`) |
| `validateVoteEvent` **accepts** a renounce by any responsabile on an existing question | `Vote/Validate.lean:65-70` returns `.ok ()` |
| So on the integrated route a renounce **succeeds and changes nothing** | `Reactivegas/Step.lean:189` routes `.renounce qid` to `voteApply` |
| The bare economic `step` **refuses** `.renounce` | `Reactivegas/Step.lean:142` `\| .renounce _ => none` — the two levels treat the same constructor differently |

**The statement/semantic obligation, stated for the cross-lane closure map:**

1. A refusal vocabulary wider than the machine means any theorem quantifying
   over `VoteError` carries **vacuous arms** for two constructors. Vacuity is
   exactly what the standard's witness rule exists to catch.
2. **Accept-and-no-op is not the same as refuse.** A caller who renounces is
   told it worked. **Corrected by NOTE-008: this is not an open ruling question.**
   V-5 already rules it — *proposer leaves or renounces implies close, running the
   negative continuation; escrow must refund, silent deletion strands money* — so
   the renounce no-op is **unfinished implementation against a settled ruling**,
   owned by **#81**, not a question for the operator. It remains the second
   machine's half of the composition edge the standard requires when two machines
   share an object.

3. **The two dormant `VoteError` constructors are the opposite case.** #81
   excludes them deliberately: a non-proposer's `renounce` and a non-designee's
   ballot are **unruled**, and #81 declines to read a refusal off a dormant
   constructor name. `notProposer`/`notDesignee` express an intention, not a
   ruling. They stay open operator questions, and today's accept-and-do-not-decide
   behaviour is not authority to widen them either. The vacuous-arm observation
   above stands regardless of how they are eventually ruled.

**Disposition:** recorded here as an uncovered statement/semantic obligation
rather than silently accepted as quality-complete. **The desk owns any new
semantic ticket. S1 is not widened, and no repair is commissioned from this
lane.** #67's packet already records these as unfinished Slice B work; this row
exists so #66 cannot close over them by omission.

## S4 — authorized, with the desk's scope

| | |
|---|---|
| **Status** | **AUTHORIZED** (NOTE-006) |
| **Extent** | the corrected, discovered extent — re-derived in this build, never copied from a note |
| **Method** | **classify before writing**: actual simulator consumers, definitional equivalence already present, and genuinely missing correspondences — three classes, established first |
| **Explicitly not required** | deciding arbitrary reachability (`Reach` is not owed a decision procedure); any blanket mirror quota over the instance-less `def`s |
| **Close-out** | a **fresh independent audit** must validate both the classification and the proofs. Not closeable on this lane's word |

## Revised list of what is owed before #66 could be proposed complete

- **S1** — verdict from `%513`, then acceptance or repair. Candidate `09f8230`
  remains under independent audit; **no merge authorization**.
- **S2** — total axiom gate over the discovered extent (§1c); decoy checks (§4);
  doc path (§5d); **executable controls proving a newly discovered module cannot
  evade the mandatory CI path** (§9, new).
- **S3** — theorem-keyed mutant ledger (§2a) under the counting rule the audit
  requires; `LEAN-CLARITY.md` (§7).
- **S4** — Prop/Bool correspondence, classify-then-write, fresh audit (§6c).
- **S5** — no-expiry / retention statement-completeness assessment (§5b-R).
- **#71** — §5a, §5c, §8b content, C7.
- **Desk** — any semantic ticket arising from §10.

Retained and preserved as required: the choices audit
(`../auditor-choices-codex/handoffs/AUDIT-REPORT.md`) and the corrected report
with its C1–C7 block.

---

# NOTE-008 routing — #81 V-5 into S5

**S5's statement/retention assessment now carries #81's ruled content.** The
distinction NOTE-008 draws is the one that matters and it corrects my §10 above:

| item | status | owner |
|---|---|---|
| proposer **renounce** ⇒ close, negative continuation, cause `.renounced` | **RULED** (V-5). Unfinished implementation, **not** an operator question | **#81**, desk-owned |
| proposer **departure** ⇒ close their open questions, cause `.proposerDeparted`, atomic with the departure | **RULED** (V-5) | **#81** |
| the closure is **retained** in `VoteState.closed`, not dropped (#81 row L-5) | **RULED** | **#81** — and this is the row S5 must not duplicate or contradict |
| a **non-proposer's** `renounce` | **UNRULED**. `VoteError.notProposer` is an intention, not a ruling; #81 explicitly declines to read a refusal off a dormant constructor name | open operator question |
| a **non-designee's** ballot on a permission question | **UNRULED**, same reasoning | open operator question |
| prose for any of it | — | **#71** |

**What this changes for S5.** S5 asks whether a statement asserting *retention*
of a previously open question exists. #81's L-5 rules retention for the V-5
closure routes specifically. So S5 must:

- **not** re-open V-5 lifecycle semantics — that is #81's, and S5 has no mandate
  to specify or implement it;
- assess statement/retention completeness for the routes **outside** V-5, where
  no ruling supplies the answer;
- where a retention gap coincides with a V-5 route, record **both**: it is a
  finding about the current model's completeness **and** it has a named owner in
  #81. **Corrected by NOTE-010:** a dependency does not stop being a finding
  because it carries another ticket number. The gap is not erased, and the model
  is not complete because someone else owns closing it.

The two dormant `VoteError` constructors remain a **vacuity** observation for
this lane — a theorem quantifying over `VoteError` carries arms nothing can
reach — which is true however they are eventually ruled, and is not a claim that
they should be refused.

---

# S1 audit residuals — as corrected by NOTE-009

Two ADVISORY residuals came out of the `09f8230` audit. Both are recorded here
with the desk's corrections applied, because my first wording of the first one
was wrong in two ways.

## R1 — last-component resolution has no declared precedence

**Accepted only as the documented syntactic existence check in this slice**,
with the measured **zero** current `Event` last-component collisions among its
fourteen bindings.

Two corrections to how I first wrote this:

- **It is not imposed by the unqualified-rendering rule.** Unqualified *public
  rendering* of the `declaration` field does not logically require an *ambiguous
  internal resolver*. Those are independent choices; the residual is a property
  of this slice's resolver, not a tail of the desk's rule. My earlier framing
  put the cause in the wrong place.
- **The probe does not prove a production hijack.** `Other.step_alpha_inv` binds
  a **dummy** inductive. There is no matching production witness, and
  `Trace.lean` already declares this limit explicitly. Nothing here shows a
  production `Event` inversion can be hijacked.

**Not a permanent waiver and not proof of future safety.** The resolver's
semantic-checking limit is carried into the **S2/S3** assessment.

## R2 — M2 compares counts, not covered-sets by name

Appending `PERMUTED-NAMES` to a `14/14/0` line leaves the agreement script green
(`evidence/wrap-perm.out`). At `missing=0` a same-count permutation is vacuous.
Instrument A's name↔hypothesis bind and B's `checkCoveredDeclarationBound` reject
that class when they run; M2 itself does not see it. Unchanged by NOTE-009.

## The totality repair, and the evidence rule it establishes

`INV-S1-GETSTRING-TOTAL` was promoted to a required repair and closed at
`fa01779`. NOTE-009 established that my own justification was too weak: I called
it a *latent* panic; it was **already firing**. At `09f8230` the retained
`probe-hijack-v2.out` carries **70** `PANIC at Lean.Name.getString!` lines from
line 4, before `HIJACK-V2-BEGIN` at 7144 — and that run printed every expected
row and exited **0**.

**Standing rule for this lane, from that:** exit status and correct output are
not evidence of totality when a partial call can panic and fall through. A
totality control must assert **absence of the panic string**.

Control run with the auditor's own frozen instrument, unmodified:

| | `09f8230` | `fa01779` |
|---|---|---|
| `PANIC at Lean.Name.getString` | **70** | **0** |
| output | 1,142,334 bytes / 7,147 lines | 7 lines |
| exit | 0 | 0 |

Exit is 0 on both sides, which is exactly why it cannot be the control. The
`09f8230` log is retained as the positive control and cited by path and count;
its backtraces are not reproduced anywhere.
