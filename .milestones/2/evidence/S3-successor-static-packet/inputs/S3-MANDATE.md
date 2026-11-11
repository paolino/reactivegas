# S3 contract — revision 2

Revised per NOTE-019. **Preparation only. Not dispatchable until the desk
accepts this revision; no seat before the accepted S2 base.** Revision 1 is
preserved at `S3-MANDATE-v1-superseded.md`; what changed and why is at the end.

S3 is **not** satisfied by a file whose empty rows are honestly labelled, and
**an assessment checkpoint that truthfully returns missing required evidence does
not complete S3's required coverage and does not finish #66.**

---

## D1 — Inventory and semantic domain, discovered from the accepted base

### D1a — authored theorem inventory

Classified per identity, inventory never quietly shrunk:

| class | may it leave the required denominator? |
|---|---|
| **AUTHORED-STATEMENT** — states a property of the model | **No** |
| **HELPER-FACT** — a mathematical or structural lemma true independently of this model | Only **per identity**, with reasoning and a desk disposition |
| **COMPILER-GENERATED** — `.eq_def`, `.eq_N`, `.injEq`, `.sizeOf_spec`, `match_N.eq_N`, `Reach.below`, deriving output | Yes by construction — but the **rule** is stated and the excluded identities **listed** |

`private` theorems are **in**.

**Helper facts are characterized on their own terms.** A lemma about
`assocInsert` or list `Nodup` has no state-machine antecedent, and **none is to
be invented for it** so that a reachability column can be filled. Its
"reachability" obligation is that its hypotheses are satisfiable, not that a
machine state reaches it. **Any exemption is per identity with reasoning — never
a class sweep, and never a convenient denominator shrink.**

**The figures 76, 29 and 224 are historical measurements from the S1 assessment,
not discovery constants.** They are recorded for comparison only; S3 re-derives
its own extent and must not seed, validate or bound itself with them.

### D1b — semantic domain, from the actual entry points on the accepted base

`stepFn` **does not exist in this codebase.** It is `system-design` vocabulary
that revision 1 imported instead of naming the real code — corrected here. The
production entry points, discovered at the accepted base:

| module | entry points |
|---|---|
| `Reactivegas.Step` | `step`, `stepEvent`, `voteApply`, `appFold`, `apply` |
| `KelGroups.Fold` | `enact`, `tryEnact`, `applyEvent`, `applyEventDetailed` |
| `KelGroups.Vote.Fold` | `effectedState`, `sweepClosures`, `applyVoteEvent`, `foldVote` |
| `KelGroups.Vote.Validate` | `validateVoteEvent` |
| `Reactivegas.Invariants` | `voteApplyHardPolicy` |

| `KelGroups.Validate` | `validateProposal`, `validateApproval`, `validateDirectAdmission`, `validateBaseMutation`, `validateBaseApproval`, `validateBase`, `validateEvent` |
| `KelGroups.Integration` | `applyIntegratedEvent`, `foldIntegrated`, `tryEnactBase`, `commitBaseChange`, `admitMemberInto`, `enactMutation`, and the `BaseHook` / `IntegratedAppFold` reached hooks |

**This table is NOT an allowlist and must not be used to seed one.** Revision 2's
version omitted `KelGroups.Integration` and most of `KelGroups.Validate`
entirely — corrected above after the desk caught it, which is itself the evidence
that a hand-written entry-point list goes stale the moment it is written.

**S3 re-derives the entry points and their reached hooks at its own base**, from
the sources, and treats any disagreement with this table as a finding about the
table. The table exists only so a reader can check that derivation, and so the
`stepFn` error is not repeated silently.

The guard/effect/error axis is then: the `Event` and `VoteEvent` constructors,
the guard conjuncts and refusing `match` arms of those entry points, the
`GuardId` and `VoteError` vocabularies, and the base-hook effects.

**D1a and D1b are different axes, related by ownership — not multiplied.**
"One mutant per guard" and "a killed mutant per owning theorem" are separate
obligations and the contract needs both, but `REQUIRED-INPUT` is **not** the
Cartesian product of every theorem with every guard.

Derive the two sets, then derive the **actual semantic ownership relation**
between them: which theorem properties a given guard or effect atom actually
constrains. The obligation is that **every required guard/effect atom** and
**every in-scope theorem property** has its *relevant* coverage — not that every
pairing exists.

**A single admitted atom may constrain several theorem rows**, and may close all
of them, provided each row's **property-specific failure is actually observed**.
**Never manufacture unrelated mutants to fill a cross product**: an atom paired
with a theorem it does not constrain produces a meaningless row and inflates the
denominator.

---

## D2 — The map, with evidence direction stated per instrument

One row per required identity: **theorem identity · precise property (read the
statement, never the name) · reachable witness or satisfiable hypotheses ·
production definition · single semantic atom mutated · falsification evidence**.

### The three mutation kinds, and exactly what each can establish

| kind | establishes | does **not** establish |
|---|---|---|
| **FIXTURE** — a test datum or frozen constant | that a check reads its input | anything about whether a theorem constrains the model |
| **CHECKER** — the check or oracle itself | **sensitivity of the checks that inspect or exercise that checker**, *when the correct failure is observed* | that the mutated checker "can fail" — `always-true` is the counterexample. **But that case has its own conditional value:** a surrounding test *can and should* detect an always-true checker, and **if the mutant survives the required surrounding check, that survival is itself evidence of an assurance gap.** What it never shows is that the altered checker can reject |
| **PRODUCTION-DEFINITION** — the shipped definition the theorem quantifies over | **the only kind that can kill a theorem row** | — |

Never summed. Each row names its kind.

### Admission conditions for a production mutation

**Revision 3 corrects this. Revision 2 banned the main Lean falsification
method.** It required "the module still elaborates" and said "a mutant that
fails to compile has demonstrated nothing" — which would have excluded exactly
the evidence a Lean mutation campaign produces, since mutating a definition
normally makes the dependent theorem's *proof* stop elaborating, and that **is
the kill**.

A production mutation counts when **all** of these hold, each evidenced:

1. it is **admitted as the intended single semantic atom** — the diff is that
   atom and nothing else;
2. the **mutated production definition itself remains well-typed**. The
   requirement is on the *definition*, never on the theorem meant to fail;
3. the observed failure is **because that definition no longer has the claimed
   property**. Any of these is relevant RED evidence: a named theorem's proof
   failing, a `#guard` failing, a semantic check failing;
4. the failing subject is named, with the command and output.

**Excluded** are syntax errors, import errors, setup or harness failures, and
elaboration failures unrelated to the claimed property.

**An intended failed theorem proof is the evidence, not infrastructure noise.**
Do not require the theorem meant to fail to keep compiling, and do not
reclassify its failure as a tooling problem.

---

## D3 — Receipt reuse requires a bound dependency footprint, not unchanged bytes

**Unchanged subject-file bytes do not preserve a receipt.** A receipt is reusable
only when its whole context is bound and shown unchanged:

| must be bound | |
|---|---|
| subject identity | the definition or theorem mutated |
| the actual mutation | the exact atom applied |
| checker / gate | which instrument observed the failure |
| fixture / input | the data it ran on |
| **relevant transitive dependency footprint** | **with evidence.** A change outside the footprint may be irrelevant — but the footprint must be *demonstrated*, not asserted |
| toolchain | the Lean pin that elaborated it |
| command | what was actually run |

Each receipt is marked **REUSABLE-BOUNDED** (context bound and unchanged; usable
for its stated scope), **STALE** (context moved; needs a fresh run) or
**UNUSABLE** (identity or provenance unrecoverable).

**Reusable bounded historical evidence is not the same as the fresh full
acceptance required at the final candidate.** The final candidate needs its own
acceptance run regardless of how much history is reusable.

The archived `/tmp` ledgers
(`/home/paolino/reactivegas-ms2-runtime-archive/ms2-runtime-20260905-0833.tar.gz`,
6489 files, 43 ledgers) are keyed to `INV-` rows and carry explicit OPEN rows.
They are **input to D3 to be re-keyed**, never evidence for D2 to be re-labelled.
**A historical aggregate GREEN closes no current theorem row.**

---

## D4 — Quantification

Six sets, each reported **with explicit identities**, identities primary:

`DISCOVERED` · `REQUIRED-INPUT` · `EXECUTED` · `KILLED` · `SURVIVED` · `BLOCKED`

**Nonzero checks apply to `DISCOVERED`, `REQUIRED-INPUT`, `EXECUTED` and
`KILLED` wherever coverage is claimed.** `SURVIVED` and `BLOCKED` **may be
empty** — an empty survivor set is what successful completion looks like, and
revision 1 was wrong to require all six nonzero.

**Reconciliation, by identity, with nothing silently absorbed:**
every `REQUIRED-INPUT` identity is accounted for as executed-and-killed,
executed-and-survived, blocked, or missing-input. **No unresolved blocking row
is counted as covered.**

**On ratios:** a **correct homogeneous ratio is not intrinsically invalid**, and
revision 1's blanket prohibition is withdrawn. The old `6/224` was defective for
two specific reasons — **unlike units** (fixture comparisons summed with
production-definition kills) and a **false denominator**. A ratio is admissible
when its numerator and denominator are the **same kind of thing**, the
denominator is `REQUIRED-INPUT` derived in D1, and both sets are published by
identity. **The denominator is never derived from the mutants that happened to
execute** — that measures the campaign, not the model.

---

## D5 — Completion plan, replacing revision 1's expected-outcome row

Revision 1 promised to close S3 with missing required rows after ten builds.
**Withdrawn.** A truthful "missing" is a valid *checkpoint*; it is not
*completion*, and the coverage obligation **stays open until satisfied or
explicitly dispositioned by the desk**.

**Ten builds is withdrawn as an unapproved total campaign ceiling.** Revision 1
also assumed one full rebuild per mutant row without checking — that assumption
is withdrawn too, and its replacement is a measurement, not a different guess.

### Phase 1 — discovery and costing. Claims no coverage.

| | |
|---|---|
| **produces** | the finite `REQUIRED-INPUT` extent by identity — see the ownership relation below, **not** a Cartesian product; the D3 receipt inventory with each entry marked REUSABLE-BOUNDED / STALE / UNUSABLE with its bound footprint; and a **measured** cost model |
| **the cost measurement** | measure whether mutant rows can share a compilation. **Neither assume separate full rebuilds nor claim a batched arrangement exists without evidence.** Report measured per-row and per-batch cost, and **state which kind each measurement is**: full cold build, incremental production rebuild, proof/check elaboration, or runtime replay. Those costs differ by orders of magnitude and must be compared honestly, never averaged together |
| **batching preserves isolation** | separately admitted single-atom variants **may** share compilation infrastructure or be scheduled together. **Mutating several atoms in one subject cannot count as an independent kill of each** — that is one mutant with an ambiguous cause, however many rows it appears to touch |
| **ceiling** | **3 substantive builds**, for the cost measurement only |
| **returns** | required extent, receipt inventory, measured costs, and a **phase plan with numeric ceilings derived from those measurements** |

### Phases 2..n — required coverage

Proposed **after** Phase 1, from its measurements. Binding shape:

- every phase carries a **numeric ceiling**; there is no "whatever it costs";
- **every required identity is preserved across phase boundaries** — a phase may
  defer an identity, never drop it;
- each phase returns its D4 reconciliation by identity;
- the coverage obligation **remains open** until every required identity is
  killed or explicitly dispositioned;
- **exploration budget 0** unless separately proposed and granted.

### What "done" means

S3 completes when every `REQUIRED-INPUT` identity is either **killed by an
admitted production-definition mutant** or **explicitly dispositioned by the desk
with per-identity reasoning**. Missing required evidence at any checkpoint is an
**owned blocking finding**, not a deliverable.

---

## D6 — `LEAN-CLARITY.md`

Unchanged from revision 1. Historical experiment recorded **VOID** — the
simulator authors received explanation, so "does the Lean carry a reader" was
never run and cannot be reconstructed. Then the parts that are not void: known
ambiguities **with actual evidence** (several already produced — the "eighteen
identities" and "8/10 split" prose against a 14-constructor `allGuardIds`; the
`Predicates.lean` design-page path; the `renounce` accept-and-no-op against V-5),
and future observations for the next isolated author. **No fabricated
reconstruction.**

**Assigned row OD74-S1-COMMENT** (forwarded by NOTE-020, and this is its concrete
home rather than a widening of the frozen S2 repair):
`lean/Reactivegas/Trace.lean:357-359`, the doc comment above
`seedDenyPermissionRefunds`, still reads *"exercises an `UNPROVED` claim row"*.
S1 resolved that withdrawal refusal to `step_withdraw_inv`, so the comment is
**stale inherited prose** — **not** an exporter defect and **not** a model
semantic defect. It belongs in D6's known-ambiguities set as a
doc-comment-versus-definition disagreement with evidence, alongside the
"eighteen identities" / "8/10 split" prose and the `Predicates.lean` path. Any
`docs/en/design/` content arising from it routes through the desk to #71.

---

## Bounds

- **A semantic defect is not a quality repair.** A reachable counterexample is
  routed — to the desk, or to #81 where V-5 owns it — never absorbed by
  strengthening a theorem. **No antecedent strengthening in S3.**
- **S4 and S5 are not discharged by S3.**
- **#71 alone writes `docs/en/design/`.**
- No model change, no theorem statement change, no push, PR or merge.
- Local-only report delivery in both briefs.
- Seats: commit owner `muse`; auditor fresh `codex`/`grok`, never `muse`.

## What is being asked of the desk now

Accept or amend **this revision**, and approve **Phase 1 only** — required-extent
discovery, receipt inventory, and cost measurement at a **3-build ceiling**.
Phases 2..n come back as a costed proposal, not as an assumption.

---

## Changes from revision 1

| # | revision 1 | revision 2 |
|---|---|---|
| 1 | promised to close S3 with a non-empty missing set after 10 builds | that is a checkpoint, not completion. Executable phase plan; 10 builds withdrawn as unapproved; per-row rebuild cost is now **measured**, not assumed |
| 2 | required all six D4 sets nonzero | nonzero for `DISCOVERED`/`REQUIRED-INPUT`/`EXECUTED`/`KILLED` where coverage is claimed; **`SURVIVED` and `BLOCKED` may be empty** |
| 2 | banned every percentage | withdrawn. A **homogeneous** ratio with a D1-derived denominator, published by identity, is admissible. `6/224` was defective for unlike units and a false denominator, not for being a ratio |
| 3 | receipt survival from unchanged subject bytes | full context binding **with an evidenced dependency footprint**; REUSABLE-BOUNDED / STALE / UNUSABLE; reuse ≠ final acceptance |
| 4 | checker mutation listed without evidence direction | a checker mutant shows **sensitivity of the checks that exercise it**, only when the correct failure is observed — `always-true` is the counterexample. Production mutants carry four admission conditions |
| 5 | wrote `stepFn`, a name absent from this codebase | actual entry points enumerated from the accepted base, and S3 re-derives them |
| 5 | treated 76/29/224 loosely | explicitly **historical**, never discovery constants |
| 5 | helper facts implicitly owed a reachable witness | characterized on their own terms; **no invented state-machine antecedents**; exemption per identity with reasoning |

---

## Changes from revision 2 (NOTE-021)

| # | revision 2 | revision 3 |
|---|---|---|
| 1 | required the module to "still elaborate" and said a mutant that fails to compile "demonstrated nothing" | **banned the main Lean falsification method.** Corrected: the requirement is on the **mutated definition remaining well-typed**, never on the theorem meant to fail. A failing theorem proof, `#guard` or semantic check **is** the RED evidence; only syntax/import/setup and unrelated elaboration failures are excluded |
| 2 | `REQUIRED-INPUT` as "D1a × D1b" | **not a Cartesian product.** Two sets plus the **actual semantic ownership relation**; every required atom and every in-scope theorem property needs *relevant* coverage. One atom may close several rows if each property-specific failure is observed. No manufactured pairings |
| 3 | "batched atoms per module" costing, undifferentiated | **isolation preserved**: shared infrastructure and scheduling are fine, but several atoms mutated in one subject is **one mutant with an ambiguous cause**, not independent kills. Each measurement states its kind — cold build, incremental rebuild, proof/check elaboration, runtime replay — and they are never averaged |
| 3 | `always-true` checker: "nothing fails, nothing learned" | given its **conditional limit**: a surrounding test can and should detect it, and **survival of the required surrounding check is itself evidence of an assurance gap**. It still never shows the altered checker can reject |
| 4 | entry-point table treated as the domain | **not an allowlist and must not seed one.** Revision 2 omitted `KelGroups.Integration` and most of `KelGroups.Validate`; added, and S3 **re-derives** entry points and reached hooks at its own base, treating disagreement with the table as a finding about the table |

## Authorization status

**Phase 1 AUTHORIZED** by NOTE-021, to be dispatched **on the accepted, landed
S2 base** without a further desk checkpoint: runtime-only discovery, receipt
inventory and measured costing, **maximum 3 substantive builds**, **no production,
model or statement change**, and **no S3 coverage-complete claim**.

**Phases 2..n remain unauthorized** and require the measured proposal plus
explicit numeric authorization.
