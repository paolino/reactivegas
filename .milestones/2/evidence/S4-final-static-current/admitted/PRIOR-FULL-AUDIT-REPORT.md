# S4-B submission 2 — independent full audit

**Terminal verdict: AUDIT-FINDINGS.** The executed finite correspondence,
body-sensitivity, mandatory-path and final trust controls succeeded within their
stated scopes. Acceptance remains incomplete on **F-001: Reach consumer-axis
authority**, which is OPEN. This is not a failed candidate build or proof, and it
does not assert that an arbitrary Reach oracle is required or implementable.

Candidate **94bb7bb64324a48f7361252556b4d15e45b3923f**, tree
**3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced**, over accepted base
**3590c0015b84fd58004bf6fb44dd18b107304c48**. The entire seven-commit range was
audited: 189e1ed, 59309d6, 0f3ad01, 4d0a324, b667648, ba623667, 94bb7bb.
No predecessor verdict, command fit or owner execution was counted as independent
coverage. This is submission **2 of 2**; no third submission is granted.

This seat spent **12 substantive and 73 targeted** against **12 and 80**,
separately. Historical auditor 6/59 plus this seat is campaign **18/132** against
18/139. Four auditor setup/placement failures remain charged. No candidate source
was repaired. All delivery is local; no remote CI execution, acceptance, push,
PR, merge, comment or author contact is claimed.

## F-001 — Reach consumer classification lacks its asserted authority — OPEN

The candidate's `lean/Reactivegas/Mirrors.lean:29` calls Reach bounded
NOT-EXECUTABLE and says that no arbitrary-Reach oracle is required “under the
standing boundary.” `scripts/check-lean-mirrors:152` calls the named exceptions
legitimate and includes Reach with “no oracle required.” The final mandatory run
accepts Reach through that exception; it does not settle the authority claim.

The admitted original S4 contract's **Axis 2** requires contract authority to
distinguish REQUIRED-CONSUMER-UNIMPLEMENTED from NOT-REQUIRED, independently of
decidability. Its Audit and Completion paragraphs expressly retain this issue.
The admitted R1 finite-mirror fence and R2 ban on new monitors limit implementation
scope; neither identifies the specific standing exemption cited for Reach or
settles that identity's #66 consumer requirement. The standing-exceptions ruling
concerns audit-window placement, ceiling history and onward ownership. The later
v3.1 amendment is explicitly confined to P01/P07.

My `ReachDecision` query compiled the existing stalled decision instance and
failed synthesis for one concrete `Decidable (Reach … State.empty)`. That supports
only **NOT-ESTABLISHED executable decision** at this bounded query. It proves
neither undecidability nor a NOT-REQUIRED consumer classification. Source absence
and the candidate's own exception label cannot supply the missing authority.

**Exact outstanding obligation:** establish the consumer-axis disposition of
`Reach` in `lean/Reactivegas/Predicates.lean`, with the authoritative #66/S4
boundary that governs this identity. Until then the classification row is PARTLY
and the consumer axis OPEN. This is an acceptance-blocking classification gap,
not a demonstrated false theorem and not a prescription to implement an oracle.
The named onward owner is commissioner **%503**, through local commissioning
records to desk %510. No additional build can answer this authority question.

## Independent evidence and its limits

The final fresh `.lake` world S10 ran the literal candidate-bound
`timeout --signal=TERM --kill-after=30s 1800s nix develop --quiet -c just ci`.
It exited 0. Its complete stdout hash is
`c0d10f287764f222b46f804aba22ffd62ab6e08bb478cbb40b687a6e02ee54fa`; stderr hash is
`b52f164044a7f30051a39b242933fca8827238913d17d2123a2d3054b4df00f0`.
The cold world was a new clone at the candidate without the warm M1 cache.
FinalInventory and Axioms resolved only its freshly built owned module artifacts.
The local run also completed the corpus/exporter path. CI source wiring was read;
no remote workflow run is implied.

Final independent discovery retained **3,478 declarations**, **24 predicate
candidates**, and **29 owned modules**, with zero unresolved sorts. The mandatory
receipt has exactly the same predicate names and tracked module names. Its nonce
`1788673975635087591` matches the final nonce file. The complete retained JSON,
not a partial prefix or preselected count, establishes this extent. Every
declaration has a row in `DECLARATION-DISPOSITIONS.jsonl`; every predicate has both
axes in `S4-CLASSIFICATION.md`. Discovery was bounded at 20,000 declarations and
256 MiB, with explicit stop on overflow, partial output, identity drift or unknown
sort. No denominator was truncated to fit.

The independent cold accepted-base inventory contains **3,382 declarations**.
`expr-compare` established every original identity's kind, type Expr, value/proof
Expr and universe parameters unchanged; final-reconcile then established both
directions between the planning and final candidate inventories. The **96** added
declarations are listed separately, not treated as a required quota. The entire
source diff is 946 additions in four files: two new 259-line mirror modules, the
418-line checker, and ten added justfile lines. There are no deletions, changes
to old Lean statements/definitions, Invariants edits, docs/design writes or new
runtime call sites. Source review and Expr evidence are distinct.

All **1,285 owned theorem identities** received final `#print axioms` and
`collectAxioms` checks. Only `propext`, `Classical.choice`, and `Quot.sound` were
permitted; no forbidden dependency survived. The negative instrument introduced
`auditForbidden` and failed with `AUDIT-FORBIDDEN-AXIOM` and
`AUDIT-AXIOM-FAILED`. The final theorem-name set reconciles exactly with the final
inventory. This does not turn classical logical evidence into executable
decision capability. The final both-stream `PANIC at` check passed; injected
stdout, stderr and both-stream controls had separately demonstrated failure.
This is the required bounded panic check, not a universal runtime-totality proof.

## Requirements and controls

| Requirement | Disposition | Independent evidence |
|---|---|---|
| Original Phase A, both axes | PARTLY | All 24 identities retained; F-001 leaves Reach's consumer authority OPEN |
| R1 existing definitions/statements; finite scope | CLOSED for preservation and implementation fence | Whole diff; cold BaseInventory, expr-compare, final-reconcile; F-001 separately limits classification acceptance |
| R2 no monitor/coordinator behaviour | CLOSED | Four-file diff adds no production call site or guard |
| R3 new modules only | CLOSED | Lean additions confined to the two new Mirrors modules |
| R4 reuse P01/P07 expressions | CLOSED | Original expression correspondences preserved; no duplicate Bool; v3.1 body obligations separately exercised |
| R5 generic equality only in new statements | CLOSED | K5 new counterpart/correctness use DecidableEq; all original Exprs unchanged |
| R6 callable threshold | CLOSED | Source parameter retained; GroupWitnesses executes n+1 and constant-1 policies; A44 detects bypass |
| R7 nonempty per-identity reconciliation, named exceptions | PARTLY | Complete final identity reconciliation succeeds; Reach exception authority remains F-001 |
| R8 missing counterpart mandatory control | CLOSED | S11 names both added def and opaque predicates; S02 is retained failed placement, not coverage |
| R9 missing theorem mandatory control | CLOSED | S03 names auditMissingTheorem at MIRROR-THEOREM-MISSING |
| R10 checker ineffective while present | CLOSED | S06 executable checker becomes no-op success; permanent mandatory path rejects missing operational receipt |
| R11 original proof detects well-typed body fault | CLOSED under v3.1 | A01–A44 plus P01-neg and P07-negR; separate named statements and proof observations |
| R12 no first-failure masking | CLOSED within reported targets | 44 distinct invocations; selected P01 helpers reached; P07 inversion reached; no outer-chain or earlier-module failure credited independently |
| R13 final proof axioms and panic checks | CLOSED | S10 → FinalInventory/Axioms → final-reconcile/panic-final, with negative instrument controls |
| R14 arbitrary states, duplicates and defaults | CLOSED | Unrestricted correspondence statements; first-lookup/default proofs; nonempty duplicate/absent/nonzero witnesses |
| R15 no unruled well-formedness strengthening | CLOSED | New proofs inspected; original statements/Exprs preserved |
| R16 exact fence | CLOSED | Four-file candidate diff, scratch worlds separated; candidate remains clean |
| R17 own justfile lines only | CLOSED | Ten additions, zero deletion or sibling-line replacement |
| R18 actual operation classes and failures | CLOSED under current v2 ceiling | Complete command outcomes and ledger: 12 substantive/73 targeted; no pooling/refund |

| Control | Disposition | Actual observation |
|---|---|---|
| C1 | CLOSED | S01 actual just lean exit 0; warm build honestly charged |
| C2 | CLOSED | S11 actual just lean exits 1 naming auditMissingCounterpart and auditOpaquePredicate |
| C3 | CLOSED | S03 actual just lean exits 1 naming auditMissingTheorem |
| C4 | CLOSED | S06 exits 1 with MIRROR-RECEIPT-ABSENT while checker remains executable and returns 0; no missing-file 127 |
| C5 / P01 | CLOSED for v3.1 replacement only | P01-compile 0; selected original helper proofs fail in P01-neg; clean P01-pos 0; membership present/absent witnesses distinguish the constant-false body |
| C6 / P02 | CLOSED | A01–A03, conservation_corr |
| C7 / P03 | CLOSED | A04–A08, solvent_corr |
| C8 / P04 | CLOSED | A09–A10, insolvent_corr |
| C9 / P05 | CLOSED | A11–A14, uniquePledges_corr |
| C10 / P06 | CLOSED | A15, allUniquePledges_corr |
| C11 / P07 | CLOSED for current v3.1 replacement only | P07-negR original step_close_inv failure and successful forbidden-close witness; clean P07-pos; historical isolation remains OPEN |
| C12 / P08 | CLOSED | A16–A17, escrowHeld_corr |
| C13 / P09 | CLOSED | A18, governanceEnacts_corr |
| C14 / P10 | CLOSED | A19–A20, doubleEntry_corr |
| C15 / P12 | CLOSED | A21–A23, canCloseGroup_corr |
| C16 / K1 | CLOSED | A24–A25, KelGroups.pendingWellFormed_corr |
| C17 / K2 | CLOSED | A26, KelGroups.membersCoherent_corr |
| C18 / K3 | CLOSED | A27, KelGroups.pendingCoherent_corr |
| C19 / K4 | CLOSED | A28–A31, KelGroups.wellFormed_corr |
| C20 / K5 | CLOSED | A32–A33, KelGroups.enacts_corr |
| C21 / V1 | CLOSED | A34–A36, KelGroups.Vote.questionClean_corr |
| C22 / V2 | CLOSED | A37–A42, KelGroups.Vote.sweepReady_corr |
| C23 / V3 | CLOSED | A43–A44, KelGroups.Vote.voteWellFormed_corr |
| C24 | CLOSED | Final Axioms exit 0 after cold S10; all measured owned theorem names reconciled |
| C25 | CLOSED in required bounded scope | Both-stream panic-final exit 0, detector negative controls passed |
| C26 | CLOSED | Cold final actual just ci exit 0 at full candidate over accepted base |

`ATOM-DISPOSITIONS.md` and JSON enumerate all 44 atoms with exact substitution,
original theorem, proof-error location and receipt. Each definition remained
well-typed, and its original proof bytes were retained. Failures such as A32's
`simp made no progress` are observed failures of the original proof under the
specified body edit, not a claim that every syntactic proof failure establishes
a different theorem false. The finite witnesses and inspected body changes give
the semantic context. No claim covers every possible implementation fault.

The economy witness driver evaluated 28 rows (including its sentinel); the group
driver evaluated 29. They include duplicate first-match balances, absent default
zero, positive/negative amounts, nonempty pledge and collection carriers, both
double-entry effects, real payload 73 versus 74, and independently varied voting
conditions. WitnessNegative failed its sentinel while continuing to print the
remaining assertions; it is a driver control, not a production mutation kill.
ExceptionsR established all 14 Event projections and positive/negative
evaluations, the existing stalled decision, and actual V4 question preservation
and change. Final metadata reconciles the Event constructor set. Failed
Exceptions did not establish those runtime rows merely by printing labels.

P01's original correspondence is value-parametric and survives its body fault.
P07's correspondence is inline and does not depend on closePurchase. Their
relatum controls are only relatum controls. In the P07 body experiment the
original `step_close_inv` fails on the permission mismatch; the dependent outer
`close_permission_to_close` is not credited as a separate independent failure.
The complete P07 overlay retained all clean dependencies and substituted only
the previously compiled Step mutant. Its Types digest is clean and distinct from
the P01 mutant. The witness selects collection 7, retains unrelated collection 9,
closes 23 against cassa 40 to leave 17, and varies only permission for authorized
versus forbidden close. This meets the current amended binding without changing
the historical result.

Additional completeness controls: S04 refused a newly tracked unimported module
by name. S05 removed the theorem-kind classification branch and reached actual
`MIRROR-UNCLASSIFIED-KIND` errors; Lean's 100-error cap means those diagnostics are
not a negative extent of 1,285. EmptyInventory and UnknownInventory test this
auditor's independent inventory refusal paths; the latter does not execute or
close the historical census path. Artifact validators rejected empty/truncated,
wrong-home, unknown-sort and Expr-corruption inputs. These controls are recorded
at their actual layer and do not replace mandatory-path executions.

## Historical limitations — all remain OPEN

| ID | Disposition | Exact retained limit |
|---|---|---|
| H-01 | OPEN | Historical P07 single-variable isolation is unestablished; the old Step diagnostic cannot prove which Types.olean was loaded. Today's verified overlay does not reconstruct that evidence |
| H-02 | OPEN | Historical census sortUndecided→fail path remains source-verified-not-executed. A clean zero-unknown census and this auditor's own negative instrument do not close it |
| H-03 | OPEN | ba623667 receipt provenance remains recovered-from-snapshot after overwrite, never “never-overwritten” |

The six owner packet record defects are handled by the admitted commissioning
supplement: valid 40-character candidate, seven-commit range, distinct failed and
successful P07 receipts, submission 2/2 and owner spend, prior-grant chronology
for identified O1 retries, and O6's bound 3,117-line log/hash. Identity/range were
independently checked. Historical chronology and owner spend are admitted record
authority, not freshly executed coverage. The superseded 9/10 budget line has no
operative effect. The historical P07 and O6 limitations are not closed by the
supplement's correction or by this report.

| Corrected record | Disposition | Binding and limit |
|---|---|---|
| REC-01 candidate identity | CLOSED | Independent HEAD/tree and 269 tracked input checks use the valid 40-character commit |
| REC-02 full range | CLOSED | Independent seven-commit range and full four-file diff over 3590c001 |
| REC-03 distinct P07 receipts | PARTLY | Admitted hashes distinguish setup failure from intended inversion failure; historical single-variable isolation H-01 remains OPEN |
| REC-04 submissions/spend | CLOSED as authoritative record | Submission 2/2, author 18/52; own spend independently reconciled 12/73 under the superseding v2 grant |
| REC-05 prior authorization chronology | CLOSED as authoritative correction | Commissioning supplement binds grants before the identified O1 retries; no independent replay of historical authorization is claimed |
| REC-06 O6 identity/count | PARTLY | Authoritative 3,117-line/hash binding retained; historical sortUndecided execution H-02 remains OPEN |

## Audit provenance and stopping receipt

The first model incident occurred before START and launched no audit operation.
The second overlapped deterministic execution of the previously frozen final
chain. Both launch identity and owner-observed active session model/effort are
separately recorded. `SECOND-DEVIATION-REVIEW.md` identifies every interpretation
reconsidered after the queued notice and the precise execution chronology. The
second review cost **0 substantive / 0 targeted**. No terminal judgment made
under a deviated model is inherited. The original report-v1 draft and both failed
or declined assembly attempts remain history; this is versioned restored-model
output.

All 82 full-sheet rows completed, plus the three M1/M1R operations. Complete
revised command fit was admitted before each dependent execution: full-v2
11/71, v3 12/71, v4 12/72, v5 12/73, all within 12/80. Four counted setup failures
are M1-T (reserved identifier), S02 (import-graph placement masks target),
Exceptions (Decidable elaboration setup), and P07-neg (incomplete package overlay).
They are not candidate findings and were not refunded. No execution remains in
the frozen plan. The actual unresolved acceptance obligation is F-001; it is not
trimmed, silently passed or converted to an optional row because execution is
complete. The three historical limitations also remain open. #66 is not closed.

The evidence index is `MANIFEST.sha256` beside this report, with runtime-relative
paths and no self-entry. It binds the reports, original and versioned command
freezes, complete raw execution streams, inventories and dependency receipts.
`COMMAND-OUTCOMES.json` retains every full-run argv/cwd/time/class and stream hash;
`CAMPAIGN-LEDGER.md` is the reconciled ledger. The prior M1 return is archived
unchanged under `handoffs/pre-full-v2/`. The three canonical handoff paths point
to this final version after assembly. Acceptance and onward disposition belong
to commissioner %503; no author work or remote action is authorized by this
report.
