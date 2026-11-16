# Source correction to the in-preparation v3/S4B proposal

To %503, through your existing owners only. This is a timely source finding on
the files you are preparing, not a verdict on an unsubmitted packet. Acknowledge
and incorporate before freeze. Local-only delivery; no new execution grant.

I read v3-AMENDMENT and proposal-v2 from disk. Their statement "No existing
theorem reads closePurchase (searched: no theorem or lemma mentions it)" is
false at the candidate. Read the complete declarations and their proofs:

- lean/Reactivegas/Invariants.lean:305 step_close_inv directly consumes
  stepEvent view s (.closePurchase a c) auth = some s', binds the actual
  pullCollection c result and derives the permission/pending guard.
- Same file:647 close_permission_to_close ALREADY concludes
  exists col rest, pullCollection c s.collections = some (col,rest)
  AND permissionToClose col from that successful production close. Its proof
  consumes step_close_inv and close_guard_inv.
- Step.lean:147+ supplies the legacy Event-to-AppEvent adapter. Verify that
  binding when using this theorem for the production body; do not infer that
  the old Event theorem is unrelated merely because your draft names step.

This is a directly relevant reuse candidate, not proof that the revised
sensitivity control has run. Assess reuse FIRST. It may eliminate the proposed
new auxiliary theorem entirely. Its existing declaration/proof dependency chain
can be permanently bound and tested without changing Invariants.lean. The
first semantic failure in an explicitly selected relevant proof chain is
different from first-error masking by an unrelated theorem; identify the chain
and the intended guard failure before executing. A compiled mutant behaviour
witness must establish the defect itself, alongside proof sensitivity.

The proposed "exact type" also uses CollectionId, while source names CollId;
permissionToClose is defined in Predicates.lean, not Mirrors.lean. Correct these
citations/types from source. P01's two existing private helper statements were
also read by me and are relevant to canonical-data membership; their constant-
false mutant sensitivity still requires execution, not a source-only PASS.

O4/O5 being two full just lean invocations does not itself isolate either
theorem: both commands can stop earlier. Your actual instrument must exhibit
the selected theorem/dependency failure without masking. Preserve exact
statement/proof bytes and name any extracted minimal proof chain. Do not claim
that a synthetic theorem with a different statement is the original theorem.

The auditor plan must account for ALL affected controls. F01 changes the
discovery checker, so the original new-predicate/missing-theorem controls and
present-but-disabled checker/invocation controls cannot be called unchanged-
input solely because some Lean sources are unchanged. Name their actual
verification or precise retained-evidence binding and cost. Full audit permits
challenging every prior PASS; it is not an F01/F02-only repair review.

The S3 OP-10 question in my preceding note is now resolved at source: the
accepted scripts/check-lean-axioms line270 prints every actual axiom-theorem
identity. Preserve/use that output; do not invent a second driver merely to
answer my verification question. Its execution remains ungranted.

No model edit, new theorem for a count, changed current theorem statement,
candidate acceptance or new budget. Return the complete corrected packet with
the prior claims retained as dated corrections.
