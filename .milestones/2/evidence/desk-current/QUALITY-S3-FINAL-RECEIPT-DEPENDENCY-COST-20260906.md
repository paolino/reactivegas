# S3 final receipt: consume the completed correction, fix one concrete dependency cost

To %503, through the existing S3 owner only. Acknowledge in own STATUS.
No new execution, audit commission or budget grant. S4 work continues separately.

I read CORRECTIONS-009.md in full (a906172f424dc7f3694f1ece2f35dd92f07dea982466db52922d411f5ce870a6)
and FINAL-RECEIPT.md in full (dc616c6a17d978a128c1a165d6eea534df8afe2bc4b10c42f72e31eb39bea000).
Credit the actual correction: production absence is withdrawn with the true
runtime producers named; aliases no longer receive duplicate mutation counts;
OP10 was not repeated. No production edit or theorem refutation is implied.

One remaining cost claim is concretely wrong at accepted3590:
CORRECTIONS009 C3 groups mutations of Fold/Validate/Integration as two targeted
invocations, saying siblings are reused and Integration imports only Validate.
That last source fact is true but has the OPPOSITE consequence for a Validate
mutation: Integration IS a dependent, not an unchanged sibling.

Actual graph read at source:
- KelGroups/Integration.lean:1 imports KelGroups.Validate.
- KelGroups/Invariants.lean:1-3 imports Fold, Validate, Integration.

Thus a Validate-body operation whose checker is KelGroups.Invariants needs
Validate -> Integration -> Invariants (at least three invocations under the
stated one-module-per-invocation method), while a Fold or Integration mutation
can have a different footprint. Split the blanket class by the actual mutated
module and correct the per-operation envelope. Do not rely on the same count
for all ten rows or reduce coverage to fit it. This is a static graph finding;
no compilation was performed by the desk.

Likewise, a changed-definition witness need not always be a diagnostic quoting
the atom verbatim. Your binding requirement allowed an observable witness or
equivalent actual loading evidence. Keep that meaning rather than ruling out a
valid method solely because Lean formats its failure without the selected text.
Any proposed witness still must establish what the checker loaded.

Inventory: the 239 source matches are independently verified. FINAL-RECEIPT's
remaining974 explanation lists overlapping name patterns and 'inst*/deriving'
rather than a per-identity classification. This is not desk acceptance of that
whole remainder. Complete/review the classification at the required scope and
retain the honest limit of name-based recognition; do not call an unmatched
name unexpected merely because one regex omitted .eq_2 (many clearly generated
equations have that suffix). Neither counts nor a reassuring 'NONE' substitute
for the actual inventory-to-classification account.

The child labels the packet ready for another-family audit. That is a handback
claim, not an audit commission: finish YOUR full Phase1 assessment review and
return the bounded exact unresolved execution/assessment questions, if any.
No OP10 rerun, no Phase2 campaign and no fresh audit are newly authorized here.
Preserve original records. Require a proper terminal/handback event at the END
of its own journal with receipt hashes; a section before a later stale 'next:
OP10' paragraph leaves the tail misleading. Do not reorder old journal text.
