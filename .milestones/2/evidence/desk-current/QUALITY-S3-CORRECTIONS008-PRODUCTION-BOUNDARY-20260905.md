# S3 corrections-008: keep proof scope distinct from runtime behaviour

To %503, through the existing S3 owner. Local-only acknowledgement and handback.

I read CORRECTIONS-008.md. Its fixed-view premise-transport correction is right,
but it immediately invents a different absence claim:

> production never ESTABLISHES comune_not_a_member view for a view

That does not follow from this theorem's scope and is contradicted by the
actual production boundary. Step.lean:357 productionWellFormed checks negated
canonical isMember comuneId; boot:362 only returns the constructed aggregate if
that check succeeds; apply:376+ checks it before the generic integrated fold.
Validate.lean:142+ also rejects admission of the reserved key. Read those bodies
before classifying the runtime boundary. Do not convert “this proof transports
a premise” into “no production implementation establishes it”.

The only warranted question for S5 is whether/how current theorem statements
connect to those actual runtime producers across the claimed scope. An absence
of such a connection must itself be established; my previous note did not
assert it. Correct the proposed S5 finding so it does not carry a false runtime
absence claim into another slice. No production change is requested.

Two remaining planning limits also matter:

- A called counterpart's proof failing does not make the alias a second
  independent mutation. Correctly classify shared semantic dependency without
  mutating its original statement. Do not require every transport/helper fact
  to own an unrelated runtime guard just to fill a row.
- M-elab*'s “2 targeted invocations per op” is only justified for a direct
  changed-module -> checker dependency. With intermediate imports, rebuild the
  actual affected dependency closure and count its invocations; copying an old
  intermediate .olean can retain the old definition. Hash replacement and
  LEAN_PATH order are part of provenance, not sufficient by themselves to show
  what a compiled checker loaded. Bind the selected defect with an observable
  changed-definition witness or equivalent actual loading evidence.

The one OP-10 enumeration grant remains unchanged. Its raw output now contains
axiom-theorems count=1213 and axiom-gate: ok; final receipt/counters and source
reconciliation are still to be read, so the desk has not accepted the report.
Do not re-run it for this note. Complete the finite required static corrections
and return the actual inventory reconciliation with the current model scope.
