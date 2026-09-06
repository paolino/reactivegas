# NOTE-031 — substantive 16 → 17 GRANTED. Static preflight, then run to handback.

## GRANT

Substantive ceiling **16 → 17**. Spent **10 retained** (nothing recounted, nothing
reset). Seven remaining, exactly: `O1retry2`, `O2`, `O3`, `O4`, `O5`, `noop`, `O6`.
Targeted stays **52 allocated and spent within the hard 60, with ZERO further
targeted authority** — anything that appears to need one is a gap to report.
Submission cap and auditor 15/69 unchanged.

No extra automatic retry, no compression, and **no relabelling a build as a
probe** to create room.

## Static preflight before `O1retry2` — no Lean

Inspect the **concrete generated text**, produced from the actual script template
and the actual discovered input:

- the placeholder token is **absent** from the generated driver;
- the array syntax is intact — `#[` … `]` still delimit the literal;
- the discovered module literal is **non-empty** (a substitution that yields
  `#[]` passes the first two checks and still guts the gate);
- exact hashes retained for the changed script, the generated input and the
  output.

This is a static preflight and **not** validation of Lean semantics. Say so in
the receipt. The corrected shell operation must leave the surrounding literal and
the module set intact.

Then commit and journal the owned fix, and bind the actual executing identity
before running. Preserve `b667648`, the old failed generated driver and its log,
and all receipt history.

## Bookkeeping: the `1/2` is a notation collision, not a counting error

Your journal writes `1/2`; the desk writes "submission 2/2". Both are right about
different things — you count submissions **delivered** (submission 1 went final at
candidate `189e1ed`; submission 2 is in preparation), the desk names **which
submission of the cap** this is. Nothing to reset.

Going forward write it unambiguously, e.g. *"submissions delivered 1 of 2 allowed;
this phase prepares submission 2 (the last)."* Keep the history as written.

The consequence is worth stating plainly: **there is no third submission.**

## P07: credited, and the limitation stays open

The desk read the diagnostic directly and confirms it fails inside
`step_close_inv` and prints `true` where `col.permitted` is required. That scoped
result is credited.

**Single-variable isolation is not established by that receipt**, because the
overlay verification was not retained. Keep that limitation explicit and open **to
the fresh auditor** — do not recreate today's overlay and present it as proof of
the historical one; today's bytes are a different claim.

## The final audit's scope

The fresh full audit covers the **entire original unaccepted candidate at the
final SHA over accepted `3590c001`** — never just this fix, and never on the
argument that an audit of earlier bytes covers later ones. The remaining original
final audit must establish its required isolation **on its own retained
evidence**.

If that full mandate cannot fit its existing budget, **return a command-level gap
BEFORE launch**. Not a narrowed mandate, not a weakened acceptance.

## Then

Run the sequence through validation, submission, the fresh full audit and
handback. Do not stop to report progress. A genuine unexpected failure returns its
concrete cost before further execution. No production widening, no merge, no push,
no PR, no comment.
