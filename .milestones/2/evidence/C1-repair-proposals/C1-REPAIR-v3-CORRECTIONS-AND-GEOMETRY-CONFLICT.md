# C1 repair v3 — three corrections applied, and the exact geometry conflict returned

Addendum to v2 (`46e9132d…`, preserved frozen). Grant
`9de52657266a4ab213197c827fc6e849fbdf9ce33b976529165076fa0817d284` read in full.
**No push, publication, PR, merge or release.**

## Grant recorded

Owner **33 substantive** (28 spent, **5 available**), **48 targeted** (41 spent,
**7 available**) — an exceptional sixth increase for this three-class repair
only. Parent **4 substantive / 10 targeted from this grant**, with historical
parent spend retained separately as **UNKNOWN, not zero**. Every invocation
counts at its actual layer, including failed, setup and warm retries, and **a
focused mode that executes a full suite is still a full suite.** No automatic
further raise.

## Correction 1 — my source attribution was wrong, and the truth is sharper

v2 said `author` is the machine-owned field *"per the Lean AppEvent schema"*.
**That is false and I withdraw it.** Verified in the candidate:

- `lean/Reactivegas/Types.lean` — **`AppEvent` has no `author` field**; `grep`
  returns **0**, and the source comment says so outright: *"with no author field
  — the signer arrives [separately]"*. `openPurchase (c : CollId)`.
- `lean/Reactivegas/Step.lean:181+` — `appFold` takes `signer` and passes it
  into `step` separately.
- **Legacy `Event`** (`Types.lean:43+`) **does** declare it:
  `openPurchase (author : KelGroups.Key) (c : CollId)`.
- The JS `attempt` machine consumes `e.author` (`core:222+`) — it implements the
  **legacy Event** surface, where `author` is a legitimately declared argument.

**These are two different surfaces**, and the adapter sits between them. So the
schema-agreement control binds the **actual two sides**:

1. **imported application-event args ↔ Lean `AppEvent`** — `author` is **not** a
   field here, so a payload carrying it is foreign to this surface;
2. **constructed machine event ↔ Lean legacy `Event`** — `author` **is** declared
   here, and the **signer must be its only source**.

The control is therefore *"the adapter's per-tag argument set agrees with the
`AppEvent` constructor arity, and `author` on the Event side originates solely
from the signer"* — checkable against the actual Lean source, and **not** a
checker built around a field `AppEvent` does not have.

**Equal-author legacy normalization stays a separate compatibility claim** to be
verified against which real exports carry it — not a failure merely because a
helper strips it.

**The file-picker / import / adoption boundary is REQUIRED in final acceptance**,
exercised — not carried forward as a named untested caveat. I had it as a
caveat; that is upgraded to an obligation.

## Correction 2 — the ring formula is not proof of rendered usability. The exact conflict follows.

Two faults in v2, both mine:

- **Internal inconsistency:** v2 tabulated `R(8) = 120.2` while asserting
  `R(n) ≥ 132`. For small `n` the required radius is **below** the current
  fallback, so the base case is `max(R(n), 132)` — otherwise the layout would
  *shrink* for small counts. v2 stated it wrongly.
- **World coordinates are not pixels.** Keeping `rr = 42` in SVG units does not
  preserve readable or clickable size once the view is scaled to fit a growing
  ring. That is the rejected shrinking repair arriving by another route, and I
  did not say so.

### The conflict, with evidence

The scene is a **fixed** `viewBox="0 0 760 680"` (`economics-simulator.html:3598`).
Centre `(380, 340)`; the largest ring radius that still keeps a 42-unit glyph
inside is `340 − 42 = 298`.

| constraint set | bound |
|---|---|
| single ring, `rr=42`, `sep=92`, inside the box | **n ≤ 20** — `R(20)=294.1` fits, `R(21)=308.6 > 298` |
| **any** arrangement, area bound at optimal hex packing | **n ≲ 54, ever** — usable `402896` / disc `6648` × `0.9069` |

**So the original constraints are mutually incompatible with unbounded `n`, and
this is geometry, not implementation.** At `rr=42` and `sep=92` inside a fixed
760×680 scene, **no arrangement whatsoever exceeds ~54**, and a single ring stops
at **20**. `R(n) = 46/sin(π/n)` is a valid *construction* only while the ring
fits; past that it must clip (containment lost) or enlarge the viewBox (rendered
size lost).

**Exactly one of these must yield, and it is the desk's call, not mine:**

1. **containment** — allow pan/zoom, so "accessible" means reachable rather than
   simultaneously visible (glyph pixel size preserved at fixed zoom);
2. **rendered size** — accept scaling down past some count, which is what was
   just rejected, and would need an explicit readability floor;
3. **separation** — accept a smaller guaranteed distance, also already rejected;
4. **reachable extent** — a bound that is *justified by the domain*, not a cap
   invented to make the display pass.

**I am not choosing.** v2 claimed a design satisfying the original constraints;
it does not, and cannot, for unbounded `n`. **F-02 implementation is therefore
held** pending that decision. No count cap, no glyph collapse, no drag exclusion,
no human-session bound and no semantic restriction will be introduced to make it
pass.

Still owed once a constraint is chosen, and not settled by any chord formula:
member collisions, text overlap, offscreen access, dragged placements,
floating-point behaviour, and **rendered interaction tested above eight and at
the old 103 boundary**, with the analytical claim and the finite test kept
distinct.

## Correction 3 — cheap controls must exercise the mandatory predicate

Accepted. A focused row mode must invoke **the same operative predicate as the
full run**, not an alternate cheap checker — *"an alternate cheap checker is not
evidence about the mandatory one."* It must also **prove its own invocation and
wiring cannot be silently removed**, or a future edit disables the control while
the gate still reports green — this campaign's founding defect.

So: focused modes call the identical predicate function used by the full run; a
wiring control removes the invocation and the gate must go **RED**; controls are
**frozen with raw per-invocation inputs and outputs retained before** anything
relies on them; and every property-class control carries **both a real failure
and a positive calibration with prerequisites intact**.

**The owner's earlier T-A/T-B/T-C demonstrations remain historical unverified
receipts.** Final validation will not present them as archived.

## Immediate plan

- **F-01 and F-03 continue** within the granted budgets, per the grant.
- **F-02 is held** pending the constraint decision above.
- Auditor dispatch stays blocked until a **concrete complete 17-row command plan**
  is frozen within 5 substantive / 53 targeted, after reading the complete
  subject — and if that plan is infeasible it returns a blocker, never a thinner
  audit.
