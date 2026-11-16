# S5 contract — revision 2

Revised per NOTE-027. v1 preserved at `S5-CONTRACT-v1-superseded.md`; change
table at the end. **Runtime preparation only. No build grant here. Not
dispatched.**

S5 owns **two** obligations; neither discharges the other.

---

## Obligation 1 — retention completeness, outside the ruled #81 lifecycle

Does a statement asserting **retention** of a previously open question exist?

`open_questions_are_open` does not: it constrains a question **still present**,
and the S1 choices audit showed that condition survives replacing every open
question with an empty map. `no_expiry` is a one-step preservation lemma whose
premise, `PreservesQuestionSemantics`, is defined by observing equality of the
lookup after the event — so preservation is substantially assumed in the premise.

**Bound:** `#81` owns the V-5 lifecycle, including retention row **L-5**. S5
does **not** re-open V-5 semantics. It assesses the routes **outside** V-5, and
where a gap coincides with a V-5 route it records **both** a finding about
current completeness **and** its named owner.

---

## Obligation 2 — `ONWARD-68-INV-01`, exactness per consumer

### What the inherited finding actually is — v1 overstated it

The #68 audit's **"11/14"** is that audit's **assessment**. It is **not** evidence
of eleven machine-checked unrestricted converse proofs, and v1 implied it was
while its own next paragraph said otherwise. The accurate statement:

- the **mandatory path machine-checks the converse for six** —
  `requiredInversions`, via `tightnessProved`;
- the other **eight carry bindings with no machine-checked converse**;
- of those eight, the #68 audit **assessed three** — `step_pledge_inv`,
  `step_accept_inv`, `step_close_inv` — as having a **refutable unrestricted
  converse**, omitting the live non-stalled guard;
- **metadata 14/14 is a binding count and establishes no exactness.**

### Three distinct things, never merged

| | |
|---|---|
| **ASSERTED-FALSE** | a false statement is actually asserted somewhere — a theorem, a doc claim, a public contract |
| **UNASSERTED-FALSE-CONVERSE** | the converse is false, and **nothing asserts it**. The theorem as written is true |
| **TRUE-NECESSARY-CONDITION** | the theorem is true and gives necessary conditions; nothing is wrong with it |

**v1 said an unused false claim "lives in a proof corpus". Withdrawn — no such
claim is asserted there.** The three named theorems' **forward implications are
true**; branding them false would be as wrong as calling them exact.

**And the point that survives:** a **public exactness requirement can be unmet
without any false theorem existing.** That unmet requirement is a finding in its
own right, and it does not need a false statement to justify it. That is the
honest form of v1's "a finding even if no consumer uses it".

### The per-claim table

One row per inversion claim:

| column | requirement |
|---|---|
| **real statement** | the compiled statement, read — never the name |
| **classification** | ASSERTED-FALSE / UNASSERTED-FALSE-CONVERSE / TRUE-NECESSARY-CONDITION / NOT-ESTABLISHED |
| **source / ruling authority** | what licenses the claim: a ruling, a consumer's requirement, or nothing |
| **instrument and its honest limit** | see below |
| **owned repair or dependency** | who closes it, under which ticket |

### The instrument column — and the limit v1 got wrong

v1 offered "a control showing there is none". **A finite unsuccessful witness
search is not a proof that no counterexample exists.** Every row states its
**instrument** and its **honest logical limit**:

- **a counterexample** — an executed witness. Settles the row;
- **a proof** of the converse. Settles the row;
- **a bounded search that found nothing** — states its bound and concludes
  **NOT-ESTABLISHED**, never "no counterexample exists".

### Per-consumer contract

For **each actual consumer**, state whether the inversion is relied on as **EXACT
PREMISES** or as **NECESSARY CONDITIONS**. Different contracts; consumers may
disagree with each other, and a disagreement is a finding **about the consumers**.

---

## Bounds

- **No statement strengthening and no semantics patch inside the assessment.** A
  needed antecedent change is a **routed repair obligation**.
- `#76` and `#81` own semantic work; `#71` owns design prose.
- No model change, no theorem statement change, no push, PR or merge.
- Local-only delivery; nothing is ever written into a human desk pane.

## Budget

| phase | work | budget |
|---|---|---|
| **A** | current signatures, guard comparison, per-consumer exact-vs-necessary classification for all fourteen, plus the retention question | **2** builds, **≤16 targeted queries** separately counted |
| **B** | executed instruments for each contested row | proposed after A, from A's contested count; numeric build **and** query ceilings |

**Audit:** fresh independent auditor from the **inherited restricted set —
`codex` or `grok`; never `muse`, never `claude`** — with numeric build and query
ceilings and **authority-bound** completeness checks.

## Completion

**Two tables with named blocked rows do not finish #66's required work.** Every
contested row's **owned repair or dependency remains on the milestone completion
map** until discharged. Open questions are permitted as outcomes, not as closures.

## Changes from revision 1

| # | v1 | v2 |
|---|---|---|
| 1 | implied "11/14" meant eleven machine-checked converse proofs | it is the #68 audit's **assessment**. The mandatory path checks **six**; eight have none; three of those eight were **assessed** refutable |
| 2 | one undifferentiated notion of a false converse | **three classes**: ASSERTED-FALSE, UNASSERTED-FALSE-CONVERSE, TRUE-NECESSARY-CONDITION — plus NOT-ESTABLISHED |
| 3 | "an unused false claim in a proof corpus" | withdrawn — **no such claim is asserted**. Replaced by: **a public exactness requirement can be unmet without any false theorem**, and that is the finding |
| 4 | "a control showing there is none" | a **bounded search that finds nothing yields NOT-ESTABLISHED**, never "no counterexample exists". Every row states instrument and honest limit |
| 5 | "another family, never `muse`" | **inherited restricted set: `codex` or `grok`**; never `muse`, never `claude` |
| 6 | build budgets only | **numeric targeted-query ceilings**; **authority-bound** completeness checks |
| 7 | ended in two tables and a routed list | blocked rows **stay on the milestone completion map**; tables do not finish #66 |
