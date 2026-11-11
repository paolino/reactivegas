# C1 defensive repair proposal — F-01 / F-02 / F-03, and submission-2 feasibility

Ticket owner `%313`. **Proposal only. No repair, build, test, audit or merge is
authorised or performed by this document.** Candidate
`9717405e52664c9a520fcd0c65edb4e90612110a` remains **unaccepted**.

Report `bfc2b8c2fe3da9e7b3c4100740df622e1c9e09fa359f731d52ea0d4ada2173fb`.
Gate GREEN and cold CI 0 remain **valid bounded execution evidence, not
acceptance**.

---

## 1. Acceptance scope — unchanged, and it is larger than the repair

Acceptance scope stays the **whole unaccepted subject**
`6879970f… → repaired final`: **all 17 rows**, including the **13 never
examined**, every prefix requirement, and every original explicit limit.

**A repair of three findings cannot close what the partial first audit did not
look at.** Submission 2 is therefore a fresh **FULL** audit, not a repair-only
search, and **no untouched row inherits an aggregate PASS** from submission 1 or
from any earlier campaign.

Current row state: **4 FAIL/BLOCKED** (INV-3, INV-11, R-GEO, C-KEY),
**13 OPEN/UNJUDGED**, 0 KILLED, 0 RESIDUAL.

## 2. Claim limits that travel with the evidence, unweakened

| finding | what was demonstrated | what was **not** claimed |
|---|---|---|
| F-01 | actor binding broken in the **simulator's** production import / replay / adoption path | **not** a cryptographic, signature or deployed-service claim; the **file-picker path was not exercised** |
| F-02 | packing violated **beyond eight reachable purchases** (90.29 at 9, 81.58 at 10) | no screenshot retained; drag, closest-referente optimality and stability across membership changes unsettled |
| F-03 | **instrument blindness** to a bracket-notation read, with a dot-notation calibration proving transport | **not** a claim that the candidate ships such a handler — it has **0** today |

The **file-picker boundary** is unexercised and must be named as an explicit
in-scope boundary in final acceptance, not quietly assumed.

**Instrument custody limit, preserved rather than repaired away:** `probe.mjs`
was frozen **only at handback** (`ce66e7da…`); **per-invocation pre-run source
hashes for T4–T7 do not exist.** They will **not** be reconstructed. Final
acceptance requires **reproducible frozen instruments and real can-fail controls
frozen before their runs** — that requirement applies forward, and does not
retro-fit a hash onto a run that never had one.

---

## 3. F-01 — bind the actor at a single choke point, and **refuse** rather than override

**Class:** *the authenticated caller identity must remain bound through decoding,
validation and replay/adoption; a payload field must never substitute an actor.*

Defect: `core:1610` `{ tag, author: signer, ...ae[tag] }` and `page:2576`
`{ author: st.signer, ...ae[atag] }` — identity set **before** the spread, so
spread-is-last-wins.

**Proposed defensive shape:**

1. **One choke point**, used by every site that merges event arguments over a
   caller identity — `core` application path, page adoption path, and any future
   site. Not two edits at two lines.
2. **Refuse, do not silently override.** If an imported argument object carries a
   machine-owned identity field, the event is **rejected** with a distinct
   refusal reason. Reordering to `{ ...args, author: signer }` would make the
   attack silently ineffective; refusal makes a hostile or malformed import
   **visible**. Silent correction is how this class hides.
3. **Derive the identity-field set from the machine**, not from a hardcoded
   `author`. Any actor-bearing field the transition derives from the signer is in
   the set. Hardcoding one name is the F-03 mistake in a new costume.
4. **Derive the site set.** A check that enumerates every argument-merge site and
   fails closed on an unregistered one — otherwise a fourth site added later
   escapes exactly as `page:2576` escaped the attention paid to `core:1610`.

**Can-fail controls (all must be able to go the other way):**
- payload carrying `author` → **REFUSED**, with the refusal reason asserted, not
  merely a non-zero exit;
- **calibration:** the same event **without** `author` → accepted, proving the
  refusal is not blanket;
- **mutant:** restore the old precedence → the control must go **RED**;
- **site-derivation mutant:** add an unregistered merge site → **RED**.

Replay of the retained `authority-forged-session.json`
(`a00067f9…`) must be refused; that artifact is the regression fixture.

## 4. F-02 — make separation a function of reachable count, not a fixture

**Class:** *layout separation must hold across reachable collection
cardinalities, including the packed fallback.*

Defect: fallback pinned at radius 132; largest non-overlap fixture stops at
**eight**; separation fails at **nine**.

**Proposed defensive shape:**

1. **Solve the radius for the count** so the minimum centre distance satisfies
   the declared separation for every reachable `n`, rather than a constant
   fallback that happens to work at eight.
2. **Quantify the check over reachable counts** — derive the bound from what the
   application can actually reach, and assert separation across that range. A
   fixed fixture count cannot stand for an extent; that is precisely what failed.
3. State the reachable bound explicitly. If it is unbounded, the check asserts
   the invariant **parametrically** and names the assumption.

**Can-fail controls:**
- pin the radius back to the constant → check must go **RED at n = 9**;
- **calibration:** at n = 8 the same check is **GREEN**, so it is not
  reddening on everything;
- assert the **numeric** minimum distance against the declared separation, not a
  visual impression.

## 5. F-03 — discovery must be spelling-independent and **fail closed**

**Class:** *discovery and witness reconciliation must cover actual key-writing
handlers independently of how the property access is spelled.*

Defect: `deriveExtent` scans `/\.dataset\.([A-Za-z]+)/g` — dot notation only.

**Proposed defensive shape:**

1. Recognise **every static spelling**: `.dataset.name`, `dataset['name']`,
   `dataset["name"]`, backtick form.
2. **Fail closed on what cannot be classified statically.** A computed access
   (`dataset[expr]`) must be an explicit **RED** — "cannot be statically
   classified, extend the harness" — never invisible. Invisibility is the defect;
   an unreadable access must be loud.
3. Keep the existing count-drift check per classified name.

**Can-fail controls:**
- bracket-read handler inserted → **RED**;
- **calibration:** dot-read handler inserted → **RED** (already demonstrated);
- computed-read handler inserted → **RED**;
- production page → **GREEN**, so it is not reddening unconditionally.

**Ticket-owner note, on the record:** C-KEY instructed derivation from *actual
handlers, not from a memory*. A derivation keyed to one spelling is a memory in
disguise, and it passed **my** gate — `gate-v16`'s derive row drives the owner's
derivation and cannot be stronger than it. The successor gate must not simply
re-drive a repaired derivation; it needs its **own** can-fail control that the
derivation is spelling-independent.

---

## 6. Submission-2 feasibility — feasible, with **zero substantive margin**

### Audit allocation — explicitly BOUND, not inferred

| | substantive | targeted |
|---|---|---|
| campaign total (unchanged) | **10** | **60** |
| **actual spent, submission 1** | **5** | **7** |
| remaining | **5** | **53** |
| **proposed binding, submission 2** | **5** | **53** |

Submission 2 is bound to **all** remaining targeted. Binding it to 30 would
strand 23 with no third submission to spend them, while submission 2 carries
**more** work than submission 1 attempted — 17 rows, 13 of them never examined.

**Honest risk, stated rather than smoothed:** 5 substantive is a full gate (4)
plus one cold `just ci` (1) and **nothing else**. A single failed or interrupted
run leaves no margin, and submission 1 spent its full 5 while reaching only 4
rows before it was interrupted. Feasibility depends on the mutation campaign
living in **targeted** — the established pattern, and the prior campaign killed
9 of 11 rows on 5 of 30 builds. If the auditor measures otherwise it must return
a concrete over-budget list **before** overrun, never a thinner audit.

### Owner budget — requested separately, as instructed

Owner is at **28/28 substantive, 37/40 targeted**; the five prior raises stay
**spent history** and are not refunded or netted. Requested for the repair, as a
separate exact request:

| item | kind | count |
|---|---|---|
| code repair, three classes | editing | **0** |
| F-01 controls (refuse, calibration, precedence mutant, site mutant) | targeted | 4 |
| F-02 controls (RED at 9, GREEN at 8) | targeted | 2 |
| F-03 controls (bracket, dot, computed, production) | targeted | 4 |
| **targeted subtotal** | | **10** vs **3 held → gap 7** |
| final successor gate on the repaired candidate | substantive | 4 |
| full `just ci` | substantive | 1 |
| **substantive subtotal** | | **5** vs **0 held → gap 5** |

**Requested owner budget: +5 substantive, +7 targeted.** Nothing is spent
against it, and I do not plan around it being granted.

### Instrument work — mine, outside the owner's counters

The successor gate must be re-derived and **falsified before freezing**, with its
own can-fail control for the spelling-independence property. `gate-v16`
(`70523191…`) stays immutable as the record of what certified `9717405e`.

---

## 7. If it does not fit

If the desk judges the repair or the full 17-row submission 2 cannot fit these
bounds, the alternative is a **recut successor campaign** carrying **every**
unfinished requirement forward — all 13 unjudged rows, all prefix requirements,
all stated limits, and the three findings — with no row closed by inheritance.
I am not proposing that now; I am naming it so the choice stays visible.

## 8. Provider restriction — surfaced, not evaded

The provider notice in `%562` remains **observed and unexplained**. Its
completed context will **not** be restarted, its refused request will **not** be
repeated or reworded, and no provider or model will be switched to bypass it. The
terminal report is the safe retained-evidence output. The repair and final
acceptance proposed here are **ordinary defensive implementation work** on the
lane's own code, distinct from whatever was refused; if any concrete restriction
blocks that ordinary work, I will surface it as a blocker rather than route
around it.
