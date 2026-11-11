# C1 repair proposal v2 — revised against actual state, spend and desk corrections

Supersedes v1 (`1162b52e…`), **preserved** as
`C1-DEFENSIVE-REPAIR-PROPOSAL-v1-SUPERSEDED.md`. **Proposal only.** No code,
build, test, audit, push or merge authority exists or is exercised here.

## 0. Reconciliation — two separate failures, neither excused

**Delayed withdrawal is mine.** The desk's proposal-only note reached me
22:25:27Z; I wrote the proposal at 22:27:13Z and did not relay the withdrawal.
`%540` committed at 22:32:17 / :30 / :57 under my NOTE-020; my stop was written
22:33:00, three seconds late. The owner executed what it held and stopped on
receipt. **The parent owns this.**

**The targeted overrun is separate and real.** T-C ran **twice** — the first
aborted on a typo in the owner's throwaway probe. That is **4 executions against
3 available: 41 spent against a ceiling of 40.** A retry is spent. Three
demonstrations are not three executions, and delegation does not make counting
optional.

**No retrospective grant, no refund, no reclassification.** The overrun stands on
the record as an overrun that happened. Any ceiling movement below is
**forward-looking only** and does not bless it. The aborted run's raw receipt is
recovered locally where it exists; **where it does not, it stays unavailable** and
is not reconstructed.

## 1. Corrections the desk owed me, accepted

- **"Authenticated" was wrong.** The property is *recorded-signer authority
  preserved, with malformed actor substitution refused at the intended import
  boundary*. This is **not** a cryptographic authentication claim, and I withdraw
  that wording from v1.
- **"Any stripping is the finding" overstated the evidence.** I withdraw it.
  Equal-author legacy normalization and conflicting-author refusal are
  **different cases** and must each be assessed against the actual schema and
  compatibility contract. A strip helper is **not per se** a substitution defect.
- **A bounded explicit schema is not invalid merely because its fields have
  names.** My v1 objection to `author` being "hardcoded" was wrong as stated. A
  bounded, explicit field set **with independently enforced agreement** is a
  legitimate design.
- **Grep enumeration is not complete discovery**, and I am not proposing
  universal JavaScript discovery — that was an over-promise in v1. What is
  required is **stated coverage plus a can-fail mechanism**, not an unbounded
  claim.

## 2. F-02 — the repair is rejected, and the real geometry is provable

`5ee08ed9` replaces the fixed 42-unit glyph radius with **shrinking** radii,
weakens the separation assertion to `2*r0 + 8`, sweeps only `k = 1..103`, and
calls larger counts unreachable without evidence. **The requirement is not
whatever range makes the new formula pass.** Shrinking glyphs trades a geometry
failure for a readability and interaction failure the original requirements
already forbid.

**The incompatibility is provable, not sampled.** Centres on a ring of radius `R`
at `n` slots have chord `2R·sin(π/n)`:

| n | chord on R=132 | |
|---|---|---|
| 8 | **101.03** | ok |
| 9 | **90.29** | fails < 92 |
| 10 | **81.58** | fails < 92 |

These reproduce the auditor's measured values exactly. Closed form: a **fixed**
ring `R=132` at separation 92 admits at most
`⌊π / asin(46/132)⌋` = **8** — which is precisely where it broke. So *fixed ring
+ fixed 42 glyph + 92 separation* is **mutually incompatible beyond 8**. That is
the incompatibility the desk asked to see named with evidence.

**Proposed coherent design — grow the ring, keep the glyph and the guarantee:**

```
R(n) = 46 / sin(π / n)        (n ≥ 2; R(n) ≥ the current fallback)
```

| n | 8 | 9 | 10 | 20 | 103 | 500 |
|---|---|---|---|---|---|---|
| R(n) | 120.2 | 134.5 | 148.9 | 294.1 | 1508.4 | 7321.2 |

This keeps `rr = 42` and the **92-unit guarantee intact for every n**, closed
form, **no sampling, no count cap, no semantic restriction**, no shrinking or
disappearing controls, no drag exclusion. Viewport fit is a **view** concern
(pan/zoom), which is not the same as shrinking the glyph's semantics.

**Can-fail controls:** pin the ring back to constant 132 → **RED at n = 9**;
n = 8 stays **GREEN**; assert the numeric minimum against 92 and assert
`rr == 42` unchanged, so a future "fix" cannot pass by shrinking again.

**Finite samples do not prove the unbounded claim** — the closed form does, and
the sweep exists only as a corroborating check, never as the proof.

## 3. F-01 — state the actual coverage, and its can-fail mechanism

Assessed for shape only; **not** an acceptance.

`9127e452` refuses a **mismatching** payload `author` with a distinct
`author-mismatch` reason at verification and replay, and strips only the
**matching** case. Against the corrected property — *recorded-signer authority
preserved, malformed substitution refused at the import boundary* — the refusal
case is the security-relevant one and it is present.

What the packet must **state**, rather than claim universally:

- **Field coverage:** exactly which identity field(s) the machine owns
  (`author` today, per the Lean `AppEvent` schema) and the **agreement check**
  that independently enforces the schema, so the set cannot silently drift.
- **Site coverage:** exactly which argument-merge sites are covered, enumerated
  and stated as **enumerated** — not as complete discovery.
- **Can-fail mechanism:** a control that reddens if a *new* merge site bypasses
  the choke point, and one that reddens if the field set and the schema disagree.
  Coverage is credible because it can fail, not because it is exhaustive.
- **Equal-author normalization** must be justified against the actual
  compatibility contract (which legacy or canonical exports carry it), separately
  from the refusal case.

## 4. F-03 — same discipline: stated coverage, no universal claim

`f85ff597` recognises quoted-bracket and bare reads and fails closed on dynamic
keys. That is the right **shape**: an unclassifiable access must be loud.

The packet must state the **recognised spelling set** and the **fail-closed
residue**, and must not claim complete discovery of all JavaScript property
access. Controls: bracket → RED, dot → RED, computed → RED, production → GREEN.

## 5. Costed invocation plan — including parent work, with a ceiling

The desk is right that I have been treating my own instrument work as outside any
counter. It is counted here.

**Design change that makes this affordable:** the successor gate gains **focused
row modes** (one row, no full body), so its own falsification is *targeted* and
repeatable instead of costing a full gate run per control. Cheap falsifiability
is a property the instrument should have had from the start.

| party | invocation | kind | count |
|---|---|---|---|
| owner | F-02 redesign controls (RED@9, GREEN@8, `rr==42` held) | targeted | 3 |
| owner | F-01 field-agreement + new-site can-fail | targeted | 2 |
| owner | F-03 bracket / production re-demo after edits | targeted | 2 |
| owner | final successor gate | substantive | 4 |
| owner | final clean `just ci` | substantive | 1 |
| **parent** | successor-gate focused row controls, both directions | targeted | 8 |
| **parent** | one full successor-gate green run before freezing | substantive | 4 |
| auditor | full frozen gate | substantive | 4 |
| auditor | cold `just ci` | substantive | 1 |
| auditor | full 17-row mutation campaign | targeted | ≤53 |

**Requested ceilings — forward-looking, nothing spent against them:**

| counter | now | requested |
|---|---|---|
| owner substantive | 28/28 | **33** (+5) |
| owner targeted | **41/40 (over by 1)** | **48** (+8: 7 needed, and the ceiling must exceed the 41 already recorded) |
| **parent substantive** | uncounted | **4** — a real ceiling where there was none |
| **parent targeted** | uncounted | **10** |
| auditor | 10/60, 5+7 spent | unchanged |

Submission 2 stays bound at **5 substantive / 53 targeted** and remains a
**proposal** until this plan is judged feasible. **No audit dispatch.**

## 6. Scope, unchanged

All **17** original rows, all **13** unjudged obligations, every prefix
requirement and every original limit remain the acceptance scope. F-02 is now
also **not repaired**. Preserved limits: F-01 is an import/replay/adoption
property and not a cryptographic claim; the **file-picker path is unexercised**
and must be named in final acceptance; F-03 demonstrates instrument blindness,
not a shipped handler; `probe.mjs` was frozen only at handback and the T4–T7
pre-run hashes do not exist and will not be reconstructed.

`%562` remains terminal and preserved — no restart, no reword, no provider
evasion.
