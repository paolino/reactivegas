# S4-B command reconciliation — v2, corrected before dependent execution

Corrected per desk NOTE-038. **v1 preserved** at
`S4B-COMMAND-RECONCILIATION-v1-superseded.md`.

**Actual spend at the time of this amendment: submissions 0/2, substantive 0/6,
targeted 0/60.** The S4-B owner had journalled its START and read both documents
but had **executed nothing** — worktree clean, zero oleans. **No backdating; this
is an honest amendment recorded before dependent execution.**

## My error in v1

I placed the **mandatory-path omission control** and the **checker-disable
control** in the **targeted** column.

**That classification was not established.** Those controls must exercise the
**actual mandatory path** — `just lean`, a whole project build. **By the very
rule I enforce in S2R, that makes them substantive.**

A direct leaf-checker probe *may* be targeted **if that is what actually runs** —
but a leaf probe **does not by itself prove the mandatory invocation's presence
or operation**, which is the whole point of those two rows.

Having mis-classified them, I then generated an approximate **~62 sketch**, a
**batching waiver** and a **two-query desk gate** from it. **All three are
withdrawn.** The correct first move was to **reconcile the actual complete
command set**, which is what follows.

## Corrected substantive allocation — 6

Intermediate whole builds "after modules exist", "after mirrors", "after proofs"
**need not each be a separate substantive command** if **targeted elaboration
establishes those phases** and a **later complete invocation covers the final
tree**. That frees the substantive slots for the rows that genuinely require the
mandatory path.

| # | invocation | why substantive |
|---|---|---|
| 1 | **clean mandatory path** — mirrors and checker wired, green | whole project build |
| 2 | **introduced owned predicate, counterpart absent** — must be detected **through the actual mandatory path** | whole project build |
| 3 | **introduced owned predicate, theorem absent** — same | whole project build |
| 4 | **checker-disable control** — disabling the checker must be detected | whole project build |
| 5 | **restored final full `nix develop --quiet -c just ci`** | whole project build |
| 6 | **one absorbed failure** | — |

**This is a sequencing observation, not a prescription, and not a claim that all
controls are already costed.** Whether the intermediate builds can genuinely be
carried by targeted elaboration is **a measurement the owner makes**, not an
assumption I impose. **If the actual runner does not permit it, the extra
commands are retained and returned as an exact gap.**

## Corrected targeted allocation — 60

| purpose | count |
|---|---|
| mirror definitions — shape, evaluate, fix | ~17 |
| correspondence proofs — attempt and repair | ~19 |
| **per-identity falsification controls** | **~19** |
| proof-axiom and totality at the **final** tree | 2 |
| inventory / counterpart reconciliation | ~2 |

**≈59 against 60.**

**The v1 gap dissolves once the classification is corrected** — not by a waiver,
and not by trading anything away. The two controls I had wrongly placed here have
moved to substantive, where they belong.

## What does not change

- **6/60 owner and 8/60 auditor stand. No cap reset. No new raise.**
- **No row may be dropped**, and **no row may be relabelled to fit** — the
  mandatory omission and checker-disable rows in particular **cannot disappear
  into a leaf probe**.
- **Single-definition sensitivity remains per identity, with no first-error
  masking.**
- **The statement-shaping and proof-rate figures remain estimates, never proven
  minimum command counts.**
- **Measure and classify the actual paths.** A wrapper that rebuilds the whole
  module list is **substantive regardless of its label**; check what it runs.
- Genuine necessary extra commands are **retained and returned as an exact gap
  before overrun** — never absorbed.

## Auditor

Reconciled **separately before the auditor's START**, unchanged from v1.
