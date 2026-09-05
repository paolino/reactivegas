# S2 successor campaign — proposal for desk disposition

Per NOTE-029. **Proposal only. No implementation dispatch until the desk
dispositions the cost, fence and contract.** No merge, no #66 closure.

## What is being replaced, and what is preserved

The previous campaign is **exhausted and preserved unchanged**: three
submissions including the exceptional third, owner **14/14**, final audit
**6/6 builds + 24/24 probes**. All reports, counts, instruments and evidence stay
as they are — **including the unused `G-001` instrument and the contaminated
fixture-cache label**. `b0c2cdb` remains **NOT ACCEPTED**; PR #85 stays draft.

**This is a new campaign, not a fourth bounce.** Desk NOTE-030 chose **re-cut**:
no fourth old-campaign submission, **no narrowed acceptance of `b0c2cdb`**. **The
original ownership requirement is still owed and will not be redefined to the
easiest passing default environment in order to get a landing.**

## Two binding items are outstanding, not one

My earlier question said everything closed except F-004. **That was wrong — it
was written against a partially-read report**, and the desk corrected it.

**F-004 — ownership nonconformance.** A valid equivalent **relative** project
path yields **B=0, T=0**. The completed Std-alias probe — the pinned `Std`
exposed through `lean/.lake/vendor/stdlib/lib/lean/Std`, a symlink whose resolved
artifact is still the pinned Nix-store olean — finished **exit 1 in 712 s with
S=26/26, B=288, T=15707**, misclassifying **all 262 dependency `Std` modules as
project-built**, with a secondary T-derivation disagreement, **no panic and no
import error**.

**G-001 — BLOCKING coverage gap, AMENDMENT-2 row 6.** The `audit` entry guard for
**empty or unset `LEAN_PATH`** was **never fired**. Missing `REACTIVEGAS_ROOT`,
unavailable non-empty entries and `builtModules []` were each fired — **none of
them is that branch**, and none was relabelled as it. The instrument is
**prepared, hashed and explicitly UNEXECUTED**; the remaining cost is **one
targeted elaboration, zero builds**. It is an **assurance gap, not proof the
guard is broken**, and no "unreachable" claim closes it.

## Language corrected — what may and may not be said

I wrote that the gate "fails safe" and that the mechanism is "correct and safe in
the default environment". **Both are withdrawn as generalizations over an
untested domain.**

The supported statements, and only these:

- the **audited default current inventory agrees**, and **CI passes**;
- the **adverse-path controls are loud false refusals**, which is **not** evidence
  of current silent poison acceptance.

**The original full ownership contract remains a required repair — not only a
prose overclaim.** That was my framing and it understated the obligation.

## Why a campaign rather than another patch

Three candidates each repaired the previous finding and exposed the next, and
each answered *"which modules are ours?"* with a **guess about the
environment** — a namespace prefix, then Lean's own import closure, then a path
spelling. **The successor must establish an ownership authority, not guess
better.**

**Withdrawn (NOTE-031):** my earlier claim that the four-attempt budget *caused*
each repair to break the next thing. **The successive incorrect ownership
assumptions are evidenced; that causal budget explanation is not.** The budget
below is sized from the enumerated control list, not from that story.

## Subject and credit

Subject is the **entire `4a6cd87..final SHA`**. **Every original row — renames,
census, axiom policy, inversion obligations — remains independently
falsifiable.** Prior fresh evidence at `b0c2cdb` is **credited as evidence**;
**nothing is inherited as acceptance.**

## The contract — five named elements, each with its control

| # | element | what must be established | control |
|---|---|---|---|
| 1 | **Resolver / ownership authority and canonicalization** | the actual authority that decides project-vs-dependency, and how paths are canonicalized before comparison. Not a spelling test | the authority documented, plus its behaviour on canonically-equal but textually-different inputs |
| 2 | **Actual project/dependency source-output relation** | the real relation between a tracked source and the artifact it produces — the thing that makes a module *ours* | derived, not asserted |
| 3 | **Equivalent-path controls** | classification is **invariant** under equivalent loader paths | relative vs absolute project entry; symlinked and aliased entries; each must classify **identically** |
| 4 | **Independent source omission** | `S \ B` and `B \ S` both fire on genuinely project-owned subjects | the two layers kept **explicitly distinct**, as the terminal audit did |
| 5 | **Missing-authority behaviour and its control** | what happens when the ownership authority cannot answer | **Two lawful outcomes, and exactly one must be evidenced.** If the new implementation **retains** the old empty/unset `LEAN_PATH` guard, **execute that exact guard control** — the instrument is prepared and hashed, one targeted elaboration, zero builds. If the new implementation **legitimately replaces** that authority and **retires** the branch, **record the retirement and execute the replacement missing-authority control instead.** **Do not preserve a dead branch merely to keep the old probe runnable** |

**Forbidden as substitutes, carried forward:** `B := S`, any name or namespace
whitelist, and the `import Lean` closure. A fourth guess is not a repair.

## Bounded advisories — no scope smuggling

**`CI-T-SHARED-FILTER`** (both T derivations share `thmInfo` and B membership —
two views of one inventory) and the **shadow-name invariant** remain **bounded
advisory**. They are **not** in this campaign's scope unless the desk separately
justifies adding them, and they may not be smuggled in as incidental work.

Likewise the **root-selection algorithm comparison stays advisory** with its
stated unsuccessful-import limit, and is not to be inflated.

## Shape — one coherent candidate

**Recommended: a single coherent full candidate**, as NOTE-029 prefers.

Splitting is possible but I do **not** recommend it here: the five elements share
one mechanism, and separating them would produce partial landings whose
acceptance depends on the unlanded remainder. If the desk nonetheless wants
separation, the condition holds — **the original required S2 trust mechanism
stays explicit and fully commissioned**, **no partial landing is labelled "S2
complete"**, and **splitting is never used to inherit acceptance**.

## Numeric allocations — itemized to complete the original mandate

Sized to finish the mandate, **not to demonstrate one failing example**. The
previous campaign's four-attempt owner budget is exactly what made each round
able to fix one thing and re-break another.

### Owner: 9 substantive build/gate attempts, 30 targeted queries

| # | attempt |
|---|---|
| 1 | implement the authority; `just lean` clean baseline |
| 2 | **equivalent-path control** — relative vs absolute project entry, must classify identically |
| 3 | **alias/symlink control** — aliased dependency (the pinned `Std` vendor case) must stay dependency |
| 4 | **`B \ S`** on a genuinely project-owned module |
| 5 | **`S \ B`** and source-omission |
| 6 | **`G-001` missing-authority entry guard**, executed |
| 7 | sorry / non-standard-axiom dependency through the mandatory path |
| 8 | slack for **one failed attempt** — failures count, and pretending they will not is how the last budget ran out |
| 9 | **final full `nix develop --quiet -c just ci`** |

### Auditor: 8 substantive build/gate attempts, 30 targeted queries

| # | attempt |
|---|---|
| 1 | **one actual full cold CI** at the final SHA, `.lake` absent |
| 2–6 | each of the five contract elements, re-derived independently |
| 7 | **rebuilt base `4a6cd87`** — `Expr` equality for the renames **and** the base consumer scan, one build serving both |
| 8 | slack for one failed setup |

**Totals: 17 substantive builds, 60 targeted queries** — authorized by NOTE-031.
**ONE submission under this grant**, not two funded by one audit. Findings return
to the desk for disposition: **no automatic successor, no automatic ceiling
raise, no additional submission.** No automatic raises. If either seat finds the
allocation insufficient, it reports the **concrete command and cost gap before
exceeding it**.

**Counting rule:** enumerate **actual nested compiling / full-gate invocations,
including failed and warm attempts**. **Read-only and version interrogations cost
zero.**

## Seats

- **Implementation owner: fresh `muse`** context.
- **Auditor: fresh FULL `codex`**, both **model and reasoning effort pinned in
  argv**, fresh context/root/START, `.lake` initially absent.
- Never `muse` auditing `muse`; never `claude` as auditor.
- **Local-only reporting** in both briefs; nothing written into any human pane.

## Bounds

No model, semantics or theorem statement change. No `docs/en/design/` edits. No
push, PR or merge without exact desk authorization. **No #66 closure.**

## What the desk is asked to disposition

1. the **five-element contract** above;
2. the **17 builds / 60 queries** allocation;
3. **one coherent candidate** versus separation;
4. the **fence** — I propose the existing S2 fence plus
   `scripts/check-lean-axioms` and directly necessary associated scripts.

**Nothing is dispatched until that ruling arrives.**
