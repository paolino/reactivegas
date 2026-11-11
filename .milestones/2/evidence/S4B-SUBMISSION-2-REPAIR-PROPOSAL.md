# S4-B submission-2 repair proposal

**This is a proposal. It is not a build grant, not a second-audit grant, and no
execution or candidate edit has been performed under it.** Static assessment,
brief and costing only. Every number below is a **request**.

## The audit that produced it

Terminal **AUDIT-FINDINGS** at `22:53:19Z`, submission 1, candidate
`189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`. Report
`90cd9d9bb9bade9d4e0e08c3fb09334ffa108a685974c199511362192a2cac00`; evidence
manifest `4a2e2181563f6c286bea2f441ab358655b41f079a50096d720f22eb32e59604a`,
693 entries. **89 obligations: 78 CLOSED, 11 PARTLY, 2 BLOCKING, 0 OPEN.** The
44 genuinely killed atoms and the final cold CI exit 0 are credited at their
actual scope. Onward set: **explicitly empty**, with F01/F02 kept in scope rather
than exported.

Not an acceptance.

## F01 — owned opaque predicates bypass mandatory discovery

**What was shown.** Adding one declaration to the already tracked and imported
`lean/Reactivegas/Predicates.lean`:

```lean
opaque auditOpaque (s : State) : Prop := s.conti = []
```

left `nix develop --quiet -c just lean` at **exit 0**, printing
`MIRROR-CHECK-OK … discovered=24` and writing a valid fresh-nonce receipt that
omits it, while a compiled probe found **25** identities including
`Reactivegas.Predicates :: auditOpaque` with `opaqueInfo : State → Prop`. A real
compiled omission — not an uncompiled file, namespace escape, or count-only
inference.

**Root cause, verified at source.** `scripts/check-lean-mirrors`:

```
165    let isPred : Bool :=
166      match ci with
167      | .defnInfo _ => true
168      | .inductInfo _ => true
169      | _ => false          -- silently drops opaqueInfo
…
268    | .defnInfo _ => pure ()  -- same narrowing in the orphan scan
```

The catch-all discards `opaqueInfo` before correspondence and exception
accounting, so a new owned predicate can bypass the "any `Prop`" obligation on
both axes.

**Proposed repair — classification, not blanket inclusion.** Replace both
`Bool`-valued filters with a **total classification over `ConstantInfo` kinds**
where every kind is either predicate-bearing or **explicitly excluded with a
named reason**, and an **unrecognised kind fails closed**. That last clause is
the future-discovery guarantee: a Lean version adding a declaration form cannot
silently reintroduce the hole.

Explicitly **not** proposed: blanket-including `thmInfo` as predicates (a theorem
is not a predicate declaration, and doing so would inflate the census and destroy
the classification), and **not** retaining any silent exclusion.

**Not claimed:** that today's model predicate was already omitted. The
candidate's **current 24 entries reconcile**; it is the *future-discovery
guarantee* that fails.

## F02 — P01/P07 relatum controls do not meet v2 executable-body sensitivity

**What was shown.** Operative v2 Amendment 1 requires mutating the **executable
expression/operative definition**, preserving the original theorem, and observing
that theorem **stop elaborating**. The submission's P01/P07 controls change the
**relatum** instead — a different observation — and the packet nonetheless labels
all controls closed. Independent probes reproduce the distinction:
`isMember` forced false, and the `closePurchase` permission atom forced true,
each with the exact original theorem statement and proof bytes, unrelated helper
proofs excluded to prevent first-error masking.

**Explicitly NOT claimed, and this must survive into any restatement:** no
theorem refutation; no acceptance of a fully mutated machine; **no actual
deployed economic vulnerability**. The current correspondence theorems are
**valid**. `C5`/`C11` are **PARTLY**, not executable-body KILLED — the theorem is
value-parametric or relates an inline expression independently of its production
use.

**Two admissible dispositions, and the choice is the reviewer's — not mine and
not the owner's:**

- **(a) Bridge.** Add owned proof/bridge/check work connecting each correspondence
  theorem to the **operative consumer/body**, so that mutating the body does stop
  the original theorem elaborating. Concrete, and it satisfies v2 as written.
- **(b) Exact conflict.** Return the precise logical/contract conflict showing why
  v2's condition cannot hold for these two rows as stated, leaving them PARTLY
  with the disclosure intact.

**Forbidden either way:** relabelling PARTLY as CLOSED; any waiver from
disclosure; unruled runtime semantics; and **edits to existing theorem
statements**. The reviewer may also choose **no production fix**.

## Changed-file fence — exact

| file | permitted change |
|---|---|
| `scripts/check-lean-mirrors` | F01 only: total kind classification + fail-closed on unrecognised kind |
| `lean/Reactivegas/Predicates.lean` **or** a new owned bridge module | F02 **(a)** only: added bridge/consumer lemmas. **No edit to any existing theorem statement** |
| `lean/<S4-B mirror table source>` | classification/exception rows required by the repaired filter |

**Nothing else.** No model or guard semantics, no `justfile` recipe change, no
`docs/en/design/` writes, no S2R/S3 files, no test or fixture rewrites.

Original **S4 obligations and operative v2 are preserved in full** — the repair
adds accountability, it does not narrow the target. All 78 CLOSED rows keep their
evidence; the 11 PARTLY keep their labels unless genuinely re-established.

## Commands required — by layer, with the cost honestly attributed

| # | class | command | establishes |
|---|---|---|---|
| R1 | **substantive, cold** | `nix develop --quiet -c just lean` on a fresh `.lake` at the repair SHA | repaired filter admits the real census; mandatory path green |
| R2 | **substantive** | `just lean` with the F01 `opaque` specimen re-added | the specimen is now **discovered and rejected** — the control that failed before |
| R3 | **substantive** | `just lean` with an unrecognised/newly-shaped declaration | **fail-closed** future-discovery guarantee actually fires |
| R4 | **substantive** | `just lean` with F02(a) bridge present and the operative body mutated | the original theorem **stops elaborating** — v2's actual condition |
| R5 | **substantive, cold** | `nix develop --quiet -c just ci` at the final committed SHA | acceptance receipt |
| T1–T3 | targeted | compiled-identity probes: census before/after, opaque specimen, bridge identities | classification evidence |

**Owner cost: 5 substantive + 3 targeted.** If F02 disposition **(b)** is chosen
instead of the bridge, R4 and one targeted drop out: **4 substantive + 2
targeted**.

## Counters — kept separate, as required

| party | spent | remaining under current authority |
|---|---|---|
| **owner** | **8/8 substantive, 42/60 targeted** | **0 substantive.** The proposal above needs **+4 or +5**, which does not exist |
| **auditor** | **6/8 substantive, 59/60 targeted** | 2 substantive, 1 targeted — **explicitly not assumed sufficient for a fresh full audit** |
| parent (me) | no build spend; static assessment only | — |
| submissions | **1 of 2 spent** | submission 2 available |
| ceiling raises | **one**: owner 6 → 8 | retained, not reset |

The auditor's own counter correction is retained: it conservatively charged an
accidental nonexistent-node invocation as a **failed targeted setup** under
no-refund discipline (58 Lean targeted + 1 setup = 59/60).

## Final-SHA audit plan

1. Owner performs the fenced repair; commits **one** candidate; no amend after
   evidence begins binding.
2. Owner runs R1–R5 (or R1–R3, R5), returns submission 2 binding the **final
   SHA**, every raw mutant, and every failed setup receipt.
3. A **fresh** auditor of the restricted family re-derives **all 89 obligations**
   at that SHA — F01 and F02 first, then the 11 PARTLY, then the 78 CLOSED as
   unchanged-input with explicit byte identity where the bytes truly did not move.
4. **The remaining 2/1 is not enough for that.** A fresh full audit needs its own
   allowance; I am not assuming it.

## Preservation

The archived **pre-final cache is kept distinct from the final candidate cache**;
all raw mutants and failed setup receipts are preserved; the 693-entry manifest
and both terminal reports stand as history.

## What is not requested and not done here

No push, PR, merge, `#66` closure, or `#68` interruption. No new execution, no
candidate edit, no relabelling of any row. S3 static work continues
independently and is untouched by this proposal.
