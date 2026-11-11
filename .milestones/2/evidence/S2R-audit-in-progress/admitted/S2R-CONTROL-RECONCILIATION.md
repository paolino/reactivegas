# S2 successor — full control reconciliation, before START

Required by NOTE-031: *"Reconcile the entire original mandatory controls, not
just the new five, before START. Report any specific gap then; no scope reduction
to fit."*

Subject is the **entire `4a6cd87..final`**, so the successor **redoes** every
original row from the accepted base. Prior evidence is **evidence at its SHA**,
never inherited acceptance.

## Owner — 9 substantive invocations against every mandatory row

Counting rule: **actual nested compiling / full-gate invocations, including
failed and warm attempts**. Read-only and version interrogations cost zero.

| # | invocation | mandatory rows it discharges |
|---|---|---|
| 1 | implement; cold `just lean` baseline | A1 S/B/T derivation · A2′ census with no quota · A3 cold provenance · A7 mandatory path · Row B renames · Row C doc path · fence |
| 2 | `just lean`, registered root **outside the old namespaces** importing `Std.Data.DHashMap`, clean | **element 3** equivalent-path (absolute leg) · A5 added-module through the mandatory path · A2′ c3 valid add passes |
| 3 | `just lean` with the project entry expressed as an **equivalent relative path** | **element 1** authority + canonicalization · **element 3** equivalent-path invariance |
| 4 | `just lean` with the pinned `Std` exposed through an **aliased/symlinked** vendor path | **element 2** project/dependency source-output relation · dependency-not-misclassified |
| 5 | `just lean`, genuinely **project-owned module withheld** | **element 4** `B \ S` on a project-owned subject |
| 6 | **missing-authority control** — the retained guard, or its replacement if the branch is retired | **element 5** / **G-001** |
| 7 | `just lean`, existing theorem made **`by sorry`** | A5 sorry rejected **for its dependency** |
| 8 | `just lean`, **non-standard axiom** plus a theorem using it | A6 policy · A6 using-theorem shape · AMENDMENT-1 transitivity |
| 9 | final full `nix develop --quiet -c just ci` | acceptance receipt · remote-CI precondition |

**Candidate probe-level rows, against the 30 targeted queries:** A2 no hardcoded
extent · A2′ c4 omission by identity · A4 zero S / zero B / zero T · A5 truncated
inventory · A5 removed module · one-sided T truncation both ways · A8
panic-string totality on both streams · Row B4 dead re-exports · `S \ B` source
omission · census reconciliation.

**Corrected by NOTE-033 — these are not cost-free by listing.** The class of an
invocation is decided by **what it actually runs**, not by where it appears in
this table:

- **a compiling targeted elaboration is a probe** (against the 30);
- **a whole build is substantive — even under a wrapper.** A control that
  invokes `lake build` over the module list, or a wrapper that does so on its
  behalf, **counts against the 9** however it is labelled here.

**The owner enumerates the actual invocation class of every control it runs**,
and where a row above turns out to require a whole build, that is a **concrete
gap to report before exceeding**, not a reclassification to make it fit.

**Reported honestly: there is no slack attempt left.** The nine are fully
allocated, and a single failed invocation consumes a row. The proposal's earlier
"slack for one failed attempt" is therefore **not** available once every
mandatory row is enumerated. **This is the specific gap NOTE-031 asked for**, and
I am reporting it rather than reducing scope to fit: **either the owner accepts
that any failure costs a mandatory row and must then report a cost gap, or the
desk grants a tenth invocation.** I do **not** take that grant.

## Auditor — 8 substantive invocations

| # | invocation | rows |
|---|---|---|
| 1 | cold full `just ci` at final SHA, `.lake` absent | acceptance receipt · cold provenance |
| 2 | equivalent-path control, both legs | elements 1 and 3 |
| 3 | alias/vendor dependency control | element 2 |
| 4 | `B \ S` on a project-owned module | element 4 |
| 5 | missing-authority control (retained or replacement) | element 5 / G-001 |
| 6 | existing theorem `by sorry` through the mandatory path | A5 |
| 7 | **rebuilt base `4a6cd87`** | Row B `Expr` equality **and** base consumer scan — one build, both purposes |
| 8 | slack / one failed setup | — |

**Corrected by NOTE-033.** My earlier framing — that the non-standard-axiom row
rides a "warm fixture" and the auditor should judge whether cached warmth
suffices — was wrong twice. **Warmth does not make a control weaker, and it does
not make a full gate invocation free.**

What actually matters for that row, and must be preserved and enumerated:

- **restoration of the previous `sorry` mutation** before the axiom control runs;
- **actual compilation of the new axiom and its consuming theorem**;
- **execution through the mandatory path**;
- **exact provenance** of the tree the control ran on.

**If the command is a full gate or build, it counts as substantive** — warmth is
irrelevant to its class. The auditor is **not** asked to decide whether cached
warmth is sufficient; it enumerates the **actual invocation class** and reports a
concrete gap if the allocation cannot carry it.

## Advisories that are not requirements

**`CI-T-SHARED-FILTER`** and the **shadow-name invariant** remain **bounded
advisories**. They are **not hidden new requirements**, are not in this
campaign's scope, and may not be smuggled in.

## The acceptance instrument

**The parent version-freezes the acceptance instrument and the whole-subject
requirements.** No author-controlled silent gate adaptation: if the owner needs
the executable contract changed, it asks; it does not adapt the gate under
itself.
