# S2 frozen packet

Frozen at dispatch, per NOTE-014. Supersedes the preparation note
`S2-MANDATE.md`, which remains as the record of what was prepared before the
desk's five clarifications.

| field | value |
|---|---|
| base | `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (landed S1 on `master`) |
| worktree | `/code/reactivegas-66-s2`, cold, no `lean/.lake` |
| branch | `chore/66-s2-axiom-gate` |
| **frozen mandate hash** | **`0a1db9887ccc9d8f`** — sha256 first 16 of `commit-owner-s2-muse/brief.md` |
| **frozen gate hash** | **`39d6aa4e2c0c0170`** — sha256 first 16 of the executable contract at base: `justfile`, `check-reactivegas-inversion-coverage`, `check-trace-coverage-agreement`, `lean-dependency-direction.sh`, `check-lean-toolchain`, `ci.yaml` |
| submission cap | **2**, no third |
| build budget | **8** (owner), **5** (auditor) |
| ceiling raises | 0 / 2 |
| commit owner | `muse` |
| auditor | fresh independent, `codex` or `grok`, **never `muse`** |
| report delivery | **local only**, in both briefs |

## The five desk clarifications, and where each lands in the mandate

| # | clarification | landed as |
|---|---|---|
| 1 | reconcile SOURCE discovery, the import/build set and the compiled extent **independently**; an import list cannot detect its own omission; added-module control **through the mandatory path**; explicit identities, never a quota | **A1** three independently-derived sets S/B/T with `S \ B` failing the gate; **A5** added-module control demonstrated through `just lean`/`just ci`; A1 forbids any expected-count constant |
| 2 | define the allowed axiom set and justify it from the existing trust contract; the non-standard-axiom control must be a theorem that **actually depends** on the axiom | **A6**, with the set justified as exactly `permittedAxioms` at `check-reactivegas-inversion-coverage:101` — the policy the repo already enforces on six inversions, extended to the discovered extent — and the control shaped as `axiom` + a theorem using it |
| 3 | remove the false evidentiary claims of all three decoys **and their wrappers/consumers**; residual-in-a-report does not repair; preserve useful checks under accurate names; bind real obligations to real gates; unenforced behaviour becomes an owned finding | **B1–B4, B6**. My earlier "named residuals" proposal is explicitly withdrawn in the brief |
| 4 | do not claim import/toolchain checks are inherently impossible in Lean | **B5**, naming it as my error and stating the precise claim instead |
| 5 | a new root gate driver must be declared to Lake and in discovered coverage; do not weaken the scanner or hide sources; #70 owns its registrations | **§5**, with overlap routed to me and then to the desk, never owner-to-owner |

## Also carried

- **A3** cold provenance retained, without spending builds re-establishing it.
- **A8** totality by panic-string absence, inspecting emitted results — the rule
  this lane established at `09f8230`, where 70 panics coexisted with exit 0.
- **A4** nonzero discovery: a sweep that finds nothing fails.
- **Row C** the `Predicates.lean` doc path, with `docs/` itself untouched.
- File fence with everything outside it a question, not an edit.

## Not in S2

S3 (theorem-keyed mutant ledger, `LEAN-CLARITY.md`), S4 (Prop/Bool
correspondence), S5 (retention statement-completeness). #71 content. The
desk-owned semantic ticket from §10. No merge without exact desk authorization.

---

# Packet version 2 — AMENDMENT-1, NOTE-015

**Version 1 is preserved above and remains the record of what was frozen at
dispatch.** Frozen mandate v1 `0a1db9887ccc9d8f` over `brief.md` alone.

| field | value |
|---|---|
| **frozen mandate v2** | **`7cfb7aec95a37448`** — sha256 first 16 of `brief.md` + `inbox/NOTE-001-amendment-1-remove-the-quota.md`, concatenated in that order |
| frozen gate | `39d6aa4e2c0c0170` — **unchanged**, the executable contract at base did not move |
| builds spent at amendment | **5 of 8** — preserved, none invalidated |
| caps | unchanged: submissions 2 (no third), owner builds 8, auditor 5, ceiling 0/2 |

## What v2 adds

**A2′ replaces A2.** `expectedDeclarations := 163` (line 106) and its two
`== 163` assertions (lines 402–404) are now **in scope and must be removed**,
replaced by nonzero independently discovered source-declaration /
elaborated-identity reconciliation. My v1 wording — "not yours to touch" — was
too broad: the standing prohibition is against replacing 163 with **another
quota**, never against replacing the quota with **real discovery**.

Eight binding constraints, of which the load-bearing ones are: the
reconciliation must not be deleted; two counters from the *same* inventory do
not satisfy it; adding a valid theorem or module must **pass**; omitting it must
**fail**; and a `sorry`/non-standard-axiom dependency must fail **for its
dependency**, not because a count changed. The six `requiredInversions`
obligations remain independent of the census.

The file fence is amended to permit editing
`scripts/check-reactivegas-inversion-coverage` for exactly that change.

## Why now, and the evidence for it

#68 is adding genuine proof helpers; the constant would **reject valid added
theorems for the wrong reason**. The commit owner had already, independently,
reached the control that demonstrates it: an existing non-inversion theorem made
`by sorry` leaves the count at 163 and so passes the quota, while the axiom
gate catches it. A check blind to the defect it appears to guard, that will
nonetheless fail an unrelated lane — that is the amendment's justification, and
it goes in the submission packet as such.

Both of the commit owner's proposed follow-ups are approved and required: the
transitivity probe, and Run B redone as proof-poisoning through `just lean`.

**#68 integrates the accepted S2 once landed rather than bumping its own quota.**
That coordination is the desk's.

## For the auditor

The fresh FULL auditor receives the **complete final contract**: `brief.md`
plus AMENDMENT-1, mandate hash `7cfb7aec95a37448`, and this versioned packet including
version 1.
