# S4-B revised repair/acceptance packet (supersedes proposal v1)

Proposal v1 is **preserved as history** at `S4B-SUBMISSION-2-REPAIR-PROPOSAL.md`;
its bridge claim is **withdrawn** — see the v3 amendment
(`S4B-ACCEPTANCE-INSTRUMENT-v3-AMENDMENT.md`). **No build, edit, second-audit or
merge grant is assumed or taken here.** Every number is a request.

## Actual file paths — no placeholders

| path | change |
|---|---|
| `scripts/check-lean-mirrors` | F01 total `ConstantInfo` classification; **and** this file *is* the mirror-table source — the tabled correspondence rows live here (`:11`–`:17`, `:41`), so no separate table file exists or is invented |
| `lean/Reactivegas/Mirrors.lean` | P01: promote existing `view_mem_of_isMember` (:71) / `isMember_of_view_mem` (:81) to tabled obligations. P07: one new auxiliary theorem, type below |
| `lean/KelGroups/Mirrors.lean` | only if P01 promotion requires the companion row |

The S4-B fence is exactly `justfile`, `lean/KelGroups/Mirrors.lean`,
`lean/Reactivegas/Mirrors.lean`, `scripts/check-lean-mirrors`. **No `justfile`
recipe change is proposed.** No model or guard semantics, no `docs/en/design/`.

## P01 — reuse, not invention

Inspect and confirm that `isMember_of_view_mem` meets Obligation 2: it concludes
`KelGroups.GroupView.isMember u view = true` from `(u, v) ∈ view.members`, via
`assocLookup_some_of_mem_nodupfree`, so it is **unprovable under the audited
constant-false mutant** — the required negative case. `view_mem_of_isMember`
supplies the positive direction against canonical member data. **If confirmed,
promote both to tabled mandatory-path obligations and add no new theorem.**

If inspection shows they do not meet it, that is returned as an exact statement,
not patched around.

## P07 — exact type and dependency footprint, stated before implementation

Production body, `lean/Reactivegas/Step.lean:126-132`:

```lean
| .closePurchase c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer && col.referente == signer
      && col.permitted && col.pending.isEmpty && !(decide (stalled s)))
    …
```

**Proposed auxiliary theorem — exact type:**

```lean
theorem closePurchase_requires_permissionToClose
    (s : State) (view : KelGroups.GroupView) (signer : KelGroups.Key)
    (c : CollectionId) (s' : State)
    (h : step view s signer (.closePurchase c) auth = some s') :
    ∃ col rest, pullCollection c s.collections = some (col, rest)
              ∧ permissionToClose col
```

**Dependency footprint:** `step` (`Step.lean:126`), `pullCollection`, `demand`,
`permissionToClose` (`Mirrors.lean`), `Collection` fields `permitted`/`pending`.
It reads the **production body**, so breaking the permission atom — the audited
`closePurchase` permission-bypass mutant — makes it **fail**.

It binds the **actual selected collection** (via `pullCollection c`), not an
unrelated one, and it is **not** a copied inline guard. Satisfiable successful
inputs and the unpermitted case are supplied as the two controls below. **No
broader equivalence with all close guards is claimed or required.**

## F01 — and the R3 control replaced, because my R3 was not executable

Repair: replace both `Bool` filters in `scripts/check-lean-mirrors` (`:165`–`:169`
and `:268`) with a **total `ConstantInfo` classification** — every kind either
predicate-bearing or **explicitly excluded with a named reason** — and **fail
closed on an unclassified kind**.

**My earlier R3 ("an unrecognised/newly-shaped declaration") is withdrawn: it is
not an executable control at the current toolchain**, and a Lean constructor or
type error would not be discovery assurance. Replaced by:

- **R3′ — isolated classifier-omission mutant.** Remove one kind from the
  repaired classification's admitted set while leaving the specimen present.
  **Expected semantic diagnostic:** the mandatory path rejects with the
  *classifier's own* unclassified-kind message naming the dropped kind — **not** a
  Lean elaboration error. That is what proves fail-closed actually fires.
- **No toolchain upgrade** is proposed or required.

Accepted and carried: the source-level finding stands, and **the current
24-entry census was not shown incomplete** — it is the future-discovery guarantee
that failed.

## Owner command plan — subprocesses counted at their layer

`just lean` runs, per the recipe: dependency-direction script, inversion coverage
×2, axiom gate, mirror checker, trace agreement, `lake build` — **one substantive
invocation, seven subprocesses**. `just ci` adds toolchain contract, Haskell
build, format, hlint, corpus gate, exporter verify.

| # | class | command | establishes |
|---|---|---|---|
| O1 | substantive, cold | `nix develop --quiet -c just lean` at repair SHA | repaired classifier admits real census; mandatory path green |
| O2 | substantive | `just lean` + F01 `opaque` specimen | specimen now **discovered and rejected** |
| O3 | substantive | `just lean` + R3′ classifier-omission mutant | **fail-closed fires with the classifier's own diagnostic** |
| O4 | substantive | `just lean` + `closePurchase` permission-atom mutant | **P07 auxiliary fails** — body sensitivity |
| O5 | substantive | `just lean` + constant-false `isMember` mutant | **P01 promoted theorem fails** — body sensitivity |
| O6 | substantive, cold | `nix develop --quiet -c just ci` at final SHA | acceptance receipt |
| OT1–OT3 | targeted | compiled-identity probes: census before/after, promoted P01 identities, P07 footprint | classification evidence |

**O4 and O5 are separate invocations by necessity** — my v1 folded them into one
R4, which would have let the **first error mask the second**. Each mutant must
isolate its own target theorem, so each gets its own build.

**Owner request: 6 substantive + 3 targeted.**

## Final-candidate audit command plan — full

| # | class | purpose |
|---|---|---|
| A1 | substantive, cold | independent `just ci` at final SHA — acceptance receipt |
| A2 | substantive | re-derive F01: specimen discovered/rejected |
| A3 | substantive | re-derive R3′ fail-closed with its own diagnostic |
| A4 | substantive | re-derive P07 body sensitivity, isolated |
| A5 | substantive | re-derive P01 body sensitivity, isolated |
| A6 | substantive | reserve for one failed setup (three prior seats hit setup failures) |
| AT1–AT8 | targeted | census/identity probes, promoted-row inspection, P07 footprint, restoration checks, the 11 PARTLY rows |

Then: **all remaining original S4 and v2 obligations re-derived** — the 78 CLOSED
as *unchanged-input with explicit byte identity* where bytes truly did not move,
the 11 PARTLY re-judged. **89 is not carried as a quota**; the denominator is the
obligation set as amended.

**Auditor request: 6 substantive + 8 targeted.**

## Numeric allowance request — per party, explicit

| party | spent | **requested** | note |
|---|---|---|---|
| **owner** | 8/8 substantive, 42/60 targeted | **+6 substantive, +3 targeted** | owner has **zero** substantive left; without this the repair cannot be executed at all |
| **auditor** | 6/8 substantive, 59/60 targeted | **+6 substantive, +8 targeted** | remaining 2/1 is **not** enough for a fresh full audit; v1 made no numeric auditor request — that omission is corrected here |
| parent | no build spend | **none** | static assessment only |
| submissions | **1/2 spent** | submission **2** | no third |
| ceiling raises | one (owner 6→8) | retained | not reset |

## Preservation and bounds

Archived pre-final cache kept **distinct** from the final candidate cache; all
raw mutants and failed setup receipts preserved; both terminal auditor reports
and the 693-entry manifest stand as history.

No push, PR, merge, `#66` closure, `#68` interruption, other-slice change, or
`docs/en/design/` write. S3 static work continues independently.
