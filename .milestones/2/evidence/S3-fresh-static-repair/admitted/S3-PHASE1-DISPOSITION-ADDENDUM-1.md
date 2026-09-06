# S3 disposition — addendum 1: mechanical verification of OPMAP-v7, and one new finding

Owner `%503`. Static only; no execution. Continues
`S3-PHASE1-PARENT-DISPOSITION.md` (`57506531…`) against
`OPMAP-v7-requirement-verdict-grounds.txt` (`89337291…`). `%558` is terminal and
was **not** woken for this.

## 1. Verdict counts — verified, all seven exact

I said in the disposition that the counts were unaudited by me. They are now
audited, mechanically:

| verdict | claimed | counted |
|---|---|---|
| KILL | 75 | **75** |
| ELAB-STATIC | 60 | **60** |
| OPEN-KILL | 31 | **31** |
| OBSERVED | 29 | **29** |
| RECOVERED | 9 | **9** |
| STATIC | 2 | **2** |
| ACCEPT | 1 | **1** |

Sum 207 = the file's 207 lines, with no header row inflating it. Two structural
claims also hold exactly: **every KILL row carries a `GROUND:` field** (0 without)
and **every OBSERVED row names an upstream** (0 without). Those were the central
methodological promises of the correction and they are kept.

## 2. NEW FINDING — the requirement set is 160 distinct tokens, not 158

`OPMAP-v7` contains **160** distinct requirement tokens. Compared against
`RELATION-v2`'s authored column (158, excluding the two explicit `OPEN-R` rows),
nothing is missing and **two are extra**:

```
KelGroups.base_change_recomputes_votes
KelGroups.baseHook_votes
```

Both name **real theorems** — but not at that namespace. `Reactivegas/Invariants.lean`
opens `namespace Reactivegas` at `:1297`, and both sit inside it:

```
:1600  theorem baseHook_votes …
:1616  theorem base_change_recomputes_votes …
```

There is exactly one definition of each in the tree. So the correct full names are
`Reactivegas.baseHook_votes` and `Reactivegas.base_change_recomputes_votes` — and
those **already appear**, at `OPMAP-v7:97-98` under `OP-63`. The `KelGroups.`
spellings at `:89-90` under `OP-59` are the same two theorems misnamespaced.

Three consequences:

1. **The authored set really is 158.** The 160 is inflated by two misnamespaced
   duplicates, not by two extra identities.
2. **"Machine-audited: 158/158 authored present, 0 helpers" masked this.** It
   verified *presence*, not that every token names a real identity *at its stated
   namespace*. A token can be present, plain-looking, and still not exist. That is
   the same shape as the errors this map has been correcting: a check that passes
   without being able to fail in the direction that matters.
3. **Two of the 75 KILL rows are duplicates**, so the distinct theorem/mutant KILL
   count is **73**.

And a question the map must answer rather than one I will force: `OP-59:89-90` and
`OP-63:97-98` cite an **identical mutant** (`Step.lean:baseHook:sweepClosures-θ-post→θ-pre`)
and an **identical ground** (`(a) votes projection (sweep-post)`) for the **same two
theorems**. Either `OP-59` and `OP-63` are one operation counted twice — in which
case the Step-closure class in the envelope double-counts — or the map does not say
how they differ. Both appear in that class's 20-op list.

## 3. Effect on my disposition

**F-06 stays PARTLY** and this is now a named reason, not only the funding one: the
map's own completeness statistic is inflated by two rows. **F-03 stays PARTLY.** No
finding moves, and nothing closes on this addendum.

**Not a semantic finding against the Lean.** Both theorems exist and are proved; this
is a defect in the map's namespacing and in the audit that certified it.

## 4. Coverage now, stated exactly

Audited by me: the seven verdict counts, the line total, the presence of `GROUND:`
on every KILL, the presence of an upstream on every OBSERVED, the full
requirement-token set against RELATION-v2's authored column, and the source
namespace of the two extras — plus the six rows in the base disposition.

Still unaudited by me: the **content** of the 75 KILL grounds beyond those six, the
31 OPEN-KILL bounded searches, whether each OBSERVED upstream is a genuine consumer,
and the closure multipliers behind the 143+1 envelope. Those are not accepted here.

The smallest thing that would settle the remainder is a per-row source read of the
73 distinct KILL grounds and the 29 OBSERVED upstream links — static, no execution —
which is the same method used above and needs no grant, only time.
