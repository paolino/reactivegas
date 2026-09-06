# S3 disposition — addendum 3: Addendum 2's central Group B claim is WRONG. Correction.

Owner `%503`. Append-only correction; Addendum 2 (`efce7b18…`) is **preserved
unchanged** as history. Static only, no execution, no author wake.

## What I got wrong

Addendum 2 §2 claimed **9 spurious pairs** and concluded the map's 160
requirement tokens correspond to **151 distinct declarations**, and that four
OBSERVED rows were **aliases, not cascades**.

**Seven of those nine are actual distinct declarations, the 151 is wrong, and the
four are genuine cascades.** The corrected numbers:

| claim in Addendum 2 | corrected |
|---|---|
| 9 spurious pairs | **2** |
| 151 distinct declarations | **158** |
| 4 OBSERVED aliases, not cascades | **0** — all four are genuine cascades |
| OBSERVED verified | **28** of 29 (was 24) |

**158 matches the map's own claimed authored count.** On this point the map's
statistic was right and my correction was the error.

## The cause of the error, determined and not guessed

My instrument was:

```sh
grep -rc "theorem $b\b" lean/ | grep -v ':0$' | wc -l
```

`grep -rc` reports **one line per file with a count**; piping through `wc -l`
therefore counts **files containing a match, never declaration sites**. Two
declarations in one file collapse to 1. Demonstrated directly:

```
old instrument (files with a match)  approvals_nodup -> 1
correct (declaration sites)          approvals_nodup -> 2
```

So I stated the right discriminator in Addendum 2 — *how many declarations exist,
never how many spellings* — and then measured files. That is precisely the defect
class I have spent this assessment finding in the map: **a check that reports
something adjacent to what it claims.** Mine was worse than the map's, because I
had already written the correct rule one paragraph above the wrong measurement.

## The seven are root-namespace wrappers, verified at source

`lean/KelGroups/Invariants.lean` (`86f200cb8dccd63d5d14a362e46286b7781040e2df1b214baedfb23e065a88e2`,
identical in both trees) closes `end KelGroups` at **:872**. Each of the seven has
one declaration inside the namespace and a second **after** it:

| theorem | in-namespace | root wrapper |
|---|---|---|
| `approvals_nodup` | 312 | **877** |
| `proposer_mem_approvals` | 317 | **883** |
| `enact_implies_threshold_met` | 342 | **889** |
| `member_key_coherent` | 374 | **909** |
| `members_change_implies_enacted` | 379 | **899** |
| `majority_table` | 450 | **914** |
| `majority_not_strict_on_even` | 459 | **923** |

Their shape, and the file's own comment at :875:

```lean
-- "Keep root theorem aliases while the portable implementation remains in
--  the `KelGroups` namespace."
theorem majority_table : <statement restated over KelGroups.majority> :=
  KelGroups.majority_table
theorem approvals_nodup {…} (h : KelGroups.WellFormed gs) … :=
  KelGroups.approvals_nodup h entry hentry
```

**Two things are true at once and I collapsed them.** They are *distinct Lean
declarations* — the identity question — **and** their proof term is an
application of the counterpart — the cascade question. The source comment's word
"alias" describes their **role**, not their **identity**. Asserting "there is no
sibling; there is one theorem" was false on the identity half.

So `OP-64a`, `OP-64c` and both `OP-67a` rows are **correct as OBSERVED**: a root
wrapper whose proof term is `KelGroups.foo` genuinely consumes another
declaration and genuinely fails when it fails.

## What survives from Addenda 1 and 2

- **The two misnamespaced rows stand**, re-checked with the corrected instrument:
  `baseHook_votes` and `base_change_recomputes_votes` each have **exactly one**
  declaration site (`Reactivegas/Invariants.lean:1600`, `:1616`) yet appear under
  both `KelGroups.` and `Reactivegas.` spellings. Distinct KILL rows remain **73**.
- **Group A's 8 production/`TraceTests` pairs** are genuine two-declaration pairs.
  Unaffected.
- **The whole OBSERVED class was examined, 29/29** — corrected outcome **28
  verified cascades, 1 underspecified citation**.
- **KILL structure**: all 75 rows carry a well-formed ground — 54 (a), 6 (c),
  15 (P), 0 (b). All 15 (P) rows name a specific failing-obligation shape; two
  sampled against source and accurate.

## The one real OBSERVED weakness, now with its answer

`OP-62 KelGroups.enact_implies_threshold_met` cites "upstream **threshold
lemma**" — a lemma class, not a row, while the map's own promise is to name the
upstream **row**. That is a repairable citation defect and **not** a claim that no
sibling exists. The actual sibling is
**`tryEnactDetailed_enactment_threshold_met`**, consumed at `:357` and `:366` in
both base arms of the proof at `:342`. The repair is to cite it.

## Effect on the disposition

**F-06 loses one of its three named reasons.** The requirement basis is inflated
by **2** duplicate spellings, not 9, and 158 distinct declarations is consistent
with the authored count. F-06 stays **PARTLY** on its remaining grounds — the map
is finite and verdict-tagged, but unfunded and not authority, and the OP-62
citation defect stands. **F-03 stays PARTLY.** Nothing closes.

Still not a semantic finding against the Lean: every theorem named exists and is
proved.

## Method change I am adopting

Namespace-aware identity, measured by **declaration sites**, never by files and
never by spellings — `grep -rn … | wc -l`, not `grep -rc … | wc -l`. Root
wrappers and production/test pairs are preserved as distinct obligations. Every
count in the remaining review is produced by the corrected instrument.
