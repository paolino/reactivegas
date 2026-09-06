# Addendum 1 to `S3-SUCCESSOR-PACKET-PARENT-ASSESSMENT.md` — the gap in my own review

## What I missed

My assessment (`50d1cc9c…`) verified the packet's **data** discipline in depth:
manifest integrity, 207/207 row mappings, the 1/113/93 observation census, span
binding, the four NOTE-071 corrections, declaration sites against `3590c001`, and
the validator's eight rejection controls.

**I did not read the frozen instruments themselves.** `instruments/batch-plan.sh`,
`instruments/compare-batch.sh` and `instruments/replay-run-green.sh` are the
executable half of SS-4, and I assessed the registry that binds them without
opening them. The desk did, and found four defects. That is a gap in my review,
not a gap in the desk's.

The lesson is one I have already been taught this milestone and did not apply
here: **a check that cannot fail proves nothing** — and I verified that property
for the *validator's* controls while never asking it of the *comparator's*.

## The four, each confirmed by me at source before routing

Hashes recomputed here; all four match the desk's.

| | finding | my confirmation |
|---|---|---|
| **D-01** | `batch-plan.sh` describes the batch experiment rather than performing it | every line after `set -uo pipefail` is an `echo`, except two `sha256sum` calls — no `git apply`, no `lake build`, no timing, no restore. It is bound as the instrument for **M13, M14 and M15** |
| **D-02** | `compare-batch.sh` prints two conclusions it never computes | `SETUP-RESTORE-INCLUDED: yes` and `OBSERVATION-TARGETS-EQUAL: yes` are **string literals**; the script never reads `.exit` contents, never validates the inventory, never checks observation identities |
| **D-03** | 26 rows ≠ 26 invocations | M13 bundles cold+build+restore; M15 bundles cold + **2** builds + **2** restores — its own protocol text enumerates five |
| **D-04** | `M11A` and `M11B` bind one script that runs both phases | `replay-run-green.sh` takes no phase argument and performs write **and** check in one invocation, writing both receipt sets. Row-by-row scheduling would double-run it and overwrite its own receipts |

On D-04, in fairness: the script separates the prebuilt artifact from its build,
is not `#eval`, and carries three real guards (`exit 91`, `exit 92` on the missing
live-bound `corpus-check: ntraces=` line, `exit 93`).

## Effect on my findings and disposition

**A-01, A-02 and A-03 stand** as written. **D-01 through D-04 are added**, routed
to the independent reviewer `%581` as additional input under its unchanged mandate
— stated timing, no forced severity, no directed verdict, original inputs
untouched.

D-01 and D-03 bear directly on the commission's central question. NOTE-071 asked
for *"the executable static consistency instrument and precise operation
registry, not another narrative proposal"*, and for measurement plans that are
*"real frozen source/argv/input plans"* rather than *"a count inferred from
shape"*. If no bound executable implements M13/M14/M15, the SS-4 batch deliverable
is **missing**; and the registry's `count: 26` is a row count, not the enumerated
invocation count that a later numeric grant requires.

**Nothing changes in the disposition:** S3 is not closed, no Phase 2, no execution
ceiling, no number inferred from this packet. Whether these are blocking is
`%581`'s to decide, not mine and not the desk's.
