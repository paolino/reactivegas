# NOTE-026 — BINDING LINE. Execute the full sequence now. Do not wait for me again.

## BOUND

I read the returned delta in full and recomputed every hash myself. **Bound.**

Execution set — these bytes are what runs, bound at exact identity:

```
f4d4b64ada9b58dde91a2600d30a5aea2f5efb1098ba158b809f39d79c35e062  S2-census.lean
b64a4cabaa8630b99761a191ad11884fffcb4029bc961cdd3b2cc91fd59dbcba  S2-chain-P01.lean
075f6f22e9c920615068e452058dc4df69c4cb73db3cfab9aa15e64251559253  S2-chain-P07.lean
531eb3e919ae02be00df3a6dbd3e6619648b9fa75125f7c50e901503de676274  S2-witness-close.lean
85e250ac574f7795596bb86bf481737244bc3111ed34c79f43b026c61ecb686e  S2-mut-isMember-false.diff
95edbfa32c81ac7a9c3b438e99cf3471646c7d7753e04be1543334abbc0616dc  S2-mut-close-perm.diff
e16b035b03181437b174fbecb125e92337515164c52af63eb60dedc1ecd28fa3  S2-mut-classifier-omit.diff
b3fc68ae43afc4b47d1ec0e22a32e4b8945edc76c005bbb913df0d3187cabaed  S2-mut-opaque.diff
```

Procedure — argv, cwds, search paths, shadow worlds, order, receipt paths and
the cost table — bound as stated in `S2-shadow-resolved-commands.md`
`f2f30a6efc93c4b14506af842adefaf28b5fcc6ad6b6dd6e0dc65f93630b76e6`, whose
manifest `b633fe48f44478de461c62206de231ea333b15bd4bcbcf13b319503e4bae2fc0`
I ran `sha256sum -c` against: 15/15 OK, no self-entry.

### What I verified, not accepted

- All four returned hashes recomputed here and matching.
- The driver corrections are **mechanically comment-only**: I diffed both
  against the retained prior copies and counted **zero** changed non-comment
  lines in each. Elaboration, statements and neg/pos expectations are
  untouched — SH-P01neg still expects errors exactly at `view_mem_of_isMember`
  and `isMember_of_view_mem`, SH-P07neg exactly at `step_close_inv`.
- The new header text states the measured mechanism correctly: project first,
  toolchain second, inherited `LEAN_PATH` last, so the clean project olean wins
  under `lake env` and shadow-first requires explicit `LEAN_PATH`.
- The sheet delta is exactly the two edits asked for and nothing else: the
  OT4retry retitle with `S2-OT4retry.log`, and §4's prose now reading gap NONE.

## One record defect — corrected WITHOUT waiting for me

Sheet lines **83** and **109** still cite the pre-correction driver digests
`7bc5c01f…` and `9dab73e2…` as the SH-P01neg / SH-P07neg inputs. The files now
hash `b64a4cab…` and `075f6f22…`. The manifest is right; those two inline
citations contradict it.

This is a record correction only — it touches no argv, no expectation, no cost.
Fix both lines, regenerate the manifest, journal the two new hashes.

**It does not void this binding, and you do not stop for a new binding line.**
That is deliberate: I bound the execution set by its own hashes rather than by
the sheet's citations of them, precisely so a record fix cannot block execution.
The manifest is authoritative for file identity; the sheet is authoritative for
procedure.

## Execute

Run the whole authorized sequence now, with no further checkpoint from me:

```
OT4retry → SH-P01compile → SH-P01neg → SH-P01pos
         → SH-P07compile → SH-P07neg → SH-P07pos
         → O1retry → O2 → O3 → O4 → O5 → noop → O6
         → validation → submission → fresh full audit → handback
```

Budgets as bound: targeted 44 spent of 51 allocated, 7 remaining, exact fit,
ceiling 60. Substantive 9 of 16, 7 remaining, exact fit. Submission 2/2.
Auditor 15/69.

Standing conditions, unchanged:

- `S2-OT4.log` is the counted failure and is never overwritten.
- Every original required control and the final clean CI still run. A source
  repair is not their success.
- The census `sortUndecided → fail` path stays recorded as source-verified,
  **not** executed — the bucket is expected empty on a clean tree and nothing
  is granted to seed one. Do not claim executed coverage or a surviving identity.
- O1's module builds are retained at their proven scope, never as a whole-O1 success.
- A genuine unexpected failure returns its concrete cost before further
  execution and never silently consumes a required negative control or the final CI.
- No production widening. No merge, no push, no PR, no comment.

Hand back on your own journal with the terminal result. If you hit a real
blocker, name it and stop there — do not narrow the obligations to fit.
