# NOTE-024 — OT4 retry GRANTED; census result-contract defect blocks it; sheet edits then I bind

Not yet bound. Read this in full. No run until I return a binding line.

## 0. What I verified MYSELF, at source, before writing anything below

I did not relay the desk. Every claim here I recomputed or read:

- `sha256sum` on the files: `S2-chain-P01.lean` = `7bc5c01f971ece0df537156a1e405a6bde42c77c481d12df39c7ec14d8d079e0`,
  `S2-chain-P07.lean` = `9dab73e2a543ab2dcd3e1356debb34cf68d3a089635a8a601b93463da11962eb`.
  **The sheet's mapping at lines 79 and 105 is CORRECT.**
- `diff -u` readback `ab3dd269…` -> current `c69988c3…` on `S2-census.lean`: the
  ENTIRE delta is two hunks and exactly three changed lines — `let mut failed`
  -> `IO.mkRef false`, `failed := true` -> `failedRef.set true`, and the added
  `let failed ← failedRef.get`. Nothing else. This explains the byte difference
  between those two identified files; it does not reconstruct every intervening
  edit, and I do not claim it does.
- `git log`/`git status` in `/code/reactivegas-66-s4b`: HEAD is
  `b667648752b8fa8a7b890f115413a99ba04518dc` on `chore/66-s4b-mirrors`, tree clean.
- Import closure of both neg runs, by reading the drivers and the sources:
  `S2-chain-P01.lean` imports **only** `KelGroups.Types`, and
  `lean/KelGroups/Types.lean` has **zero** import lines. So `P01WORLD` holding
  only `KelGroups/Types.olean` resolves the whole closure: SH-P01neg's exit≠0
  cannot be a module-not-found artefact. `S2-chain-P07.lean` imports
  `KelGroups.Types`, `Reactivegas.State`, `Reactivegas.Step`; `P07WORLD` supplies
  Step and `<lib>` supplies the rest. **Neither neg control is masked.** I checked
  this rather than accept §0's assertion.

## 1. Credit, specifically

The corrected sheet fixes what NOTE-023 withdrew the binding over, and fixes it
properly rather than cosmetically: `P01WORLD`/`P07WORLD` are separate directories;
`rm -rf <WORLD> && mkdir -p <WORLD>/…` is INSIDE the counted compile argv, so stale
exclusion is by construction and visible in the receipt — you did not cite `mkdir -p`
as an empty world; §3 is one executable order; §4 is one cost table; the digests are
full 64-char and per file.

## 2. My error, owned

I relayed the desk's "driver digests are swapped in the sheet" without computing
them. The sheet was right. What was reversed was the consolidated return. Your
journal already records this correctly. It cost nothing only because the instruction
attached to it was "resolve by file" and you did exactly that — the relay was still
mine, not yours.

## 3. GRANTED — targeted +1 for OT4retry

Phase allocation 8 -> 9. 42 historical + 9 = **51**. Hard ceiling **60** unchanged.
Spent **44**, remaining **7** = OT4retry + the six shadow operations. **Exact fit,
gap NONE.** Substantive **9/16 with 7 remaining**, unchanged. Submission 2/2.
Auditor 15/69. No other automatic raise and no other automatic retry.
OT3's success is preserved and is NOT rerun; the OT4 failure stays counted as a
failed attempt, not a free setup.

## 4. BLOCKING before OT4retry — the census does not keep its own stated contract

I traced the whole result-flag path in `S2-census.lean` at the current bytes:

- `:94` pushes an elaboration failure into `sortUndecided`.
- `:109` only `logInfo`s that bucket.
- `:117-120` compare `oldSorted` against `newSorted`. An undecided **owned**
  identity is absent from BOTH — `:62`'s `| _ => pure ()` swallows `none` on the
  old side too — so neither `S2-CENSUS-REGRESSION` nor `S2-CENSUS-DELTA` fires.
- `:121-123` assert `unclassified`, `opaque-pred`, `thm-excluded`. None covers the bucket.
- `:124` reads the ref, `:126` prints `S2-CENSUS-OK`.

So an undecided identity is **indistinguishable from success**, while the file's own
header at `:15-17` promises the failures "land in their OWN named bucket" and that
"S2-CENSUS-OK is printed ONLY with zero errors". The code does not keep that promise.
This is pre-existing — the bucket was log-only before the IO.Ref repair too — so it is
a defect exposed by review, not a regression your repair introduced.

Repair, inside the instrument's existing scope:

- Keep `sortUndecided` as its own bucket. Do **not** reclassify undecided as
  non-predicate; an error is not a classification, exactly as your header says.
- Add the assertion **alongside `:121-123` and BEFORE `:124`'s `let failed ← failedRef.get`**.
  Placement is load-bearing: after `:124` the OK/FAILED summary still reads the stale value.
- Route it through `fail`, not through the ref directly. **The mechanism that makes
  `lean` exit nonzero is the `logError` inside `fail`; the ref only selects the
  OK/FAILED summary line.** A repair that sets the ref without going through `fail`
  leaves exit 0 — that is the "a renamed log is not enforcement" trap.
- Name the identities in the message; they are already printed by `:109`.

## 5. The gap I am naming rather than papering over

After this repair the `sortUndecided` -> `fail` path is still **not executed** by
OT4retry: on the clean tree that bucket is expected empty, and no operation is
granted to seed an undecided identity. So this is a **source-verified contract
repair, not executed enforcement** — the same status as the existing expected-zero
assertions at `:121-123`. Write it that way in the receipt. Do not claim executed
coverage, and do not claim a surviving identity from a static observation.

## 6. Sheet edits required before I bind

1. `:11` "repair commits through `4d0a324`" -> `b667648` (verified HEAD, tree clean).
2. §1 `:32-34`: DELETE the obsolete shared-world line
   `mkdir -p /tmp/s2shadow/KelGroups /tmp/s2shadow/Reactivegas`. It contradicts §0
   and the actual argv at `:67`/`:96`, and a reader binding §1 rebuilds the very
   collision that cost the last binding.
3. `:96`: the P07compile argv ends with a doubled backtick. Fix it — I am binding
   literal argv, so the literal has to be the literal.
4. `:57`: replace "a prior `ab3dd269…` hash predates an unexplained pre-handoff
   drift" with the verified fact — the delta is exactly the IO.Ref repair, per the
   diff above — then re-anchor to the NEW census hash after §4's repair.
5. §2 header `:46` "(8; exact fit for the +8 authorization)" -> 9 / +9.
6. §4 targeted row: spent 44/60, allowance 51-44 = **7**, need 7, **gap NONE**.
7. Regenerate `HASH-MANIFEST-BINDING.txt` for the new census hash. It must not
   contain a self-entry — an entry that can never verify is worse than none.

## 7. Then

Return the corrected sheet and its `sha256`. I read it and hash-bind it. After that
binding line you run the **entire** authorized sequence — OT4retry -> SH-P01{compile,neg,pos}
-> SH-P07{compile,neg,pos} -> O-phase -> validation -> submission -> fresh full audit
-> handback — **with no further checkpoint from me**. Do not wait on me to re-approve
anything already settled here.

Avoid wholesale recomputation of valid oleans; retain O1's module evidence honestly
at its proven scope and never as a successful whole O1. A genuine unexpected failure
returns its concrete cost before more execution — it does not silently consume a
required negative control or the final CI. No production widening. No merge, no push,
no PR, no comment.
