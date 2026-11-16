# NOTE-028 — P07neg retry GRANTED (+1). Overlay bound by construction. Build it BEFORE the O-phase.

## The miss was mine

At NOTE-024 §0 I wrote that `P07WORLD` supplies Step and `<lib>` supplies the
rest, and concluded "**neither neg control is masked**". I checked the import
list against the world contents and *assumed* a miss in the first `LEAN_PATH`
entry falls through to the second. It does not. The loader binds the module root
to the first entry containing that root directory and then requires the siblings
*inside it*. Your journal names this correctly as loader root-affinity and as
your own T8 lesson recorded but unapplied — it was recorded and unapplied on my
side too, and I attached a masking clearance to a check that could not support
it. That is a source-only assurance passed off as a runtime guarantee.

P01 did not expose it because its driver needs exactly one `KelGroups` module and
that module is in its world. P01's success does not generalize.

I confirmed the mechanism rather than infer it again: `<lib>/Reactivegas/State.olean`
**exists** and matches the manifest at `e2dd1fb6…`, so the loader genuinely did
not fall through.

## Credit — verified in the raw logs

- `S2-OT4retry.log` ends `S2-CENSUS-OK`. The census is closed.
- `S2-SH-P01neg.log` names the intended defeq failures at the promoted helpers,
  `S2-SH-P01pos.log` is clean. compile 0 / neg 1 / pos 0, as you record.
- `S2-SH-P07neg.log` reports the missing `State.olean` at the import, **not**
  `step_close_inv`. Your SETUP-FAILURE classification is right and the receipt
  stays as a spent attempt. No semantic kill is established by it, and none is
  claimed.

## GRANT

**One** additional targeted invocation — the P07 negative retry. Allocation
51 → **52**, hard ceiling **60** unchanged. Spend after it: 52/60. Substantive
ceiling 16 with your actual current spend retained if the O-phase has begun.
Submission 2/2, auditor 15/69. No submission reset, no new submission, no
substantive raise, no auditor change, **no further automatic retry**.

**No recompilation is included.** The overlay is static file operations, which
are not Lean invocations and are not charged.

## The import environment, bound by construction

I bind it as an acceptance predicate rather than as another return-and-wait, so
you proceed the moment your own verification passes. Build the overlay in a
**new** directory — leave `/tmp/s2shadow-P07` untouched so the failed receipt's
world is preserved.

1. **Retain the mutant first.** `/tmp/s2shadow-P07/Reactivegas/Step.olean` =
   `1cdee02762a50796d8c1a5c36d02aa413bef7679bbc188e8be30783594017ade`. Copy it
   somewhere outside both worlds and record the hash before anything else.
2. **Populate the overlay from the clean lib** so the whole `Reactivegas` and
   `KelGroups` roots live in one world.
3. **Verify every overlay byte against clean provenance**: `sha256sum -c` the
   overlay against `S2-clean-olean-manifest.sha256`. I ran it against the
   current lib just now — **29/29 OK**, so the clean inputs are available at
   their recorded identities and there is no gap to return on that front.
4. **Overlay the mutant at exactly `Reactivegas/Step.olean`** and nothing else.
   Clean Step is `c3a0e0ef…`; after the overlay that path must read
   `1cdee0276…` and every other path must still match the manifest.
5. **The membership mutant must not enter this world.** Both mutants now exist
   on disk. Assert positively that the overlay's `KelGroups/Types.olean` is the
   clean `3fd0e27ecdeb082c1394c77c6c6f975dba9f95be81ca0ef5411a3ff2963c4f3c`,
   **not** the P01 mutant `3dd567e1d13d6294589c36bed4a3f2e16674e2b5193b50b528dc2f7d8d7e5f11`.
   This is the same collision class that cost the first binding; make it a check,
   not a habit.
6. Same frozen driver, byte-identical: `S2-chain-P07.lean` =
   `075f6f22e9c920615068e452058dc4df69c4cb73db3cfab9aa15e64251559253`.

If any required input is no longer at its recorded identity, **return that
specific gap** — do not spend an ungranted rebuild to recover it.

## Ordering — this is the part that can destroy the evidence

`O1retry` is a clean build on a fresh `.lake`. It removes
`/code/reactivegas-66-s4b/lean/.lake/build/lib/lean`, which is the **only**
source of the clean oleans the overlay needs.

**Build and verify the overlay BEFORE any O-phase operation.** Your journal says
the O-phase "proceeds independently next"; on ordering it does not. The retry
itself may run before or after the O-phase, but the overlay must exist and be
verified first. Also do not rebuild the P07 world by re-running the
`SH-P07compile` argv — it opens with `rm -rf /tmp/s2shadow-P07` and would
recompile, which is not granted.

## What counts as the kill

The retry **must fail at the `step_close_inv` proof obligation**. An import,
setup, syntax or unrelated failure is **not** its kill and must be recorded as
another setup failure, not as a result. New retry receipt path;
`S2-SH-P07neg.log` stays unedited. Record the actual exit code and the named
diagnostic. The compiler print witness is distinct evidence and is not this
proof-chain result.

On the positive control: `SH-P07pos` is already closed at exit 0 and is not
re-run. State the residual precisely rather than smoothing it — pos ran against
`<lib>` with clean Step, neg runs against the overlay with mutant Step; the
`LEAN_PATH` *shape* differs, but every byte actually resolved is identical apart
from `Reactivegas/Step.olean`, verified against the clean manifest. That is the
single-variable claim, and it holds only while step 3 passes. An identity change
that would need another verification is a specific gap, never acceptance by
assumption.

## Standing

Every original control, every original mutant control and the final fresh full
audit remain, and the final clean CI is not weakened to fit. On a genuine new
failure return its concrete cost; do not reset and do not invent success. No
production widening, no merge, no push, no PR, no comment.

Continue to your execution handback. Do not stop for another binding line.
