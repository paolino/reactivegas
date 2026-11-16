# NOTE-029 — P07 kill accepted. One named evidence gap. Run the O-phase now.

## The kill is real — verified in the raw receipt, not from your summary

`S2-SH-P07neg-retry.log` is a single error at `S2-chain-P07.lean:86:48`, inside
`step_close_inv`. It is a proof-obligation failure, not import, setup or syntax.

The strongest part is one you understated. The diagnostic **authenticates the
loaded olean by itself**:

```
has type     demand (… && col.referente == a && true && col.pending.isEmpty && …)
is expected  demand (… && col.referente == a && col.permitted && … )
```

`col.permitted` has become `true` in the environment's `Step`. That is the mutant
speaking in the compiler's own words — independent of exit codes and of your
claim about which world was on the path. Exactly one error, so nothing unrelated
masks it. `S2-SH-P07neg.log` is unedited, the new receipt path was used, and both
shadow worlds and every mutant are retained. Targeted 52/60, allocation exact,
phase closed. Accepted.

## One gap, named rather than smoothed

**No overlay verification receipt was retained.** The overlay directory is gone
and "28 manifest + mutant Step + clean Types + driver hash" exists only as a
journal sentence. So the control splits in two:

- *The mutant Step was loaded* — **authenticated by the diagnostic itself.** Solid.
- *Every other resolved byte was clean* — **rests on a check whose output was not
  kept.** Note precisely why it matters: the quoted type mentions `isResponsabile`,
  `col.permitted` and `stalled`, but never `isMember`, so this diagnostic would
  look identical whether the overlay's `KelGroups/Types.olean` was clean or the
  P01 mutant. Nothing in the retained evidence excludes that.

Do **not** spend anything to repair it. Rebuilding the overlay now would verify
today's bytes, not the ones that ran — that is not the same claim. Record it as a
named limitation of this control and carry it into the submission as such. Going
forward, when you construct a world, retain the `sha256sum -c` output as a
receipt; a constructed world without its check is a declaration.

I hold my share of this: NOTE-028 told you to verify every overlay byte and did
not tell you to retain the proof of it.

## Run the O-phase — no new grant is needed and none is coming

You stopped to report. The O-phase was released at NOTE-026, restated at
NOTE-027, and is funded and bound: substantive **9/16, 7 remaining** —
`O1retry` (fresh `.lake`), `O2`, `O3`, `O4`, `O5`, `noop`, `O6` — then
validation, submission, fresh full audit, handback.

Targeted is exhausted at 52/60 and **no further targeted operation is
authorized**; if something in the O-phase appears to need one, that is a
concrete gap to report, not a spend.

Standing conditions unchanged: `#print axioms` on a genuinely clean build, every
original control and the final clean CI intact, the census `sortUndecided` path
still recorded as source-verified and not executed, O1's module builds at proven
scope only, no production widening, no merge, no push, no PR, no comment.

Hand back on your journal with the terminal result. Do not stop again to report
progress.
