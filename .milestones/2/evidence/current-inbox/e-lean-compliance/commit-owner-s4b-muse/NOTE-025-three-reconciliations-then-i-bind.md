# NOTE-025 — three reconciliations, then I bind. Not bound yet.

Your return is received and read in full. `S2-shadow-resolved-commands.md` =
`7c936a22402a7205f7c613049dcc03a0b24ad1e3bfd80866751b81456eb62a09`, recomputed
here, matches your journal. This is one short round, not a new checkpoint: fix
these three, return the four hashes, and the binding line follows immediately.

## Credited, verified at source

The census repair is correct and correctly placed: `:124`
`if !sortUndecided.isEmpty then fail …` sits with the other assertions and
**before** `:125`'s `let failed ← failedRef.get`, and it routes through `fail`,
so it both logErrors (non-zero exit) and flips the summary to FAILED. Census is
`f4d4b64ada9b58dde91a2600d30a5aea2f5efb1098ba158b809f39d79c35e062`. You also
recorded its status honestly as source-verified-not-executed. All seven sheet
edits are in place — `b667648`, the obsolete shared-world `mkdir` line gone,
the doubled backtick gone, §2 at 9/+9, §4's row at exact fit, manifest
regenerated with no self-entry.

## 1. §4's prose contradicts §4's own table

The table row now reads `51-44 = 7 (+1 granted NOTE-024) | NONE (exact fit)`.
The paragraph below it still reads "The single open gap is +1 targeted for the
OT4-retry necessitated by the counted OT4 setup failure." That gap is closed.
A sheet that states its own resolved gap as open is not one cost table.

## 2. Both chain-driver headers state the search order backwards

`S2-chain-P01.lean:7-8` and `S2-chain-P07.lean:8-9` both say: "measured
`lake env` appends its paths LAST, which would let the clean olean shadow the
mutant silently."

The measured receipt `S2-lean-env-search-order.receipt.txt` is:

```
/code/reactivegas-66-s4b/lean/.lake/build/lib/lean:/nix/store/…-lean4-4.25.0/lib/lean:/tmp/PROBE_MARKER
```

Project **first**, toolchain second, the inherited `LEAN_PATH` marker **last**.
The sheet's §0 says this correctly; the two driver headers say the opposite.

And the headers' stated consequence does not follow from their own premise: if
`lake env` really appended its paths last, the inherited shadow entry would come
*first* and would **win**, not lose. The real reason direct `lean` is required is
the measured one — project paths come first, so a shadow entry can never win
under `lake env`. Same conclusion, opposite mechanism. Write the measured
mechanism.

This is **comment-only**: no elaboration changes, no statement changes, the
neg/pos expectations are untouched. It still changes both driver digests, so
lines 79 and 105 and `HASH-MANIFEST-BINDING.txt` must move with it. Precedent is
the S2R `check-lean-axioms` header defect, which landed comment-only for exactly
this reason. Distinguish the two in your journal: explanatory correction, not an
executable change.

## 3. OT4retry would overwrite the counted failed attempt

§2's OT4 entry still gives `handoffs/evidence/S2-OT4.log`. Those are the retained
bytes of the **counted** OT4 failure. Running the retry to that same path
destroys the evidence that the failure was charged. Name the retry receipt
`handoffs/evidence/S2-OT4retry.log`, and retitle the section `OT4retry` so §2
and §3's order agree. This is evidence retention, not a new validation demand —
the failed attempt stays counted at 44 either way.

## Then

Return `S2-shadow-resolved-commands.md`, `S2-chain-P01.lean`, `S2-chain-P07.lean`
and `HASH-MANIFEST-BINDING.txt` with their four `sha256` values. I bind those
exact bytes and return the binding line. After it you run the **entire**
authorized sequence — OT4retry -> SH-P01{compile,neg,pos} -> SH-P07{compile,neg,pos}
-> O-phase -> validation -> submission -> fresh full audit -> handback — with no
further checkpoint from me.

All original required controls and the final clean CI remain. A source repair is
not their success. A genuine unexpected failure returns its concrete cost before
more execution and never silently consumes a required negative control or the
final CI. No production widening. No merge, no push, no PR, no comment.
