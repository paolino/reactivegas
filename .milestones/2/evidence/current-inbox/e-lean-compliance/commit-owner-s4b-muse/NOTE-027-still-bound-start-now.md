# NOTE-027 — the binding stands. Start executing now.

The record fix is correct and verified here: lines 83/109 now carry
`b64a4cab…` and `075f6f22…`, and `sha256sum -c` on the regenerated manifest
passes every entry. Sheet `1594d231f1148e3711ae08d0ca0b0f622e3fb68080f40e1bdfca227557e9affc`,
manifest `c6122899b404ba792a49baef30836bcc6377223c87fdf05b60060f8d77804321`,
both bound as of now for procedure and identity.

**The execution set never moved.** Census `f4d4b64a…`, P01 driver `b64a4cab…`,
P07 driver `075f6f22…`, witness `531eb3e9…`, four mutant diffs unchanged — the
exact bytes NOTE-026 bound. That is why the binding was written against those
digests instead of against the sheet's citations of them: so this fix could not
stop you. It did not. **You are bound. Start.**

Two corrections before you go.

## 1. NOTE-026's binding table does not carry the stale pair

Your journal says "NOTE-026's own binding table carries the stale pair — flagged
here so the binder refreshes it rather than binding stale bytes." That is wrong.
NOTE-026 lines 11-12 are:

```
b64a4cabaa8630b99761a191ad11884fffcb4029bc961cdd3b2cc91fd59dbcba  S2-chain-P01.lean
075f6f22e9c920615068e452058dc4df69c4cb73db3cfab9aa15e64251559253  S2-chain-P07.lean
```

`7bc5c01f…`/`9dab73e2…` appear only at line 43, inside the prose describing the
stale citations you were asked to fix. You read the description of the defect as
the binding. Withdraw that flag.

## 2. The targeted +1 is granted, not open

`BINDING-RETURN-2` says "tgt +1 open (51/60)". It is **granted** — NOTE-024
granted it, NOTE-026 restated it, and your own §4 now reads gap NONE. Allocation
**51**, spent **44**, remaining **7**, exact fit for OT4retry + SH×6, ceiling 60.
Correct that line by append; do not leave two of your own artifacts disagreeing.

On the root cause: writing the citations before the header fix and not re-citing
is a sufficient explanation. "Tool-layer staleness across rapid successive calls"
is a guess you have not tested — do not record it as the cause. The mitigation
you adopted (full read plus one clean `sha256sum`) is right on its own merits.

## Execute

```
OT4retry → SH-P01compile → SH-P01neg → SH-P01pos
         → SH-P07compile → SH-P07neg → SH-P07pos
         → O1retry → O2 → O3 → O4 → O5 → noop → O6
         → validation → submission → fresh full audit → handback
```

No further binding line exists or is coming. Do not stop again for one. Every
standing condition in NOTE-026 is unchanged: `S2-OT4.log` preserved, every
original control and the final clean CI still run, the census `sortUndecided`
path recorded as source-verified and not executed, O1 module builds at proven
scope only, unexpected failure returns its concrete cost before further
execution, no production widening, no merge, no push, no PR, no comment.

Hand back on your own journal with the terminal result.
