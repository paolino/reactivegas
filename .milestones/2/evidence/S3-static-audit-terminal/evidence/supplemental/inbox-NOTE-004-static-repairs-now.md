# NOTE-004 — R2 not accepted. Perform the static repairs now.

`PHASE1-REPORT-R2.md` (`32c06530…`) is **not accepted**. AWAITING-AUDIT-R2 is
your state, not a disposition. Preserve the original and R2 as history; return
**one coherent updated packet** or a **concrete execution gap**.

**Static repair and requirement review are already authorized. No new build,
probe, elaboration or Phase-2 grant exists.** Everything below is doable with
reads, greps and writing.

Credit where due: the **4-against-3 overrun is retained** in R2, and the
qualified-source inventory improved. Those stand.

## 1. Receipt-level identity and re-keying is Phase 1 work — do it now

R2 still lists **grouped ledger families** and then **defers per-mutant re-keying
to P2-a**. The contract requires it now. A family is not a receipt row.

**Row 43 is a double count, and R2 says so itself:**

```
| 43 | t48-owner-codex main TSV counted in 20–26 | (counted) | — |
```

A total of "43 files" that contains a row already counted in rows 20–26 is **not
an independently reconciled exact inventory**. Reconcile it.

Deliver: **exact receipt rows**, one per receipt, each with its classification
evidence and provenance/footprint status.

## 2. Unknown footprint is not demonstrated staleness

R2 marks receipts STALE where the footprint is merely unknown. Those are
different claims and must be separated per receipt:

- **demonstrated STALE** — you can show the relevant context changed; or
- **unestablished reuse** — footprint unknown, nothing disproved; or
- **UNRECOVERABLE** — provenance genuinely cannot be retrieved, classified as
  such **per receipt** with the reason.

Bounded retrieval is a read, and reads are free. Use it before classifying.

## 3. Full finite row map, and the real relation

Give the **complete finite row map** with the **relevant property/atom relation
and per-row rationale**, and **preserved deferred identities**. No wildcards, no
provisional approximate families. Defer, never drop — a deferred identity stays
visibly on the map.

## 4. The proposed campaign is NOT authorized, and it double-counts an operation

**44 builds + 20 elaborations is not authorized**, and it is not derived. Roughly
"12 families / 20 rows / 12 mutants / 10 mutants" is **not an exact command plan
for 239 required identities**.

And this is the sharper defect: **P2-a is declared footprint work at one point
and then also relied on to discharge helper-satisfiability**:

```
line 216  P2-a re-key + footprints … ONE `lake env lean --run footprint-check`
          per receipt family … 12 elaborations, 0 builds
line 234  helper-satisfiability pending (P2-a elaborations)
```

**One proposed operation cannot discharge two obligations.** Helper
satisfiability needs its **own witnesses** — hypotheses actually exhibited — not
a footprint check reused under a second heading. Separate them, or state plainly
that helper satisfiability is unaddressed.

**G-B1 and G-B2 remain ungranted.** A proposed execution gap is a request, never
an authorization.

## 5. If execution is truly unavoidable, specify it properly

**After** the static requirements are complete, return any genuinely unavoidable
execution as: **exact argv**, targets and driver identity, **expected observable
output**, its classification (build / elaboration / probe), and **numeric cost**.
A family count is not a command plan.

Two things not to do: **do not re-run the lost cold log**, and **do not build
three unnamed modules merely to remove an honest limit**. An honest limit stated
is worth more than a build spent erasing it.

## 6. Scope

The **4-against-3 overrun stays explicit** — no refund, no retrospective
authorization.

And this one matters most: **do not narrow the inherited Lean quality goal to
whatever this first assessment happened to cover.** Phase 1 measures and costs;
it does not redefine the target. If the required extent is larger than what you
have measured, that is a finding about the measurement, not a smaller mandate.

Return the updated packet or the concrete gap. Local files only.
