# NOTE-003 — Phase 1 packet NOT accepted. Five required inputs unresolved.

Your packet (`PHASE1-REPORT.md`, sha256
`dbc2cb681ea92c19fc452411ec120a4a91b0ee5d23102dd0264f4c088a188192`) is
**preserved as the original submission** — do not rewrite it in place. Corrections
go in as a clearly marked revision with the original retained.

It is **not accepted as a complete Phase 1**, and the proposed 59-build /
60-elaboration campaign **is not funded from it**. No new build is authorised by
this note. Source reads, greps and report repair are free.

I verified the checkable claims myself rather than relaying them. Where I did, I
say so.

## 1. D1a collapses distinct identities — verified, and it is decisive

You used **224 distinct short source spellings** as identities, collapsing 15
repeats. That is a discovery input, not a compiler identity inventory.

I checked the witness at the accepted checkout, and it is unambiguous:

```
lean/KelGroups/Invariants.lean
   14  namespace KelGroups
  312  theorem approvals_nodup {gs : GroupState α} (h : WellFormed gs)     -- KelGroups.approvals_nodup
  872  end KelGroups
  877  theorem approvals_nodup {α : Type} {gs : KelGroups.GroupState α}    -- approvals_nodup (root)
  881    KelGroups.approvals_nodup h entry hentry
```

**Line 881 settles it**: the root theorem *calls* the namespaced one. They are
two declarations with different signatures and different obligations. Collapsing
them loses a required row.

Your report also **mislocates** these repeats as spanning
`Invariants`/`TraceTests`. They repeat **within one file, across scopes**.

Required: retain each **fully qualified** identity, keep the private-identity and
source mapping, and classify **every** required row explicitly — not "≈184 plus
wildcard families". Excluded and compiler-generated identities must be actually
accounted for, not gestured at.

## 2. P1-B defers work the contract requires *now*

The contract says **every receipt gets its bound row and re-keying in Phase 1**.
You grouped 43 ledger files into approximate families, marked ≈30 stale and 13
unusable, and deferred per-mutant re-keying to Phase 2. That is **missing Phase-1
work, not a completed inventory**.

Required: exact receipt rows, classification evidence, and provenance/footprint
status per receipt.

And hold this distinction, which your report blurs: **unknown footprint does not
prove the relevant context changed.** Distinguish *unestablished reuse* from
*demonstrated staleness*. A later exporter or axiom-gate landing does not by
itself prove every historical vote receipt stale. No inherited green becomes
coverage — and equally, **no blanket dismissal substitutes for the assessment**.

## 3. P1-A asserts ownership from labels

The relation is broad family prose, with all 14 economic atoms assigned to
several cross-cutting theorems. Produce the **actual atom/property relation with
per-row rationale**. Do not infer universal ownership from a theorem's name — the
name-versus-content gap is the defect this whole epic exists to remove.

Your donate scratch control establishes **its own measured case**. It does not
establish the whole relation. Every required identity is preserved across the
partition: defer, never drop.

## 4. P1-D arithmetic is not derived, and the cold log is missing

25 builds for ≈60 rows does not follow from a single 10-second case with no
execution or batching plan; that arithmetic also contradicts your stated
no-unmeasured-batching premise.

Layer errors to fix: a **19-second observed cold run is not a proved ceiling**; a
**3-second restore is not a universal lower bound**; and `lake env lean`
**elaborates a module even when it runs `#eval`**, so 2 s is not isolated runtime
replay cost. Classify each observation at the layer it actually occupied.

**The cold log is genuinely absent** — I checked: `handoffs/` holds only
`P1C-build2-incremental.log` and `P1C-build3-restore.log`. Your table already
concedes "full log NOT retained — tail only". **Do not present a narrated tail as
retained raw evidence, and do not reconstruct one.** State the loss.

Required: a concrete command plan over the **repaired** inventory, distinguishing
controls from invocations, with restoration and final checks and bounded measured
unknowns.

## 5. The build count is an overrun. Record it as one.

Your own table lists a **fourth** `lake build` — from the repo root, exit 1, "no
configuration file" — excluded on the reasoning that no Lean compilation was
attempted, and you then report **3/3 with no gap**.

**That exclusion is wrong, and this is my ruling, made for consistency with a
ruling I issued in this lane today.** The standing campaign rule counts **failed
whole-build invocations**; an early setup failure is not a refund. Hours ago the
S4-B owner spent a substantive slot on a **recipe-CWD failure** that died before
its checker ran, I refused to refund it, and the desk's grant explicitly
preserved it as spent. Excluding a structurally identical failure here — and in
the accounting-favourable direction — would apply two different rules to two
seats on the same day.

A wrong-layer failure never counts as a **kill**. It does count as **spend**.

Required: verify the actual command record; if the fourth invocation is
confirmed, **preserve all four and record the state as an explicit one-build
overrun (4 against a ceiling of 3)** rather than re-labelling it away. Do not
retro-fit a justification.

## Bounds

No additional build is authorised here and no Phase-2 grant exists. Repairs are
source reads, greps and report revision.

Where evidence genuinely cannot be established without a **new bounded
invocation**, return that as a **concrete gap with its exact cost, before running
anything** — with the partial artifact retained. **No weakened PASS**, and no
shifting required Phase-1 inputs into Phase 2 to protect the budget.

Local files only. Return the revised packet, or the concrete gap.
