# A-002 — M1-S retained; ONE additional targeted elaboration authorized for the instrument repair.

## The failure is yours to repair and partly mine to have missed

`build.exit=0`, `census.exit=1`, runner exit 1. `census.stdout` names
`Inventory.lean:37:5: unexpected token 'prefix'` with the placeholder and `:=`
diagnostics after it, and **no `inventory.jsonl` exists**. This is a **failed
auditor instrument setup** — it is **not** evidence against candidate `94bb7bb`
and **not** a measured semantic extent. Nothing about the candidate is implied.

**My share:** I ran `bash -n` over your three shell scripts and reported that as
verification, then affirmed your own "statically prepared, not compile-tested"
caveat and authorized anyway. I checked what was easy to check and applied no
equivalent scrutiny to the one file where the risk actually lived — a 69-line
Lean instrument with the collision visible on **line 6**. The desk records the
same miss on its own side. Your caveat was honest and disclosed; the review was
the weak part, not the disclosure.

## Root cause, confirmed at source

```lean
:6   let prefix := root.toString ++ "/"
:19  let localArtifact := prefix.isPrefixOf path.toString
```

`prefix` is a Lean **notation command keyword** (`prefix:max "…" => …`), so the
parser reads a notation declaration where a binder was meant. It is a
one-identifier rename in two places.

I also swept the whole instrument for other Lean reserved words used as `let`,
`fun` or `for` binders, or bound with `:=` / `←`. **`prefix` on line 6 is the only
hit in all 69 lines.** That is my observation offered as input — **the repair and
the full static review for related setup defects remain yours**, and you should
not treat my sweep as discharging that review.

## GRANTED — one additional targeted elaboration

At most **ONE** additional targeted elaboration to repair and rerun the inventory
instrument, **within** the existing 12 / 80 ceiling. **This is not a raise and not
a reset.**

- Current actual spend: **1 substantive / 1 targeted**.
- After this maximum attempt: **1 / 2** for this seat; cumulative **7 / 61**
  including historical 6/59; leaving **11 substantive / 78 targeted**.
- **No whole build. No candidate source change. No fresh seat. No audit START.**
- Every additional setup failure is still charged. **Do not silently run extra
  compile probes during preparation** — a probe is an elaboration.

## The M1-S build output is admitted as a prerequisite — I verified it

You may reuse M1-S's build output rather than rebuilding. I established the
prerequisite myself before saying so:

- `evidence/M1/oleans.sha256` records **30** oleans and **every one still matches**
  on disk.
- Candidate source unchanged: `94bb7bb64324a48f7361252556b4d15e45b3923f`, tree
  `3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced`, porcelain empty.
- All **269** frozen tracked inputs still match `candidate-inputs.sha256`.

These are the retained outputs of the **explicitly recorded fresh M1 build** —
not another cold build, and not inherited owner evidence.

## Before the one attempt

Freeze a **VERSIONED** revised instrument and command manifest: exact argv,
prerequisite identities, and a **NEW output directory**. Preserve `evidence/M1`
and the original eight-file `MEASUREMENT.sha256` **unchanged**, including the
failed diagnostics and the original instrument bytes. **No reuse of output names,
and no relabelling that failure as never having occurred.**

If the prerequisite cannot be established, or the repair needs **more than one**
targeted elaboration, **return the actual gap before executing.**

## After

If the attempt **fails**: stop, return diagnostics and spend. **No next automatic
retry.**

If it **succeeds**: the metadata is **planning-only**. Unknown or partial
inventory still cannot establish extent, and **no row closes because a JSON file
was emitted.** Then finish the full original command-to-obligation fit and all
required final cold / CI / axiom controls **under your existing authorization** —
**do not add a checkpoint where none is required**, and do not wait on me.

No author wake, no third submission, no push, PR, merge, comment or scope
reduction.
