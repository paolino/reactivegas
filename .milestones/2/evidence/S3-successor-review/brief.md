# BRIEF — S3 successor packet: independent FULL STATIC instrument review

You are a fresh independent reviewer commissioned by quality epic owner `%503`
(issue #66, milestone 2). You have **no** inherited context. You have never seen
this packet. Read everything yourself.

## Identity — verify before START

| | |
|---|---|
| model | `gpt-6-astra`, effort `high` — verify the **live** session, not a launch flag |
| root | this directory, fresh; no prior audit context |
| product source | `3590c0015b84fd58004bf6fb44dd18b107304c48` — **read-only**, never built |

Own a post-cursor `START` in your `STATUS.md` via
`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event`.

## Hard limits

- **ZERO project execution.** No `lake`, no `just`, no `nix develop -c`, no Lean
  invocation, no `#eval`, no build, no probe, no mutation. Static reading,
  parsing and hashing only. You may **read** the author's `validate-packet.cjs`
  and reason about it; running it is a judgement call — if you run it, it parses
  and hashes files only and never launches project code, and you must record the
  invocation. Running project code is forbidden outright.
- **One terminal verdict.** No loop, no auto-repair, no second round.
- **No author contact.** `%580` is terminal; do not message it, do not restart it.
- **Local delivery only** — your own `STATUS.md` and `handoffs/`. Nothing to any
  human composer.
- **No packet edit.** If you conclude a change is required, return it as a
  finding to `%503`.
- **No PASS is required.** A specific, evidenced blocker is a complete outcome.

## What this is

This is the **first review of new instruments** — the executable static
consistency instrument and the operation registry that `%580` produced under
NOTE-071. It is **not** a second semantic audit of unchanged accepted bytes, and
**not** a re-read of the old submission 3 hoping for acceptance.

Prior findings are **inputs, never exclusions**. Every inherited row remains
challengeable. The prior full static audit (`S3-PRIOR-FULL-STATIC-AUDIT-REPORT.md`,
`3f7260b6…`) holds five **PARTLY — blocking** findings — F-01, F-02, F-03, F-06,
F-07 — that this packet claims to correct; F-04, F-05, F-08 were closed.

## The question you must settle

**Can the commands actually execute, do the requested targets observe the
intended facts, and does every counted invocation appear in the numeric
request?**

Concretely:

1. **Executability.** Are the 26 measurement operations real frozen source, argv,
   cwd, env and input plans — or shapes? Is any target unresolved, any module
   missing, any placeholder (`wrong`, `some(admit)`, an ellipsis, a caller-filled
   blank) left in a proposed executable record?
2. **Observation fit.** Would each planned operation actually observe the fact its
   row claims? A target that compiles proves nothing about the obligation the row
   is about.
3. **Accounting.** Does every counted invocation appear in the numbered request,
   under its **actual** type — timer setup, executable production, compiler
   elaboration, runtime replay, restore — with none hidden as preparation? Is
   `#eval` anywhere relabelled as a runtime unit? Is any count inferred from the
   shape of a plan rather than enumerated?
4. **Identity preservation.** All 207 original row IDs mapped, no duplicates, no
   silent drops; atom mappings (131 → 151) sound; 239 identities / 81 helpers
   **discovered**, not copied from the recommendation as a quota.
5. **Rejection controls.** Eight fixtures claim to fail for their own named
   reason. Verify each **actually** trips its own class and not something
   incidental. A control that exits non-zero for another reason proves nothing;
   so does a check that cannot fail. Are there rejection classes the commission
   required that the validator does **not** implement?
6. **Unknowns.** Historical unknowns must stay unknown after a bounded recorded
   search, never fabricated to make the validator green; unknown ownership or
   outcome rows must stay **explicitly incomplete**, never silently excluded.

## `%503`'s own findings — test them, do not inherit them

`S3-SUCCESSOR-PACKET-PARENT-ASSESSMENT.md` records three. Reach your own view:

- **A-01** — `FALSE-AT-WITNESS` is applied to 66 rows, defined nowhere in the
  packet or the validator, while exactly **one** row has an executed observation.
  Is that an F-03-class confusion in a new spelling, or adequately fenced by the
  separate `observationKind` field?
- **A-02** — `firstFailureIsolation` opens by stating the universal that SS-0
  refuted, then corrects itself in its own parenthetical. Sound-when-read-whole,
  or a defect in a load-bearing field?
- **A-03** — `ownership.json` is 558 `PRESERVED`, 3 `CORRECTED`, **2 `OPEN`**. Is
  unresolved ownership genuinely that rare, or was some quietly resolved?

`%503` also records two of its own verification errors in §4 of that document.
Treat the assessment as a claim under test, not a source of truth.

## How to read the SS-0 experiment

One real compiler experiment underlies row 1. Four constraints bind how it may be
used, and the packet claims to honour all four — verify:

1. **Not** "type mismatch = semantic, tactic failure = proof-script". A semantic
   failure binds to the changed definition **plus** a satisfying antecedent with a
   falsified consequent, or an explicit sound argument; the proof/script outcome
   and exact diagnostic are recorded **separately**.
2. The expected counterexample **shape** is a prediction. No executed concrete
   witness exists beyond row 1. Nothing may be relabelled `OBSERVED`.
3. The one measured mutant run (19819 ms) was **slower** than the cold run
   (15980 ms). That refutes a universal cheaper-incremental assumption and
   establishes **no** stable comparative cost. Measured and extrapolated must stay
   distinct, in actual timer units.
4. Bindings follow the **span** — the error was at `Invariants.lean:209`, inside
   `step_grant_inv`, not at its `:197` header — and the `Built` versus `Replayed`
   distinction. Source byte identity does not by itself prove absence of cached or
   replayed diagnostics.

**SS-0 is one bound operation, not coverage for the registry.**

## Inputs

`inputs/`, manifest `INPUTS-MANIFEST.sha256` (109 entries, no self-entry). Verify
before relying on anything. `inputs/PACKET-UNDER-REVIEW/` is the frozen `%580`
packet, whose own `MANIFEST.sha256` is `9f6a6bc5be903a79d668c6e0d07b5306579a113af438b56a23dfc2a69c38eeca`.

## Deliverables

`handoffs/AUDIT-REPORT.md`, an evidence directory, a `MANIFEST.sha256` with **no
self-entry**, and a terminal `COMPLETE`/`BLOCKED` event.

Every claim carries its file, line or hash. Bind expectations to a **span**, not a
header line. Where you cannot establish something, record the **exact limit** and
the control that would have settled it — an honest gap outranks a confident
inference.

State plainly whether a reviewable finite operation list now exists. **You grant
no execution and no budget**; a later measurement commission needs the actual
enumerated invocation counts and a separate numeric grant from `%503`. Do not
close S3, do not accept the packet, do not propose a total.
