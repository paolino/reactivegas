# BRIEF — #92 successor commit owner: the complete checker repair

Commissioned by quality epic owner `%503`. Issue:
https://github.com/paolino/reactivegas/issues/92 (milestone 2).

You are a **fresh** seat. The previous campaign is **terminal at 12/13 with two
rejected submissions**. You have **no inherited context**; read your inputs
yourself. Nothing in the rejected work is a premise — it is all input to test.

## Identity — verify before START

`gpt-5.6-sol`, effort `high`, **live session verified**, this root. Own a
post-cursor `START` via
`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event`. **Known
tag vocabulary only** — `START`, `NOTE`, `BLOCKED`, `RESUMED`,
`GATE-PASS`/`GATE-FAIL`, `COMMIT`, `PUSHED`, `COMPLETE`. **Invent no tag.** **Every
way you stop carries `COMPLETE` or `BLOCKED`** — a progress summary is not a
terminal event.

## Why the last campaign was rejected — do not repeat its shape

Submission 1 selected root imports by **name shape** (`grep -v '\.'`), excluding
namespaced roots. Submission 2 replaced that with an **awk parser over
`lakefile.lean` text** — which handled a **subset of spellings**:

- `:67` strips from the first non-alphanumeric character, so `lean_lib Extra.Probe`
  yields **`Extra`** — the legitimate default root lost and replaced by a wrong
  name;
- `:87` enters multi-line mode **only if the assignment line itself carries the
  array opener**, so `roots :=` with ``#[`Extra.Probe]`` on the next line **silently
  drops** the root.

Both failures share one shape: **a filter that passes the cases it was tested
against.** A green fixture proved one recognized spelling, not the requirement.

**Your subject is the complete checker repair, not a patch to that parser.**

## Binding requirements

1. **Resolve the project's declared Lake roots from the pinned Lake configuration
   or tool API, as evaluated data.** A textual subset parser over `lakefile.lean`
   spellings is **not acceptable**, however careful. If the toolchain exposes no
   such interface, that is a **finding to return**, not a licence to parse text.
2. **Preserve omission detection.** A built project module absent from the
   checker's import closure must still be detected, and **B-minus-S omission
   detection must be preserved — do not shrink B to S.**
3. **Do not classify toolchain `Lean`/`Std` modules as owned project sources.**
4. **Exercise at least these three legitimate root declarations through the
   mandatory path**, each proving the module was **actually built and covered**:
   - a **namespaced default root**, e.g. `lean_lib Extra.Probe`, with **no parent
     module and no aggregator rescue**;
   - an explicit `roots := #[...]`;
   - an explicit roots assignment whose **array opener begins on the following
     line**.
5. **Re-establish** the prior omission, bypass, invalid-import, base-identical and
   theorem-inventory controls **against your bytes**. Earlier observations are
   **inputs only** — they prove nothing about your candidate.
6. **Retain exact fixture bytes**, or reconstructible one-edit deltas, for **every**
   control — **including the lakefile and the staged probe**. The last campaign
   lost X4's and X6's fixture bytes and could not establish their provenance. Every
   log binds **candidate SHA, tree, checker hash, command, toolchain and fixture
   identity**.
7. **Address the shared-filter assurance limit.** Two derivations that consume one
   filtered inventory **may agree while both are wrong**. Keep the required
   **independent** omission control, and state any **surviving non-binding
   assurance limit precisely** rather than implying it away.
8. **No Lean product semantics, simulator bytes, or model definitions change.**

## Budget — 14, and you preflight before the first product command

One declared successor ceiling of **14 substantive executions** across author,
blind inspection, the one adjudicated repair batch, delta inspection and the final
exact-candidate gate. **At most 8 may be spent by the author side — that is your
cap.** At most **two submissions** and **one repair batch**.

**Preflight the complete schedule before the first product command.** Enumerate
every invocation under its actual type; nested stages are numbered individually and
no bundle is called one operation. **If it does not fit 14, return the exact
schedule gap** as a `BLOCKED` event — **do not weaken the contract to fit, and do
not launch an unfunded command.**

A mandatory `nix develop --quiet -c just ci` is **one** execution including its
recipe and dependency expansion; internal stages are not charged recursively. Each
separate aggregate invocation counts again; **never wrap independent runs or
retries into one script to relabel them as one unit.** A setup failure is uncharged
only if it does no product work and fails before the assertions.

## Deliverables and process

Open or update the **draft PR at the first coherent successor candidate**. **A
green aggregate gate is not acceptance.** The current parallel blind-inspection and
delta process applies exactly as loaded from disk at dispatch — two blind
inspectors on submission 1, one adjudication by `%503`, one repair batch, one delta
inspector on submission 2.

Freeze each submission in its **own** directory with a `MANIFEST.sha256` carrying
**no self-entry**. `REJECTED-SUBMISSION-2/` and `PRIOR-EXECUTION-EVIDENCE/` are
**read-only copies**; writes are refused by the filesystem, and you never `cd` into
them.

## Standing rules

A check that cannot fail proves nothing. Never rename a prediction as an
observation. Bind expectations to a **span**, not a header line. **When asserting
an absence, read the complete output** — never a `head`, never only a tail. Where
you cannot establish something, record the **exact limit** and the control that
would settle it.

## Fences

No product-semantics change, no simulator-lane edit, **no driver deregistration**,
**no fixed name allowlist**, no quota shortcut, no `docs/en/design/` write, no
issue comment. **No merge** — merge needs a separate exact-head desk authorization
that does not exist.

**#92 and #66 remain open.** Nothing you write closes either.
