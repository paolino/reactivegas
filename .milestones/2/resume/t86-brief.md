# Ticket owner brief — `reactivegas#86`, the exporter successor

## Identity

| | |
|---|---|
| role | **ticket owner** for one issue-backed PR |
| worker id | `t86-exporter-successor` |
| runtime root | `/tmp/reactivegas/ms2/e-haskell-impl/t86-exporter-successor/` |
| seat | `muse` — wrapper pins `pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh`; launch `muse --approve` |
| parent | epic owner `claude-opus-5[1m]`, pane `%504`, root `/tmp/reactivegas/ms2/e-haskell-impl/` |
| issue | https://github.com/paolino/reactivegas/issues/86 |
| parents | `#67`, `#72`, milestone "Reactivegas on kelgroups" |
| inherited candidate | **`9c8756a252c46bc5745badafcc9126abd3e5d9a7`** — frozen, **unaccepted** |
| base | accepted S1 **`4a6cd87`**; model base `e6c5924`; prior mandate base `507bc79` |
| worktree | **create** `/code/reactivegas-issue-86`, branch `feat/86-exporter-successor` |

Not `/code/reactivegas-haskell-impl` — that is my pane's worktree.

**This is a new campaign, not a third `#74` submission.** `#74` and PR `#78`
stay open, frozen and undelivered. Write no closing keyword for them; their
supersession is proposed later, with the accepted successor packet.

## Seats

- ticket owner **`muse`**; commit owner **`muse`** (the operator suspended the
  alternation fence between those two seats);
- **auditor `codex` or `grok` only.** Never `muse`, never `glm`, and — learn
  this from `#74` — **never `claude`**. A helper returning a family from its
  five-family default is **not** authority over this restricted set. If the
  helper's answer is outside `{codex, grok}`, the helper is wrong for this
  epic, not the fence.
- Verify **live argv against the durable identity** *and* require a
  **post-cursor `START`**. Both, every seat, every time.

## Load chain

`orchestrator-contract` → `ticket-orchestrator` → `resolve-ticket` →
`worker-protocol` → `tmux-orchestrator` → `verification` → `invariants` →
`lean4`, `nix`, `gate-script`.

## What went wrong under #74, so you do not repeat it

The exact full gate passed and the full mandate did not. Three blockers stood
under a green gate. Read `handoffs/RECUT-74-PROPOSAL.md` (sha256
`bee72ccc4c301ea44e9a267b132de8960b2cf8c9d72912eb5fe504a66808bd1c`) and
`.archived/`/`audit-s3` under the `t74-corpus-exporter` root before you freeze
anything. Two seats were lost to contract errors — one to an unauthorized
auditor family and repair-only scope, one to a mandate that reached the auditor
after its build was allocated.

**A green gate is not the mandate.** Build the gate so its rows *are* the
mandate.

## The mandate

The issue body is authoritative and complete: rows **A** (committed CI path),
**B** (declared `jq`), **C** (whole-wrapper value binding, four named
controls), **D** (arity refuses without writing), **E** (coverage handoff), plus
the four inherited rows re-established. Read it in full and freeze against it.

Five points I will check specifically, because they are where this can go wrong
again:

1. **Row A is local execution of the committed path plus real remote CI on the
   clean SHA.** No mutant push — pushing deliberately corrupted bytes is not
   authorized and not required. Neither half substitutes for the other. Bind
   the executed command's identity to what the workflow file contains, and
   demonstrate that removing the invocation is detected.
2. **Row C's independence claim is bounded.** The selected live-call /
   derived-`ToJson` method **does not establish** serializer-instance
   independence. Do not write that such independence is unachievable in Lean —
   that is false as stated. Name all four controls explicitly, **including
   integrated `.auth`**.
3. **The emitter's output stays byte-identical to `9c8756a`'s inputs** unless a
   separately accepted upstream integration forces a re-emission, recorded
   separately. Do not anticipate `#68`/`#69`. If master advances, integrate and
   re-establish complete acceptance at that actual final base.
4. **Every inherited row is open to falsification.** "Not reopened for
   re-search" does not bar an auditor from challenging a prior PASS. Prior
   receipts are inputs, never acceptance by inheritance.
5. **Row D proves absence of writes**, with sentinels and a directory
   comparison. An exit code alone does not show nothing was written.

## Ceilings — approved, and you account for them

| | |
|---|---|
| **owner substantive build/gate attempts** | **8** across the campaign |
| candidate submissions | **2** — initial plus one findings-driven repair |
| building audits | **3**, each new candidate getting a fresh FULL `codex`/`grok` audit |
| ceiling raises | **0 automatic** |

**Maintain an owner build counter from your first command.** `#74` had none, so
its owner spend is permanently unknown and I had to report it that way to the
desk. Do not hand me that again: journal each substantive build/gate attempt
with command, exit and elapsed time.

Warm bounded controls are still accounted — command, exit, time. **No unlimited
loop hidden as free probes.**

Any increase needs a concrete request with cumulative spend **before**
execution. Not after.

## Fence

**Owned:** `lean/Reactivegas/CorpusExport.lean`, `lean/corpus/*`,
`lean/lakefile.lean`, the corpus recipes in `justfile`,
`.github/workflows/ci.yaml` **additively**, the dev-shell tool declaration under
`nix/`.

**Forbidden:** any Lean theorem, guard, `step`, `stepEvent`, `appFold`,
`baseHook`, state type, `Trace`, `reactivegas.trace/v1`; existing corpus
content, `seedView`, `corpusInitial`, `seedAuth`; `docs/en/design/` (`#71` owns
it); any implementation in `paolino/kelgroups`.

If a row appears to require crossing that fence, **stop and file a question.**

## Authority

- **Commit:** yes, locally, after the corrected mandate and gate are frozen.
- **Push / draft PR:** yes, **after full local `nix develop --quiet -c just ci`
  GREEN**, quoted with its exit. The body states the unaccepted provisional
  state and every semantic and context dependency.
- **No merge, no publication, no deployment, no issue or PR comments.**
- Auditor reports are **local only**. No gist, no external paste.
- **Every commit gets a journal line.** I verify by diffing `git log` against
  your STATUS; that check caught an unjournalled commit under `#74`.

## Questions, inbox, stop conditions

File `questions/Q-NNN-<slug>.md`, append `BLOCKED Q-NNN`, park. Check `inbox/`
before each new phase, before an expensive command, before freezing evidence,
and before `COMPLETE`.

Stop and journal a terminal event for **every** way you can stop — including
capacity: `COMPLETE capacity-limit handoff=...`. A wait is a state: journal
`NOTE PARKED: ... wake=<condition>`.

Escalation: me, epic owner, pane `%504`.

## What I need back

Your frozen mandate and gate hashes, the owner build counter as it goes, and a
final acceptance packet. Report ticket-level state; supervise your own pair.

You are not alone in the codebase; do not revert edits made by others.
