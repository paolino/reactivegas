# Q-001 — frozen gate v1 cannot reach GREEN for any tree: `local` self-reference under `set -u`

Filed by: commit-owner-s57-a-glm (pane %168), 2026-08-29T13:47Z
Candidate: 400f5b2829eeae27faeb0994ba8cfcc03c37dd3d (base bb3ac41a)
Gate: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v1`
(sha256 `f020731a0948880ee14fb39e4a9da6333de871f21672c5d89427bcaeed9028ff`)

## The defect, exactly

Gate line 74 (and the same pattern at line 84):

```bash
run_green() {
  local name=$1 file=$2 log="$gate_root/evidence-${name}.log"
```

Under the gate's own `set -euo pipefail`, bash 5.3 expands every word of the
`local` command before binding any of the locals, so `${name}` is expanded
while still unset and the shell aborts:

```text
/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v1: line 74: name: unbound variable
```

Minimal reproduction on this machine's bash
(5.3.9(1)-release, the same interpreter via `/usr/bin/env bash`):

```bash
set -euo pipefail
f() { local name=$1 log="x-${name}.log"; echo "$log"; }
f hello   # -> bash: name: unbound variable
```

## Why it never fired before

The gate was only ever executed against the planning base, where it stops at
the R-45 preflight (frozen RED receipt `gate-s57-a-red-v2.log`) — lines 73+
were never reached. The falsification and negative-control scripts invoke the
instruments directly and bypass `run_green`/`run_red`. The bug therefore
fires exactly when a candidate passes every check before the instruments:
a correct candidate is what first reaches the broken line.

## Evidence that everything the gate checks does pass on the candidate

Full gate run through `run-receipt` (exit 1 only at the crash above):

- evidence: `evidence/gate-s57-a-green.log`
  sha256 `bf305b0da995cb8eafa24d506ffb744e337d7ae0de6a14c48db07b4aff467f2d`
- `gate: ok toolchain Lean 4.25.0`
- `gate: ok source fence changed_paths=4`
- `gate: ok Slice-1 modules blob-identical to ccdda830`
- `gate: ok KelGroups to Reactivegas dependency direction`
- `gate: ok proof-source hygiene`
- `gate: ok exhaustive VoteEvent authorization constructors=6 wildcard=absent`
- `gate: ok R-45 preflight Lean-4.25.0`
- focused build exit 0; `gate: ok axiom evidence contractual_theorems=9`
  — all nine required axiom lines clean
  (`propext`/`Classical.choice`/`Quot.sound` only; no `sorryAx`,
  `Lean.ofReduceBool`).

## What I did NOT do

I did not edit, move, wrap, re-hash, or re-run a modified gate. The gate and
its directory are read-only for me. I did not run the gate under a relaxed
shell (`+u`) or through any wrapper: that would be running a different gate
than the one frozen in the manifest.

## Decision needed

Freeze a gate **v2** with the mechanical fix only — e.g. split the `local`
so `name` is bound in its own statement before `log` references it (both in
`run_green` line 74 and `run_red` line 84) — publish its path + sha256, and
point me at it. I will then verify the new hash, re-run the full v2 gate
through `run-receipt`, and submit.

Recommendation: no semantic change of any gate check; the check list, order,
instruments, and thresholds stay byte-identical. A one-line mechanical
repair, versioned, is exactly the "new gate version" path the contract
prescribes; re-falsification of the instruments is unchanged (they are
gate-version-independent and already bound in the manifest), but if you want
a fresh negative-control pass under v2 I will run it as part of the same
receipt.

## Alternatives considered and rejected

- Running the gate with `bash +u`: runs a different gate than frozen — rejected.
- Editing line 74 in place: forbidden (gate immutability) — rejected.
- Submitting without the full gate GREEN: the submission contract requires
  the immutable slice gate exit 0 — rejected.
