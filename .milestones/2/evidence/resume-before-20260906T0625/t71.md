# Resume fragment — #71 design-record TICKET OWNER (Opus successor, PARKED)

Resurrection-grade. Replaces the predecessor fragment at
`/tmp/reactivegas/ms2/t71-design-record/RESUME.md` (kept, pointer prepended).

## Identity and launch (replay exactly, quotes included)

```sh
claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
```

- family=claude, pane `%516`, window `reactivegas:11` (`design-wait-model`),
  cwd `/code/reactivegas-issue-71`, pid 3358067. Argv and active session model
  both verified as `claude-opus-5[1m]`, effort high.
- Live runtime root: `/tmp/reactivegas/ms2/t71-design-record-opus-20260906`
  (brief sha256 `d89a92d4…`). Preserved scope root:
  `/tmp/reactivegas/ms2/t71-design-record` (brief sha256 `8a72a5a6…`;
  its initial base/zero-spend values are historical).
- Parent: milestone desk `%510`, `/tmp/reactivegas/ms2`.
- Authority: `artifacts/ROLE-SUBSTITUTION-OPUS-20260906.md`,
  POINTER-1788674093-3359899. Role substitution only.
- Ownership record: `<preserved scope root>/OWNER-CURRENT.md`.

## Stage

PARKED, admitted 2026-09-06. Campaign S71-B, Round-B repair terminal VERIFIED
and UNACCEPTED. Nothing in flight; no builds, auditors, monitors or schedules.

## Ticket state (verified at admission, read-only)

- `/code/reactivegas-issue-71`, branch `docs/71-design-record`,
  HEAD `77f8be62b6bbe6d2f3e2117464b0c72d0e736e58` == origin, clean.
- **No accepted candidate.** `77f8be6` = UNACCEPTED Round-B terminal
  (full v8 GREEN receipt `2af22b6e`, frozen diff `460411b2` + manifest, pushed).
  Rejected: `36666dc` (`b5d3199f`), `67877b1` (`a6a0d9f5` F-01/02/03).
- Mandate: `specs/71-design-record` at planning commit `90dae99`
  (R71-01..12 + claim-syntax definition + T71-06/07/08).
- Gate: frozen v8, `./gate.sh` sha256
  `7aa3f2b5c3f4b23447a9e32e5ddecf2510a10ec978af843d0b885a4512fa7939`
  == `evidence/gate-v8.sh`. v1–v7 + batteries + falsification logs preserved.
  If a hash here disagrees with the files, the FILES win — re-hash before use.
- Base: merge-base `d670323` (PR87); 5 ahead / 1 behind `origin/master`
  `3590c00` (PR88, #66 S2R). **No rebase performed.** Old pins are not current:
  S2R moves Invariants source/line identities and the justfile path.
- PR #77 OPEN, **draft held**, head `77f8be6`.

## Children

- `%542` commit owner, pid 1296754,
  `pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve`,
  zero descendants, PARKED write-idle since `2026-09-05T15:03:20Z`.
  Adopted without restart or wake; root `<preserved>/commit-owner-s2/`.
  Sole authorized writer of `docs/en/design/`. This owner writes no
  product/docs implementation.
- No auditor alive. Retired panes: `%518`, `%521`, `%533`, `%537`, `%546`.

## Counters and ceilings (carried verbatim; unlike denominators NOT reconciled)

- Owner S71-B 2/4 full + targeted-per-journal (recount from
  `commit-owner-s2/STATUS.md` before spending). Owner S71-A 4/6, closed.
- Auditors 2/3 builds + 20/40 targeted spent; **reserve 2 builds + 20 targeted
  untouched for the final FULL audit** (the reserve is 2+20 under both readings).
  Those two denominators come from two different ledgers and are **left
  unreconciled on purpose**: `2/3` is the ticket-wide audit-build ledger
  (predecessor journal 11:03:39Z, plus audit-b1's 1 build); `20/40` is the S71-B
  campaign grant (11:30:39Z `auditors-4-builds-plus-40-targeted-total-max-2-submissions`,
  13:11:53Z `spent-1-build-plus-20-targeted reserve-final-2-plus-20`). Do not
  merge or guess them; ask the desk if a spend decision ever turns on it.
- T.O. static work ledgered, 0 builds, no seat cap consumed.
- Submissions 1 of max 2 — one repair→re-audit cycle remains. No reset.
- Ceiling raises 0/2. `draft=NONE`.

## Open Q/A and inbox

None open in either root. Predecessor consumed NOTE-001..008 (ticket),
NOTE-009..019 (artifacts), UPWARD-REPORTING, SEQ, RELEASE, substitution order.
Monitor both `<live root>/inbox/` and `<preserved scope root>/inbox/`.

## Wake condition and next authorized action

WAKE ONLY on desk announcement of the **accepted final model/quality base**,
followed by its concrete authorized rebind sequence. Then, in order:

1. Verify announced base vs `origin/master`; rebase the lane branch iff it
   differs (abort + escalate on conflict).
2. Re-read required anchor lines + discovery count at the new PIN; re-verify
   the gate end-to-end; version/freeze the gate delta if the base moved it.
3. Final full validation + **fresh FULL independent audit** on the rebased
   candidate (reserved 2+20; row-level AUTH/pending/claim truth; local-only).
   Auditor seat: **codex**. The preserved brief's `grok-4.6` pin is STALE —
   operator-ordered NOTE-008 (predecessor journal 2026-09-05T11:01:55Z and
   11:03:39Z) narrowed the set to codex-or-grok and recorded the grok cap as
   exhausted ticket-wide; audit-s2r (`%537`) and audit-b1 (`%546`) both ran
   codex-gpt-6-astra-high. Never muse/GLM; never reuse a terminal auditor; fresh
   pane, root, detached worktree and context per submission. Do not reintroduce
   the stale brief pin, and do not widen the recorded set on this seat's
   authority.
4. Accept/handback packet for merge authorization, or one bounded repair +
   re-audit within the remaining submission cap, or re-cut.

Stop and escalate on: scope contradiction, missing honest citation, cap
pressure (state the exact gap; never silently overrun or narrow), base
ambiguity.

## Standing prohibitions

No Lean/model/theorem edits to make prose true. No merge, publication, issue
or PR comments, semantic edit, submission/cap reset, new implementation seat,
or terminal-auditor reuse. No tests, citation rerun, audit dispatch or build
was authorized during substitution. Upward delivery is **local files + own
STATUS only** — never type into `%510` or any human composer. Do not wake the
parked commit owner for acknowledgements it does not owe; do not restart
terminal historical owners; do not anticipate #68/#69. Deferred with rationale:
#75/#76 rows; #68 rebind only after landing (handoff + precision rule held).
No stale snapshot may be called final.

## Operational warnings carried forward

- `wait-status` patterns must match the two-space tag column; preflight every
  wait with a non-zero grep count; keep blocking calls <= 60s.
- Bare `grep -o` counts in `$()` under pipefail need `|| true`;
  `out=$(failing)` bare under `set -e` exits silently — capture with `|| rc=$?`.
- Recorded mistakes not to repeat: leg-12 authorized after NOTE-018 withheld it
  (spent within cap, owned); grok-seat contract breach on audit-s2
  (terminalized, archived); one stale gate hash in an admission line
  (corrected, old line preserved).
