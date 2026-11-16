# Resurrection fragment — e-kelgroups-substrate (epic owner, CANONICAL)

Parent desk `%510`, runtime `/tmp/reactivegas/ms2`.
Epic root `/tmp/reactivegas/ms2/e-kelgroups-substrate/`.
Epic owner pane `%532` (reactivegas:12.1; window `kelgroups` @157 after root
reorganizations 2026-09-05/06; six panes %532/%534/%545/%554/%557/%567).
Alias history (reconciled, not competing): reactivegas:8 (intake) →
reactivegas:11 (pause/release) → reactivegas:12 (current).
Worktree `/code/kelgroups` @ `368b596…` (accepted base, rechecked static).
Launch: `muse --approve` =
`pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve`.
Brief: `artifacts/ASK-kelgroups-e29-substrate.md` sha256 `399d9268…`.
START in `STATUS.md` 2026-09-05T10:57:46Z.

## Current state (2026-09-06)

- Terminal R1: audit FINDINGS incomplete (F1/R2/R6 lost-updates + F2/R4
  vacuous BLOCKING); candidate `84a2dae` NEVER accepted (superseded as
  working candidate, preserved as history).
- Current R2 candidate: `ab25cd1` (chain …→`e4022c2`→`ab25cd1`, test-only
  +8/−5, tracked-clean), BINDING-ready, GREEN unrun. Owner spend 0/14 +
  targeted 4/24 + probes 1/4. Next action: BINDING-GREEN review → M8v10.1
  freeze → full GREEN → fresh FULL audit commission + START.
- Authority/caps: S28-R2 owner 14/24 + auditor 12/24, ONE submission, zero
  auto-raises. Standing: mandates, fences, local-only delivery, no
  merge/push/PR/comments. Prior ledgers separate (S28-1 34/34 + 9/12+7/24;
  S28-R1 13/16 + 10/12+16/24).
- Verified seats: ticket `%534` (muse, same launch family); S28-R2 owner
  `%545` (respawned fresh, muse); S28-R1 owner parked post-submission.
- S28 initial-84a2dae audit history (closed): s28 seat (report `93a16836…`,
  contract-blocked, zero inspection, root archived) → s28b seat (FULL
  execution: gate PASS + F1 lead + F2 survivor, then provider interruption
  mid-course; completed via administrative handback to terminal FINDINGS
  `b7b793a3…`: F1/R2/R6 + F2/R4 BLOCKING, 3 OPEN + 3 BLOCKED; candidate NEVER
  accepted).
- S28-R1 3af3d06 audit history (closed): s28r1 seat (report `22c79c04…`,
  dispatch-invalid contract-blocked, 0/0, pane %566 on reactivegas:16
  retained) → s28r1b seat %567 (FULL execution → terminal FINDINGS
  `24252ef1…` COMPLETE 00:39:44: F1/F2 resolved-observed, F3 BLOCKING; 5
  killed / 1 blocked / 0 open; 10/12+16/24; ledger `7731f5b6…`). Terminal
  seats NEVER RESTART (panes %554/%566 retained as-is).

## To resurrect

Re-verify `git -C /code/kelgroups-issue-28 rev-parse HEAD`, re-list
`tmux list-panes -t reactivegas:12`, read `STATUS.md` tail + `EPIC-MAP.md` +
`t28-app-api/STATUS.md` + active owner/auditor `STATUS.md` tails, require
fresh `START` from any respawned worker before admitting claims. Upward
reports: local inbox files + STATUS only, never desk `%510` composer
(`artifacts/UPWARD-REPORTING-LOCAL-ONLY.md`).
