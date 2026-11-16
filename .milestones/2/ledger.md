# Reactivegas milestone 2 — desk ledger

Updated 2026-11-16T08:25Z by the desk in pane %420. Snapshot only; history lives in the journals,
issues, pull requests and git.

## Outcome test

A released coordinator, browser client and command-line client implement the buying-group laws end to
end for one group (milestone description). Nothing of that is released yet. Conformance of the Haskell
code to the Lean model is not measured.

## Authority and team

The 2026-11-08 release lifted the 2026-10-02 pause for milestone 2 only. Standing team per ticket, ruled
by the operator on 2026-11-08: ticket owner Claude Opus 5.5, commit owner and auditor Claude Sonnet 5.5,
effort high (binding text /tmp/reactivegas/ms2/artifacts/TEAM-AND-AUTHORITY-20261108.md). For the #73
lane the operator said on 2026-11-16 "use opus ticket owner and sonnet coder and sol auditor"; the
auditor ran as Codex gpt-6.1-sol, effort high. Whether Sol is the auditor for the next lane too is not
ruled: ask the operator in one line when staffing it. Lean work follows the two-slot batch rule
(lean.lock for builds, lean-long.lock for mutation campaigns, 24G each). Seats merge only on a desk
authorization naming the exact SHA, through guard-merge.

The desk seat was taken on 2026-11-16 by a fresh owner (Claude Fable 5.2) on the operator's instruction.

## State

Reactivegas master is 73e9440 (PR77, #71 design record, merged 2026-11-11). Kelgroups main is abe3648:
the vote machine (kelgroups PR53) merged on 2026-11-16, post-merge build green. No lane is running and
no seat is live except the desk. Open reactivegas pull requests: PR85 (#66 S2, superseded by PR88, still
a draft) and PR61 (release-please). Milestone 2 on GitHub: 8 open, 22 closed.

| lane | issue | state | next |
|---|---|---|---|
| t73 | #73 kelgroups substrate (kelgroups #30) | COMPLETE and accepted 2026-11-16: kelgroups PR53 merged at 464bc43, main abe3648; window closed; root /tmp/reactivegas/ms2/t73-opus-20261112 kept (handoffs/resume-final.md, answers A-001 to A-003); worktree removed, remote branch feat/30-vote-machine kept | none for the lane; the issues stay open, see the parked decision |
| t71, t66, t70, t69, t75, t76, t81, t68, t92 | closed issues | complete; windows closed; roots kept under /tmp/reactivegas/ms2 | none |

Not staffed, each a staffing question for the operator: #82 to #84 clients, the rest of #67, #51 first
release.

## Parked decision: how the membership contract is restated

What landed: a pure vote library in kelgroups over a read-only view of the member roster (questions,
ballots, verdicts, closure records, explicit threshold, typed refusals, closure sweep, vote fold). Desk
recount on kelgroups main abe3648: the 7 vote names and GroupView are present; DirectCommand,
PendingBase, IntegratedEvent and foldIntegrated are absent, as are the rest of the 18 names kelgroups
removed on 2026-10-02 (kelgroups PR50).

What is not done, and why reactivegas #73 and kelgroups #30 and #29 are still open: votes are not bound
to the group action line (waits for kelgroups #45, whose children #42, #43, #44 are open); nothing joins
group membership to the economic application; lean/KelGroups/Integration.lean on reactivegas master
still describes the removed single-group architecture. #67 therefore has no accepted membership
interface to build on.

Options to put to the operator (desk recommends the first): re-specify Integration.lean for
member-owned logs as a Lean ticket in reactivegas, then re-cut #73 against it; or close #73 as far as it
went and open the remainder as new tickets; or wait for kelgroups #45 before deciding. Nobody has told
the kelgroups side that #30 was partly done from here (its epic #45 text still says #30 is held); there
is no live kelgroups desk.

What unblocks it: the operator's answer at the desk.

## Debt

kelgroups #54: the store test that interrupts an admission at varying points gave two verdicts on one
tree. Under the standing rule on flaky tests it is marked pending on kelgroups main, naming #54. Until
#54 is fixed, kelgroups CI does not check atomicity under interruption; the insert-failure atomicity
test stays. The test belongs to the kelgroups member-log milestone.

## Priority

The membership decision first, because #67, then #82 to #84, then #51 all build on it.

## Decisions on the record

Operator, 2026-10-01: explicit threshold policy at group setup with no shipped default; renounce by a
non-proposer refused; ballot by a non-designee refused. Desk, 2026-11-08: the L-7 refund uses the same
negative continuation as every closure (a responsabile signs denyPermission); a product note, not a
blocker. Operator, 2026-11-16: #73 builds the library-only vote machine; the hold of kelgroups #45 on
#30 was lifted for that and nothing else.

## Publication

Wiki https://github.com/paolino/reactivegas/wiki/Milestone-2 published at 2fd29c0 (2026-11-16).
Register: Milestone-2-Stories.json in the wiki repository, copied under wiki/ here. The evidence folder
is retained from the September snapshots and not refreshed.

## Constraints carried forward

The hosted Lean mutant ledger job took 97 of its 120 minutes on PR77 (stop line 110). The next slice that
adds ledger rows must first split or shard that job, which overrides the single-job ruling only with the
operator's say. Known model limit from #66: comuneReserved at the state builder; no integrated-to-Reach
bridge (named non-goal). Simulator residuals from #70: no policy sidecar for the economic and integrated
corpora; pledge offers under a stalled comune not exercised. The local checkout /code/reactivegas sits
detached at d9a479c, behind master; the desk does not build there.
