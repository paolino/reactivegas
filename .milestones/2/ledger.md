# Reactivegas milestone 2 — desk ledger

Updated 2026-11-11T12:15Z by the desk in pane %341. Snapshot only; history lives in the journals,
issues, pull requests and git.

## Outcome test

A released coordinator, browser client and command-line client implement the buying-group laws end to
end for one group (milestone description). Nothing of that is released yet. Conformance of the Haskell
code to the Lean model is not measured.

## Authority and team

The 2026-11-08 release lifted the 2026-10-02 pause for milestone 2 only (kelgroups out of scope).
Standing team per ticket, ruled by the operator on 2026-11-08: the ticket owner is Claude Opus 5.5, the
commit owner and the auditor are Claude Sonnet 5.5, effort high. The binding text is
/tmp/reactivegas/ms2/artifacts/TEAM-AND-AUTHORITY-20261108.md, which also carries the rule that every
Claude seat waits through a Monitor loop of wait-channels. Lean work follows the two-slot batch rule
(lean.lock for builds, lean-long.lock for mutation campaigns, 24G each). Seats do not merge without a desk
authorization naming the exact SHA, and they merge only through guard-merge.

## State

Master is 73e9440: #71 design record rewritten to the merged model, with a design-record citation checker in Build and check that fails closed (PR77), merged 2026-11-11T12:07Z, #71 closed; no lane is running. Before that, master ddb1703: #66 S5-C finite-history correspondence (PR116) merged 2026-11-10T23:23Z, #66 closed; no lane is running. Before that, master b73b8c4: #70 Lean-replay oracle (PR115) and pending versus accepted display (PR117) merged 2026-11-10, #70 closed. Before that, master 72bde8a: #75 integrated vote corpus (PR113) merged 2026-11-10T12:01Z on fe5ee0f, where #69 member pledge control (PR114) merged on 222ad9a; #69 and #75 closed. Since 15:21Z #66 (S5-C) and #70 (display + Lean-replay oracle) run in parallel; no order between them, the later lander takes master in. The paragraph below is the state before it.

Master is 222ad9a: #66 S5 mirror auxiliaries (PR112), the last S5 slice, on 77ca1e3, which is #66 S5 statement triage (PR111) and question retention (PR110) on 9fd4dc9, which is #66 S5 observed refusals (PR108, no surviving refusal mutant) on a6285cc, where #70 G3 page checks (PR109), G2 shrinker (PR107), #66 S5 exact inversions (PR103) and G1 generator (PR106) landed on 5418ca2, which is #70 simulator PR105 (bound questions, #68 assent, L-7 refund on the page) on PR104 (claim gate derives its Lean pin), on d9a479c, where #81 L-7 (PR102) landed on 2026-11-09 at 01:05Z, closing #81, on top of 67438a8, where #66 S3 (theorem-keyed mutant ledger, PR98) landed at 17:49Z on top of 17d6a7a, where #76 landed on 2026-11-08 through PR93, on top of the CI runner fix 7cfdd59 (PR100,
closing #101: the new runner image keeps its node under /opt, which the workflows used to delete).
Earlier landings in this tenure of the milestone: #92 (PR97), #81 rows L-1 to L-6b (PR96), the simulator
C1 rebind (PR94), the claim-gate fix (PR99) and #68 (PR80).

| lane | issue | state | next |
|---|---|---|---|
| t71 | #71 design record | COMPLETE: PR77 merged (master 73e9440); #71 closed; window closed; root /tmp/reactivegas/ms2/t71-opus-20261111 kept; old worktree /code/reactivegas-issue-71 and roots t71-design-record* are read-only evidence | none |
| t66 | #66 Lean quality | COMPLETE: PR116 merged (master ddb1703); #66 closed; window closed; root kept | none |
| t70 | #70 simulator | COMPLETE: PR115 and PR117 merged (master b73b8c4); #70 closed; window closed; root kept (handoffs/70-closure-map.md) | none |
| t69 | #69 member pledge control | COMPLETE: PR114 merged at fe5ee0f; window closed; root /tmp/reactivegas/ms2/t69-opus-20261110 kept (handoffs/handoff-70-simulator-display.md for #70) | none |
| t75 | #75 integrated vote corpus | COMPLETE: PR113 merged at 72bde8a; window closed; root /tmp/reactivegas/ms2/t75-opus-20261110 kept (handoffs/DOWNSTREAM-75.md for #66 S5-C and #70) | none |

Not staffed, each a staffing question for the operator: #82 to #84 clients,
the rest of #67, #51 first release, #73 substrate (held on the kelgroups side). PR85 (#66 S2) is superseded by PR88
and still open.

## Priority

The CI fix went first because it broke every pull request. Then #76, because the mutant ledger is derived
from the model and a model change should land before the ledger. Then #66 S3, because it is ready and a
re-derivation is due anyway, and then #81 L-7, which adds the ledger rows for its own new theorems.

## Decisions on the record

Operator, 2026-10-01: explicit threshold policy at group setup with no shipped default; renounce by a
non-proposer refused; ballot by a non-designee refused. Desk, 2026-11-08: the L-7 refund uses the same
negative continuation as every closure (a responsabile signs denyPermission); this is a product note
for the operator, not a blocker.

## Publication

Wiki https://github.com/paolino/reactivegas/wiki/Milestone-2 published at
ecab01c (2026-11-11T12:15Z).
The evidence folder is retained from the September snapshots and not refreshed.

## Priority update 2026-11-10

Operator, 2026-11-10: staff #69 and #75 now, same team. They run in parallel. Merge order is #69 before #75,
because #69 changes the model and #75 freezes a corpus from it (the issue says re-freeze after #69). When #69
lands, wake #70 for the pending versus accepted display. When #75 lands, wake #66 for the finite-history
correspondence and #70 for the Lean-replay oracle.

## Constraints carried forward (2026-11-10)

The hosted Lean mutant ledger took 93 of its 120 minutes on PR116 and 97 on PR77, which did not touch it (stop line 110). The next slice that adds
ledger rows must first split or shard that job, which overrides the single-job ruling of NOTE-D22 only with
the operator's say. Known model limit from #66: comuneReserved at the state builder; no integrated-to-Reach
bridge (named non-goal). Simulator residuals from #70: no policy sidecar for the economic and integrated
corpora; pledge offers under a stalled comune not exercised.
