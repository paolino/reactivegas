# Commit audit — S28-R1 replacement preflight

- Verdict: **AUDIT-CONTRACT-BLOCKED**, reason=R5-C1-lifecycle-entrypoint-gap.
- Worker: commit-auditor-s28r1b; marker AUDIT-S28R1; FULL audit requested,
  one S28-R1 submission. No semantic acceptance or rejection of the candidate.
- Base: `368b596fef0b6d393c2ac7afc631d236c55d86d1`.
- Candidate: `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`.
- Tree: `b4eb37f2187c39db4950072e309c0d125d5c27af`.
- Mandate v2: `eff38e3bca5022a0bcacdbc37deec44766917c755d2ac68fc96c7fdcfdc3c9e0`,
  with the replacement brief and NOTE-024/025/026/028/029.
- Gate v9 full: `dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815`.
- Gate normalized: `3c433effb967052aa91aef2302268c05ab27b3d0f3e54c979504d6978611d340`.
- Spending: **0/12 builds; 0/24 targeted probes**. Charge-zero identity,
  source/contract reads, tool-version recon and local evidence writes only.
- Campaign: OPEN; stopped at command-fit preflight, not set-point, tail,
  capacity or budget exhaustion. No automatic replacement execution.

## Admission identity

Live pane-specific evidence places %567 in reactivegas:11 alongside ticket
owner %534 and Muse subject owner %545. Auditor PID 2708047 is distinct and
its live argv pins Codex gpt-6-astra, model_reasoning_effort=high and the named
checkout. The helper returns codex with muse as author and grok/claude
excluded for the pinned selection. The previous %566 root/process/context
was not reused. This is not a repeat of the previous placement blocker.

The untargeted `tmux display-message -p '#S:#I'` returned reactivegas:15.
The targeted `$TMUX_PANE` query returns reactivegas:11. The tmux skill
explicitly explains that untargeted queries can follow attached-client
focus; the process-bound seat check is the co-location evidence.

HEAD is detached, the worktree is clean, ancestry reaches the accepted base,
and both gate hashes match. START was withheld because the mandatory fit
precondition below is unresolved. The full-brief acknowledgement was written
as an append-only NOTE through status-event, including the user pointer.

## CB-001 — the required R5 lifecycle comparison names the wrong starting-state interface

The replacement brief, lines 48–49, requires a propose→approve→enact trace
with `stepwise validate-then-fold == foldIntegrated throughout + replay
equality`. Its lines 67–69 require stopping before spend when a row lacks a
conforming executable command. The mandatory cases must be conformed to
before discretionary extension (lines 33–35).

The frozen API distinguishes two starting-state interfaces:

- `Fold.hs:458–470`: `foldIntegrated` receives an application payload and
  unconditionally starts with `emptyState initial`.
- `Fold.hs:477–488`: `foldIntegratedFrom` receives a complete `GroupState`,
  permitting a founding member/admin relation.

Direct admission requires an existing admin (`Validate.hs:270–274`), as do
both proposal arms and approvals (`:280–302`); app events require a member
(`Fold.hs:439–452`). Thus no choice of ordinary trace arguments gives the
empty-start entrypoint the founding membership needed for an accepted
proposal/approval/enactment lifecycle. If the independently stepped side
starts from a founding aggregate, equality already fails at its initial
prefix. If it starts empty, proposal/approval events are refusals and the
required accepted lifecycle is absent.

This restriction is not inferred as a new product requirement: preserved r5
§D5, lines 80–99, explicitly identifies the first-member impossibility,
provides founding through the store's initial aggregate, forbids a bootstrap
arm, and leaves `foldIntegrated` taking only `s`. Existing founding-prefix
and replay tests use `foldIntegratedFrom` accordingly.

**Exact gap:** the required successful R5-C1 trace has no conforming call to
the explicitly named `foldIntegrated` entrypoint under the preserved
founding contract. Source/contract inspection supports this fit objection;
no compiled behavioral result is claimed. This is not F1/F2, a newly proved
product defect, a shortage of allocation, or a claim that the lifecycle
cannot be exercised through the founding-aware API.

Q-001 proposes an explicit mapping correction: use `foldIntegratedFrom` for
the independently checked accepted lifecycle and persisted-row replay, and
retain a separate empty-start `foldIntegrated` refusal-prefix comparison.
Both can remain within the planned one compilation/one run allocation;
the proposed instrument has not been authored or compiled, so this is not
a completed fit certification. No candidate edit or budget increase is
requested. Returning the gap follows the brief's fit-first stop; silently
substituting the entrypoint or presenting all-refusal equality as lifecycle
coverage would not discharge its mandatory case.

The commit-auditor skill says: “Use `AUDIT-CONTRACT-BLOCKED` when the mandate
itself is contradictory or unverifiable”. The shared auditor skill requires
exact scope, claims and commands before execution. The direct stopping
authority here is the replacement brief.

## Coverage and limits

All six requirements and five reliances remain unjudged in this run; see
REQUIREMENT-LEDGER.md for carried states and per-row coverage. R1/R3/R5 are
not converted to residuals. The complete accepted-base-to-candidate diff,
full failure-mode assessment, actual automation path, full gate, all seven
mutants, repaired StoreProbe, row-4 shadow tests and new concrete traces
were not completed or executed after this preflight objection.

Read material includes the complete replacement brief and named commission
notes, plan v2, gate body, both prior terminal reports, prior requirement
ledger, owner SUBMISSION and BINDING/freezing journal entries, retained
StoreProbe/Row4Probe and receipts, and relevant candidate API/demo/test
sections for constructing the command map. Owner GREEN and SLIM artifact
hashes are bound in the input manifest; their summaries do not count as
independent candidate checks. Full raw SLIM/owner-GREEN logs were not read
end to end in this blocked run. Submission's stale hash/spend prose is
superseded by the dated correction/validation journal, not adopted as live
spending. No inherited execution was repeated or refunded.

Failure modes altered: **NOT FULLY EXAMINED**; no correctness or preservation
claim. Candidate invariants, semantic blocking findings, advisories and onward
discoveries: none issued. Broader command-fit adequacy remains outstanding;
identifying CB-001 does not certify every other proposed probe.

S28-1 owner 34/34 and audit 9/12+7/24 remain historical spent allocations,
including their recorded funding history. S28-R1 owner 13/16 and the prior
failed-admission auditor's 0/0 are parent-recorded, separately retained.
This replacement adds 0/0 and assumes no new ceiling grant. The new campaign
is explicitly commissioned by NOTE-024 and replacement authorized by
NOTE-029; generic campaign caps are not used to reopen that settled authority.

## Receipts and retention

evidence/command-receipts.json contains mechanically captured argv, cwd,
timestamps, exits, durations and output hashes for eleven fresh identity
checks. The detached check's exit 1 is expected absence of a symbolic branch;
it is not a test failure. Earlier interactive recon included a version batch
whose enclosing exit 1 came from absent optional python3/GHC_PACKAGE_PATH
lookups; every displayed gate tool pin matched. That recon is not a build,
probe, provider refusal or semantic failure, and was not repeated as a test.

The source and authority snapshots are bound in evidence/input-manifest.json.
EVIDENCE-INVENTORY and its JSON companion bind the retained handoff artifacts;
the final STATUS events bind the report, ledger and inventory hashes.
No build trees were created, retired or cleaned; reclaimed bytes=0. The
detached worktree remains unchanged under ticket-owner control. No author
contact, process move/kill, remote write, candidate repair, staging, commit,
push, merge or memory update occurred. The ticket owner owns the Q-001
disposition and any subsequent commissioned execution.
