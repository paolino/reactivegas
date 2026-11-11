# Design content handoff for #71 — #68 proposer-is-not-an-assent (V-2)

Status at handoff: implemented in branch `feat/68-proposer-assent`
(HEAD 7952759, PR #80 draft, UNMERGED — final record must track merged
semantics; re-verify against master at publication time). Authoritative
sources: operator ruling questions/A-V2-AND-PLEDGE-AGENCY.md (V-2), issue
#68 body, desk A-001, branch Lean at 4cdb6078 content.

## Ruled semantics (publishable)

- A proposal opens at ZERO assents on both base channels
  (`KelGroups.Fold.applyProposeDetailed` → `pendingProposals`;
  `KelGroups.Integration .propose` → `pendingBase`). The proposer
  signature contributes none.
- Majority arithmetic `(adminCount + 1) / 2` (`KelGroups.State.majority`)
  is UNCHANGED (`majority_table`: 0,1,1,2,2,3).
- Above one admin, the proposer cannot supply a counted assent: refused
  at the validating boundary (`validateApproval`, `validateBaseApproval`)
  under `ValidationError.proposerSelfApproval key proposalId` — distinct
  from `alreadyApproved` (duplication) by meaning and payload.
- Exactly one admin: propose pends, then the SAME sole admin's separate
  `.approve` enacts (agency preserved; one-event enactment gone as
  incompatible with opens-at-zero).
- Enactment sets are other-assents: n=2→1, n=3→2, n=5→3; threshold read
  from current canonical membership at enactment time (admission-driven
  count changes move it).
- This aligns the base channel with the vote machine, which already opens
  questions with empty tallies (`Vote.Fold`, "divergenza deliberata dal
  legacy"). The vote machine's parameterized threshold got NO new default.

## Boundary architecture (F-01 resolution, publishable with care)

- Enforcement lives at the validating boundary, never in the raw fold:
  `foldGroup` still executes raw events without validation;
  `foldIntegrated` drops refused events, leaving state unchanged.
- Governance preservation (`WellFormed`, count-indexed) holds for
  boundary-admitted historical traces (`TraceAdmissible`) and successful
  integrated transitions. The raw fold keeps unconditional structural
  guarantees only (key uniqueness, member coherence, approvals-Nodup,
  threshold-evidence shape, app isolation, duplicate idempotence).
- The excluded class (raw self-approval above n=1 and stale aftermath) is
  exhibited by a retained 7-event executable regression in `Tests.lean`,
  refused at the boundary with the exact error identity.

## Explicitly unchanged (do not let the record imply otherwise)

- Majority arithmetic; vote machine; economics/Step; sealed hook path
  (still runs on every commit); direct-only admission; one membership
  store; S2/quota (163 held); #69 pledge agency (separate slice).
- Inversion coverage 14/14 structural; exact-premise 11/14 with three
  inherited stall omissions (pledge/acceptPledge/closePurchase) owned by
  the inversion backlog (#66 S5), not this slice.

## Pointers for the record author

- Ticket runtime /tmp/reactivegas/ms2/t68-proposer-assent (brief, STATUS,
  gate backups, acceptance-scope-reconciliation, auditor reports ×2).
- Audits: submission-1 codex FINDINGS (scope + provenance) → single
  authorized repair → submission-2 grok FULL PASS, blocking 0.
- Reconcile against MERGED master symbols at publication; content handoff
  is not accepted fact merely because stated here.
