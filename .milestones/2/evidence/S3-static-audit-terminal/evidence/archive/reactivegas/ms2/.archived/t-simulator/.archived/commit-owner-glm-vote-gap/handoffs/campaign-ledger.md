# Campaign ledger — commit-owner-glm-vote-gap

- slice: expose unmodelled governance votes (operator correction NOTE-009)
- base: 9c521e56b742475051d9b15a24832bf83c5ccc4e
- builds_spent / builds_budget: 0/3 (the later auditor spends these; owner
  runs are the free readiness + frozen-gate runs priced in round 2's pattern)
- ceiling_raises: 0/2
- submission cap: 2 (submission 1 = this slice; a repair only after a
  ticket-owner-forwarded AUDIT-REPAIR-AUTHORIZED)
- gate: handoffs/verify-vote-gap.mjs (frozen before implementation; RED run
  against clean base; mutation controls verified applied before each run)
- regression gate: archived round-2 verify-round2.mjs requiring 197/197
- draft: NONE
