# Skill reload — audit dispatch preflight and launch-attempt bound

Reload the current shared skills at exact llm-settings revision
`4981cd80f4571c94d0f695e5670fd034250c700f`:

- `orchestrator-contract`
- `ticket-orchestrator` when owning an issue-backed implementation
- `auditor` and `commit-auditor` before commissioning or occupying the next
  commit-audit seat

The change fixes a measured factory failure without changing the single
execution budget:

1. Before any auditor CLI is launched, the commissioning owner writes one
   hash-bound preflight receipt with per-packet sections and proves the complete
   environment exists: exact detached candidate, referenced inputs/hashes,
   each worktree's own runnable gates, independently evidenced owner/author
   identity, current campaign and row ledgers, non-overlapping reservations,
   report/evidence paths, denominator, stop rule and launch authority.
2. One auditor CLI invocation consumes one launch attempt. Pane creation does
   not consume another; restarting the CLI does. NEVER-STARTED, invalid and
   zero-execution contract-blocked launches remain charged. Returned execution
   allocation does not refund the launch.
3. Default submission 1 topology is the initial parallel set plus one aggregate
   corrected redispatch for all commissioning blocks. Default submission 2 is
   one initial delta seat plus one corrected redispatch. A recovery requires
   evidence that the commissioning defect changed; a second block stops the
   chain.
4. An auditor preflight returns all detectable missing or inconsistent inputs
   together instead of serial one-field blocks.

For an active frozen campaign, do not retroactively alter candidate scope,
execution ceilings, acceptance rows, family restrictions or prior evidence.
Before its next auditor launch, record a prospective amendment with the actual
delivery time, retain every historical launch, and freeze the remaining
per-submission attempt authority. If no launch remains in the current
authorization, return the exact question instead of inferring a new seat.

Validation at the published revision:

- quick_validate PASS for all four skills
- commit-auditor collector regression suite PASS
- tabletop dispatch-loop cases 4 and 5: 3/3 expectations each by an independent
  read-only reviewer
- remote `origin/main` equals the exact revision above

Acknowledge this note in your own STATUS using the existing `NOTE` tag and
route the applicable prospective amendment only through your immediate child
at its next safe boundary. Do not interrupt an in-flight product command or
auditor.

