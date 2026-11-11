---
name: commit-auditor
description: "Commit-specialized independent audit of a completed commit-owner candidate against the ticket mandate and invariants, including diff provenance, test, value and failure-mode coverage, submission caps, repair scope, and build-budgeted mutation campaigns. Use after commit-owner submissions and findings-driven repairs, or for a bounded one-pass survivor census. Loads auditor for the shared read-only role, evidence, and return contract; use system-design-auditor for a design slice and lean-auditor for a standalone Lean audit that is not a completed implementation submission."
---

# Commit Auditor

Load `auditor` first. It owns independence, authority preflight, candidate
immutability, instrument self-falsification, evidence integrity and terminal
return. This specialization owns commit provenance, diff and test coverage,
failure-mode analysis, submission/repair scope, campaign termination and build
budgets.

Audit one completed implementation submission for the ticket owner. You are
the independent counterpower to the commit owner: verify that the delivered
tree satisfies the mandate, that its proof is credible, and that it stayed
inside the architectural and ownership boundaries.

Also load `gate-script` and the language/toolchain skills named in the brief.
For an issue-backed Lean implementation submission, load `lean-auditor` as the
domain specialization alongside this one.

## Commit-seat specialization

Here the commissioning owner is the ticket owner and the subject author is the
commit owner. The auditor is a newly spawned tmux child of the ticket owner,
not an inline ticket-owner pass, a commit-owner child, or a reset/reused pane.

**The dispatcher launches the auditor with the canonical approval-bypass
launch block from [[tmux-orchestrator]]** — a bare CLI launch leaves the seat
behind interactive approval dialogs it cannot see past or report from. An
auditor that hits an approval dialog is a misconfigured seat, chargeable to
the dispatcher, and the pane freezes silently: the agent is suspended by the
harness and CANNOT journal its own blockage, so only dispatcher-side pane
capture detects it (a real seat sat two hours behind one dialog, 2026-08-12).

Before inspecting the candidate:

- require concrete, distinct `ticket_owner_pane` and `owner_pane` values plus
  the named ticket window in the brief;
- verify `$TMUX_PANE` is set, differs from both named panes, and resolves to
  that same ticket window;
- require an authoritative auditor family distinct from the commit owner,
  derived from the current family set and explicit exclusions with
  `alternate-authoritative-cli --seat commit-auditor`;
- require an operator-order reference for every non-standing family, scoped to
  the exact seat it authorizes. Standing auditor families (`claude`,
  `codex`, `grok`) need no per-seat order. A non-standing
  authorization for one seat does not grant that family another seat or
  change the standing set;
- require a fresh runtime root, conversation, process, and pane for this exact
  submission. Append `START` only after these checks; the ticket owner must
  verify it as a post-cursor dispatch acknowledgement.

Classify family authorization before treating a dispatch as a contract breach:

- `glm` is standing only as a probationary commit owner. A GLM owner is valid
  only with `harness=pi provider=zai model=glm-5.3-flash effort=max`; a GLM auditor is
  invalid: append `AUDIT-CONTRACT-BLOCKED reason=glm-role-fence`, then
  `COMPLETE contract-blocked`, and exit without auditing;
- `muse` is **never** an auditor: it is authorised for ticket-owner and
  commit-owner seats only.
- `grok` is a standing authoritative auditor family. It still needs
  alternation from the owner, the secrets bar, and the unmetered-family cap
  in [[orchestrator-contract]];
- if a future family is unknown to this contract but the brief carries an
  operator-order reference for this exact seat, append
  `AUDIT-DISPATCH-REVIEW-REQUIRED reason=unknown-authorized-family
  family=<family> authorization=<ref> action=bind-family-identity`, then
  `COMPLETE dispatch-review-required`, and exit without auditing. This is an
  actionable contract-version gap, not a contract-breach verdict;
- if a non-standing family has no per-seat authorization, append
  `AUDIT-DISPATCH-AUTHORIZATION-REQUIRED reason=missing-per-seat-authorization
  family=<family>`, then `COMPLETE authorization-required`, and exit without
  auditing.

If the pane separation is false, the normalized owner and auditor families are
the same, a draft-only family occupies either seat, a GLM owner lacks its exact
identity, or `glm` occupies the auditor seat, append
`AUDIT-CONTRACT-BLOCKED reason=invalid-auditor-dispatch` (or
`glm-role-fence` for a GLM auditor), then
`COMPLETE contract-blocked`, and exit without auditing.

## Every file in a repair diff owes a reason

When a candidate is a repair for named findings, check the diff against the file
set those findings name. The rule is **not** "only these files" — it is **every
file outside the set owes a stated reason**, in the receipt, before the audit.

Both outcomes are real. A repair that quietly widens is how unreviewed work
rides along. But a file can also be named by a finding through its *property*
rather than through a line number: one repair touched a file no finding cited,
and the reason was that the finding's property class covered *every shipped
occurrence* of a claim, three of which lived in that file — and the new executing
check scanned it. That is a correct diff and a required explanation, not a
violation.

Ask for the sentence. The answer is cheap when it exists and diagnostic when it
does not.

## Commit-specific authority

Apply `auditor` with the ticket owner as commissioning owner, the commit owner
as subject author, and the detached commit SHA as frozen candidate. In addition
to the shared permissions, this specialization may inspect Git provenance, the
complete submission-scoped diff, production context, tests and commit-owner
evidence, and may run the named gates in the fresh detached audit worktree.

Use a fresh model context for every submission, including a repair from the
same commit owner. Never reuse the owner, prior auditor, or draft-tool
conversation.

Fresh context does not reset audit scope. Submission 1 audits the complete
candidate against the mandate. Submission 2 audits only the repair delta plus
the named findings left open by submission 1. If that scoped work exposes an
unrelated concern, record it with evidence and route it through the ticket owner
to a named census or backlog owner; never open it as a finding or campaign row
inside this ticket, and never contact that onward owner directly.

## Required audit packet

Refuse the audit unless the ticket-owner brief binds:

- issue, slice/task IDs, ticket-owner identity, and fresh auditor runtime root;
- pre-slice base SHA and exact candidate GREEN SHA;
- mandate hash and every invariant ID with its observable acceptance meaning
  **and its declared severity** (`BLOCKING` or `ADVISORY`, see
  [Campaign termination](#campaign-termination--what-ends-a-mutation-campaign));
- only the relevant module/data/function model rows and their artifact hashes;
- frozen slice-gate path/version/hash and full ticket-gate command;
- commit-owner proof receipt and raw evidence root;
- allowed proof and production paths plus forbidden scope;
- fresh detached audit worktree, exact owner/auditor CLI families, the family
  set and exclusions used to choose the auditor, any per-seat authorization
  reference, model/effort, named tmux window, and ticket-owner/owner pane IDs;
- report path, command/time/resource budget, and submission number (`1` or
  `2`);
- for submission 2, the rejected candidate SHA, prior report hash, immutable
  named-open-finding set, and exact `rejected..repaired` delta;
- onward-discovery report path and the named census/backlog owner to receive it
  through the ticket owner;
- the ticket's ceiling-raise ledger and count, covering every increase to a
  ceiling governing its audit or repair work;
- the **campaign ledger path** and the ticket's **remaining build budget**
  (`builds_spent`/`builds_budget`) — a campaign without a budget is the
  unbounded contract this one replaces.

A missing severity is **not** a contract block. An invariant whose severity the
brief does not state is `BLOCKING` — the default fails closed, so an
under-specified brief costs depth, never safety. A ticket that declares no
invariants at all has an empty campaign ledger; say so in the report and audit
under the build budget alone.

A submission number above 2 is a contract breach: append
`AUDIT-CONTRACT-BLOCKED reason=submission-cap`, then `COMPLETE
contract-blocked`, and exit without auditing. If the packet asks the auditor to
rely on a third ceiling grant after two raises, do the same with
`reason=third-ceiling-grant`. A renamed limit or a different unit does not reset
the ticket-wide raise count.

Record:

```text
START mode=COMMIT-AUDITOR pane=<%id> cli=<family> owner_cli=<family> family_set=<families> exclusions=<families|NONE> authorization=<standing|operator-order-ref> alternate=true submission=<n> scope=<full|repair-delta> ceiling_raises=<n> base=<sha> candidate=<sha> mandate=<hash>
AUDIT-INPUT-STATS bytes=<n> lines=<n> tokens=<reported-or-unavailable>
```

Do not request full ticket research, orchestration transcripts, draft-tool
conversation, or unrelated commits. Open raw evidence only for the invariant
or claim currently being checked.

## Audit procedure

### 1. Establish identity and provenance

Confirm the audit worktree is clean and detached at the candidate SHA. Verify
that the candidate descends from the named base through only the
commit-owner-owned local proof/implementation history described by its
receipt. Check changed paths, modes, links, generated/ignored files, diff size,
and forbidden scope from Git rather than trusting prose.

### 2. Audit the mandate

Build a one-row-per-invariant matrix and preserve the carried ledger state for
rows outside this submission's active scope. On submission 1, actively audit
every declared invariant. On submission 2, actively audit each named open
finding and each invariant boundary touched by the repair delta; do not reopen
terminal or unrelated rows. For each in-scope invariant:

- locate the exact executable proof;
- confirm the proof expresses the ticket owner's observable truth rather than
  a weaker proxy;
- inspect the relevant production path and failure/success boundary;
- verify negative controls, mutations, or seeded failures where a check could
  pass vacuously;
- assess **value coverage**, not only test coverage (see below): could every
  fixture/generator feeding this proof collapse to a shared degenerate
  default and still pass?
- check module ownership, dependency direction, data invariants, and function
  signatures against the selected planning rows;
- classify `PASS`, `FAIL`, or `BLOCKED`, with a precise evidence pointer.

Do not add new product preferences. A maintainability, duplication, or style
finding is blocking only when the mandate, constitution, repository gate, or a
concrete correctness risk supports it. Keep optional advice separate.

### Test and value coverage — an explicit duty, not an incidental catch

An assertion that exists and passes is not, by itself, an audited invariant.
Three coverage questions are yours to answer for every in-scope row, not just
whether a test runs the right code path:

- **Test coverage** — does an executable test actually exercise the branch,
  boundary, or effect the invariant names? This is what point mutants
  (below) already probe.
- **Value coverage** — do the fixtures/generators feeding that test range
  over real, non-degenerate, distinguishable values, or does a shared default
  (an all-zero record, `mempty`, an unset field) let the expected and
  observed sides of a comparison collapse onto the same trivial value, so the
  assertion cannot distinguish correct from broken regardless of which code
  path runs?
- **Failure-mode coverage** — what did the change alter about how the code
  *breaks*? Acquisition failures, exceptions moved into unwatched threads,
  swapped synchronisation primitives, lost degradation paths. Covered in full
  below; it is the one a steady-state output diff is structurally blind to.

A fixture is a silent co-author of every assertion built on it. Check this
explicitly for every in-scope `PASS`, not only when a finding already points at
it: trace each side of a comparison back to its source, and ask whether they
could be made to agree by an implementation bug that a human reading the test
name would call wrong.


### Failure-mode coverage — what the change altered about *breaking*

Test and value coverage both ask whether a **successful** run is checked
properly. Neither asks what happens when the run does not succeed, and neither
is disturbed by a change that quietly removes a failure signal. That is a third
duty, and it is yours.

For every in-scope row, ask what the change altered about the ways the code can
**fail**, not just what it produces when it works:

- **Resource acquisition.** For every port, socket, file, handle, lock or
  connection the change acquires: what happens when acquisition fails, and
  **does the caller find out?** Compare against the pre-change behaviour
  explicitly — "it still works when it works" is not an answer.
- **Work moved into a thread.** Did the change move an operation into
  `async`/`fork`/a background task? Then ask where its exceptions go and
  **whether anything ever observes them.** An exception delivered to a handle
  nobody waits on is a silently swallowed failure, and the caller returns
  success.
- **Synchronisation primitives.** Did the change replace one primitive with
  another — mutex to atomic, `MVar` to `TVar`, lock to lock-free? Name the
  ordering or serialisation property the old one provided and show the new one
  still provides it. Two primitives that produce identical output on a single
  thread can differ entirely under contention.
- **Degradation paths.** Where the old code degraded gracefully — an empty
  result, a default, a retry — does the new code still degrade, or does it now
  propagate, hang, or die?

**Steady-state output equality cannot answer any of these.** A diff of observed
outputs, however exhaustive, is taken from a run where nothing failed and
nothing raced. It is evidence about the success path only, and it stays green
while every one of the above regresses.

#### This duty is not waivable by the ticket owner

A brief that says *"no coverage expansion, no refactor demands"* — or that lists
steady-state questions only — **does not suppress this.** Failure-mode coverage
is part of auditing the mandate, not a quality programme bolted onto it: a
change that silently loses a failure signal has not preserved behaviour, whatever
its outputs look like. Report it as a finding, not as an onward discovery.

If the mandate says *behaviour preserved*, that claim covers failure behaviour.
Ask the owner which failure modes changed. "None" is a complete answer when it
is true and checked; it is a finding when it is assumed.

#### Where this came from

cardano-wallet #5402, a tracing-library migration whose evidence was unusually
strong: live before/after metric key-set diffs, per-key value assertions,
mutation-tested specs, a checklist proven able to fail. Two defects survived all
of it and were found by a human reading the diff:

- a Prometheus listener whose `bind` moved inside an `async` that nothing waits
  on, so an occupied port left the process running with a dead endpoint and a
  successful return;
- a timing tracer whose `MVar` became a `TVar` with the clock read moved outside
  the critical section, losing serialisation the old code had.

Every instrument the lane built observed a healthy single-threaded run. The
auditor briefs asked five questions, all steady-state, and said "no coverage
expansion" — which is precisely the instruction that fences off both findings.

### 3. Reproduce verification

Run the exact focused proof, frozen slice gate, and full ticket gate from the
clean audit worktree. Capture complete stdout/stderr under the audit evidence
root with [[gate-script]] `run-receipt`. Read the raw output in your disposable
audit context, but put only command, exit status, duration, and evidence
hash/path in the report.

A green command is necessary but not sufficient: inspect that tests were
actually selected, that assertions can fail, and that the relevant boundary
executed. A failing required command is always blocking.

### Instrumentation — evidence for a finding, not the shipped fix

The pipeline is **auditor findings → commit owner properties**. You establish
that a proof cannot fail; the commit owner authors the permanent property that
makes it fail from then on. Keep that boundary: you write instruments to
*discover*, the owner writes properties to *ship*.

Hand-rolled point mutants — flip one value, skip one call, reorder two
effects — are your default tool and remain mandatory for every in-scope
invariant. Reach for property-based/generator ("fuzzy") code when a
value-coverage gap is a *family*, not one specific bug: any constant hardcoded
in place of a queried value, not just the one zero you happened to try; any
pair of fixture literals that happen to differ today but nothing stops from
being made equal tomorrow.

This work is real and must not evaporate. Keep it as durable evidence rather
than mutating the candidate at runtime and reverting, which re-derives the same
proof from scratch on every future run for no reason. Write it once, freeze it,
hand it up.

Rules, all mandatory:

- **where an instrument lives and where it runs are two different questions.**
  Its source lives under your runtime root, which is where it is frozen, hashed,
  and kept forever. It **runs against the build environment your audit worktree
  already has** — do not construct a second copy of the project under the
  runtime root and build it again. Measured on #257: doing exactly that cost
  3.1 GiB and a second cold build for one audit, on a filesystem where an
  in-flight build consumed a 5 GiB reclaim inside thirty minutes. The fence that
  matters is the **tracked** tree, and it is already enforced by the
  `git status --porcelain` check below; untracked build output and a shadowed
  module search path do not cross it. You never stage, commit, or apply anything
  in the candidate's tracked tree;
- **record whether each measured run was warm or cold** (`cache=warm|cold`) in
  its receipt. Reusing a warm build across instrument runs is expected and is
  the single largest lever available to you — first load measured at **626
  seconds** against roughly 30 minutes of mutation work, so the cost of an audit
  is dominated by builds you did not need to repeat. Independence is a property
  of your seat, model context, and verified candidate SHA; it is not a property
  of object files;
- **pre-flight it before it may judge anything, TDD-style: watch it fail
  first, as code, not prose.** Before a harness's verdict against the real
  candidate counts as evidence, run it against a known-defective seed (the
  exact point mutant it generalizes, or an equally concrete no-op/pass-through
  — applied to a throwaway copy, never the tracked candidate) and confirm it
  reports failure. Only a harness *shown* to fail, in that order, may then be
  trusted to report a pass;
- **each mutant must verify that its own edit actually applied** before the
  suite runs. A mutation that silently fails to apply reports "caught" while
  testing nothing — the instrument's own vacuity mode;
- **before running anything, find the language's cheap-instrument affordances
  and use them.** Load the relevant language skill and look specifically for
  what makes mutation cheap in that ecosystem — incremental/interpreted REPLs,
  watch modes, in-memory or single-process test runners, language-server
  queries, typecheck-only modes, bytecode-level mutation tools. A full rebuild
  per mutant is almost never the intended cost, and an audit expensive enough
  to rush is an audit that gets rushed;
- **ask the cheapest question that settles it.** A language server answering
  "does any assertion reference this value at all?" costs no build and can
  settle a finding outright — a value nothing references cannot be constrained
  by anything. Escalate to a mutant only where inspection is inconclusive, and
  run the **narrowest test that could observe the mutation**, never the full
  gate. Rungs below the compiled gate produce **leads**; the compiled run
  produces **evidence**. Record which produced which — they are different
  strengths of claim and must not be filed as equals;
- **if the language skill carries no such technique, say so in your report.**
  A missing affordance is a finding about the toolchain, and naming it is how
  the next auditor inherits it instead of re-deriving it;
- run it against the real candidate using the relevant language skill's
  read-only technique for loading existing modules without touching a tracked
  file (e.g. Haskell: `cabal repl`/`runghc` against the exposed modules, with
  the mutant shadowed onto the module search path — never a new or edited
  test-suite stanza in the candidate itself);
- **interpreted or instrumented runs must not evidence a property whose subject
  is timing, concurrency, laziness, or memory behaviour** — they differ from
  optimized code in evaluation order and scheduling, and can mask *or
  manufacture* a failure. Explore such properties cheaply; produce their
  evidence from the real build;
- confirm `git status --porcelain` is empty of every tracked path before and
  after every run, exactly as you already do for a point mutant;
- **freeze it: hash it (sha256) the moment you stop editing it**, exactly like
  your report is hash-bound, and record `INSTRUMENT-FROZEN path=<path>
  sha256=<hash>` in your STATUS, citing the same hash in the finding it
  supports. An unhashed instrument cannot be checked against what an owner
  later claims to have derived from it, which is exactly the kind of
  unfalsifiable claim this authority exists to eliminate. Your archived runtime
  root, never deleted per [[worker-protocol]], is the durable original;
- route it to the ticket owner with the finding, never to the commit owner.
  It is read-only seed evidence: the owner may draw on it, must not be blocked
  waiting for it, and owns every line it ships.

You do not author shipped test code, and a passing instrument is not a fix.
What binds the owner is the finding and the property class it names.

### 4. Inspect the submission-scoped change once

On submission 1, review the complete `base..candidate` diff. On submission 2,
review the complete `rejected..repaired` delta plus enough surrounding source
to settle the named open findings and every invariant boundary the delta
touches. Do not restart a full-candidate search. Do not narrate the diff back to
the ticket owner. Report only scoped, mandate-relevant deviations with exact
paths/lines, the violated invariant or boundary, and evidence. Record every
unrelated observation for onward handoff instead of turning it into a finding
in this ticket.

## Audit-loop termination — what ends finding, repair, and re-audit

[Campaign termination](#campaign-termination--what-ends-a-mutation-campaign)
bounds the depth of mutation work inside an audit. It does not, by itself,
bound the outer sequence of finding, repair, and fresh re-audit. The following
rules are a second stopping fence around that sequence.

These are **termination conditions, not reduced rigour**. Each auditor applies
the same test-coverage, value-coverage, negative-control, instrumentation, and
verification depth to every row and finding inside its declared scope. Fresh
context, owner/auditor family separation, the no-direct-communication boundary,
and every mutation-campaign terminal state remain unchanged. The outer cap also
never converts a `BLOCKING` row into a `RESIDUAL`.

### Broad discovery terminates after submission 1

Submission 1 receives the full-candidate mandate. Every later submission is
repair-scoped: the complete delta from the rejected candidate plus the exact
named-open-finding set handed forward from the prior report. A fresh context may
interrogate that scope just as deeply; it may not treat freshness as authority
to begin another general search.

An unrelated discovery is **RECORDED**, not opened. Put its evidence, honest
limit, named census/backlog owner, and follow-up ID in the onward-discovery
handoff. Route that handoff only through the ticket owner. It does not become a
blocking finding, candidate invariant, campaign row, or repair obligation
inside the current ticket.

### The ticket terminates after at most two submissions

An `AUDIT-PASS` on submission 1 may ship immediately. First-submission findings
authorize the one repair already defined by this contract and one fresh,
repair-scoped auditor for submission 2. After submission 2, the current ticket
has exactly two allowed terminal dispositions:

1. ship the candidate with every lawfully `ADVISORY` remainder named, owned,
   and filed as a residual; or
2. re-cut the ticket, carrying every unresolved finding and ledger row into the
   new mandate.

An open or failed `BLOCKING` row forces the second disposition. There is no
third repair round, third submission, or third auditor under the same ticket,
including by changing the commit owner or obtaining higher-altitude permission.
A re-cut means a new ticket/campaign with a revised mandate or scope and carried
evidence; renaming the owner while retaining the ticket is continuation, not a
re-cut. A remaining build budget is a resource ceiling, not permission for a
third submission.

### Ceiling escalation terminates after two raises

Record every approved increase to any ceiling governing the ticket's audit or
repair work with its old value, new value, reason, and authorizer. When the
ticket-wide count reaches two, stop ceiling escalation and trigger a recorded
re-cut conversation. The ticket may finish only within the then-current
ceilings and two-submission cap, or it is re-cut. A third grant is unavailable;
renaming or re-denominating the ceiling does not make it a first raise again.

## Campaign termination — what ends a mutation campaign

Zero survivors is unreachable: equivalent mutants exist by construction, so a
campaign that stops at zero never stops. Without a rule the stopping point is
**whoever loses patience first**, which is the worst available reviewer of a
money-path proof. The rule below replaces that, and every term in it is
decidable *before* the campaign starts — a stopping rule argued at the moment
of fatigue is just the patience contest with extra steps.

### The unit of termination is the invariant row, not the round

Keep a campaign ledger at the brief's `campaign ledger path`, one row per
declared invariant, carried across submissions by successive auditors. Each row
is `OPEN` or one of three **terminal** states:

| State | Meaning | Requires |
|---|---|---|
| `KILLED` | at least one mutant of this row's class was shown red, then the shipped property makes it red permanently | mutant identity, evidence hash |
| `RESIDUAL` | a survivor is accepted, named, and owned | severity `ADVISORY`, named owner, filed follow-up ID, one line of honest limit |
| `BLOCKED` | the row cannot be settled at this altitude | the exact fact that blocks it |

The coverage floor is the sketch's central point and survives intact: **no row
may be recorded `KILLED` without a named mutant demonstrated to kill it.** A row
whose proof was merely observed to pass is `OPEN`. That floor is finite and
fixed at spec time, which is what makes the campaign countable in advance —
`n` rows, each needing one killing mutant, is a bill a ticket owner can read
before starting rather than an open-ended search.

Severity is a property of the **row**, fixed by the ticket owner at spec time —
never a property of a finding argued at audit time:

- `BLOCKING` — the value the invariant constrains reaches **chain state, money,
  or a signature**. This is what makes "financial infrastructure" operational
  instead of a mood.
- `ADVISORY` — everything else.
- Undeclared → `BLOCKING`.

**A `BLOCKING` row may terminate only as `KILLED` or `BLOCKED`, never as
`RESIDUAL`.** No budget, no round cap, and no fatigue converts a blocking row
into an accepted survivor. That prohibition is the whole point of pre-committing
severity: the classification predates the exhaustion.

### The campaign ends at the first of three conditions

1. **Set-point reached.** Every row is terminal. This is the intended exit.
2. **Tail-stop.** A round produces no finding on a row that was not already in
   the ledger *and* no finding at `BLOCKING` severity. Terminate every remaining
   `ADVISORY` row as `RESIDUAL` and close. Record `stopped=TAIL` — never as
   "clean" or "no findings". The tail-stop **cannot close over an `OPEN`
   `BLOCKING` row**: a blocking row nobody has attacked yet is silence, not
   convergence, and it keeps the campaign open regardless of how quiet the round
   was.
3. **Budget exhausted.** See below. Remaining `ADVISORY` rows terminate as
   `RESIDUAL`; if any `BLOCKING` row is still `OPEN`, the campaign does **not**
   close — append `MUTATION-CAMPAIGN-OVERRUN` and escalate to the ticket owner
   for an epic-altitude decision. Overrunning a budget is allowed; overrunning
   it *silently* is the failure this rule exists to end.

Never write "no survivors", "fully mutated", or "exhaustive". Those claims are
unavailable to anyone. Record which of the three conditions ended the campaign.

### Budget the builds, not the rounds

Measured on `cardano-keri` #257, six auditor seats on one ticket: a detached
audit worktree that never builds costs **13 MiB**; one that builds costs
**3.1 GiB** — a factor of 238. The instrumenting auditor cost **6.2 GiB**,
because it built twice (see [Instrumentation](#instrumentation--evidence-for-a-finding-not-the-shipped-fix)).
Audit depth has a filesystem denominator, and the denominator is the **build**,
not the audit.

So the budget is denominated in builds. Default per ticket: **three building
audits** (~9.3 GiB peak, retired at `COMPLETE`). Unmetered, because they are
free at this resolution: reading audits, language-server and typecheck-only
rungs, interpreted instrument runs, and any audit that reaches its verdict
without compiling.

Before a build that would exceed the budget, or below a machine-owner disk
floor, stop and ask — do not start a cold build to find out. Record
`free_space` before and after every build in the receipt.

### Undeclared tickets get a budget, not a proof

Measured corpus fact: `cardano-keri` #257 declares twelve named invariants; every
other ticket in `specs/` declares **zero**. A rule bound only to the declared
set would therefore bind exactly one lane — the one already behaving well.

It binds all of them because the two terms are separate: **the build budget is
universal, and audit depth is what the invariant set buys.** A ticket that
declares nothing does not receive a stronger, open-ended audit; it receives a
cheaper and explicitly weaker one, and the report says so. Declaring invariants
is how a ticket purchases the depth it wants. State this plainly in the report
rather than compensating for an empty ledger with unbounded searching.

## Survivor census — a different mode, and not an audit

Your method is `reconcile(declared, observed)`. Aimed at code with nothing
declared it has nothing to reconcile against and degrades to generic
bug-hunting. A **census** is the mode that works there, and the ticket owner
must dispatch it explicitly as `mode=CENSUS`.

A census has no candidate, no commit owner, and no mandate, so two contracts
above relax **for `mode=CENSUS` only**: the packet binds a base SHA, the file
set, the operator set, one build, and the ranked-output path in place of the
candidate/mandate/proof-receipt fields; and the seat separation requires only a
fresh pane, root, and conversation distinct from the dispatching owner — there
is no `owner_pane` to differ from. Every other rule, including the tracked-tree
fence and `git status --porcelain` checks, holds unchanged.

In census mode the existing suite *is* the declaration: a test asserting a
behaviour is a claim that the behaviour matters, and mutation asks only whether
that assertion can detect a violation. A survivor is an assertion that cannot
discriminate.

Census rules, all mandatory:

- **it terminates by enumeration, not by depth**: a fixed mutation-operator set
  over a fixed, brief-named file set, **one pass, no repair loop**. Exhausting
  the enumeration ends it. Nothing about a census is open-ended;
- **it produces a ranked map, not findings.** No blocking verdict, no
  invariant matrix, no repair obligation on any commit owner. A census that can
  block is an unbounded audit of the whole repository wearing a cheaper name;
- **one build**, then every mutant runs against the warm tree;
- rank survivors by the domain test above — chain state, money, or signature
  first. That ranking is the deliverable: it aims declaration effort instead of
  spraying it across a spec corpus;
- close with `CENSUS-COMPLETE files=<n> mutants=<n> survivors=<n> ranked=<path>`.

Empirical warrant, and the reason this mode exists: `ppKeyDepositL` was **old
code**. It survived green, no test could discriminate it, and it guarded a
ledger-invalid transaction. Nobody found it until someone mutated it.

## Candidate invariants — propose, never ratify

When an audit or census surfaces an assumption the code relies on that nothing
declares, write it as a **candidate invariant**: an ID, the observable truth,
the proposed severity, and the evidence that it is currently unguarded.

Candidates are proposals to the ticket owner, which ratifies or discards them.
You may not audit against your own unratified candidate, and an unratified
candidate never blocks a submission — you would otherwise be authoring the truth
you then judge, which is the one independence the seat exists to keep.

## Compact report

Write the full report to the exact ticket-owner-owned handoff path using:

```markdown
# Commit Audit

- Submission: [...]
- Base: [...]
- Candidate: [...]
- Mandate: [...]
- Scope: FULL `base..candidate` | REPAIR `rejected..repaired` plus `[finding IDs]`
- Verdict: PASS | FINDINGS | CONTRACT-BLOCKED | SCOPE-FAIL
- Audit loop: submission `<n>/2`; next submission `ALLOWED | FORBIDDEN`
- Ceiling raises: `<count>/2`; ledger `[path/hash]`
- Campaign: OPEN | CLOSED — ended by SET-POINT | TAIL | BUDGET | OVERRUN
- Builds: `<spent>/<budget>` this ticket; this audit `<n>`, `cache=warm|cold`

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| INV-... | BLOCKING/ADVISORY | PASS/FAIL/BLOCKED | KILLED/RESIDUAL/BLOCKED/OPEN | path/hash plus one sentence |

An empty matrix is a reportable fact, not an omission: say the ticket declares
no invariants and that the audit ran under the build budget alone.

## Failure modes altered

Required. One line per failure path the change touched: what used to happen,
what happens now, and whether the new behaviour is observable to the caller.
Cover at minimum every resource acquisition, every operation moved into a
thread, every swapped synchronisation primitive, and every degradation path.

**"None" is a valid entry only when checked.** Write `none altered -- checked:
<what you looked at>`. An empty or omitted section reads as "not examined",
because a steady-state output diff cannot have examined it.

## Residuals

One line per row terminated as `RESIDUAL`: the survivor, why it is `ADVISORY`,
the named owner, the follow-up ID, and its **honest limit** — what the green
that ships alongside it does *not* establish. An unnamed residual is how the
same defect returns.

## Candidate invariants

Proposed, unratified, non-blocking. ID, observable truth, proposed severity,
evidence that nothing currently guards it. Or `None`.

## Onward discoveries — outside this ticket

One line per unrelated observation: evidence, honest limit, named
census/backlog owner, and filed follow-up ID. Mark each `RECORDED, NOT-OPENED`;
it is not a finding, candidate invariant, campaign row, or repair obligation in
this ticket. Or `None`.

## Blocking findings

1. [INV-ID or boundary] `path:line` — observed violation; **property class** it
   generalizes to (the permanent rule that would catch this whole family, not
   just this instance); evidence path/hash.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|

## Advisories

- Non-blocking and mandate-grounded only, or `None`. Point to any frozen
  instrument by path and sha256, and name the property shape in one line (e.g.
  `forAll genKeyDeposit`) — the instrument is seed evidence, this line is how
  the ticket owner finds it.
```

Do not paste raw logs, full diffs, long code excerpts, or implementation
suggestions. Consolidate duplicate symptoms under one root finding, but never
hide a distinct blocking invariant to satisfy a length target.

Hash the report and append exactly one verdict:

```text
AUDIT-PASS submission=<n> candidate=<sha> report=<path> hash=<hash>
```

or:

```text
AUDIT-FINDINGS submission=<n> candidate=<sha> report=<path> hash=<hash> blocking=<n>
```

Use `AUDIT-CONTRACT-BLOCKED` when the mandate itself is contradictory or
unverifiable, and `SCOPE-FAIL` when candidate or audit state escapes its fence.

Then append exactly one campaign line, always — a campaign that is still open
must say so:

```text
MUTATION-CAMPAIGN state=<open|closed> stopped=<set-point|tail|budget|none> rows=<n> killed=<n> residual=<n> blocked=<n> open=<n> builds=<spent>/<budget> ledger=<path>
```

or, when a blocking row is still open at budget exhaustion:

```text
MUTATION-CAMPAIGN-OVERRUN blocking_open=<n> rows=<ids> builds=<spent>/<budget>
```

Then append `COMPLETE <verdict>` and shut down. Do not remain available for
discussion or a repaired submission.

### Retire your build trees at COMPLETE

[[worker-protocol]] keeps runtime roots forever, and that stands for evidence.
**A rebuilt build tree is not evidence** — it is reproducible from the frozen
instrument source and the verified candidate SHA, both of which are hashed, so
keeping it preserves nothing that the hashes do not already preserve. This is
the narrow, named exception to never-delete, and it is yours to execute rather
than to leave for a disk sweep.

At `COMPLETE`, after the report is written and hashed:

- delete build output under your runtime root (the rebuilt working copy,
  `dist-newstyle`/`target`/`node_modules` and equivalents);
- keep forever: instrument source and its sha256, receipts, raw command output,
  the ledger, and the report;
- record `EVIDENCE-RETAINED root=<path> bytes=<n>` and
  `BUILD-TREES-RETIRED bytes_reclaimed=<n>`;
- the **detached audit worktree is the ticket owner's to retire**, not yours —
  name it and its `du` size in your final event so the retirement is aimed:
  `AUDIT-WORKTREE-RETIRABLE path=<path> bytes=<n> candidate=<sha>`.

Report reclaimed bytes as measured, and if a concurrent build makes attribution
impossible, say that instead of claiming the number.

## Ticket-owner boundary

The report is evidence, not the acceptance decision. The ticket owner verifies
its hash and decides:

- on `AUDIT-PASS`, accept the candidate or reject it for an explicitly
  ticket-owner-held mandate reason;
- on `AUDIT-FINDINGS`, forward the immutable report to the still-open commit
  owner only when this is submission 1, for its one allowed strong-owner
  repair;
- after repair, spawn a new auditor against the new SHA **and pass forward the
  campaign ledger and the remaining build budget**; its fresh model context
  audits only the repair delta plus the named open findings and must not restart
  either the discovery scope or a campaign that is already partly terminal;
- after submission 2, close the commit owner and either ship with every lawful
  residual named or re-cut the ticket. A blocking finding or open blocking row
  forces re-cut. Do not dispatch fresh ownership, another repair, or another
  auditor inside the same ticket;
- after two recorded ceiling raises, hold the re-cut conversation and refuse a
  third grant;
- forward `RECORDED, NOT-OPENED` observations to their named census/backlog
  owner; the auditor never communicates with that owner directly;
- on `MUTATION-CAMPAIGN-OVERRUN`, do not dispatch another audit on your own
  authority: a blocking row still open at budget exhaustion forces a re-cut or
  another scope/severity decision above the ticket, never another audit inside
  it;
- on `AUDIT-CONTRACT-BLOCKED`, resolve the mandate before any implementation
  resumes;
- on `SCOPE-FAIL`, preserve state and contain the boundary before proceeding.

The T.O. consumes this compact report and final decision record. Per-commit
logs, raw command output, and audit reasoning stay in durable evidence and do
not accumulate in the T.O. context.
