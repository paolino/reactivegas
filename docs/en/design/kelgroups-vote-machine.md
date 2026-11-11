# The group substrate and vote machine

This page describes the group substrate (`lean/KelGroups`) and the vote machine
inside it, as they are on `master`. The economic machine that sits on top of them
has its own page, [the reactivegas state machine](state-machine.md), which also
explains the citation convention used here: every code span that starts with the
prefix lean: names a declaration of the built Lean library and is resolved by the
hosted build, and a code span naming a declaration without the prefix is refused.

Two machines live in the substrate, and they have different standing.

- The **base machine** keeps the group's members and the proposals that change
  them. Its historical fold (`lean:KelGroups.foldGroup`) is kept for an accepted theorem
  and has no production responsibility; the production path is the integrated
  transition (`lean:KelGroups.applyIntegratedEvent`). This page describes the Lean
  model only and makes no claim about its correspondence with any Haskell
  implementation.
- The **vote machine** (`lean:KelGroups.Vote.applyVoteEventChecked`) is, by its own
  module documentation, required of the substrate rather than a reproduction of
  existing code. No claim of fidelity is made about it.

Claims say whether they rest on a **law** (a theorem quantified over every state
and event it names) or a **finite witness** (a definition evaluated to `true` on
one exhibit, or an elaboration-time check). A witness shows one case.

## Group state and the read-only view

`lean:KelGroups.GroupState` has four fields: `lean:KelGroups.GroupState.members` (the one
writable membership relation, an association list from key to `lean:KelGroups.Member`),
`lean:KelGroups.GroupState.pendingProposals` (the historical store of
`lean:KelGroups.PendingProposal`), `lean:KelGroups.GroupState.pendingBase` (the integrated
store of `lean:KelGroups.PendingBase`) and `lean:KelGroups.GroupState.appFold` (the
application payload, a type parameter). A member holds roles
(`lean:KelGroups.Role`); a responsabile is a member holding an admin role
(`lean:KelGroups.hasAdmin`).

`lean:KelGroups.GroupView` is the canonical read-only projection, obtained with
`lean:KelGroups.groupView`: it holds only the members and has no path back to a
group state. Everything that observes membership (the application fold, the base
hook, the vote machine) reads a view, so there is no second copy that could
disagree. `lean:KelGroups.GroupView.isAdmin` is the one notion of responsabile, and the
franchise (`lean:KelGroups.GroupView.admins`) is derived from the view on demand, never
stored.

## The integrated transition

`lean:KelGroups.applyIntegratedEvent` is the one transition production uses. Its events
(`lean:KelGroups.IntegratedEvent`) are four: a direct command
(`lean:KelGroups.IntegratedEvent.direct`), a base proposal
(`lean:KelGroups.IntegratedEvent.propose`), an approval (`lean:KelGroups.IntegratedEvent.approve`)
and an application event (`lean:KelGroups.IntegratedEvent.app`). The base proposal and the
application event are distinct type parameters, so an application event cannot
be a proposal and no membership action can arrive as application payload. The
application supplies a bundle (`lean:KelGroups.Integration`): the reserved key, the
proposal's identity and its reading as a mutation, the application fold and the
base hook.

- **Validation dominates the effect on every route.** An application event from a
  non-member reaches no fold (`lean:KelGroups.ValidationError.notAMember`). A successful
  application event replaces the payload and nothing else: the members relation
  is unchanged (`lean:KelGroups.app_event_preserves_members`) and there is no base change
  (`lean:KelGroups.app_event_has_no_base_change`). Both are laws.
- **A base change and its consequences are one transition.** Every successful
  membership or role effect goes through `lean:KelGroups.commitBaseChange`, which runs
  the base hook against the exact pre and post views and rejects the whole
  transition if the hook rejects (`lean:KelGroups.base_change_runs_hook`,
  `lean:KelGroups.tryEnactBase_runs_hook`; laws). The result
  (`lean:KelGroups.IntegratedResult`) carries the new aggregate and the base change it
  committed (`lean:KelGroups.BaseChange`), so a consumer reads what happened rather
  than inferring it.
- **Membership grows in one place.** `lean:KelGroups.admitMemberInto` is the only
  insertion, reachable only from the direct command behind
  `lean:KelGroups.validateDirectAdmission`. Whenever a key that was absent is present
  afterwards, the signer was an admin, the key is not the reserved key, and the
  change is an admission (`lean:KelGroups.membership_growth_is_direct_admission`, law).
  A non-admin admission is refused and a fold over it leaves the aggregate as it
  was (`lean:KelGroups.non_admin_admission_is_noop`, law).
- **Voted proposals cannot admit.** A voted proposal is a
  `lean:KelGroups.BaseMutation`: `lean:KelGroups.BaseMutation.removeMember` or
  `lean:KelGroups.BaseMutation.changeRoles`. The pending store is typed by it, so no
  pending approval, however the aggregate was built, can enact an admission
  (`lean:KelGroups.enactMutation_preserves_absence`, `lean:KelGroups.tryEnactBase_preserves_absence`;
  laws). The historical `lean:KelGroups.Proposal` still has
  `lean:KelGroups.Proposal.introduceMember`; the production path does not reach it.

**A group with no admin admits nobody.** The integrated validators have no
bootstrap arm: the founding admin arrives through the application's guarded
initial aggregate (`lean:Reactivegas.boot`), never through a self-admitting first
event.

## Base proposals and approval

A voted proposal opens with an **empty approval list**: the proposer's opening is
not an assent (`lean:KelGroups.applyIntegratedEvent`). Approving it needs a responsabile,
an existing proposal and no earlier approval from the same key; and, while more
than one responsabile exists, the proposer may not approve their own proposal
(`lean:KelGroups.validateBaseApproval`, `lean:KelGroups.ValidationError.proposerSelfApproval`).
A sole responsabile's separate approval passes. The proposal enacts when its
approvals reach `lean:KelGroups.majority`, which is `lean:KelGroups.adminCount` plus one, halved,
by natural division. `lean:KelGroups.majority_table` (finite witness) lists its values
for zero to five responsabili, and `lean:KelGroups.majority_not_strict_on_even` (law) says
that with an even positive count the majority is no more than half. With zero
responsabili the majority is zero and a proposal enacts at once.

The historical fold shows the same: its proposals open empty and enact only from a
branch that met the threshold (`lean:KelGroups.enact_implies_threshold_met`, law), a
membership change has an enactment behind it
(`lean:KelGroups.members_change_implies_enacted`, law), pending approvals are free of
duplicates (`lean:KelGroups.approvals_nodup`, law), and member keys equal their
association-list keys (`lean:KelGroups.member_key_coherent`, law).

Validation refusals are `lean:KelGroups.ValidationError`, which has twelve constructors.
They include `lean:KelGroups.ValidationError.reservedKey`, produced by
`lean:KelGroups.validateDirectAdmission` before the already-a-member refusal, and
`lean:KelGroups.ValidationError.proposerSelfApproval`, which is distinct from a duplicate
approval.

## Executed witnesses of the historical fold

Three exhibits run the historical fold (`lean:KelGroups.foldGroup`) and are checked at
elaboration time. They are finite witnesses, not laws.

- `lean:KelGroups.stalePendingWitness`: after validated removals lower the responsabile
  count, a sibling proposal still pending holds approvals at or above the new
  majority. Enacting one proposal erases only that proposal.
- `lean:KelGroups.bootstrapNonMemberWitness`: from an empty group, a proposal to
  introduce an admin enacts with no approvals, because the majority is zero; the
  signer, who is not a member, is not inspected in bootstrap mode.
- `lean:KelGroups.bootstrapPendingWitness`: validated removals can enact the last
  responsabili while the stale sibling above is still pending, returning the group
  to bootstrap mode with a proposal pending.

## The vote machine

The vote payload (`lean:KelGroups.Vote.VoteState`) holds the open questions and an
append-only closure log of `lean:KelGroups.Vote.ClosureRecord`. It holds no membership:
the franchise (`lean:KelGroups.Vote.franchise`) is read from the canonical view, so a
tally can never be evaluated against a stale membership. It has no clock and no
time-like field, which is what makes the no-expiry theorem provable
(`lean:KelGroups.Vote.no_expiry`, law).

**Events.** The vote vocabulary (`lean:KelGroups.Vote.VoteEvent`) has three events:
`lean:KelGroups.Vote.VoteEvent.openQuestion`, `lean:KelGroups.Vote.VoteEvent.cast` and
`lean:KelGroups.Vote.VoteEvent.renounce`. There is no membership event in it: members
change through the base machine only.

**Questions.** A question (`lean:KelGroups.Vote.Question`) records its kind, its
proposer and two tallies. The kind (`lean:KelGroups.Vote.QuestionKind`) is
`lean:KelGroups.Vote.QuestionKind.collective`, tallied against the threshold, or
`lean:KelGroups.Vote.QuestionKind.permission`, which names exactly one designee. A
question opens with **empty tallies**: the proposer is not counted as an assent
(`lean:KelGroups.Vote.effectedState`). Opening never overwrites or revives an id that
is open or already decided.

**Ballots.** A responsabile holds one position per question
(`lean:KelGroups.Vote.placeBallot`): casting assent inserts into the assent list and
erases from the dissent list, and the reverse. A repeated cast changes neither
tally.

**Verdict.** `lean:KelGroups.Vote.verdictOf` is the only place a verdict is decided, and
it has three outcomes (`lean:KelGroups.Vote.Verdict`): positive, negative or open. For a
collective question it compares recorded assents with the threshold at the current
franchise size, then recorded dissents with the same threshold, else open. For a
permission question it never consults the threshold or a count: the designee's
recorded assent is positive, their recorded dissent negative, and neither leaves it
open.

**Threshold.** `lean:KelGroups.Vote.Threshold` is a function from the franchise size to the
required count, and every evaluation takes one explicitly. The model ships two
exhibits, `lean:KelGroups.Vote.legacyThreshold` (`(n+1)/2`) and
`lean:KelGroups.Vote.zeroThreshold` (constantly zero), and no default. The proposer's
opening is not an assent, so a question never closes at opening unless the
threshold is zero.

**Sweep.** `lean:KelGroups.Vote.sweepClosures` re-evaluates every open question against
the supplied view and threshold and closes, in one step, each that has reached a
verdict, moving it to the closure log with its cause. Closing is removal from the
open set plus an appended record, as one operation, so no question is erased without
a record. The sweep is idempotent (`lean:KelGroups.Vote.sweepClosures_idempotent`, law)
and a question that remains open has verdict open
(`lean:KelGroups.Vote.open_questions_are_open`, law). Open and closed sets partition the
question ids and no record is open-verdict (`lean:KelGroups.Vote.questions_partition`, law).
Because the verdict reads the current franchise, a question can close after a base
change although no ballot was cast; the sealed base hook runs the sweep on the
post-change view.

**Closure causes.** `lean:KelGroups.Vote.ClosureCause` has four constructors:
`lean:KelGroups.Vote.ClosureCause.tally`, `lean:KelGroups.Vote.ClosureCause.franchiseChange`,
`lean:KelGroups.Vote.ClosureCause.proposerDeparted` and
`lean:KelGroups.Vote.ClosureCause.renounced`. `lean:KelGroups.Vote.closureCause` decides between
the first two from the view: a closure whose deciding side holds a key the current
franchise no longer counts is a franchise change. The other two are written by
the vote machine itself: a proposer's renounce closes their question negatively
with cause renounced (`lean:KelGroups.Vote.effectedState`), and
`lean:KelGroups.Vote.closeProposerQuestions` closes every open question of a departing
proposer negatively with cause proposerDeparted, in open-set order, before the
post-change sweep.

**Validation.** `lean:KelGroups.Vote.validateVoteEvent` is the one authorization decision,
exhaustive over the three events with no wildcard, so a new event cannot acquire a
default. Every event needs a signer who is a responsabile of the canonical view
(`lean:KelGroups.Vote.VoteError.notResponsabile`). A cast or renounce then needs the
question to exist (`lean:KelGroups.Vote.VoteError.questionNotFound`). A cast on a
permission question by anyone but the named designee is refused
(`lean:KelGroups.Vote.VoteError.notDesignee`), and a renounce by anyone but the proposer is
refused (`lean:KelGroups.Vote.VoteError.notProposer`). A refused event reaches neither
effect nor sweep; the state-returning step gives back the payload it was handed
(`lean:KelGroups.Vote.inadmissible_is_noop`, law), and an unfranchised cast changes
nothing (`lean:KelGroups.Vote.unfranchised_cast_noop`, law).

**Representation invariants.** The fold preserves `lean:KelGroups.Vote.VoteWellFormed`
from the empty payload (`lean:KelGroups.Vote.foldVote_wellFormed`, law): tallies are free
of duplicates and disjoint (`lean:KelGroups.Vote.ballots_nodup_disjoint`, law), and every
key in a tally was once a cast from a responsabile
(`lean:KelGroups.Vote.franchise_of_tallies`, law).

## Named gaps

- **No dissent for base proposals.** `lean:KelGroups.BaseEvent` has only propose and
  approve: there is no rejection, expiry or withdrawal event for a base proposal.
  Rejection exists in the vote machine (a negative verdict) but not here.
- **Removing the last admin returns the group to bootstrap mode** in the
  historical fold (`lean:KelGroups.authMode`); no prevention is modelled, and the
  integrated transition has no bootstrap arm at all.
- **Abstract parameters.** The proposal identity is supplied by the application
  (`lean:Reactivegas.proposalDigest`) and the historical validators take a key-validity
  predicate and role predicates (`lean:KelGroups.RoleDef`) as arguments; the model does
  not implement a digest or a key encoding.
- **Redundant lookup.** The historical role-change validation looks the member up a
  second time after the caller has already required it; the second branch is
  unreachable and retained.
