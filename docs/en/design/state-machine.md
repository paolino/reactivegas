# The reactivegas state machine

This page describes the Lean model on `master`: the economic machine that
manages a group's money, its purchases and its votes, and the way it sits on the
group substrate. It is derived from the Lean sources under
[`lean/`](https://github.com/paolino/reactivegas/tree/master/lean) and rewritten
when they change. Every statement that names a Lean declaration is a citation,
and the hosted build resolves each one against the built library (next section).

The substrate (group state, base proposals, the vote machine) has its own page,
[the group substrate and vote machine](kelgroups-vote-machine.md). This page
covers what the economic machine adds to it.

## How to cite this page

**Citation.** A citation is a code span made of the prefix lean: and the full
name of a declaration of the Lean library, for example `lean:KelGroups.GroupView`.
Some names have no namespace: the economic payload is `lean:State`, its event
vocabulary is `lean:Event`, and the transition is `lean:step`. The sealed production
root is `lean:Reactivegas.apply`. Cite by name; a line number is never a citation.

**What resolves.** `scripts/design-citations/check` imports every Lean module it
finds under `lean/` and asks the resulting Lean environment. A name resolves when
it is a declaration those modules define and an author wrote: private
declarations, compiler-internal names, recursors and the eliminators Lean
derives for every inductive type do not count. Run it with `just design-citations`
after `just lean`; the hosted build runs it right after the Lean build.

**When a code span names a declaration.** Every code span that does not start
with the prefix is read as code, and a declaration it names must be cited instead.
The rule: take the identifier tokens of the span (each maximal run of identifier
characters and dots, and each dot-separated component of such a run); a token
names a declaration when it equals a trailing run of the components of some
declaration's name. The span is then refused, wherever the token sits: alone,
inside an expression, or after a variable and a dot. A bare field name or a bare
function name in code font is therefore refused, and is cited with its full name
instead, for example `lean:State.conti`. Lean decides, from the environment; no list is
kept. A word that happens to be a declaration name is written in plain text when
it is meant as a word.

**What the check refuses.** An unresolved citation; a code span that names a
declaration without the prefix; the prefix followed by a name outside a code
span; a fenced block tagged lean; a record that carries no citation; and a Lean
library that cannot be imported (not built, or a module missing). Each refusal
names the file, the line and the span, and the exit status is non-zero. Fenced
blocks of other kinds are not scanned and hold no declaration names.

**Controls.** `just design-citations-controls` builds each refusal from the record
itself in a throwaway copy (an unbuilt Lean tree, a renamed citation, a citation
written once more without the prefix, a record with no citation, a directory
with no page) and requires the checker to be red for that reason and green on
the real pages. The hosted build runs it beside the check.

**Law and finite witness.** A claim here rests on one of two kinds of Lean
statement and says which. A **law** is a theorem quantified over every state and
event it names. A **finite witness** is a definition evaluated to `true` on a
fixed exhibit, or an elaboration-time check; it shows that one case behaves as
described and says nothing about any other. The tables below mark each row.
Nothing on this page claims the model complete: what a theorem does not state is
not claimed.

## State

The economic payload `lean:State` has six fields:

| field | contents |
| --- | --- |
| `lean:State.conti` | credit accounts, one entry per key, read by `lean:bal` (an absent key reads zero) |
| `lean:State.casse` | cash boxes, one entry per key |
| `lean:State.collections` | open purchase collections (`lean:Collection`) holding escrow |
| `lean:State.votes` | the vote payload (`lean:KelGroups.Vote.VoteState`): open questions and an append-only closure log |
| `lean:State.bindings` | the target each open bound question will authorize |
| `lean:State.live` | unspent authorizations (`lean:LiveAuth`) minted by closed bound questions |

**Where membership lives.** The payload holds no membership. The group state
`lean:KelGroups.GroupState` has one writable membership store, `lean:KelGroups.GroupState.members`.
The application sees it only through `lean:KelGroups.GroupView`, obtained by
`lean:KelGroups.groupView`: a read-only projection with no path back to a group
state. An application fold (`lean:KelGroups.IntegratedAppFold`) and the hook that
follows a base change (`lean:KelGroups.BaseHook`) take views and return payload or a
refusal, so neither can return a member list. Law: a successful application
event leaves the members relation as it was (`lean:KelGroups.app_event_preserves_members`)
and commits no base change (`lean:KelGroups.app_event_has_no_base_change`). Finite
witness: `lean:Reactivegas.app_members_preservation_holds`, with a member-writing
mutant that the same check rejects (`lean:Reactivegas.app_members_preservation_mutant_caught`).

A collection `lean:Collection` carries its id, its referente (the responsible key),
a flag `lean:Collection.permitted` that closure permission was granted, and two pledge
lists: `lean:Collection.accepted` (money already moved to the collection) and
`lean:Collection.pending` (offers not yet accepted). A pledge `lean:Pledge` is a bare
pair of a user and an amount; it carries no product (see "Out of scope: the product
catalogue"). Balances are plain integers. The money a collection holds is the sum
of both lists (`lean:escrowOf`), and across all collections `lean:escrowSum`.

**Balance facts.**

- A zero balance for an absent key is what `lean:bal` returns. A deposit of zero
  that is accepted stores an entry at zero: `lean:bump` appends an entry when the
  key is absent. `lean:Reactivegas.absorbConto` is a bump, not an erase, so a departed member's
  entry survives at zero. Admitting a member creates no entry at all.
- The common fund is the `lean:State.conti` balance at the reserved key `lean:comuneId`
  (`lean:comuneBal`). It is never a field of `lean:State` and never a member. The empty
  payload `lean:State.empty` has no accounts, no collections, no questions.
- **Stall.** `lean:stalled` holds when the common fund is negative. While stalled,
  `lean:step` refuses `lean:AppEvent.withdraw`, `lean:AppEvent.pledge`, `lean:AppEvent.acceptPledge` and
  `lean:AppEvent.closePurchase`, and the base hook (`lean:Reactivegas.economicCleanup`) refuses a
  departure and the loss of admin status. `lean:AppEvent.donate` stays available: it
  is the cure. `lean:AppEvent.failPurchase`, `lean:AppEvent.refusePledge` and
  `lean:AppEvent.correctPledge` have no stall condition. `lean:AppEvent.backdonate` has none
  of its own; its affordability guard refuses it.

**Founding a group.** `lean:Reactivegas.boot` builds the founding aggregate and refuses
it, with `none`, when the reserved key is listed as a member
(`lean:Reactivegas.productionWellFormed`) or the payload is not a clean origin: no
closure record, no binding, no unspent authorization (`lean:Reactivegas.cleanOrigin`).
`lean:Reactivegas.apply` refuses with `lean:Reactivegas.ProductionError.comuneReserved` an aggregate
that lists the reserved key, and an event whose result would
(`lean:Reactivegas.apply_comuneReserved_iff`, law). The reserved key therefore cannot
sign: finite witness `lean:Reactivegas.comune_cannot_authorize`.

## Events

**Economic events.** `lean:Event` has 14 constructors, each carrying its author. The
application surface `lean:AppEvent` has 18: the same 14 without an author field (the
signer arrives from the fold) and four vote constructors, `lean:AppEvent.openQuestion`,
`lean:AppEvent.cast`, `lean:AppEvent.renounce` and `lean:AppEvent.openBound`. The 14 are:

| economic | application | effect |
| --- | --- | --- |
| `lean:Event.openPurchase` | `lean:AppEvent.openPurchase` | open a collection |
| `lean:Event.grantPermission` | `lean:AppEvent.grantPermission` | set the permission flag |
| `lean:Event.denyPermission` | `lean:AppEvent.denyPermission` | dissolve a collection, refunding every pledge |
| `lean:Event.deposit` | `lean:AppEvent.deposit` | credit a member and the signer's cash box |
| `lean:Event.withdraw` | `lean:AppEvent.withdraw` | debit a member and the signer's cash box |
| `lean:Event.transferCassa` | `lean:AppEvent.transferCassa` | move cash between two responsabili |
| `lean:Event.donate` | `lean:AppEvent.donate` | cash arrived for the common fund |
| `lean:Event.backdonate` | `lean:AppEvent.backdonate` | share the common fund equally among members |
| `lean:Event.pledge` | `lean:AppEvent.pledge` | escrow a member's money in a collection |
| `lean:Event.acceptPledge` | `lean:AppEvent.acceptPledge` | move a pledge from pending to accepted |
| `lean:Event.refusePledge` | `lean:AppEvent.refusePledge` | refund a pending pledge |
| `lean:Event.correctPledge` | `lean:AppEvent.correctPledge` | change or withdraw a pledge |
| `lean:Event.closePurchase` | `lean:AppEvent.closePurchase` | spend the referente's cash box, dissolve the collection |
| `lean:Event.failPurchase` | `lean:AppEvent.failPurchase` | refund every pledge, dissolve the collection |

**Retired constructors.** addUser, electResponsabile, removeResponsabile and an
economic removeMember do not exist in `lean:Event` or `lean:AppEvent`: they are not
refused, they cannot be written. Membership changes only through the substrate:
the voted `lean:KelGroups.BaseMutation.removeMember` and `lean:KelGroups.BaseMutation.changeRoles`,
and the direct `lean:KelGroups.DirectCommand.admitMember`. Those survive and are not
retired. The application's own proposal vocabulary `lean:Proposal` has two
constructors, `lean:Proposal.departure` and `lean:Proposal.changeRoles`, and no admission
constructor; `lean:Reactivegas.proposalMutation` reads it as a substrate mutation, and
the substrate type `lean:KelGroups.BaseMutation` cannot express admission either.
The historical `lean:KelGroups.Proposal.introduceMember` survives in the historical
store only, which the production root does not reach.

## The transition and who may sign

There are two layers. The economic core `lean:step` takes the view, the state, the
signer, an application event and a backdonation veto (`lean:BackdonateAuth`), and
returns the next state or `none`; `lean:stepEvent` gives the same core the 14-event
shape. The production root `lean:Reactivegas.apply` wraps the substrate transition
(`lean:KelGroups.applyIntegratedEvent`) around `lean:Reactivegas.integration`; it adds the
spending of authorizations described under "Votes" and runs the sealed base hook.
`lean:Reactivegas.apply` refuses an application event whose signer is not a member
(`lean:KelGroups.ValidationError.notAMember`) before any fold runs.

The guards of `lean:step`, per event (a responsabile is a member holding an admin
role, `lean:isResponsabile`):

| event | signer and guard |
| --- | --- |
| `lean:AppEvent.openPurchase` | a responsabile; the collection id is not in use (`lean:Reactivegas.step_open_inv`) |
| `lean:AppEvent.grantPermission` | the collection exists; a responsabile (`lean:step_grant_inv`); at the root also an unspent positive authorization for it |
| `lean:AppEvent.denyPermission` | the collection exists; a responsabile (`lean:step_deny_inv`); at the root a responsabile and an unspent negative authorization for it |
| `lean:AppEvent.deposit` | a responsabile; the user is a member other than the signer; amount at least zero (`lean:Reactivegas.step_deposit_inv`) |
| `lean:AppEvent.withdraw` | a responsabile; the user is a member other than the signer; the user's balance covers it; not stalled (`lean:Reactivegas.step_withdraw_inv`) |
| `lean:AppEvent.transferCassa` | a responsabile; cash comes from a different responsabile; amount above zero (`lean:Reactivegas.step_transferCassa_inv`) |
| `lean:AppEvent.donate` | a responsabile; amount above zero (`lean:Reactivegas.step_donate_inv`) |
| `lean:AppEvent.backdonate` | a responsabile; share above zero; the common fund covers share times member count; the veto callback agrees (`lean:Reactivegas.step_backdonate_inv`); at the root also an unspent positive authorization for exactly that share |
| `lean:AppEvent.pledge` | **the pledging member** (the signer is the user); the user is a member without a pledge in this collection; amount above zero; balance covers it; not stalled (`lean:step_pledge_inv`, `lean:pledge_signed_by_owner`) |
| `lean:AppEvent.acceptPledge` | a responsabile who is the collection's referente; the pledge is pending; not stalled (`lean:step_accept_inv`) |
| `lean:AppEvent.refusePledge` | a responsabile who is the referente; the pledge is pending (`lean:step_refuse_inv`) |
| `lean:AppEvent.correctPledge` | see below (`lean:step_correct_cases`) |
| `lean:AppEvent.closePurchase` | a responsabile who is the referente; permission granted; no pending pledge; not stalled (`lean:step_close_inv`) |
| `lean:AppEvent.failPurchase` | a responsabile who is the referente; no pending pledge (`lean:step_fail_inv`) |

**Correcting a pledge.** A pledge is signed by the member it belongs to, and
that member alone corrects a pending one: with a new amount, or with zero to
withdraw it, which removes the row and returns the money
(`lean:pending_correction_by_owner`, `lean:pending_withdrawal_removes_row`,
`lean:pending_withdrawal_leaves_no_row`). After the referente accepts it, only the
referente may correct it (`lean:accepted_correction_by_referente`). In both cases the
new amount is at least zero and the user's balance must cover the difference. A
correction with no row to correct is refused (`lean:correction_without_row_refused`).
All of these are laws.

**Authorization as a statement.** `lean:authorizedStep` says what the role check of
every event is, and `lean:step_authorized` proves that every successful
`lean:stepEvent` satisfies it (law). The predicate checks the author's role alone:
membership and argument guards are in `lean:step` and its inversion theorems above.

## Routes and the sealed base hook

**Route classifier.** `lean:Reactivegas.Composition.route` classifies each of the 14
economic events by the producer that decides it: 11 are `lean:Reactivegas.Composition.Route.direct` (the economic machine
alone), 3 are `lean:Reactivegas.Composition.Route.appDecided` (`lean:Event.grantPermission`,
`lean:Event.denyPermission`, `lean:Event.backdonate`), and none is
`lean:Reactivegas.Composition.Route.baseEnacted`. That third constructor stays
because an accepted historical theorem quantifies over it
(`lean:Reactivegas.Composition.baseEnacted_threshold_met`); no event routes to it. A
reader who counts arms must know the middle one is empty. An independent
classifier, `lean:Reactivegas.Composition.voteDerived`, is true on exactly the
app-decided three (`lean:Reactivegas.Composition.voteDerived_iff_not_direct`, law). The
faithful base vocabulary (`lean:Reactivegas.Composition.baseProposalFaithful`) admits
only `lean:KelGroups.Proposal.changeRoles` and `lean:KelGroups.Proposal.removeMember`.
The root does not consult the classifier: it dispatches on the event
constructor itself. A finite witness evaluates the classifier on the three app-decided
events and on a donation (`lean:CompositionTests.checkB9_classifier`).

**The sealed hook.** The consequences of a committed base change run inside the
same transition, from the real pre and post views, in `lean:Reactivegas.baseHook`.
`lean:KelGroups.commitBaseChange` runs it before any result is returned, and refuses the
whole transition when it refuses (`lean:KelGroups.base_change_runs_hook`, law). The
cleanup `lean:Reactivegas.economicCleanup` is exhaustive over the three
`lean:KelGroups.BaseChange` constructors, so a fourth membership effect stops
compilation. For each:

- `lean:KelGroups.BaseChange.memberAdmitted`: no economic consequence; the member arrives
  with no account, no cash box and no collection.
- `lean:KelGroups.BaseChange.memberRemoved`: refused while stalled; otherwise, when the
  leaver held admin, `lean:Reactivegas.windUpAdmin`, then `lean:Reactivegas.absorbConto`
  (the leaver's account moves to the common fund). The leaver's own open questions
  are closed negatively (`lean:KelGroups.Vote.closeProposerQuestions`) before the
  post-change sweep.
- `lean:KelGroups.BaseChange.rolesChanged`: refused while stalled when admin status is lost,
  else `lean:Reactivegas.windUpAdmin` on admin loss and nothing on admin gain.

`lean:Reactivegas.windUpAdmin` cancels the key's open collections, refunds every pledge
they held (`lean:stripCollections` and `lean:refundAll`) and moves the key's cash box to
the common fund. After it no open collection has that key as referente
(`lean:governance_enacts_windUpAdmin`, law). After any base change, every open
question is re-evaluated against the post-change franchise, so a question can
close because the electorate changed and no ballot was cast
(`lean:Reactivegas.base_change_recomputes_votes`, law).

## Votes

The vote machine (`lean:KelGroups.Vote.applyVoteEventChecked`) is described on the
substrate page. The economic machine uses it at one door: `lean:Reactivegas.appFold`
sends `lean:AppEvent.openQuestion`, `lean:AppEvent.cast`, `lean:AppEvent.renounce` and
`lean:AppEvent.openBound` to `lean:Reactivegas.voteApply`; bare `lean:step` refuses all four.

**A question opens with zero approvals.** `lean:KelGroups.Vote.effectedState` opens a
question with empty tallies: the proposer's opening is not an assent. The same
holds for base proposals: they open with no approval
(`lean:KelGroups.applyIntegratedEvent` stores an empty approval list) and the
proposer cannot approve their own proposal while more than one responsabile
exists (`lean:KelGroups.ValidationError.proposerSelfApproval`,
`lean:KelGroups.validateBaseApproval`), so under `lean:KelGroups.majority` such a proposal
needs an approval from someone other than the proposer.

**Closure effects.** Grant, deny and backdonate take their economic effect only
from a closed vote.

1. `lean:AppEvent.openBound` opens a question and records, before any ballot exists,
   the one thing its closure can authorize (`lean:EconomicTarget`):
   `lean:EconomicTarget.permission` fixes a collection identity;
   `lean:EconomicTarget.backdonation` fixes the per-member share itself. `lean:Reactivegas.bindable`
   requires a fresh question id (neither open nor ever closed) and, for permission,
   a present collection; for backdonation, a share above zero.
2. When the vote step closes a bound question, `lean:Reactivegas.syncAuth` mints one
   `lean:LiveAuth` carrying the record's question id, its verdict and the bound target.
3. `lean:AppEvent.grantPermission`, `lean:AppEvent.denyPermission` and `lean:AppEvent.backdonate`
   each spend one unspent authorization for their exact target under the verdict
   they need, atomically with the economic effect (`lean:Reactivegas.spendThen`,
   `lean:Reactivegas.denyByClosure`), and are refused without one:
   `lean:Reactivegas.Composition.apply_grant_spends`,
   `lean:Reactivegas.Composition.apply_deny_spends` and
   `lean:Reactivegas.Composition.apply_backdonate_spends` (laws). The backdonation
   veto `lean:BackdonateAuth` is consulted after the spend and can only refuse.
4. Every unspent authorization of a production history is backed by a closure
   record its own vote fold appended (`lean:Reactivegas.Composition.liveBacked_of_history`,
   law), so a planted authorization is not a production history
   (`lean:Reactivegas.ProductionHistory`). `lean:Reactivegas.pruneAuth` drops bindings and
   authorizations whose collection left, so a reused collection id inherits none
   (`lean:Reactivegas.Composition.authTargetsPresent_of_history`, law).

**Lifecycle.** A renounce by the proposer, and the proposer's departure from the
group, close the question negatively: the closure cause is
`lean:KelGroups.Vote.ClosureCause.renounced` or
`lean:KelGroups.Vote.ClosureCause.proposerDeparted`, the record keeps the question as
it stood, and the verdict is `lean:KelGroups.Vote.Verdict.negative`. The escrow refunds on that negative
closure: a closed negative bound question mints a negative authorization
(`lean:Reactivegas.Composition.apply_mints_negative`), nothing but spending it or the
collection's departure removes it (`lean:Reactivegas.Composition.apply_keeps_negative`),
spending it refunds every pledge to the member it belongs to
(`lean:Reactivegas.Composition.spend_refunds`, `lean:Reactivegas.Composition.apply_deny_refunds`),
and so no negative authorization is stranded
(`lean:Reactivegas.Composition.noStranding_of_history`), all laws. The two closures
themselves are finite witnesses (`lean:Reactivegas.Lifecycle.l1_holds` to
`lean:Reactivegas.Lifecycle.l6b_holds`), each paired with a mutant that the same check rejects (the theorems of
the same module whose names end in caught).

**Two refusals.** A renounce by a responsabile who is not the proposer
(`lean:KelGroups.Vote.VoteError.notProposer`) and a ballot on a permission question by a
responsabile who is not its named designee (`lean:KelGroups.Vote.VoteError.notDesignee`)
are refused by `lean:KelGroups.Vote.validateVoteEvent`, in the order not-responsabile,
question-not-found, then identity. A refused vote event is inert: the state-returning vote step gives back the
payload it was handed (`lean:KelGroups.Vote.inadmissible_is_noop`, law), the checked
step returns the error, and the integrated fold leaves the aggregate as it was
(`lean:Reactivegas.Lifecycle.refusal_fold_inert`, finite witness; the refusals
themselves: `lean:Reactivegas.Lifecycle.r1_holds`, `lean:Reactivegas.Lifecycle.r2_holds`,
`lean:Reactivegas.Lifecycle.order_holds`).

**No question is dropped.** Every question open before a successful root event
is, after it, still open or named by a closure record
(`lean:Reactivegas.Composition.apply_retains_open_questions`, law); the closure log is
append-only.

**Threshold.** The vote threshold is a parameter of the model, never a constant:
`lean:KelGroups.Vote.Threshold` is a function from the number of responsabili to the
required count, and `lean:Reactivegas.apply`, `lean:Reactivegas.baseHook`,
`lean:KelGroups.Vote.verdictOf` and `lean:KelGroups.Vote.sweepClosures` each take one
explicitly. The model ships two named exhibits, `lean:KelGroups.Vote.legacyThreshold`
(`(n+1)/2`) and `lean:KelGroups.Vote.zeroThreshold` (constantly zero), and no default.
A group therefore sets an explicit policy when it is set up. The exported vote
corpus states the policy it was generated under as a finite table, and a check
compares that table with the live declaration at every point of its domain
(`lean:Reactivegas.PolicyCheck.checkPolicyAgainstDeclaration`). Base proposals use a
fixed arithmetic, `lean:KelGroups.majority`, not this parameter.

## Laws

Each entry gives the story, then what Lean proves. All of these are laws over the
14-event vocabulary and `lean:stepEvent` unless marked otherwise.

- **Only entitled actors act** (`lean:step_authorized`): every successful event is signed
  by a responsabile, except a pledge, which is signed by the pledging member
  (`lean:pledge_signed_by_owner`), and a correction, signed by that member or a
  responsabile.
- **Closure needs permission** (`lean:close_permission_to_close`): a successful close
  finds the permission flag set and no pending pledge (`lean:permissionToClose`).
  At the root the flag comes from a closed vote, not from one responsabile's
  signature (see "Votes").
- **Escrow at pledge** (`lean:pledge_escrow_debit`): the user's balance drops by
  exactly the amount and the collection holds exactly that much more
  (`lean:escrowHeld`).
- **Closure spends the referente's cash box** (`lean:close_spends_referente`): the
  referente's cash box falls by the accepted pledges' total. A close needs no
  pending pledge, so that is the collection's whole escrow.
- **Double entry** (`lean:deposit_double_entry`, `lean:withdraw_double_entry`): a deposit or
  withdrawal moves the user's account and the signer's cash box by the same
  amount (`lean:doubleEntry`).
- **Conservation** (`lean:conservation_preserved`): every one of the 14 events
  preserves `lean:conservation`, the sum of cash boxes minus the sum of accounts minus
  the escrow of open collections being zero. This is a statement about
  `lean:stepEvent`: it is not proved across the base hook.
- **Solvency** (`lean:solvent_init`, `lean:solvent_preserved`, `lean:reach_solvent`,
  `lean:not_insolvent_of_reach`): no member's account goes below zero along any
  execution of `lean:Reach` (a fixed view). The common fund may go negative, which is
  the stall, and is not insolvency (`lean:solvent`, `lean:insolvent`).
- **One pledge per member per collection** (`lean:pledge_rejected_when_member`,
  `lean:pledge_preserves_allUnique`, `lean:uniquePledges`).

## Corpora

Three frozen corpora under `lean/corpus/` carry the model to other languages. The
exporter executable `corpusExport` is their sole writer; nothing else produces the
files.

| corpus | what it holds | what checks it |
| --- | --- | --- |
| economic | the seed traces of `lean:seedCorpus`: signed economic events through `lean:step` with their refusals | emitted again and compared byte for byte; read back and compared element by element with the live traces; key sets checked |
| integrated | `lean:Reactivegas.emitIntegratedCorpus`: signed events through `lean:Reactivegas.apply`, with base changes and cleanup | the same, plus the finite witness `lean:Reactivegas.checkIntegratedCorpus`, which replays the corpus and requires nine corrupted variants to fail |
| vote | `lean:Reactivegas.emitVoteCorpus`: vote events through the production root, with the refusal identity of each refused vote event; a sidecar names the threshold table | the same, plus the sidecar rules, the policy re-derivation, the digest of the source inputs and a coverage check over the constructors of the vote vocabulary (`lean:Reactivegas.voteCorpusCoversDiscovered`) |

The hosted build re-verifies them in the step that runs `just lean-corpus-verify`:
re-emission compared with the committed files, their checksums, the live-binding
check, the key-set checks and [verify-vote-corpus](https://github.com/paolino/reactivegas/blob/master/scripts/verify-vote-corpus). Each is evidence
about a fixed exhibit, not a law.

**Finite-history validation.** `lean:Reactivegas.historyRefusal` validates one supplied
history of `lean:Reactivegas.apply` steps: its initial aggregate must be exactly the
output of `lean:Reactivegas.boot`, and every stored step must be what the root
recomputes. `lean:Reactivegas.traceRefusal` does the same for a supplied trace of
`lean:stepEvent` steps under one view. Each validates one history and decides nothing
about reachability: no bridge from a validated history to `lean:Reach` is claimed, and
a state without a validated history is not thereby unreachable. The committed vote
corpus is validated this way inside [verify-vote-corpus](https://github.com/paolino/reactivegas/blob/master/scripts/verify-vote-corpus), under the
sidecar's threshold table, and planted defects (a changed post-state, a flipped
acceptance, a non-genesis start) must each be refused for their own reason. The elaboration-time fixtures are `lean:Reactivegas.fhHistory` and its
neighbours in the library build.

## Group closure

`lean:canCloseGroup` states three conjuncts: every member's account is zero, there is
no open collection, every cash box is zero. It is a definition whose only other mention is
its equivalence with a Boolean mirror (`lean:canCloseGroup_corr`). No theorem or event
consumes it, and the model proves nothing about a group closing. What the model
states about cash boxes is `lean:conservation`; closing a group is not modelled.

## Out of scope: the product catalogue

**Fact.** The legacy product catalogue is not modelled. It is 21 Haskell modules
under `Voci/` (quantities, units, volumes, weights, packaging, currency, plus
`Voci/UI`), the order type with them, and the order-bound pledges
`ImpegnoVincolato` and `CorrezioneImpegnoVincolato` of `Eventi/Impegno.hs`, which
carry a list of orders. The Lean models none of it: a `lean:Pledge` is a bare
pair of a user and an amount, with no product, no order and no quantity.

**Decision.** Out of scope for milestone 2, deferred to milestone 3
([#91](https://github.com/paolino/reactivegas/issues/91)).

**Why it is written down instead of omitted.** The outcome test of milestone 2
(election, collection, pledge, assent, purchase, refund) names no catalogue, so
leaving the catalogue out does not break it. But the absence is a decision, and a
silent gap reads as an oversight to anyone who counts the legacy modules.

**Cost curve.** Cheapest now: `lean:Event.pledge` would gain a payload and
`lean:Event.correctPledge` a difference, landing in the Lean before the application
fold is written. After the conformance corpus freezes, the same change costs a
corpus re-freeze plus an application-fold rewrite.

**Open question, carried open.** Does a group reach the outcome test (election
→ collection → pledge → assent → purchase → refund) without a catalogue, or is the
catalogue absent from the Lean because nobody asked? This page does not pick a
side.

## What is not implemented

Nothing that has landed on `master` is described above as pending. What is
absent:

- **Group closure as an event** and any theorem about it (`lean:canCloseGroup` is an
  unused definition).
- **A backdonation policy.** `lean:BackdonateAuth` is a veto only; the model supplies
  no product policy for it, and the probe `lean:Reactivegas.probeAuth` refuses
  everything.
- **A vote threshold default** (see "Threshold").
- **The product catalogue** (previous section).
- **A bridge from a validated history to `lean:Reach`** (see "Finite-history
  validation").

## Verifying

```sh
just lean                      # lake build, axiom, mirror and coverage checks
just design-citations          # every citation here resolves, none is missing
just design-citations-controls # the check is red for each defect it names
```
