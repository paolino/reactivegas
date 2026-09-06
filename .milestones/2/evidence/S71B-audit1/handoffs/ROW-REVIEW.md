# S71-B full row review

Candidate 8e4cbb8b95ac5a2063ea39cf2d2ac6a4c1d15163; source PIN
4a6cd87fcbc3e4a536bbc9f240f5efe5704022af; mandate 90dae994da67d889831726dd1f17aaae9ad84be1.
This is a fresh review. Prior S71-A reports and ledgers were inputs, never acceptance.
Source/ruling snapshots, compiled indexes and the source-to-compiled citation map are retained in evidence/.

## State, Events and the sealed hook

State.lean:23 has conti/casse/collections/votes; membership remains GroupView.members.
State.lean:39/62 distinguishes absent-zero lookup from stored-zero bump. The successful
zero deposit stores both rows; admission has identity economic effect, and absorbConto
zeroes rather than erases the departed account. comuneBal is the reserved account in
conti. Escrow is accepted plus pending; refundAll bumps once per pledge. Collection
fields and bare Pledge(user, amount) agree with Types/State. No catalogue is hidden
in the economic payload.

The fresh T2 compiler indexes define fourteen Event constructors and seventeen
AppEvent constructors. The latter adds exactly openQuestion/cast/renounce and removes
author fields. Root Event, State and Proposal are distinct from KelGroups.Proposal;
the latter retains introduceMember/removeMember/changeRoles. Root Proposal permits
only departure/changeRoles. The retired and surviving namespaces in the document
are distinguished correctly.

Composition.route explicitly maps eleven direct and three appDecided events
(grantPermission, denyPermission, backdonate); none maps to baseEnacted. Viewed step
and stepEvent signatures match their definitions, including the explicit auth
callback. The compiled BaseChange extent has three members. economicCleanup handles
admission as economic identity, removal as stall refusal then conditional admin
wind-up and account absorption, and role changes as stall refusal/wind-up only on
admin loss. baseHook subsequently sweeps votes at the post-view. No new signable
cleanup event or payload membership copy exists.

## Every authority-table row

Compared directly with Step.lean:44-142, in source evaluation order. The table is a
guard summary; collection existence is also explained in State and the AUTH prose.

| Row | Source boundary | Review |
|---|---|---|
| openPurchase | 47 | Admin and fresh id. |
| grantPermission | 52 | pullCollection first, then admin. Absent ids refuse; successful open then grant sets the flag. |
| denyPermission | 56 | Same lookup/admin prerequisite; refunds both pledge lists and removes collection. |
| deposit | 62 | Admin, target member, different signer/target, nonnegative v. Zero succeeds; negative refuses. |
| withdraw | 67 | Admin, target member, different keys, account affordability, not stalled. There is no nonnegative-v guard; the table correctly does not invent one. |
| transferCassa | 74 | Both keys admin, different, positive v. Receiver signer gains cassa, from-key loses it. |
| donate | 79 | Admin and positive v; raises cassa plus comune conto, no member credit. |
| backdonate | 85 | Admin, positive w, comune covers memberKeys.length*w, explicit auth callback. No separate stall predicate. |
| pledge | 94 | Existing collection, admin signer, target member, absent from both pledge lists, positive affordable v, not stalled. |
| acceptPledge | 104 | Existing collection and pending user pledge, admin/referente signer, not stalled. |
| refusePledge | 111 | Existing collection and pending user pledge, admin/referente signer; no stall guard. |
| correctPledge | 118 | Existing collection and accepted user pledge, admin/referente signer, nonnegative replacement and post-account balance. No pending correction route yet. |
| closePurchase | 126 | Existing collection, admin/referente, permitted, pending empty, not stalled; removes collection and debits cassa by accepted sum. |
| failPurchase | 133 | Existing collection, admin/referente, pending empty; refunds and removes, no stall guard. |

Line numbers identify each arm at PIN. AUTH itself is role-only: Predicates.lean:74
ignores both states and all arguments other than author. The corrected absent-member
example is valid and does not claim a real step accepts the event. The pledge-agency
and one-signer/group-permission tensions remain explicit and pending.

Citation compliance is separate: ten of the twelve displayed table rows have no
lean marker at all (the two combined event rows make fourteen events into twelve
rows). The grant/deny row has its pullCollection marker, and backdonate has memberKeys;
the others rely on surrounding prose. R71-11 explicitly requires these semantic rows
to carry their own marker blocks. The first three pending rows and the Voci fact
likewise have no co-located marker. F-B01 records this actual candidate gap, in
addition to the two executed checker survivors.

## Laws and economic sentences

- L1: governance_enacts_windUpAdmin establishes no surviving collection has the
  wound-up key as referente. The cleanup finite check is explicitly a finite
  companion. This does not become a new group-membership authorization theorem.
- L2: close_permission_to_close yields a pulled collection with permission and
  empty pending. Its single-admin flag limitation and missing voting wire are stated.
- L3: pledge_escrow_debit yields account debit plus escrowHeld in the resulting
  collection. The escrow delta follows the actual pledge arm; conservation is the
  separate conservation_preserved law, not an additional conjunct in this theorem.
  The document groups these model facts in prose; it must not be used as an exact
  transcription of pledge_escrow_debit's type.
- L4: close_spends_referente yields the cassa debit by accepted sum. Successful close
  requires empty pending, so this is the full escrow. The old account-credit error
  is repaired. The 30-unit journey reaches close: cassa 30 to 0, escrow 30 to 0,
  conti unchanged; false account-credit and true cassa-debit predicates distinguish
  the two descriptions.
- L5: deposit_double_entry uses +v on both sides, withdraw_double_entry uses -v.
  Deposit -1/0/+1 refuses/accepts/accepts. The stored-zero and triplet repairs agree
  with live outputs in T2 leg 14 and the retained supplemental output.
- L6: conservation has the displayed cassa-minus-conti-minus-escrow equation;
  conservation_preserved requires conservation before a successful step. This is
  model preservation, not proof of physical cash custody.
- L7: solvent includes member-account nonnegativity and pledge-amount nonnegativity.
  solvent_preserved also requires Reach; reach_solvent and not_insolvent_of_reach
  concern economic Reach under a fixed view/auth, with the reserved key excluded at
  boot. The member/comune distinction is present. No new integrated-completeness
  result is inferred.
- L8: duplicate user membership in either pledge list rejects another pledge.
  uniquePledges_pend_cons and pledge_preserves_allUnique are preservation from their
  stated uniqueness premises; they are not unconditional cleanliness of arbitrary
  hand-constructed states.

The 29-name category is explicitly not a census. The dotted majority_table marker
resolves to a finite conjunction over six admin-count exhibits. open_questions_are_open
is universally quantified but conditional on the question still being present;
the V-7/S5 retention caveat correctly states its limitation.

R71-09 nevertheless remains incomplete: the later committed-versus-available
correction in the 2026-08-26 ruling is absent. L4's corrected ledger direction does
not supply that missing product rationale. See F-B02.

## Composition, voting, corpus and pending rows

Compiler-produced direct-import indexes show Reactivegas.Step's import closure
excludes Reactivegas.Composition; the only direct importer is Reactivegas, the
library aggregator. No fresh delete-file build was spent. The document's historical
delete-file result is not reported as an independently repeated control.

appDecided_verdict_exhaustive relates a record's verdict to appVerdictAllows. The
event/record are separate inputs and the route hypothesis is unused in its proof.
Reachability, target and polarity gaps are all disclosed. PROVED-IN-MODEL and the
two-consumer NOTE-016/A-Q001/NOTE-031 chain agree with their supplied rulings.

Vote.Apply routes through one validateVoteEvent before effectedState and sweep.
Open questions have empty tallies and fresh-id protection. The renounce event
effect is identity; normal integrated normalized states retain that no-op. V-5's
required proposer closure/refund is not implemented. notDesignee/notProposer remain
declared-only; current validation constructs only notResponsabile/questionNotFound.
Neither dormant refusal is promoted to a ruling. closureCause produces tally or
franchiseChange, with proposerDeparted/renounced still unproduced.

At af9c1e5 the frozen VOTE_TRACES_V1 envelope has one V trace and fifteen signed
steps. Its initial/input/result are standalone vote payloads, not integrated
economic states. This was read from the actual Git blob, not executed or accepted
as a simulator audit. At fed19b3, the economic corpus has five traces/32 events and
the integrated corpus seven base steps; neither emits a signed vote event. The
integrated steps include the admin-loss V-3 sweep. The refusing auth wrapper and
s62bThreshold=legacyThreshold resolution agree with CORPUS-COVERAGE. legacyThreshold
and zeroThreshold remain explicitly exhibits, not a chosen product default.

| Pending row | Fresh comparison |
|---|---|
| S1 / #79 | Trace manifest is repaired at PIN; one frozen economic refusal is still labelled UNPROVED, alongside one step_close_inv row. The row uses past tense for the manifest defect, says S1 landed, and defers the byte change to corpus re-freeze. It is not six frozen bytes. |
| #68 | Pinned faithful and integrated proposal paths still start approvals with signer. New zero-open arithmetic/other-assent ruling is pending. Live PR80 read: OPEN, mergedAt null, head d68a783. No newer inversion-assessment claim is imported into this design record. |
| #69 | Pinned step still requires admin pledge and referente correction of accepted pledges. New signer==u pending regime, zero withdrawal, retained accepted regime/solvency/closure/UI obligations match the dated ruling. |
| #81 | V-5 pending behavior and source pointer are present; the repaired row has all five cells. Retention caveat remains assigned to S5. |

#75/#76 are named planned, not delivered; both live issues remain open. The prose
says to see the pending table for them, although the four-row table contains no
dedicated #75/#76 entries. This cross-reference is imprecise but the planned status
is explicit in the surrounding text. The reconciliation hook remains merge-triggered.

## Voci, closure and the companion matrix

Git's recursive blob inventory contains 21 Voci files, including two distinct
Quantita/Quantità blobs. Eventi/Impegno's order-bound variants are commented out;
the Lean Pledge remains bare. Fact, ruling, reason/cost and open question are all
present; no catalogue implementation or operator choice is invented.

canCloseGroup is one source definition and no consumer: member balances zero,
collections empty, every cash-box balance zero. The document classifies a missing
guarantee and does not invent a group-closure theorem or an event. The actual .lake
exclusion and synthetic poison/real-source positive control both hold.

The companion matrix's eight changed fragments are parenthesized citations only;
removing those additions reproduces the base byte-for-byte. Its historical fold
claims match the pinned KelGroups.Fold/Validate/State surfaces: proposer approval,
replacement, retry/idempotence, threshold-before-effect, signer-discarding generic
app fold, bootstrap omission, validation order and role predicate bypasses. The
invariant statements keep their well-formedness/production-enactment premises;
the stale/bootstrap witnesses are finite. Forty Tests.lean guards are present
and T2 built that target. This review does not re-certify the external Haskell
repository's historical fidelity independently.

One current-pin error remains: R-22 says ten validation errors. The actual fresh
compiled index contains eleven constructor definitions, including reservedKey,
which validateDirectAdmission can return. See F-B03.

## Scope and evidence limits

All 117 unique marker spellings associate with a definition in fresh T2 compiler
indexes; these indexes are retained. The supplemental #check driver successfully
read the ordinary names and theorem axiom reports but exited 1 on auditor mistakes
in private-name syntax, so neither driver attempt is a GREEN receipt. Reading the
already-built indexes subsequently resolved all three private names without another
execution. No missing/private declaration is asserted as a source defect.

Full gate, each fresh document mutant and source/compiled/ruling reads are distinct
evidence classes. No prose mutation is called a Lean mutation kill. No new model
soundness, external runtime, remote CI, or acceptance claim is made.
