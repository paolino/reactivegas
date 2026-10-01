# Functions model — #81

Ceiling 30 lines / 2000 bytes. New or changed signatures only.

| ID | Signature | Constraint |
|---|---|---|
| F81-CLOSE-PROPOSER | `KelGroups.Vote.closeProposerQuestions (proposer : Key) (gs : VoteState) : VoteState` | total; D-1, D-2; no view or threshold argument (the rule is not a tally). |
| F81-VALIDATE | `KelGroups.Vote.validateVoteEvent` — signature unchanged | gains the INV81-ORDER refusals. |
| F81-EFFECT | `KelGroups.Vote.effectedState` — signature unchanged | renounce arm implements D-1/D-4. |
| F81-HOOK | `Reactivegas.baseHook` — signature unchanged | M81-HOOK. |
