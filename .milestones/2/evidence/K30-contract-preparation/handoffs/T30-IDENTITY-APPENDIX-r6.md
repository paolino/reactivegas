# T30-IDENTITY-APPENDIX-r6 — enumerated private names + projection statement (kelgroups #30)

Appends to `T30-IDENTITY-MAP-r5.md` (retained unmodified). Preparation-only,
spend 0. Method stated (NOTE-006-compliant order): FULL FILE READS FIRST
(Vote/Invariants.lean 1228 lines + Vote/Tests.lean 397 lines read at r5;
KelGroups/Event.lean 52 + Types.lean 166 read at r5), name-lists extracted
after as a CROSS-CHECK of enumeration completeness — never as the authority.

## A. All 40 `private` identities, Vote/Invariants.lean (exact subject)

Shared exclusion reason (one reason, forty exact names — grouped exclusion
with exact declared subject + explicit review basis per NOTE-007(e)):
`private theorem` = proof term elaborated as a `Prop` proof; no runtime
content, no mirror obligation, NO Haskell requirement invented. Basis: full
file read + Lean `private theorem` semantics + name-list cross-check (40/40
accounted below; extraction file retained at
`t30-contract/scratch-privnames.txt`).

assocErase_sublist'; assocErase_keys_nodup'; assocErase_key_absent';
assocInsert_keys_nodup'; assocAdjust_keys'; assocAdjust_keys_nodup';
assocLookup_some_mem'; mem_assocLookup_some'; assocErase_other_lookup;
assocInsert_other_lookup; assocInsert_mem_cases; mem_map_fst_erase_of_ne;
mem_map_fst_insert; setInsert_mem_cases; nodup_append_mem;
setInsert_nodup'; mem_erase_inv; nodup_erase; closed_guard_absent;
assoc_entries_key_unique; filterMap_keys_nodup; sweepStep_key;
sweepClosures_open_mem; sweepClosures_closed_mem; sweepClosures_sweepReady;
sweepClosures_wellFormed; placeBallot_clean; placeBallot_tally;
effectedState_sweepReady; foldFrom_preserves_wellFormed; foldVote_append;
sweepClosures_preserves_qid; effectedState_preserves_qid;
applyVoteEvent_preserves_qid; foldFrom_preserves_qid;
sweepClosures_tallyKeys; tallyKeysOfState_erased_le;
tallyKeysOfState_insert_cases; effectedState_tally_growth;
tally_keys_franchised_from.

## B. Verified declaration lists, KelGroups context files (read in full)

`KelGroups/Event.lean` (52 lines): inductives Proposal (introduceMember /
removeMember / changeRoles — historical #54 evidence, untouched),
BaseEvent (propose / approve — historical), GroupEvent(base / app —
historical), DirectCommand (admitMember — LANDED direct-only, R30-8
context), BaseMutation (removeMember / changeRoles — LANDED non-admitting;
M10a splice site, NOT under Vote.*), BaseChange (memberAdmitted /
memberRemoved / rolesChanged — LANDED hook evidence). Reviewed doc facts:
BaseMutation "adding an admission constructor stops the exhaustive
enactment matching compiling"; BaseChange "adding a fourth stops the hook
compiling" — corroborate M10a compiler-totality framing + hook context.
`KelGroups/Types.lean` (166 lines): abbrevs Key/Email/RoleName/ProposalId;
Admin (publicAdmin/privateAdmin); Role (adminRole/appRole); Member;
isAdminRole/hasAdmin; RoleDef/GroupConfig; setInsert GUARDED
(`if values.contains value then values else value :: values` — D4 premise
source-verified; M4b inverts this exact guard); assocLookup/Erase/Insert/
Adjust + 4 assoc theorems (proofs, excluded); GroupView + lookupMember/
isMember/isAdmin/admins/adminCount (LANDED R30-6 context).

## C. Reconciled projection statement (NOTE-007(d) — what is bound, what is
projected out, verified how)

BOUND through the frozen commit (12 paths — every file whose facts this
contract uses): lean/KelGroups/Vote/{Types,State,Event,Validate,Fold,
Invariants,Tests}.lean + lean/KelGroups/{Integration,State,Validate,
Event,Types}.lean. PROJECTED OUT with reason + verification (not
whitelist-fixed): lean/KelGroups/{Fold,Invariants,Tests}.lean — zero
Vote-subtree declaration identifiers verified (Ballot/Verdict/
QuestionKind/ClosureCause/VoteEvent/VoteError/placeBallot/sweepClosures/
verdictOf counts all 0 in all three files); content = base-fold machinery
(Fold: applyEventDetailed/foldGroup historical — S28-established
non-production), base well-formedness Props + theorems (Invariants:
PendingWellFormed/MembersCoherent/WellFormed + preservation theorems incl.
proposer_mem_approvals — the #68-target theorem, cited as R30-9 context),
base fixtures + majority checks (Tests). No #30 row consumes them; any
future landing adding Vote identifiers there trips the mapping's module
review at rebind (process rule, stated). The single comment-level "Vote"
mention (KelGroups/Invariants.lean:6 header prose) is prose, not a
declaration — recorded so no one re-discovers it as a gap.

## D. Review-corroboration register (eyeballed at review + audit, never gate
kills — no invented requirements)

Threshold theorems → parameterization + no-default fence (R-48
never-freeze warning transferred with Tests.lean citation); refusal
theorems (R57-03/04) → M1/M2 rows; partition → M9 (+ M15 flip side);
no-expiry premise → REQ-NOEXPIRY (franchise-conjunct removal corroborates
R62-11 placement); franchise theorem → M7/M13 (post-view caveat carried);
idempotence theorem + sweepDuplicating → REQ-SWEEP-IDEM + M15 (Lean's own
mutant shape); `#print axioms` 9 names → Lean-side vanishing detection
(referenced mechanism); Tests.lean witnesses → per-REQ agreement eyeball;
auditor-instrument citation (`nonresponsabile-open.lean` sha256 `1f7aa80a`)
→ M1/M2 control-shape precedent; R-53 dual-view caveat → M7/M13 stated
limit (fixtures observe sensitivity, claim no produced transition).
