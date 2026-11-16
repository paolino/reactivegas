import KelGroups.Fold
import KelGroups.Validate
import KelGroups.Integration

/-!
# Vote-machine invariants and counterexamples

The first five invariants are stated over `WellFormed`, the representation
invariant established by the empty state and preserved by the fold. Enactment
provenance comes from `StepResult`: production transitions emit it only from
the successful pre-state threshold branch.
-/

namespace KelGroups

variable {α κ ν : Type}

def PendingWellFormed (pending : PendingProposal) : Prop :=
  pending.approvals.Nodup ∧ pending.proposer ∈ pending.approvals

def MembersCoherent (gs : GroupState α) : Prop :=
  ∀ key member, (key, member) ∈ gs.members → member.key = key

def PendingCoherent (gs : GroupState α) : Prop :=
  ∀ proposalId pending, (proposalId, pending) ∈ gs.pendingProposals →
    PendingWellFormed pending

structure WellFormed (gs : GroupState α) : Prop where
  memberKeys : (gs.members.map Prod.fst).Nodup
  pendingKeys : (gs.pendingProposals.map Prod.fst).Nodup
  membersCoherent : MembersCoherent gs
  pendingCoherent : PendingCoherent gs

private theorem assocErase_sublist [BEq κ] (key : κ) (entries : List (κ × ν)) :
    (assocErase key entries).Sublist entries := by
  induction entries with
  | nil => simp [assocErase]
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocErase]
      split
      · exact List.Sublist.cons (candidate, value) (List.Sublist.refl rest)
      · exact List.Sublist.cons₂ (candidate, value) ih

private theorem assocErase_keys_nodup [BEq κ] (key : κ) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    ((assocErase key entries).map Prod.fst).Nodup :=
  (assocErase_sublist key entries).map Prod.fst |>.nodup h

private theorem assocErase_key_absent [BEq κ] [LawfulBEq κ]
    (key : κ) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    key ∉ (assocErase key entries).map Prod.fst := by
  induction entries with
  | nil => simp [assocErase]
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      have hn := List.nodup_cons.mp h
      simp only [assocErase]
      split
      · next equal =>
          have : candidate = key := beq_iff_eq.mp equal
          subst candidate
          exact hn.1
      · next different =>
          simp only [List.map_cons, List.mem_cons]
          intro present
          rcases present with equal | inTail
          · exact different (beq_iff_eq.mpr equal.symm)
          · exact ih hn.2 inTail

private theorem assocInsert_keys_nodup [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    ((assocInsert key value entries).map Prod.fst).Nodup := by
  simp only [assocInsert, List.map_cons, List.nodup_cons]
  exact ⟨assocErase_key_absent key entries h, assocErase_keys_nodup key entries h⟩

private theorem assocErase_property [BEq κ] (key : κ) (entries : List (κ × ν))
    (property : κ → ν → Prop)
    (h : ∀ candidate value, (candidate, value) ∈ entries → property candidate value) :
    ∀ candidate value, (candidate, value) ∈ assocErase key entries →
      property candidate value := by
  intro candidate value hmem
  exact h candidate value ((assocErase_sublist key entries).mem hmem)

private theorem assocInsert_property [BEq κ] (key : κ) (value : ν)
    (entries : List (κ × ν)) (property : κ → ν → Prop)
    (hnew : property key value)
    (hold : ∀ candidate old, (candidate, old) ∈ entries → property candidate old) :
    ∀ candidate current, (candidate, current) ∈ assocInsert key value entries →
      property candidate current := by
  intro candidate current hmem
  simp only [assocInsert, List.mem_cons] at hmem
  rcases hmem with hnewEntry | holdEntry
  · cases hnewEntry
    exact hnew
  · exact assocErase_property key entries property hold candidate current holdEntry

private theorem assocAdjust_keys (key : Key) (f : ν → ν) (entries : List (Key × ν)) :
    (assocAdjust key f entries).map Prod.fst = entries.map Prod.fst := by
  induction entries with
  | nil => rfl
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocAdjust]
      split <;> simp [ih]

private theorem assocAdjust_property (key : Key) (f : ν → ν)
    (entries : List (Key × ν)) (property : Key → ν → Prop)
    (hchange : ∀ value, property key value → property key (f value))
    (hold : ∀ candidate value, (candidate, value) ∈ entries → property candidate value) :
    ∀ candidate value, (candidate, value) ∈ assocAdjust key f entries →
      property candidate value := by
  induction entries with
  | nil => simp [assocAdjust]
  | cons entry rest ih =>
      obtain ⟨headKey, headValue⟩ := entry
      simp only [assocAdjust]
      split
      · next equal =>
          have hkey : headKey = key := beq_iff_eq.mp equal
          subst headKey
          intro candidate value hmem
          simp only [List.mem_cons] at hmem
          rcases hmem with hhead | htail
          · injection hhead with keyEq valueEq
            subst candidate
            subst value
            exact hchange headValue (hold key headValue (by simp))
          · exact hold candidate value (List.mem_cons_of_mem _ htail)
      · next different =>
          intro candidate value hmem
          simp only [List.mem_cons] at hmem
          rcases hmem with hhead | htail
          · cases hhead
            exact hold headKey headValue (List.mem_cons_self)
          · exact ih (fun candidate value hmem =>
              hold candidate value (List.mem_cons_of_mem _ hmem)) candidate value htail

private theorem setInsert_nodup (value : Key) (values : List Key)
    (h : values.Nodup) : (setInsert value values).Nodup := by
  simp only [setInsert]
  split
  · exact h
  · next absent =>
      exact List.nodup_cons.mpr ⟨by simpa using absent, h⟩

private theorem setInsert_mem (value existing : Key) (values : List Key)
    (h : existing ∈ values) : existing ∈ setInsert value values := by
  unfold setInsert
  split
  · exact h
  · exact List.mem_cons_of_mem value h

private theorem approvePending_wellFormed (signer : Key) (pending : PendingProposal)
    (h : PendingWellFormed pending) : PendingWellFormed (approvePending signer pending) := by
  constructor
  · exact setInsert_nodup signer pending.approvals h.1
  · exact setInsert_mem signer pending.proposer pending.approvals h.2

theorem emptyState_wellFormed (initial : α) : WellFormed (emptyState initial) := by
  exact ⟨by simp [emptyState], by simp [emptyState], by simp [MembersCoherent, emptyState],
    by simp [PendingCoherent, emptyState]⟩

private theorem assocLookup_some_mem [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν))
    (h : assocLookup key entries = some value) : (key, value) ∈ entries := by
  induction entries with
  | nil => simp [assocLookup] at h
  | cons entry rest ih =>
      obtain ⟨candidate, current⟩ := entry
      simp only [assocLookup] at h
      split at h
      · next equal =>
          have keyEq : candidate = key := beq_iff_eq.mp equal
          subst candidate
          simp only [Option.some.injEq] at h
          subst current
          exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih h)

theorem enact_preserves_wellFormed (gs : GroupState α) (proposal : Proposal)
    (h : WellFormed gs) : WellFormed (enact gs proposal) := by
  cases proposal with
  | introduceMember key email roles =>
      let introduced : Member := { key, email, roles }
      refine
        { memberKeys := assocInsert_keys_nodup key introduced gs.members h.memberKeys
          pendingKeys := h.pendingKeys
          membersCoherent := ?_
          pendingCoherent := h.pendingCoherent }
      exact assocInsert_property key introduced gs.members
        (fun candidate member => member.key = candidate) rfl h.membersCoherent
  | removeMember key =>
      refine
        { memberKeys := assocErase_keys_nodup key gs.members h.memberKeys
          pendingKeys := h.pendingKeys
          membersCoherent := ?_
          pendingCoherent := h.pendingCoherent }
      exact assocErase_property key gs.members
        (fun candidate member => member.key = candidate) h.membersCoherent
  | changeRoles key roles =>
      refine
        { memberKeys := ?_
          pendingKeys := h.pendingKeys
          membersCoherent := ?_
          pendingCoherent := h.pendingCoherent }
      · simpa [enact, assocAdjust_keys key (fun member : Member => { member with roles })]
          using h.memberKeys
      · exact assocAdjust_property key (fun member : Member => { member with roles })
          gs.members (fun candidate member => member.key = candidate)
          (by intro member coherent; exact coherent) h.membersCoherent

theorem finishEnact_preserves_wellFormed (gs : GroupState α) (proposalId : ProposalId)
    (pending : PendingProposal) (h : WellFormed gs) :
    WellFormed (finishEnact gs proposalId pending) := by
  have enacted := enact_preserves_wellFormed gs pending.proposal h
  refine
    { memberKeys := enacted.memberKeys
      pendingKeys := assocErase_keys_nodup proposalId
        (enact gs pending.proposal).pendingProposals enacted.pendingKeys
      membersCoherent := enacted.membersCoherent
      pendingCoherent := ?_ }
  exact assocErase_property proposalId (enact gs pending.proposal).pendingProposals
    (fun _ pending => PendingWellFormed pending) enacted.pendingCoherent

theorem tryEnact_preserves_wellFormed (gs : GroupState α) (proposalId : ProposalId)
    (h : WellFormed gs) : WellFormed (tryEnact gs proposalId) := by
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [tryEnact, tryEnactDetailed, hlookup] using h
  | some pending =>
      by_cases threshold : pending.approvals.length ≥ majority gs
      · simpa [tryEnact, tryEnactDetailed, hlookup, threshold] using
          finishEnact_preserves_wellFormed gs proposalId pending h
      · simpa [tryEnact, tryEnactDetailed, hlookup, threshold] using h

theorem applyPropose_preserves_wellFormed (digest : Proposal → ProposalId)
    (gs : GroupState α) (signer : Key) (proposal : Proposal) (h : WellFormed gs) :
    WellFormed (applyPropose digest gs signer proposal) := by
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [signer] }
  let proposed : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  have hpending : PendingWellFormed pending := by
    simp [PendingWellFormed, pending]
  have hproposed : WellFormed proposed :=
    { memberKeys := h.memberKeys
      pendingKeys := assocInsert_keys_nodup proposalId pending gs.pendingProposals h.pendingKeys
      membersCoherent := h.membersCoherent
      pendingCoherent := assocInsert_property proposalId pending gs.pendingProposals
        (fun _ pending => PendingWellFormed pending) hpending h.pendingCoherent }
  change WellFormed (tryEnact proposed proposalId)
  exact tryEnact_preserves_wellFormed proposed proposalId hproposed

theorem applyApprove_preserves_wellFormed (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (h : WellFormed gs) :
    WellFormed (applyApprove gs signer proposalId) := by
  unfold applyApprove applyApproveDetailed
  cases hlookup : lookupPending proposalId gs with
  | none => exact h
  | some pending =>
      let approved := approvePending signer pending
      let updated : GroupState α :=
        { gs with pendingProposals := assocInsert proposalId approved gs.pendingProposals }
      have hpending : PendingWellFormed pending :=
        h.pendingCoherent proposalId pending
          (assocLookup_some_mem proposalId pending gs.pendingProposals hlookup)
      have hupdated : WellFormed updated :=
        { memberKeys := h.memberKeys
          pendingKeys := assocInsert_keys_nodup proposalId approved
            gs.pendingProposals h.pendingKeys
          membersCoherent := h.membersCoherent
          pendingCoherent := assocInsert_property proposalId approved gs.pendingProposals
            (fun _ pending => PendingWellFormed pending)
            (approvePending_wellFormed signer pending hpending) h.pendingCoherent }
      exact tryEnact_preserves_wellFormed updated proposalId hupdated

theorem applyEvent_preserves_wellFormed (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (gs : GroupState α) (signer : Key)
    (event : GroupEvent α) (h : WellFormed gs) :
    WellFormed (applyEvent digest appFoldFn gs signer event) := by
  cases event with
  | app _ =>
      exact
        { memberKeys := h.memberKeys
          pendingKeys := h.pendingKeys
          membersCoherent := h.membersCoherent
          pendingCoherent := h.pendingCoherent }
  | base baseEvent =>
      cases baseEvent with
      | propose proposal => exact applyPropose_preserves_wellFormed digest gs signer proposal h
      | approve proposalId => exact applyApprove_preserves_wellFormed gs signer proposalId h

private theorem foldEvents_preserves_wellFormed (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (events : List (Key × GroupEvent α))
    (gs : GroupState α) (h : WellFormed gs) :
    WellFormed (events.foldl
      (fun current signed => applyEvent digest appFoldFn current signed.1 signed.2) gs) := by
  induction events generalizing gs with
  | nil => exact h
  | cons signed rest ih =>
      exact ih (applyEvent digest appFoldFn gs signed.1 signed.2)
        (applyEvent_preserves_wellFormed digest appFoldFn gs signed.1 signed.2 h)

theorem foldGroup_wellFormed (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (initial : α) (events : List (Key × GroupEvent α)) :
    WellFormed (foldGroup digest appFoldFn initial events) :=
  foldEvents_preserves_wellFormed digest appFoldFn events (emptyState initial)
    (emptyState_wellFormed initial)

theorem approvals_nodup {gs : GroupState α} (h : WellFormed gs)
    (entry : ProposalId × PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) : entry.2.approvals.Nodup :=
  (h.pendingCoherent entry.1 entry.2 hentry).1

theorem proposer_mem_approvals {gs : GroupState α} (h : WellFormed gs)
    (entry : ProposalId × PendingProposal) (hentry : entry ∈ gs.pendingProposals) :
    entry.2.proposer ∈ entry.2.approvals :=
  (h.pendingCoherent entry.1 entry.2 hentry).2

def Enacts (gs : GroupState α) (proposalId : ProposalId) (result : GroupState α) : Prop :=
  ∃ enacted,
    (tryEnactDetailed gs proposalId).enactment = some enacted ∧
    result = (tryEnactDetailed gs proposalId).state

private theorem tryEnactDetailed_enactment_threshold_met
    (gs : GroupState α) (proposalId : ProposalId) (enacted : Enactment α)
    (h : (tryEnactDetailed gs proposalId).enactment = some enacted) :
    enacted.pending.approvals.length ≥ majority enacted.preState := by
  unfold tryEnactDetailed at h
  split at h
  · simp at h
  · next pending hlookup =>
      split at h
      · next threshold =>
          simp only [Option.some.injEq] at h
          cases h
          exact threshold
      · simp at h

theorem enact_implies_threshold_met
    (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α)
    (enacted : Enactment α)
    (h : (applyEventDetailed digest appFoldFn gs signer event).enactment =
      some enacted) :
    enacted.pending.approvals.length ≥ majority enacted.preState := by
  cases event with
  | app _ => simp [applyEventDetailed] at h
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          change (applyProposeDetailed digest gs signer proposal).enactment =
            some enacted at h
          unfold applyProposeDetailed at h
          exact tryEnactDetailed_enactment_threshold_met _ _ _ h
      | approve proposalId =>
          change (applyApproveDetailed gs signer proposalId).enactment =
            some enacted at h
          unfold applyApproveDetailed at h
          cases hlookup : lookupPending proposalId gs with
          | none => simp [hlookup] at h
          | some pending =>
              simp only [hlookup] at h
              exact tryEnactDetailed_enactment_threshold_met _ _ _ h

theorem tryEnact_eq_of_enacts {gs result : GroupState α} {proposalId : ProposalId}
    (h : Enacts gs proposalId result) :
    tryEnact gs proposalId = result := by
  obtain ⟨_, _, rfl⟩ := h
  rfl

theorem member_key_coherent {gs : GroupState α} (h : WellFormed gs)
    (key : Key) (member : Member)
    (hmember : (key, member) ∈ gs.members) : member.key = key :=
  h.membersCoherent key member hmember

theorem members_change_implies_enacted
    (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α)
    (hchange : (applyEvent digest appFoldFn gs signer event).members ≠ gs.members) :
    ∃ before proposalId,
      Enacts before proposalId (applyEvent digest appFoldFn gs signer event) := by
  cases event with
  | app appEvent =>
      simp [applyEvent, applyEventDetailed] at hchange
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          let proposalId := digest proposal
          let pending : PendingProposal :=
            { proposal, proposer := signer, approvals := [signer] }
          let before : GroupState α :=
            { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
          have hlookup : lookupPending proposalId before = some pending := by
            exact assocLookup_insert_self proposalId pending gs.pendingProposals
          have happly : applyEvent digest appFoldFn gs signer
              (.base (.propose proposal)) = tryEnact before proposalId := rfl
          by_cases hthreshold : pending.approvals.length ≥ majority before
          · have hout : tryEnact before proposalId =
                finishEnact before proposalId pending := by
              simp [tryEnact, tryEnactDetailed, hlookup, hthreshold]
            refine ⟨before, proposalId,
              { proposalId, pending, preState := before }, ?_, ?_⟩
            · simp [tryEnactDetailed, hlookup, hthreshold]
            · simpa [tryEnact] using happly
          · have hout : tryEnact before proposalId = before := by
              simp [tryEnact, tryEnactDetailed, hlookup, hthreshold]
            rw [happly, hout] at hchange
            exact False.elim (hchange rfl)
      | approve proposalId =>
          cases hlookup : lookupPending proposalId gs with
          | none =>
              have happly : applyEvent digest appFoldFn gs signer
                  (.base (.approve proposalId)) = gs := by
                simp [applyEvent, applyEventDetailed, applyApproveDetailed, hlookup]
              exact False.elim (hchange (congrArg GroupState.members happly))
          | some pending =>
              let approved : PendingProposal := approvePending signer pending
              let before : GroupState α :=
                { gs with pendingProposals :=
                    assocInsert proposalId approved gs.pendingProposals }
              have hupdated : lookupPending proposalId before = some approved := by
                exact assocLookup_insert_self proposalId approved gs.pendingProposals
              have happly : applyEvent digest appFoldFn gs signer
                  (.base (.approve proposalId)) = tryEnact before proposalId := by
                simp [applyEvent, applyEventDetailed, applyApproveDetailed,
                  tryEnact, hlookup, before, approved]
              by_cases hthreshold : approved.approvals.length ≥ majority before
              · have hout : tryEnact before proposalId =
                    finishEnact before proposalId approved := by
                  simp [tryEnact, tryEnactDetailed, hupdated, hthreshold]
                refine ⟨before, proposalId,
                  { proposalId, pending := approved, preState := before }, ?_, ?_⟩
                · simp [tryEnactDetailed, hupdated, hthreshold]
                · simpa [tryEnact] using happly
              · have hout : tryEnact before proposalId = before := by
                  simp [tryEnact, tryEnactDetailed, hupdated, hthreshold]
                rw [happly, hout] at hchange
                exact False.elim (hchange rfl)

private def adminMember (key : Key) : Member :=
  { key, email := key ++ "@example.test", roles := [.adminRole .publicAdmin] }

def majorityAdminState (keys : List Key) : GroupState Unit :=
  { members := keys.map fun key => (key, adminMember key),
    pendingProposals := [], pendingBase := [], appFold := () }

theorem majority_table :
    majority (majorityAdminState []) = 0 ∧
    majority (majorityAdminState ["a"]) = 1 ∧
    majority (majorityAdminState ["a", "b"]) = 1 ∧
    majority (majorityAdminState ["a", "b", "c"]) = 2 ∧
    majority (majorityAdminState ["a", "b", "c", "d"]) = 2 ∧
    majority (majorityAdminState ["a", "b", "c", "d", "e"]) = 3 := by
  decide

theorem majority_not_strict_on_even (gs : GroupState α)
    (positive : 0 < adminCount gs) (even : adminCount gs % 2 = 0) :
    2 * majority gs ≤ adminCount gs := by
  cases count : adminCount gs with
  | zero => simp [count] at positive
  | succ _ => unfold majority; omega

private def witnessDigest : Proposal → ProposalId
  | .introduceMember key _ _ => "introduce:" ++ key
  | .removeMember key => "remove:" ++ key
  | .changeRoles key _ => "roles:" ++ key

private def witnessAdmin : Role := .adminRole .publicAdmin

private def witnessValidKey (_ : Key) : Bool := true
private def witnessConfig : GroupConfig Unit := { roleDefs := [] }
private def witnessAppFold (_ _ : Unit) : Unit := ()

private def witnessTraceValidFrom (gs : GroupState Unit) :
    List (Key × GroupEvent Unit) → Bool
  | [] => true
  | (signer, event) :: rest =>
      validateEvent witnessValidKey witnessConfig gs signer event == .ok () &&
        witnessTraceValidFrom
          (applyEvent witnessDigest witnessAppFold gs signer event) rest

private def witnessTraceValid (events : List (Key × GroupEvent Unit)) : Bool :=
  witnessTraceValidFrom (emptyState ()) events

private def witnessFold (events : List (Key × GroupEvent Unit)) : GroupState Unit :=
  foldGroup witnessDigest witnessAppFold () events

private def witnessIntroduceAdmin (key : Key) : Proposal :=
  .introduceMember key (key ++ "@example.test") [witnessAdmin]

private def stalePendingEvents : List (Key × GroupEvent Unit) :=
  [ ("stranger", .base (.propose (witnessIntroduceAdmin "a")))
  , ("a", .base (.propose (witnessIntroduceAdmin "b")))
  , ("a", .base (.propose (witnessIntroduceAdmin "c")))
  , ("a", .base (.propose
      (.introduceMember "later" "later@example.test" [])))
  , ("a", .base (.propose (.removeMember "c")))
  , ("b", .base (.approve "remove:c"))
  ]

def stalePendingWitness : GroupState Unit := witnessFold stalePendingEvents

#guard
  witnessTraceValid stalePendingEvents && adminCount stalePendingWitness == 2 &&
    match lookupPending "introduce:later" stalePendingWitness with
    | some pending => pending.approvals.length ≥ majority stalePendingWitness
    | none => false

private def bootstrapProposal : Proposal :=
  .introduceMember "founder" "founder@example.test" [witnessAdmin]

private def bootstrapNonMemberEvent : GroupEvent Unit :=
  .base (.propose bootstrapProposal)

private def bootstrapNonMemberEvents : List (Key × GroupEvent Unit) :=
  [("stranger", bootstrapNonMemberEvent)]

def bootstrapNonMemberWitness : GroupState Unit :=
  witnessFold bootstrapNonMemberEvents

def bootstrapNonMemberEnactment : StepResult Unit :=
  applyEventDetailed witnessDigest witnessAppFold (emptyState ())
    "stranger" bootstrapNonMemberEvent

#guard
  witnessTraceValid bootstrapNonMemberEvents &&
    bootstrapNonMemberEnactment.state == bootstrapNonMemberWitness &&
    !isMember "stranger" bootstrapNonMemberWitness &&
    lookupMember "founder" bootstrapNonMemberWitness != none &&
    lookupPending (witnessDigest bootstrapProposal) bootstrapNonMemberWitness == none &&
    match bootstrapNonMemberEnactment.enactment with
    | some enacted =>
        enacted.pending.proposer == "stranger" &&
          enacted.pending.approvals.contains "stranger" &&
          enacted.pending.approvals.length ≥ majority enacted.preState
    | none => false

/- The proposed CI-54-BOOTSTRAP-NO-PENDING invariant is false: once VI-6 has
left a stale sibling, successive validated removals can enact the last admins
while the sibling remains pending. -/
private def bootstrapPendingEvents : List (Key × GroupEvent Unit) :=
  stalePendingEvents ++
    [ ("a", .base (.propose (.removeMember "b")))
    , ("a", .base (.propose (.removeMember "a")))
    ]

def bootstrapPendingWitness : GroupState Unit := witnessFold bootstrapPendingEvents

#guard
  witnessTraceValid bootstrapPendingEvents && adminCount bootstrapPendingWitness == 0 &&
    lookupPending "introduce:later" bootstrapPendingWitness != none


/-! ### R62-04 — the integrated app boundary preserves canonical membership

`INV-62-PAYLOAD-ONLY` stated as a theorem about the production transition.
The type of `IntegratedAppFold` already makes a member relation *unreturnable*
by an app fold; these prove the transition around it does not write one either,
so the app payload is the only thing an app event can move.

The historical `applyEventDetailed` is not the subject here: both quantify over
`applyIntegratedEvent`. Reactivegas layers a reserved-comune production
root on top of this generic transition. -/

theorem app_event_preserves_members
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (event : AppEvent)
    (result : IntegratedResult AppState)
    (h : applyIntegratedEvent integration gs signer (IntegratedEvent.app event)
      = .ok result) :
    result.state.members = gs.members := by
  simp only [applyIntegratedEvent] at h
  split at h
  case isTrue =>
    split at h
    case h_1 appState hfold =>
      simp only [Except.ok.injEq] at h
      subst h
      rfl
    case h_2 err hfold => exact Except.noConfusion h
  case isFalse => exact Except.noConfusion h


/-- Companion to `app_event_preserves_members`: an app event never reports a
base change, so no downstream consumer can read one out of an app transition. -/
theorem app_event_has_no_base_change
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (event : AppEvent)
    (result : IntegratedResult AppState)
    (h : applyIntegratedEvent integration gs signer (IntegratedEvent.app event)
      = .ok result) :
    result.change = none := by
  simp only [applyIntegratedEvent] at h
  split at h
  case isTrue =>
    split at h
    case h_1 appState hfold =>
      simp only [Except.ok.injEq] at h
      subst h
      rfl
    case h_2 err hfold => exact Except.noConfusion h
  case isFalse => exact Except.noConfusion h


/-! ### R62-06, R62-09 — the one transition system

Three obligations about `applyIntegratedEvent`, stated before the routes that
satisfy them exist.

`direct_admission_requires_admin` and `non_admin_admission_is_noop` are
`INV-62-DIRECT-ONLY` at the substrate: the single direct command is admin-only,
target-absent and reserved-key-free, and its refusal is an *exact* error
identity that advances no state — never a silently accepted no-op.

`base_change_runs_hook` is `INV-62-ATOMIC-HOOK`: whenever a successful
transition reports a concrete base change, the payload an observer reads is the
one the sealed hook returned from the exact pre- and post-transition canonical
views. A route that committed the membership change and skipped the hook, or
handed the hook the pre view twice, cannot satisfy it. -/

/-- Inversion of the direct-admission validator: `.ok` forces all three
guards, so the guards cannot be reordered away. -/
theorem validateDirectAdmission_ok {α : Type} {reserved : Key} {gs : GroupState α}
    {signer target : Key} {email : Email} {roles : List Role}
    (h : validateDirectAdmission reserved gs signer target email roles = .ok ()) :
    isAdmin signer gs = true ∧ isMember target gs = false ∧ target ≠ reserved := by
  unfold validateDirectAdmission at h
  split at h
  · next hadmin =>
    split at h
    · exact Except.noConfusion h
    · next hreserved =>
      split at h
      · exact Except.noConfusion h
      · next hmember =>
        exact ⟨hadmin, Bool.eq_false_iff.mpr hmember, hreserved⟩
  · exact Except.noConfusion h

/-- Inversion of the sealed-hook commit: the committed change is the reported
one, and the observable payload *is* the hook's output read at the observable
post view. -/
theorem commitBaseChange_ok {AppState AppEvent BaseProposal AppError : Type}
    {integration : Integration AppState AppEvent BaseProposal AppError}
    {pre post : GroupState AppState} {change : BaseChange}
    {result : IntegratedResult AppState}
    (h : commitBaseChange integration pre post change = .ok result) :
    result.change = some change ∧
      integration.baseHook change (groupView pre) (groupView result.state) pre.appFold
        = .ok result.state.appFold := by
  unfold commitBaseChange at h
  split at h
  · next appState hhook =>
    simp only [Except.ok.injEq] at h
    subst h
    exact ⟨rfl, hhook⟩
  · exact Except.noConfusion h

/-- A successful `tryEnactBase` that reports a change ran the hook for it. -/
theorem tryEnactBase_runs_hook {AppState AppEvent BaseProposal AppError : Type}
    {integration : Integration AppState AppEvent BaseProposal AppError}
    {gs : GroupState AppState} {proposalId : ProposalId}
    {result : IntegratedResult AppState} {change : BaseChange}
    (h : tryEnactBase integration gs proposalId = .ok result)
    (hchange : result.change = some change) :
    integration.baseHook change (groupView gs) (groupView result.state) gs.appFold
      = .ok result.state.appFold := by
  unfold tryEnactBase at h
  split at h
  · simp only [Except.ok.injEq] at h
    subst h
    exact Option.noConfusion hchange
  · split at h
    · obtain ⟨hreported, hhook⟩ := commitBaseChange_ok h
      rw [hreported] at hchange
      cases Option.some.inj hchange
      exact hhook
    · simp only [Except.ok.injEq] at h
      subst h
      exact Option.noConfusion hchange

/-! ### `INV-62-DIRECT-ONLY` in its general form

The theorems above constrain the direct route. This one constrains *every*
route at once, and is what makes "one insertion path" a property of behaviour
rather than of a constructor count. A second admission route — a new
`DirectCommand` constructor that is also handled, an approval that inserts, an
app fold that grows the relation — makes it false, whatever the vocabulary
looks like. -/

/-- The committed aggregate's members relation is the one the route built. -/
theorem commitBaseChange_members {AppState AppEvent BaseProposal AppError : Type}
    {integration : Integration AppState AppEvent BaseProposal AppError}
    {pre post : GroupState AppState} {change : BaseChange}
    {result : IntegratedResult AppState}
    (h : commitBaseChange integration pre post change = .ok result) :
    result.state.members = post.members := by
  unfold commitBaseChange at h
  split at h
  · simp only [Except.ok.injEq] at h
    subst h
    rfl
  · exact Except.noConfusion h

/-- Neither voted base effect can make a stranger a member: removal erases and
a role change adjusts, and both preserve absence. -/
theorem enactMutation_preserves_absence {AppState : Type}
    (gs : GroupState AppState) (mutation : BaseMutation) (key : Key)
    (h : lookupMember key gs = none) :
    lookupMember key (enactMutation gs mutation) = none := by
  cases mutation with
  | removeMember other => exact assocLookup_erase_of_none key other gs.members h
  | changeRoles other roles =>
      exact assocLookup_adjust_of_none key other _ gs.members h

/-- Enacting a pending base mutation cannot make a stranger a member. -/
theorem tryEnactBase_preserves_absence {AppState AppEvent BaseProposal AppError : Type}
    {integration : Integration AppState AppEvent BaseProposal AppError}
    {gs : GroupState AppState} {proposalId : ProposalId}
    {result : IntegratedResult AppState} {key : Key}
    (h : tryEnactBase integration gs proposalId = .ok result)
    (habsent : lookupMember key gs = none) :
    lookupMember key result.state = none := by
  unfold tryEnactBase at h
  split at h
  · simp only [Except.ok.injEq] at h
    subst h
    exact habsent
  · split at h
    · show assocLookup key result.state.members = none
      rw [commitBaseChange_members h]
      exact enactMutation_preserves_absence _ _ key habsent
    · simp only [Except.ok.injEq] at h
      subst h
      exact habsent

/-- **One insertion path.** If a successful integrated transition made `key` a
member and `key` was not one before, then the event was the direct admission of
that exact key, the signer held an admin role in the pre-state, the key is not
the reserved one, and the result reports `memberAdmitted key`.

This is the property the frozen `G62-B-DIRECT-ADMIT` constructor-count leg was
written to enforce but cannot (its `fail` is not returned, so the row reports
PASS with two constructors — Q-001). It binds what a route *does* rather than
how many constructors are declared, so a second admission route makes it false
however it is spelled. -/
theorem membership_growth_is_direct_admission
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key)
    (event : IntegratedEvent BaseProposal AppEvent)
    (result : IntegratedResult AppState) (key : Key)
    (h : applyIntegratedEvent integration gs signer event = .ok result)
    (habsent : lookupMember key gs = none)
    (hpresent : lookupMember key result.state ≠ none) :
    isAdmin signer gs = true ∧ key ≠ integration.reserved ∧
      result.change = some (BaseChange.memberAdmitted key) := by
  cases event with
  | direct command =>
    cases command with
    | admitMember target email roles =>
      simp only [applyIntegratedEvent] at h
      split at h
      · exact Except.noConfusion h
      · next hvalid =>
        obtain ⟨hadmin, _, hreserved⟩ := validateDirectAdmission_ok hvalid
        by_cases htarget : target = key
        · subst htarget
          have hchange := (commitBaseChange_ok h).1
          exact ⟨hadmin, hreserved, hchange⟩
        · exfalso
          apply hpresent
          show assocLookup key result.state.members = none
          rw [commitBaseChange_members h]
          exact assocLookup_insert_of_none key target _ gs.members htarget habsent
  | propose proposal =>
    simp only [applyIntegratedEvent] at h
    split at h
    · exact Except.noConfusion h
    · exact absurd (tryEnactBase_preserves_absence h habsent) hpresent
  | approve proposalId =>
    simp only [applyIntegratedEvent] at h
    split at h
    · exact Except.noConfusion h
    · split at h
      · exact Except.noConfusion h
      · exact absurd (tryEnactBase_preserves_absence h habsent) hpresent
  | app appEvent =>
    exfalso
    apply hpresent
    show assocLookup key result.state.members = none
    rw [app_event_preserves_members integration gs signer appEvent result h]
    exact habsent

theorem direct_admission_requires_admin
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer key : Key) (email : Email)
    (roles : List Role) (result : IntegratedResult AppState)
    (h : applyIntegratedEvent integration gs signer
      (IntegratedEvent.direct (DirectCommand.admitMember key email roles))
      = .ok result) :
    isAdmin signer gs = true ∧ isMember key gs = false ∧ key ≠ integration.reserved := by
  simp only [applyIntegratedEvent] at h
  split at h
  · exact Except.noConfusion h
  · next hvalid => exact validateDirectAdmission_ok hvalid

theorem non_admin_admission_is_noop
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer key : Key) (email : Email)
    (roles : List Role) (h : isAdmin signer gs = false) :
    applyIntegratedEvent integration gs signer
        (IntegratedEvent.direct (DirectCommand.admitMember key email roles))
      = .error (IntegratedError.validation (ValidationError.notAnAdmin signer))
    ∧ foldIntegrated integration gs
        [(signer, IntegratedEvent.direct (DirectCommand.admitMember key email roles))]
      = gs := by
  have hstep :
      applyIntegratedEvent integration gs signer
          (IntegratedEvent.direct (DirectCommand.admitMember key email roles))
        = .error (IntegratedError.validation (ValidationError.notAnAdmin signer)) := by
    simp [applyIntegratedEvent, validateDirectAdmission, h]
  exact ⟨hstep, by simp [foldIntegrated, hstep]⟩

theorem base_change_runs_hook
    {AppState AppEvent BaseProposal AppError : Type}
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key)
    (event : IntegratedEvent BaseProposal AppEvent)
    (result : IntegratedResult AppState) (change : BaseChange)
    (h : applyIntegratedEvent integration gs signer event = .ok result)
    (hchange : result.change = some change) :
    integration.baseHook change (groupView gs) (groupView result.state) gs.appFold
      = .ok result.state.appFold := by
  cases event with
  | direct command =>
    cases command with
    | admitMember key email roles =>
      simp only [applyIntegratedEvent] at h
      split at h
      · exact Except.noConfusion h
      · obtain ⟨hreported, hhook⟩ := commitBaseChange_ok h
        rw [hreported] at hchange
        cases Option.some.inj hchange
        exact hhook
  | propose proposal =>
    simp only [applyIntegratedEvent] at h
    split at h
    · exact Except.noConfusion h
    · have hb := tryEnactBase_runs_hook h hchange
      exact hb
  | approve proposalId =>
    simp only [applyIntegratedEvent] at h
    split at h
    · exact Except.noConfusion h
    · split at h
      · exact Except.noConfusion h
      · have hb := tryEnactBase_runs_hook h hchange
        exact hb
  | app appEvent =>
    rw [app_event_has_no_base_change integration gs signer appEvent result h] at hchange
    exact Option.noConfusion hchange

#print axioms base_change_runs_hook

end KelGroups

/- The frozen gate prints these mandated names unqualified from the root
module. Keep root theorem aliases while the portable implementation remains in
the `KelGroups` namespace. -/
theorem approvals_nodup {α : Type} {gs : KelGroups.GroupState α}
    (h : KelGroups.WellFormed gs)
    (entry : KelGroups.ProposalId × KelGroups.PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) : entry.2.approvals.Nodup :=
  KelGroups.approvals_nodup h entry hentry

theorem proposer_mem_approvals {α : Type} {gs : KelGroups.GroupState α}
    (h : KelGroups.WellFormed gs)
    (entry : KelGroups.ProposalId × KelGroups.PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) : entry.2.proposer ∈ entry.2.approvals :=
  KelGroups.proposer_mem_approvals h entry hentry

theorem enact_implies_threshold_met {α : Type}
    (digest : KelGroups.Proposal → KelGroups.ProposalId)
    (appFoldFn : KelGroups.AppFold α) (gs : KelGroups.GroupState α)
    (signer : KelGroups.Key) (event : KelGroups.GroupEvent α)
    (enacted : KelGroups.Enactment α)
    (h : (KelGroups.applyEventDetailed digest appFoldFn gs signer event).enactment =
      some enacted) :
    enacted.pending.approvals.length ≥ KelGroups.majority enacted.preState :=
  KelGroups.enact_implies_threshold_met digest appFoldFn gs signer event enacted h

theorem members_change_implies_enacted {α : Type}
    (digest : KelGroups.Proposal → KelGroups.ProposalId)
    (appFoldFn : KelGroups.AppFold α) (gs : KelGroups.GroupState α)
    (signer : KelGroups.Key) (event : KelGroups.GroupEvent α)
    (hchange : (KelGroups.applyEvent digest appFoldFn gs signer event).members ≠ gs.members) :
    ∃ before proposalId,
      KelGroups.Enacts before proposalId
        (KelGroups.applyEvent digest appFoldFn gs signer event) :=
  KelGroups.members_change_implies_enacted digest appFoldFn gs signer event hchange

theorem member_key_coherent {α : Type} {gs : KelGroups.GroupState α}
    (h : KelGroups.WellFormed gs) (key : KelGroups.Key) (member : KelGroups.Member)
    (hmember : (key, member) ∈ gs.members) : member.key = key :=
  KelGroups.member_key_coherent h key member hmember

theorem majority_table :
    KelGroups.majority (KelGroups.majorityAdminState []) = 0 ∧
    KelGroups.majority (KelGroups.majorityAdminState ["a"]) = 1 ∧
    KelGroups.majority (KelGroups.majorityAdminState ["a", "b"]) = 1 ∧
    KelGroups.majority (KelGroups.majorityAdminState ["a", "b", "c"]) = 2 ∧
    KelGroups.majority (KelGroups.majorityAdminState ["a", "b", "c", "d"]) = 2 ∧
    KelGroups.majority (KelGroups.majorityAdminState ["a", "b", "c", "d", "e"]) = 3 :=
  KelGroups.majority_table

theorem majority_not_strict_on_even {α : Type} (gs : KelGroups.GroupState α)
    (positive : 0 < KelGroups.adminCount gs) (even : KelGroups.adminCount gs % 2 = 0) :
    2 * KelGroups.majority gs ≤ KelGroups.adminCount gs :=
  KelGroups.majority_not_strict_on_even gs positive even
