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

/-- V-2/A-001 restatement: `Nodup` kept; above one current admin the
proposer is absent from the counted approvals. The sole-admin exception
is part of the predicate (the `1 < admins` antecedent), not a comment:
a sole admin's separate self-approval satisfies it vacuously. -/
def PendingWellFormed (admins : Nat) (pending : PendingProposal) : Prop :=
  pending.approvals.Nodup ∧ (1 < admins → pending.proposer ∉ pending.approvals)

/-- Count-free strong form: no proposer credit at any size. Reachable
operation never produces proposer credit (proposals open empty, the
boundary bars self-approval above n=1, and a sole-admin self-approval
enacts immediately and is erased), so every transition preserves this
under admissible approves; it implies `PendingWellFormed` at any count.
Carried as the inductive invariant beside `WellFormed`. -/
def PendingStrong (pending : PendingProposal) : Prop :=
  pending.approvals.Nodup ∧ pending.proposer ∉ pending.approvals

def StrongCoherent (gs : GroupState α) : Prop :=
  ∀ proposalId pending, (proposalId, pending) ∈ gs.pendingProposals →
    PendingStrong pending

/-- Integrated-store twin of `PendingWellFormed`: the production
`pendingBase` store obeys the same ruling. -/
def PendingBaseWellFormed (admins : Nat) (pending : PendingBase) : Prop :=
  pending.approvals.Nodup ∧ (1 < admins → pending.proposer ∉ pending.approvals)

def PendingBaseStrong (pending : PendingBase) : Prop :=
  pending.approvals.Nodup ∧ pending.proposer ∉ pending.approvals

def StrongBaseCoherent (gs : GroupState α) : Prop :=
  ∀ proposalId pending, (proposalId, pending) ∈ gs.pendingBase →
    PendingBaseStrong pending

def MembersCoherent (gs : GroupState α) : Prop :=
  ∀ key member, (key, member) ∈ gs.members → member.key = key

def PendingCoherent (gs : GroupState α) : Prop :=
  ∀ proposalId pending, (proposalId, pending) ∈ gs.pendingProposals →
    PendingWellFormed (adminCount gs) pending

def BasePendingCoherent (gs : GroupState α) : Prop :=
  ∀ proposalId pending, (proposalId, pending) ∈ gs.pendingBase →
    PendingBaseWellFormed (adminCount gs) pending

structure WellFormed (gs : GroupState α) : Prop where
  memberKeys : (gs.members.map Prod.fst).Nodup
  pendingKeys : (gs.pendingProposals.map Prod.fst).Nodup
  membersCoherent : MembersCoherent gs
  pendingCoherent : PendingCoherent gs
  basePendingCoherent : BasePendingCoherent gs

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

private theorem setInsert_not_mem (value existing : Key) (values : List Key)
    (hnot : existing ≠ value) (hold : existing ∉ values) :
    existing ∉ setInsert value values := by
  simp only [setInsert]
  split
  · exact hold
  · next absent =>
      simp only [List.mem_cons, not_or]
      exact ⟨hnot, hold⟩

private theorem length_setInsert_pos (value : Key) (values : List Key) :
    0 < (setInsert value values).length := by
  cases values with
  | nil => simp [setInsert]
  | cons _ _ => simp [setInsert]; split <;> simp

private theorem approvePending_appr (signer : Key) (pending : PendingProposal) :
    (approvePending signer pending).approvals =
      setInsert signer pending.approvals := rfl

private theorem strong_to_indexed (n : Nat) (pending : PendingProposal)
    (h : PendingStrong pending) : PendingWellFormed n pending :=
  ⟨h.1, fun _ => h.2⟩

private theorem baseStrong_to_indexed (n : Nat) (pending : PendingBase)
    (h : PendingBaseStrong pending) : PendingBaseWellFormed n pending :=
  ⟨h.1, fun _ => h.2⟩

private theorem approvePending_strong (signer : Key) (pending : PendingProposal)
    (h : PendingStrong pending) (hne : signer ≠ pending.proposer) :
    PendingStrong (approvePending signer pending) := by
  constructor
  · exact setInsert_nodup signer pending.approvals h.1
  · rw [approvePending_appr]
    exact setInsert_not_mem signer pending.proposer pending.approvals
      (Ne.symm hne) h.2

private theorem approvePending_wellFormed (n : Nat) (signer : Key)
    (pending : PendingProposal)
    (h : PendingWellFormed n pending)
    (hself : pending.proposer = signer → ¬ 1 < n) :
    PendingWellFormed n (approvePending signer pending) := by
  refine ⟨setInsert_nodup signer pending.approvals h.1, fun hlt hmem => ?_⟩
  rw [approvePending_appr] at hmem
  by_cases hcon : pending.approvals.contains signer
  · simp only [setInsert, if_pos hcon] at hmem
    exact h.2 hlt hmem
  · simp only [setInsert, if_neg hcon, List.mem_cons] at hmem
    rcases hmem with heq | hold
    · exact absurd hlt (hself heq)
    · exact h.2 hlt hold

private theorem baseUpdate_appr (pending : PendingBase) (signer : Key) :
    ({ pending with approvals := setInsert signer pending.approvals } :
      PendingBase).approvals = setInsert signer pending.approvals := rfl

private theorem approveBasePending_wellFormed (n : Nat) (signer : Key)
    (pending : PendingBase)
    (h : PendingBaseWellFormed n pending)
    (hself : pending.proposer = signer → ¬ 1 < n) :
    PendingBaseWellFormed n
      { pending with approvals := setInsert signer pending.approvals } := by
  refine ⟨setInsert_nodup signer pending.approvals h.1, fun hlt hmem => ?_⟩
  rw [baseUpdate_appr] at hmem
  by_cases hcon : pending.approvals.contains signer
  · simp only [setInsert, if_pos hcon] at hmem
    exact h.2 hlt hmem
  · simp only [setInsert, if_neg hcon, List.mem_cons] at hmem
    rcases hmem with heq | hold
    · exact absurd hlt (hself heq)
    · exact h.2 hlt hold

/-- Bridge: a positive `contains` test names a member (`contains`
unfolds to `elem`; the `elem` API below is core). -/
private theorem mem_of_contains_eq_true (a : Key) (l : List Key)
    (h : l.contains a = true) : a ∈ l := by
  simp only [List.contains] at h
  exact List.mem_of_elem_eq_true h

/-- Bridge: a negative `contains` test excludes. -/
private theorem not_mem_of_contains_eq_false (a : Key) (l : List Key)
    (h : l.contains a = false) : a ∉ l := by
  intro hmem
  simp only [List.contains, List.elem_eq_true_of_mem hmem] at h
  exact Bool.noConfusion h

/-- Bridge: non-membership reads back as a negative test. -/
private theorem contains_eq_false_of_not_mem (a : Key) (l : List Key)
    (h : a ∉ l) : l.contains a = false := by
  simp only [List.contains]
  cases heq : l.elem a with
  | true => exact False.elim (h (List.mem_of_elem_eq_true heq))
  | false => rfl

/-- Inversion of the historical approval validator: `.ok` forces the
three guards, so none can be reordered away. In particular a successful
approval is never a self-approval above one admin (the V-2 bar), and
never a duplicate. -/
private theorem validateApproval_ok {gs : GroupState α} {signer : Key}
    {proposalId : ProposalId}
    (h : validateApproval gs signer proposalId = .ok ()) :
    ∃ pending, lookupPending proposalId gs = some pending ∧
      (pending.proposer = signer → ¬ 1 < adminCount gs) ∧
      signer ∉ pending.approvals ∧ isAdmin signer gs = true := by
  unfold validateApproval at h
  cases hreq : requireAdmin signer gs with
  | error e =>
      rw [hreq] at h
      have hcon : (Except.error e : Except ValidationError Unit) = .ok () := h
      exact Except.noConfusion hcon
  | ok _ =>
      rw [hreq] at h
      have hmat : ((match lookupPending proposalId gs with
        | none => .error (.proposalNotFound proposalId)
        | some pending =>
            if signer == pending.proposer && decide (1 < adminCount gs) then
              .error (.proposerSelfApproval signer proposalId)
            else if pending.approvals.contains signer then
              .error (.alreadyApproved signer proposalId)
            else .ok ()) : Except ValidationError Unit) = .ok () := h
      cases hlook : lookupPending proposalId gs with
      | none =>
          simp [hlook] at hmat
      | some pending =>
          simp only [hlook] at hmat
          by_cases hbar :
            (signer == pending.proposer && decide (1 < adminCount gs)) = true
          · rw [if_pos hbar] at hmat
            exact Except.noConfusion hmat
          · rw [if_neg hbar] at hmat
            by_cases hcon : (pending.approvals.contains signer) = true
            · rw [if_pos hcon] at hmat
              exact Except.noConfusion hmat
            · rw [if_neg hcon] at hmat
              refine ⟨pending, rfl, ?_, ?_, ?_⟩
              · intro heq hlt
                apply hbar
                have ha : (signer == pending.proposer) = true := by
                  rw [heq, beq_self_eq_true]
                have hd : decide (1 < adminCount gs) = true := by
                  cases hdd : decide (1 < adminCount gs) with
                  | true => rfl
                  | false =>
                      exact False.elim ((of_decide_eq_false hdd) hlt)
                rw [ha, Bool.true_and]
                exact hd
              · exact not_mem_of_contains_eq_false signer
                  pending.approvals (by simpa using hcon)
              · have hadm : isAdmin signer gs = true := by
                  have e := hreq
                  simp only [requireAdmin] at e
                  split at e
                  · next hc => exact hc
                  · next _ => exact Except.noConfusion e
                exact hadm

/-- Inversion of the integrated approval validator: same three forces
on the production `pendingBase` store. -/
private theorem validateBaseApproval_ok {gs : GroupState α} {signer : Key}
    {proposalId : ProposalId}
    (h : validateBaseApproval gs signer proposalId = .ok ()) :
    ∃ pending, lookupPendingBase proposalId gs = some pending ∧
      (pending.proposer = signer → ¬ 1 < adminCount gs) ∧
      signer ∉ pending.approvals ∧ isAdmin signer gs = true := by
  unfold validateBaseApproval at h
  cases hreq : requireAdmin signer gs with
  | error e =>
      rw [hreq] at h
      have hcon : (Except.error e : Except ValidationError Unit) = .ok () := h
      exact Except.noConfusion hcon
  | ok _ =>
      rw [hreq] at h
      have hmat : ((match lookupPendingBase proposalId gs with
        | none => .error (.proposalNotFound proposalId)
        | some pending =>
            if signer == pending.proposer && decide (1 < adminCount gs) then
              .error (.proposerSelfApproval signer proposalId)
            else if pending.approvals.contains signer then
              .error (.alreadyApproved signer proposalId)
            else .ok ()) : Except ValidationError Unit) = .ok () := h
      cases hlook : lookupPendingBase proposalId gs with
      | none =>
          simp [hlook] at hmat
      | some pending =>
          simp only [hlook] at hmat
          by_cases hbar :
            (signer == pending.proposer && decide (1 < adminCount gs)) = true
          · rw [if_pos hbar] at hmat
            exact Except.noConfusion hmat
          · rw [if_neg hbar] at hmat
            by_cases hcon : (pending.approvals.contains signer) = true
            · rw [if_pos hcon] at hmat
              exact Except.noConfusion hmat
            · rw [if_neg hcon] at hmat
              refine ⟨pending, rfl, ?_, ?_, ?_⟩
              · intro heq hlt
                apply hbar
                have ha : (signer == pending.proposer) = true := by
                  rw [heq, beq_self_eq_true]
                have hd : decide (1 < adminCount gs) = true := by
                  cases hdd : decide (1 < adminCount gs) with
                  | true => rfl
                  | false =>
                      exact False.elim ((of_decide_eq_false hdd) hlt)
                rw [ha, Bool.true_and]
                exact hd
              · exact not_mem_of_contains_eq_false signer
                  pending.approvals (by simpa using hcon)
              · have hadm : isAdmin signer gs = true := by
                  have e := hreq
                  simp only [requireAdmin] at e
                  split at e
                  · next hc => exact hc
                  · next _ => exact Except.noConfusion e
                exact hadm

/-- Admissibility of a raw-fold trace: every event's boundary decision
is `.ok` in the state it runs in. Approval events in an admissible
trace are never barred self-approvals, which is what lets the fold
preservation induction go through.

Antecedents, exactly: `(validKey : Key → Bool)`, `(config : GroupConfig
α)`, and per event a `validateEvent validKey config cur signer event
= .ok ()` proof threaded over the evolving states (each step checked in
the state it runs in, not only at the end). Domain exclusions: any trace
containing a boundary-refused step — non-admin propose, duplicate or
ill-formed payload, unknown-proposal or above-n=1 self-approval, or
non-member app event — is inadmissible AT that step; the fold still
EXECUTES refused steps raw (no validation lives in the fold), so
admissibility is a claim ABOUT a trace, never a property the fold
enforces or produces. Needed vs convenient strength: the induction
needs stale-freedom (`StrongCoherent`, since the count-indexed
predicate alone does not survive count-increasing enactments over
stale credit); `TraceAdmissible` is the convenient sufficient bundle
for the approve case (an ok-proof inverts to nonduplicate +
proposer-bar conditions). `hadm` REMAINS an explicit caller proof
obligation — witness traces prove their own admissibility step by step,
production traces satisfy it by construction, and no theorem
manufactures it (a few tested traces discharge no other caller).
Integrated distinction: `applyIntegratedEvent` validates INTERNALLY per
route, so integrated preservation takes no `TraceAdmissible` —
refused integrated events leave the state unchanged instead. -/
def TraceAdmissible (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) : List (Key × GroupEvent α) → Prop
  | [] => True
  | (signer, event) :: rest =>
      validateEvent validKey config gs signer event = .ok () ∧
        TraceAdmissible digest appFoldFn validKey config
          (applyEvent digest appFoldFn gs signer event) rest

theorem emptyState_wellFormed (initial : α) : WellFormed (emptyState initial) := by
  exact ⟨by simp [emptyState], by simp [emptyState], by simp [MembersCoherent, emptyState],
    by simp [PendingCoherent, emptyState], by simp [BasePendingCoherent, emptyState]⟩

private theorem emptyState_strong (initial : α) :
    StrongCoherent (emptyState initial) := by
  intro proposalId pending hmem
  simp [emptyState] at hmem

private theorem emptyState_strongBase (initial : α) :
    StrongBaseCoherent (emptyState initial) := by
  intro proposalId pending hmem
  simp [emptyState] at hmem

private theorem enact_pendingProposals (gs : GroupState α) (proposal : Proposal) :
    (enact gs proposal).pendingProposals = gs.pendingProposals := by
  cases proposal <;> rfl

private theorem enact_pendingBase (gs : GroupState α) (proposal : Proposal) :
    (enact gs proposal).pendingBase = gs.pendingBase := by
  cases proposal <;> rfl

private theorem enact_memberKeys_nodup (gs : GroupState α) (proposal : Proposal)
    (hkeys : (gs.members.map Prod.fst).Nodup) :
    ((enact gs proposal).members.map Prod.fst).Nodup := by
  cases proposal with
  | introduceMember key email roles =>
      exact assocInsert_keys_nodup key { key, email, roles } gs.members hkeys
  | removeMember key =>
      exact assocErase_keys_nodup key gs.members hkeys
  | changeRoles key roles =>
      simpa [enact, assocAdjust_keys key
        (fun member : Member => { member with roles })] using hkeys

private theorem enact_members_coherent (gs : GroupState α) (proposal : Proposal)
    (h : MembersCoherent gs) : MembersCoherent (enact gs proposal) := by
  cases proposal with
  | introduceMember key email roles =>
      exact assocInsert_property key { key, email, roles } gs.members
        (fun candidate member => member.key = candidate) rfl h
  | removeMember key =>
      exact assocErase_property key gs.members
        (fun candidate member => member.key = candidate) h
  | changeRoles key roles =>
      exact assocAdjust_property key (fun member : Member => { member with roles })
        gs.members (fun candidate member => member.key = candidate)
        (by intro member coherent; exact coherent) h

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

/-- An admin member forces a positive franchise: used to discharge the
sole-admin case (a sole self-approval always meets threshold, hence
always enacts and is erased). -/
private theorem adminCount_pos_of_isAdmin {gs : GroupState α} {signer : Key}
    (h : isAdmin signer gs = true) : 1 ≤ adminCount gs := by
  have mono : ∀ (l : List (Key × Member)) (c : Nat),
      c ≤ l.foldl (fun count entry =>
        if hasAdmin entry.2.roles then count + 1 else count) c := by
    intro l c
    induction l generalizing c with
    | nil => exact Nat.le_refl c
    | cons entry rest ih =>
        by_cases hadm : hasAdmin entry.2.roles = true
        · simp only [List.foldl_cons, hadm, ↓reduceIte]
          exact Nat.le_trans (Nat.le_succ c) (ih (c + 1))
        · simp only [List.foldl_cons, hadm, ↓reduceIte]
          exact ih c
  have wit : ∀ (l : List (Key × Member)),
      (∃ k m, (k, m) ∈ l ∧ hasAdmin m.roles = true) →
      1 ≤ l.foldl (fun count entry =>
        if hasAdmin entry.2.roles then count + 1 else count) 0 := by
    intro l hl
    induction l with
    | nil => obtain ⟨_, _, hmem, _⟩ := hl; simp at hmem
    | cons entry rest ih =>
        obtain ⟨k, m, hmem, hadm⟩ := hl
        simp only [List.mem_cons] at hmem
        rcases hmem with heq | htail
        · cases heq
          simp only [List.foldl_cons]
          by_cases hadm' : hasAdmin m.roles = true
          · simp only [hadm', ↓reduceIte]
            exact mono rest 1
          · exfalso
            exact hadm' hadm
        · simp only [List.foldl_cons]
          by_cases hadm' : hasAdmin entry.2.roles = true
          · simp only [hadm', ↓reduceIte]
            exact mono rest 1
          · simp only [hadm', ↓reduceIte]
            exact ih ⟨k, m, htail, hadm⟩
  unfold isAdmin at h
  cases hlook : lookupMember signer gs with
  | none => simp [hlook] at h
  | some m =>
      simp only [hlook] at h
      have hmem : (signer, m) ∈ gs.members :=
        assocLookup_some_mem signer m gs.members hlook
      unfold adminCount
      exact wit gs.members ⟨signer, m, hmem, h⟩

theorem enact_preserves_wellFormed (gs : GroupState α) (proposal : Proposal)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs) :
    WellFormed (enact gs proposal) := by
  have hkeysE : ((enact gs proposal).pendingProposals.map Prod.fst).Nodup := by
    rw [enact_pendingProposals]
    exact h.pendingKeys
  refine
    { memberKeys := enact_memberKeys_nodup gs proposal h.memberKeys,
      pendingKeys := hkeysE,
      membersCoherent := enact_members_coherent gs proposal h.membersCoherent,
      pendingCoherent := fun proposalId pending hmem => by
        rw [enact_pendingProposals] at hmem
        exact strong_to_indexed _ _ (hs proposalId pending hmem),
      basePendingCoherent := fun proposalId pending hmem => by
        rw [enact_pendingBase] at hmem
        exact baseStrong_to_indexed _ _ (hsb proposalId pending hmem) }

/-- Erasing the just-inserted head entry restores the old list: the
V-2 sole-admin self-approval is always the head at erasure time, so
it cannot survive its own enactment — no key-uniqueness needed. -/
private theorem assocErase_cons_self [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν)) :
    assocErase key ((key, value) :: entries) = entries := by
  have hbeq : (key == key) = true := beq_self_eq_true key
  exact if_pos hbeq

/-- Entries surviving an erase-then-insert round-trip are old entries. -/
private theorem finishEnact_post_mem_old
    (upd old : List (ProposalId × PendingProposal))
    (pid0 : ProposalId) (v0 : PendingProposal)
    (hcons : upd = (pid0, v0) :: assocErase pid0 old)
    (pid : ProposalId) (p : PendingProposal)
    (hmem : (pid, p) ∈ assocErase pid0 upd) :
    (pid, p) ∈ old := by
  rw [hcons, assocErase_cons_self] at hmem
  exact (assocErase_sublist pid0 old).mem hmem

/-- An approve-then-enact step leaves only pre-existing historical
entries behind: the enactment erases the approved id, whose head
position is the just-approved entry. -/
private theorem approveErase_post_mem_old (updated : GroupState α)
    (gs : GroupState α) (pid0 : ProposalId) (approved : PendingProposal)
    (hcons : updated.pendingProposals =
      (pid0, approved) :: assocErase pid0 gs.pendingProposals)
    (pid : ProposalId) (p : PendingProposal)
    (hmem : (pid, p) ∈
      (finishEnact updated pid0 approved).pendingProposals) :
    (pid, p) ∈ gs.pendingProposals := by
  have e : (finishEnact updated pid0 approved).pendingProposals =
      assocErase pid0 updated.pendingProposals := by
    show assocErase pid0 (enact updated approved.proposal).pendingProposals = _
    rw [enact_pendingProposals]
  rw [e] at hmem
  exact finishEnact_post_mem_old updated.pendingProposals gs.pendingProposals
    pid0 approved hcons pid p hmem

theorem finishEnact_preserves_wellFormed (gs : GroupState α) (proposalId : ProposalId)
    (pending : PendingProposal) (h : WellFormed gs)
    (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs) :
    WellFormed (finishEnact gs proposalId pending) := by
  have hkeys : ((enact gs pending.proposal).pendingProposals.map Prod.fst).Nodup := by
    rw [enact_pendingProposals]
    exact h.pendingKeys
  refine
    { memberKeys := enact_memberKeys_nodup gs pending.proposal h.memberKeys,
      pendingKeys := assocErase_keys_nodup proposalId
        (enact gs pending.proposal).pendingProposals hkeys,
      membersCoherent := enact_members_coherent gs pending.proposal h.membersCoherent,
      pendingCoherent := fun pid p hmem =>
        strong_to_indexed _ _ (hs pid p (by
          have hmemE : (pid, p) ∈
              assocErase proposalId
                (enact gs pending.proposal).pendingProposals := hmem
          have hsub := (assocErase_sublist proposalId _).mem hmemE
          rw [enact_pendingProposals] at hsub
          exact hsub)),
      basePendingCoherent := fun pid pb hmem => by
        have hmemB : (pid, pb) ∈ gs.pendingBase := by
          have e : (finishEnact gs proposalId pending).pendingBase =
              gs.pendingBase := by
            show (enact gs pending.proposal).pendingBase = _
            rw [enact_pendingBase]
          rw [e] at hmem
          exact hmem
        exact baseStrong_to_indexed _ _ (hsb pid pb hmemB) }

theorem tryEnact_preserves_wellFormed (gs : GroupState α) (proposalId : ProposalId)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs) :
    WellFormed (tryEnact gs proposalId) := by
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [tryEnact, tryEnactDetailed, hlookup] using h
  | some pending =>
      by_cases threshold : pending.approvals.length ≥ majority gs
      · have hfin : tryEnact gs proposalId = finishEnact gs proposalId pending := by
          simp [tryEnact, tryEnactDetailed, hlookup, threshold]
        rw [hfin]
        exact finishEnact_preserves_wellFormed gs proposalId pending h hs hsb
      · simpa [tryEnact, tryEnactDetailed, hlookup, threshold] using h

theorem applyPropose_preserves_wellFormed (digest : Proposal → ProposalId)
    (gs : GroupState α) (signer : Key) (proposal : Proposal) (h : WellFormed gs)
    (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs) :
    WellFormed (applyPropose digest gs signer proposal) := by
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [] }
  let proposed : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  have hpending : PendingWellFormed (adminCount proposed) pending := by
    simp [PendingWellFormed, pending]
  have hstrong : PendingStrong pending := by
    simp [PendingStrong, pending]
  have hceq : adminCount proposed = adminCount gs := rfl
  have hproposed : WellFormed proposed :=
    { memberKeys := h.memberKeys
      pendingKeys := assocInsert_keys_nodup proposalId pending gs.pendingProposals h.pendingKeys
      membersCoherent := h.membersCoherent
      pendingCoherent := assocInsert_property proposalId pending gs.pendingProposals
        (fun _ p => PendingWellFormed (adminCount proposed) p) hpending
        (fun c o hm => by
          have hcoh := h.pendingCoherent c o hm
          rw [← hceq] at hcoh
          exact hcoh)
      basePendingCoherent := fun pid pb hmem => by
        have hcoh := h.basePendingCoherent pid pb hmem
        rw [← hceq] at hcoh
        exact hcoh }
  have hsproposed : StrongCoherent proposed :=
    assocInsert_property proposalId pending gs.pendingProposals
      (fun _ p => PendingStrong p) hstrong hs
  have hsbproposed : StrongBaseCoherent proposed :=
    fun pid pb hmem => hsb pid pb hmem
  change WellFormed (tryEnact proposed proposalId)
  exact tryEnact_preserves_wellFormed proposed proposalId hproposed hsproposed hsbproposed

/-- Entries produced by a historical approve are old entries or the
freshly approved one; the latter is identified exactly. -/
private theorem applyApprove_strong_entry (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (h : WellFormed gs)
    (hs : StrongCoherent gs)
    (hvalid : validateApproval gs signer proposalId = .ok ())
    (pid : ProposalId) (p : PendingProposal)
    (hmem : (pid, p) ∈ (applyApprove gs signer proposalId).pendingProposals) :
    PendingStrong p := by
  obtain ⟨pending, hlookup, hself, _hfresh, hadmin⟩ := validateApproval_ok hvalid
  have hmemgs : (proposalId, pending) ∈ gs.pendingProposals :=
    assocLookup_some_mem proposalId pending gs.pendingProposals hlookup
  have hstrong : PendingStrong pending := hs proposalId pending hmemgs
  have hcount : 1 ≤ adminCount gs := adminCount_pos_of_isAdmin hadmin
  let approved := approvePending signer pending
  let updated : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId approved gs.pendingProposals }
  have hstep : applyApprove gs signer proposalId = tryEnact updated proposalId := by
    unfold applyApprove applyApproveDetailed
    rw [hlookup]
    show (tryEnact { members := gs.members, pendingProposals := assocInsert proposalId (approvePending signer pending) gs.pendingProposals, pendingBase := gs.pendingBase, appFold := gs.appFold } proposalId) = _
    rfl
  rw [hstep] at hmem
  have hlk : lookupPending proposalId updated = some approved :=
    assocLookup_insert_self proposalId approved gs.pendingProposals
  by_cases threshold : approved.approvals.length ≥ majority updated
  · have hfin : tryEnact updated proposalId =
        finishEnact updated proposalId approved := by
      simp only [tryEnact, tryEnactDetailed, hlk, if_pos threshold]
    rw [hfin] at hmem
    have hcons : updated.pendingProposals =
        (proposalId, approved) :: assocErase proposalId gs.pendingProposals := rfl
    exact hs pid p
      (approveErase_post_mem_old updated gs proposalId approved hcons pid p hmem)
  · have hno : tryEnact updated proposalId = updated := by
      simp only [tryEnact, tryEnactDetailed, hlk, if_neg threshold]
    rw [hno] at hmem
    have hmemU : (pid, p) ∈
        (proposalId, approved) ::
          assocErase proposalId gs.pendingProposals := hmem
    simp only [List.mem_cons] at hmemU
    rcases hmemU with heq | hold
    · simp only [Prod.mk.injEq] at heq
      obtain ⟨h1, h2⟩ := heq
      subst h1
      subst h2
      constructor
      · exact setInsert_nodup signer pending.approvals hstrong.1
      · by_cases heq2 : signer = pending.proposer
        · exfalso
          have hceq : adminCount updated = adminCount gs := rfl
          have hone : adminCount gs = 1 := by
            have hle : adminCount gs ≤ 1 :=
              Classical.byContradiction fun hgt => hself heq2.symm (by omega)
            omega
          have hlen : 1 ≤ approved.approvals.length := by
            show 1 ≤ (approvePending signer pending).approvals.length
            rw [approvePending_appr]
            exact length_setInsert_pos signer pending.approvals
          have hmaj : majority updated = 1 := by
            simp [majority, hceq, hone]
          omega
        · show pending.proposer ∉ setInsert signer pending.approvals
          exact setInsert_not_mem signer pending.proposer pending.approvals
            (Ne.symm heq2) hstrong.2
    · exact hs pid p
        ((assocErase_sublist proposalId gs.pendingProposals).mem hold)

theorem applyApprove_preserves_wellFormed (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (h : WellFormed gs)
    (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (hvalid : validateApproval gs signer proposalId = .ok ()) :
    WellFormed (applyApprove gs signer proposalId) := by
  -- Excluded domain (F-01): a raw self-approval above n=1 without the
  -- boundary decision. Witness: auditor ScopeWitness instrument
  -- (source sha 3b4229fc, run sha 0a2799b7) events 1-7, plus the worker
  -- `t68Raw*` guard family in Tests.lean. The `hvalid` premise is the
  -- boundary side of that split; the raw fold never validates.
  obtain ⟨pending, hlookup, hself, _hfresh, _hadmin⟩ := validateApproval_ok hvalid
  have hmem : (proposalId, pending) ∈ gs.pendingProposals :=
    assocLookup_some_mem proposalId pending gs.pendingProposals hlookup
  let approved := approvePending signer pending
  let updated : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId approved gs.pendingProposals }
  have hceq : adminCount updated = adminCount gs := rfl
  have happ : PendingWellFormed (adminCount updated) approved := by
    have h0 := approvePending_wellFormed (adminCount gs) signer pending
      (h.pendingCoherent proposalId pending hmem) hself
    rw [← hceq] at h0
    exact h0
  have hupdated : WellFormed updated :=
    { memberKeys := h.memberKeys
      pendingKeys := assocInsert_keys_nodup proposalId approved
        gs.pendingProposals h.pendingKeys
      membersCoherent := h.membersCoherent
      pendingCoherent := assocInsert_property proposalId approved gs.pendingProposals
        (fun _ p => PendingWellFormed (adminCount updated) p) happ
        (fun c o hm => by
          have hcoh := h.pendingCoherent c o hm
          rw [← hceq] at hcoh
          exact hcoh)
      basePendingCoherent := fun pid pb hm => by
        have hcoh := h.basePendingCoherent pid pb hm
        rw [← hceq] at hcoh
        exact hcoh }
  have hlookup' : lookupPending proposalId updated = some approved :=
    assocLookup_insert_self proposalId approved gs.pendingProposals
  by_cases threshold : approved.approvals.length ≥ majority updated
  · have hstep : applyApprove gs signer proposalId =
        finishEnact updated proposalId approved := by
      have e1 : applyApprove gs signer proposalId = tryEnact updated proposalId := by
        unfold applyApprove applyApproveDetailed
        rw [hlookup]
        show (tryEnact { members := gs.members, pendingProposals := assocInsert proposalId (approvePending signer pending) gs.pendingProposals, pendingBase := gs.pendingBase, appFold := gs.appFold } proposalId) = _
        rfl
      rw [e1]
      simp only [tryEnact, tryEnactDetailed, hlookup', if_pos threshold]
    rw [hstep]
    have hcons : updated.pendingProposals =
        (proposalId, approved) :: assocErase proposalId gs.pendingProposals := rfl
    have hkeys :
        ((enact updated approved.proposal).pendingProposals.map Prod.fst).Nodup := by
      rw [enact_pendingProposals]
      exact hupdated.pendingKeys
    refine
      { memberKeys :=
          enact_memberKeys_nodup updated approved.proposal h.memberKeys,
        pendingKeys := assocErase_keys_nodup proposalId _ hkeys,
        membersCoherent :=
          enact_members_coherent updated approved.proposal h.membersCoherent,
        pendingCoherent := fun pid p hp =>
          strong_to_indexed _ _ (hs pid p
            (approveErase_post_mem_old updated gs proposalId approved hcons pid p hp)),
        basePendingCoherent := fun pid pb hp => by
          have hmemB : (pid, pb) ∈ gs.pendingBase := by
            have e : (finishEnact updated proposalId approved).pendingBase =
                gs.pendingBase := by
              show (enact updated approved.proposal).pendingBase = _
              rw [enact_pendingBase]
            rw [e] at hp
            exact hp
          exact baseStrong_to_indexed _ _ (hsb pid pb hmemB) }
  · have hstep : applyApprove gs signer proposalId = updated := by
      have e1 : applyApprove gs signer proposalId = tryEnact updated proposalId := by
        unfold applyApprove applyApproveDetailed
        rw [hlookup]
        show (tryEnact { members := gs.members, pendingProposals := assocInsert proposalId (approvePending signer pending) gs.pendingProposals, pendingBase := gs.pendingBase, appFold := gs.appFold } proposalId) = _
        rfl
      rw [e1]
      simp only [tryEnact, tryEnactDetailed, hlookup', if_neg threshold]
    rw [hstep]
    exact hupdated

private theorem applyPropose_preserves_strong (digest : Proposal → ProposalId)
    (gs : GroupState α) (signer : Key) (proposal : Proposal)
    (h : WellFormed gs) (hs : StrongCoherent gs) :
    StrongCoherent (applyPropose digest gs signer proposal) := by
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [] }
  let proposed : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  have hnew : PendingStrong pending := by
    simp [PendingStrong, pending]
  have hsproposed : StrongCoherent proposed :=
    assocInsert_property proposalId pending gs.pendingProposals
      (fun _ p => PendingStrong p) hnew hs
  have hstep : applyPropose digest gs signer proposal = tryEnact proposed proposalId := rfl
  intro pid p hmem
  rw [hstep] at hmem
  have hlk : lookupPending proposalId proposed = some pending :=
    assocLookup_insert_self proposalId pending gs.pendingProposals
  by_cases threshold : pending.approvals.length ≥ majority proposed
  · have hfin : tryEnact proposed proposalId = finishEnact proposed proposalId pending := by
      simp only [tryEnact, tryEnactDetailed, hlk, if_pos threshold]
    rw [hfin] at hmem
    have hcons : proposed.pendingProposals =
        (proposalId, pending) :: assocErase proposalId gs.pendingProposals := rfl
    exact hs pid p
      (approveErase_post_mem_old proposed gs proposalId pending hcons pid p hmem)
  · have hno : tryEnact proposed proposalId = proposed := by
      simp only [tryEnact, tryEnactDetailed, hlk, if_neg threshold]
    rw [hno] at hmem
    exact hsproposed pid p hmem

private theorem applyApprove_preserves_strong (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (h : WellFormed gs)
    (hs : StrongCoherent gs)
    (hvalid : validateApproval gs signer proposalId = .ok ()) :
    StrongCoherent (applyApprove gs signer proposalId) :=
  fun pid p hmem =>
    applyApprove_strong_entry gs signer proposalId h hs hvalid pid p hmem

private theorem tryEnact_preserves_strongBase (gs : GroupState α)
    (proposalId : ProposalId) (hsb : StrongBaseCoherent gs) :
    StrongBaseCoherent (tryEnact gs proposalId) := by
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [tryEnact, tryEnactDetailed, hlookup] using hsb
  | some pending =>
      by_cases threshold : pending.approvals.length ≥ majority gs
      · have hfin : tryEnact gs proposalId = finishEnact gs proposalId pending := by
          simp only [tryEnact, tryEnactDetailed, hlookup, if_pos threshold]
        rw [hfin]
        intro pid pb hmem
        have hmemB : (pid, pb) ∈ gs.pendingBase := by
          have e1 : (finishEnact gs proposalId pending).pendingBase =
              (enact gs pending.proposal).pendingBase := rfl
          rw [e1, enact_pendingBase] at hmem
          exact hmem
        exact hsb pid pb hmemB
      · simpa [tryEnact, tryEnactDetailed, hlookup, if_neg threshold] using hsb

private theorem applyPropose_preserves_strongBase (digest : Proposal → ProposalId)
    (gs : GroupState α) (signer : Key) (proposal : Proposal)
    (hsb : StrongBaseCoherent gs) :
    StrongBaseCoherent (applyPropose digest gs signer proposal) := by
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [] }
  let proposed : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  have hsbproposed : StrongBaseCoherent proposed :=
    fun pid pb hmem => hsb pid pb hmem
  have hstep : applyPropose digest gs signer proposal = tryEnact proposed proposalId := rfl
  rw [hstep]
  exact tryEnact_preserves_strongBase proposed proposalId hsbproposed

private theorem applyApprove_preserves_strongBase (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (hsb : StrongBaseCoherent gs) :
    StrongBaseCoherent (applyApprove gs signer proposalId) := by
  unfold applyApprove applyApproveDetailed
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [hlookup] using hsb
  | some pending =>
      simpa [hlookup] using
        (tryEnact_preserves_strongBase
          { gs with pendingProposals :=
            assocInsert proposalId (approvePending signer pending) gs.pendingProposals }
          proposalId
          (fun pid pb hmem => hsb pid pb hmem))

theorem applyEvent_preserves_wellFormed (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (gs : GroupState α) (signer : Key)
    (event : GroupEvent α) (h : WellFormed gs)
    (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (hval : ∀ proposalId, event = .base (.approve proposalId) →
      validateApproval gs signer proposalId = .ok ()) :
    WellFormed (applyEvent digest appFoldFn gs signer event) := by
  cases event with
  | app _ =>
      exact
        { memberKeys := h.memberKeys
          pendingKeys := h.pendingKeys
          membersCoherent := h.membersCoherent
          pendingCoherent := h.pendingCoherent
          basePendingCoherent := h.basePendingCoherent }
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          exact applyPropose_preserves_wellFormed digest gs signer proposal h hs hsb
      | approve proposalId =>
          exact applyApprove_preserves_wellFormed gs signer proposalId h hs hsb
            (hval proposalId rfl)

private theorem applyEvent_preserves_strong (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α)
    (h : WellFormed gs) (hs : StrongCoherent gs)
    (hok : validateEvent validKey config gs signer event = .ok ()) :
    StrongCoherent (applyEvent digest appFoldFn gs signer event) := by
  cases event with
  | app _ => exact hs
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          exact applyPropose_preserves_strong digest gs signer proposal h hs
      | approve proposalId =>
          have hvalid : validateApproval gs signer proposalId = .ok () := hok
          exact applyApprove_preserves_strong gs signer proposalId h hs hvalid

private theorem applyEvent_preserves_strongBase (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (gs : GroupState α) (signer : Key)
    (event : GroupEvent α) (hsb : StrongBaseCoherent gs) :
    StrongBaseCoherent (applyEvent digest appFoldFn gs signer event) := by
  cases event with
  | app _ => exact hsb
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          exact applyPropose_preserves_strongBase digest gs signer proposal hsb
      | approve proposalId =>
          exact applyApprove_preserves_strongBase gs signer proposalId hsb

private theorem foldEvents_preserves_all (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (validKey : Key → Bool) (config : GroupConfig α)
    (events : List (Key × GroupEvent α))
    (gs : GroupState α) (h : WellFormed gs) (hs : StrongCoherent gs)
    (hsb : StrongBaseCoherent gs)
    (hadm : TraceAdmissible digest appFoldFn validKey config gs events) :
    WellFormed (events.foldl
      (fun current signed => applyEvent digest appFoldFn current signed.1 signed.2) gs) ∧
    StrongCoherent (events.foldl
      (fun current signed => applyEvent digest appFoldFn current signed.1 signed.2) gs) ∧
    StrongBaseCoherent (events.foldl
      (fun current signed => applyEvent digest appFoldFn current signed.1 signed.2) gs) := by
  induction events generalizing gs with
  | nil => exact ⟨h, hs, hsb⟩
  | cons signed rest ih =>
      obtain ⟨signer, event⟩ := signed
      obtain ⟨hok, hrest⟩ := hadm
      have hval : ∀ proposalId, event = .base (.approve proposalId) →
          validateApproval gs signer proposalId = .ok () := by
        intro proposalId heq
        subst heq
        exact hok
      have h1 := applyEvent_preserves_wellFormed digest appFoldFn gs signer event
        h hs hsb hval
      have hs1 := applyEvent_preserves_strong digest appFoldFn validKey config gs
        signer event h hs hok
      have hsb1 := applyEvent_preserves_strongBase digest appFoldFn gs signer event hsb
      exact ih _ h1 hs1 hsb1 hrest

theorem foldGroup_wellFormed (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (validKey : Key → Bool) (config : GroupConfig α)
    (initial : α) (events : List (Key × GroupEvent α))
    (hadm : TraceAdmissible digest appFoldFn validKey config
      (emptyState initial) events) :
    WellFormed (foldGroup digest appFoldFn initial events) :=
  -- Excluded domain (F-01): raw event lists containing a
  -- boundary-refused step. The fold executes them anyway; only the
  -- structural bundle (`foldGroup_structural`) is unconditional.
  -- Witness: auditor ScopeWitness instrument (source sha 3b4229fc,
  -- run sha 0a2799b7) events 1-7, plus the worker `t68Raw*` guard
  -- family in Tests.lean (prefix-admissible + bar-refused +
  -- raw-violation, accurately scoped).
  (foldEvents_preserves_all digest appFoldFn validKey config events
    (emptyState initial) (emptyState_wellFormed initial) (emptyState_strong initial)
    (emptyState_strongBase initial) hadm).1

/-! ### F-01 repair (T68-25 correction 1): unconditional raw structural core

The validation-free raw fold preserves the structural coherence of the
stores for EVERY raw event list, with no admissibility premise: key
uniqueness, member coherence, and approval-list hygiene do not depend on
approvals CONTENT, so proposer credit cannot break them. Governance
(proposer-nonmembership) is the only conditional part (see
`TraceAdmissible` and the retained 7-event witness cited on
`foldGroup_wellFormed`). All results here are private auxiliaries;
the 163-theorem pin is untouched (correction 4).
-/

/-- Count-free structural bundle: everything about the stores that raw
execution preserves unconditionally. Deliberately excludes any
proposer-credit property (governance lives in `WellFormed`). -/
private def RawStructural (gs : GroupState α) : Prop :=
  (gs.members.map Prod.fst).Nodup
  ∧ (gs.pendingProposals.map Prod.fst).Nodup
  ∧ MembersCoherent gs
  ∧ (∀ proposalId pending, (proposalId, pending) ∈ gs.pendingProposals →
      pending.approvals.Nodup)
  ∧ (gs.pendingBase.map Prod.fst).Nodup
  ∧ (∀ proposalId pending, (proposalId, pending) ∈ gs.pendingBase →
      pending.approvals.Nodup)

private theorem emptyState_structural (initial : α) :
    RawStructural (emptyState initial) := by
  refine ⟨by simp [emptyState], by simp [emptyState],
    by simp [MembersCoherent, emptyState], ?_, by simp [emptyState], ?_⟩
  · intro proposalId pending hmem
    simp [emptyState] at hmem
  · intro proposalId pending hmem
    simp [emptyState] at hmem

private theorem enact_preserves_structural (gs : GroupState α)
    (proposal : Proposal) (h : RawStructural gs) :
    RawStructural (enact gs proposal) := by
  obtain ⟨hkeysM, hkeysP, hcoh, hnodup, hkeysB, hnodupB⟩ := h
  refine ⟨enact_memberKeys_nodup gs proposal hkeysM, ?_, ?_, ?_, ?_, ?_⟩
  · rw [enact_pendingProposals]
    exact hkeysP
  · exact enact_members_coherent gs proposal hcoh
  · intro proposalId pending hmem
    rw [enact_pendingProposals] at hmem
    exact hnodup proposalId pending hmem
  · rw [enact_pendingBase]
    exact hkeysB
  · intro proposalId pending hmem
    rw [enact_pendingBase] at hmem
    exact hnodupB proposalId pending hmem

private theorem finishEnact_preserves_structural (gs : GroupState α)
    (proposalId : ProposalId) (pending : PendingProposal)
    (h : RawStructural gs) :
    RawStructural (finishEnact gs proposalId pending) := by
  obtain ⟨hkeysM, hkeysP, hcoh, hnodup, hkeysB, hnodupB⟩ := h
  have hkeysE : ((enact gs pending.proposal).pendingProposals.map Prod.fst).Nodup := by
    rw [enact_pendingProposals]
    exact hkeysP
  have eB : (finishEnact gs proposalId pending).pendingBase = gs.pendingBase := by
    show (enact gs pending.proposal).pendingBase = _
    rw [enact_pendingBase]
  refine ⟨enact_memberKeys_nodup gs pending.proposal hkeysM,
    assocErase_keys_nodup proposalId _ hkeysE,
    enact_members_coherent gs pending.proposal hcoh, ?_, ?_, ?_⟩
  · intro pid p hmem
    have hmemE : (pid, p) ∈
        assocErase proposalId (enact gs pending.proposal).pendingProposals := hmem
    have hsub := (assocErase_sublist proposalId _).mem hmemE
    rw [enact_pendingProposals] at hsub
    exact hnodup pid p hsub
  · rw [eB]
    exact hkeysB
  · intro pid p hmem
    rw [eB] at hmem
    exact hnodupB pid p hmem

private theorem tryEnact_preserves_structural (gs : GroupState α)
    (proposalId : ProposalId) (h : RawStructural gs) :
    RawStructural (tryEnact gs proposalId) := by
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [tryEnact, tryEnactDetailed, hlookup] using h
  | some pending =>
      by_cases threshold : pending.approvals.length ≥ majority gs
      · have hfin : tryEnact gs proposalId = finishEnact gs proposalId pending := by
          simp only [tryEnact, tryEnactDetailed, hlookup, if_pos threshold]
        rw [hfin]
        exact finishEnact_preserves_structural gs proposalId pending h
      · simpa [tryEnact, tryEnactDetailed, hlookup, if_neg threshold] using h

private theorem applyPropose_structural (digest : Proposal → ProposalId)
    (gs : GroupState α) (signer : Key) (proposal : Proposal)
    (h : RawStructural gs) :
    RawStructural (applyPropose digest gs signer proposal) := by
  obtain ⟨hkeysM, hkeysP, hcoh, hnodup, hkeysB, hnodupB⟩ := h
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [] }
  let proposed : GroupState α :=
    { gs with pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  have hnew : PendingStrong pending := by
    simp [PendingStrong, pending]
  have hproposed : RawStructural proposed :=
    ⟨hkeysM,
      assocInsert_keys_nodup proposalId pending gs.pendingProposals hkeysP,
      hcoh,
      assocInsert_property proposalId pending gs.pendingProposals
        (fun _ p => p.approvals.Nodup) hnew.1
        (fun c o hm => hnodup c o hm),
      hkeysB, hnodupB⟩
  change RawStructural (tryEnact proposed proposalId)
  exact tryEnact_preserves_structural proposed proposalId hproposed

private theorem applyApprove_structural (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) (h : RawStructural gs) :
    RawStructural (applyApprove gs signer proposalId) := by
  have hcopy := h
  obtain ⟨hkeysM, hkeysP, hcoh, hnodup, hkeysB, hnodupB⟩ := hcopy
  unfold applyApprove applyApproveDetailed
  cases hlookup : lookupPending proposalId gs with
  | none => simpa [hlookup] using h
  | some pending =>
      have hnodup' : (approvePending signer pending).approvals.Nodup :=
        setInsert_nodup signer pending.approvals
          (hnodup proposalId pending
            (assocLookup_some_mem proposalId pending gs.pendingProposals hlookup))
      have hupdated : RawStructural { gs with pendingProposals := assocInsert proposalId (approvePending signer pending) gs.pendingProposals } :=
        ⟨hkeysM,
          assocInsert_keys_nodup proposalId (approvePending signer pending) gs.pendingProposals hkeysP,
          hcoh,
          assocInsert_property proposalId (approvePending signer pending) gs.pendingProposals (fun _ p => p.approvals.Nodup) hnodup' (fun c o hm => hnodup c o hm),
          hkeysB, hnodupB⟩
      simpa [hlookup] using
        (tryEnact_preserves_structural _ _ hupdated)

private theorem applyEvent_structural (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (gs : GroupState α) (signer : Key)
    (event : GroupEvent α) (h : RawStructural gs) :
    RawStructural (applyEvent digest appFoldFn gs signer event) := by
  have hcopy := h
  obtain ⟨hkeysM, hkeysP, hcoh, hnodup, hkeysB, hnodupB⟩ := hcopy
  cases event with
  | app _ => exact h
  | base baseEvent =>
      cases baseEvent with
      | propose proposal =>
          exact applyPropose_structural digest gs signer proposal h
      | approve proposalId =>
          exact applyApprove_structural gs signer proposalId h

private theorem foldEvents_structural (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (events : List (Key × GroupEvent α))
    (gs : GroupState α) (h : RawStructural gs) :
    RawStructural (events.foldl
      (fun current signed => applyEvent digest appFoldFn current signed.1 signed.2) gs) := by
  induction events generalizing gs with
  | nil => exact h
  | cons hd tl ih =>
      obtain ⟨signer, event⟩ := hd
      exact ih _ (applyEvent_structural digest appFoldFn gs signer event h)

/-- Unconditional raw fold induction: every raw event list from any
structurally coherent start preserves the structural bundle. No
admissibility, no validation, no strong/governance premise — this is
the raw-domain guarantee that V-2 does not withdraw. -/
private theorem foldGroup_structural (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (initial : α) (events : List (Key × GroupEvent α))
    (h : RawStructural (emptyState initial)) :
    RawStructural (foldGroup digest appFoldFn initial events) :=
  foldEvents_structural digest appFoldFn events (emptyState initial) h

private theorem setInsert_idempotent (value : Key) (values : List Key) :
    setInsert value (setInsert value values) = setInsert value values := by
  by_cases hcon : values.contains value = true
  · have e1 : setInsert value values = values := by unfold setInsert; rw [if_pos hcon]
    rw [e1]
    exact e1
  · show (if (setInsert value values).contains value then setInsert value values
      else value :: setInsert value values) = _
    have hcon2 : (setInsert value values).contains value = true := by
      have hmem : value ∈ setInsert value values := by
        unfold setInsert
        rw [if_neg hcon]
        exact List.mem_cons_self
      simp only [List.contains]
      exact List.elem_eq_true_of_mem hmem
    rw [if_pos hcon2]

/-- Duplicate approval is idempotent at the fold level (executable
counterpart: `duplicateApprovalIsIdempotent` in Tests). -/
private theorem approvePending_idempotent (signer : Key)
    (pending : PendingProposal) :
    approvePending signer (approvePending signer pending) =
      approvePending signer pending := by
  have e : setInsert signer (approvePending signer pending).approvals =
      setInsert signer pending.approvals := by
    rw [approvePending_appr]
    exact setInsert_idempotent signer pending.approvals
  show { pending with approvals := setInsert signer (approvePending signer pending).approvals } = { pending with approvals := setInsert signer pending.approvals }
  rw [e]

/-- App-payload isolation at the raw fold: app events move neither the
member relation nor either pending store. -/
private theorem applyEvent_app_lists (digest : Proposal → ProposalId)
    (appFoldFn : AppFold α) (gs : GroupState α) (signer : Key) (event : α) :
    (applyEvent digest appFoldFn gs signer (.app event)).members = gs.members ∧
    (applyEvent digest appFoldFn gs signer (.app event)).pendingProposals =
      gs.pendingProposals ∧
    (applyEvent digest appFoldFn gs signer (.app event)).pendingBase =
      gs.pendingBase := by
  refine ⟨rfl, rfl, rfl⟩

theorem approvals_nodup {gs : GroupState α} (h : WellFormed gs)
    (entry : ProposalId × PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) : entry.2.approvals.Nodup :=
  (h.pendingCoherent entry.1 entry.2 hentry).1

/-- V-2 ruled non-membership: above one current admin, no counted
approval is the proposer's. Replaces the superseded
`proposer_mem_approvals` (same binders plus the count hypothesis). -/
theorem proposer_absent_above_one {gs : GroupState α} (h : WellFormed gs)
    (entry : ProposalId × PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) (hmany : 1 < adminCount gs) :
    entry.2.proposer ∉ entry.2.approvals :=
  (h.pendingCoherent entry.1 entry.2 hentry).2 hmany

/-- V-2 sole-admin exception: a sole admin's fresh separate self-approval
is admissible at the boundary (no threshold special case, no auto-assent).
Together with `proposer_absent_above_one` this is the ruled pair that
replaces the `proposer_mem_approvals` family. -/
theorem sole_admin_self_approval_ok {gs : GroupState α} (signer : Key)
    (proposalId : ProposalId)
    (hadmin : isAdmin signer gs = true)
    (hpending : ∃ pending, lookupPending proposalId gs = some pending ∧
      pending.proposer = signer)
    (hsingle : adminCount gs ≤ 1)
    (hfresh : ∀ pending, lookupPending proposalId gs = some pending →
      signer ∉ pending.approvals) :
    validateApproval gs signer proposalId = .ok () := by
  obtain ⟨pending, hlook, hprop⟩ := hpending
  have hbar : (signer == pending.proposer && decide (1 < adminCount gs)) =
      false := by
    have ha : (signer == pending.proposer) = true := by
      rw [hprop, beq_self_eq_true]
    have hd : decide (1 < adminCount gs) = false := by
      cases hdd : decide (1 < adminCount gs) with
      | true =>
          exfalso
          exact absurd (of_decide_eq_true hdd) (by omega)
      | false => rfl
    rw [ha, Bool.true_and]
    exact hd
  have hcon : (pending.approvals.contains signer) = false :=
    contains_eq_false_of_not_mem signer pending.approvals (hfresh pending hlook)
  have hreq : requireAdmin signer gs = .ok () := by
    simp [requireAdmin, hadmin]
  unfold validateApproval
  rw [hreq]
  show ((match lookupPending proposalId gs with
    | none => .error (.proposalNotFound proposalId)
    | some p =>
        if signer == p.proposer && decide (1 < adminCount gs) then
          .error (.proposerSelfApproval signer proposalId)
        else if p.approvals.contains signer then
          .error (.alreadyApproved signer proposalId)
        else .ok ()) : Except ValidationError Unit) = .ok ()
  simp only [hlook, hbar, hcon]
  rfl

#print axioms proposer_absent_above_one
#print axioms sole_admin_self_approval_ok

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
            { proposal, proposer := signer, approvals := [] }
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
  , ("a", .base (.approve "introduce:b"))
  , ("a", .base (.propose (witnessIntroduceAdmin "c")))
  , ("b", .base (.approve "introduce:c"))
  , ("a", .base (.propose
      (.introduceMember "later" "later@example.test" [])))
  , ("b", .base (.approve "introduce:later"))
  , ("a", .base (.propose (.removeMember "c")))
  , ("b", .base (.approve "remove:c"))
  , ("c", .base (.approve "remove:c"))
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
          enacted.pending.approvals == [] &&
          enacted.pending.approvals.length ≥ majority enacted.preState
    | none => false

/- The proposed CI-54-BOOTSTRAP-NO-PENDING invariant is false: once VI-6 has
left a stale sibling, successive validated removals can enact the last admins
while the sibling remains pending. -/
private def bootstrapPendingEvents : List (Key × GroupEvent Unit) :=
  stalePendingEvents ++
    [ ("a", .base (.propose (.removeMember "b")))
    , ("b", .base (.approve "remove:b"))
    , ("a", .base (.propose (.removeMember "a")))
    , ("a", .base (.approve "remove:a"))
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

/-! ### V-2 preservation on the integrated production path

The production boundary (`applyIntegratedEvent`) validates before it
acts, so every successful route already carries its admissibility
proof. These are `private` solely to keep the frozen
inversion-coverage pin (exactly 163 elaborated public theorem
declarations) at zero net change; each is still a machine-checked
preservation case for I68-06 (see INV-68-THEOREM-COUNT in the campaign
reliance record). -/

private theorem baseEmptyWellFormed (n : Nat) (mutation : BaseMutation)
    (proposer : Key) :
    PendingBaseWellFormed n { mutation, proposer, approvals := [] } := by
  simp [PendingBaseWellFormed]

private theorem baseEmptyStrong (mutation : BaseMutation) (proposer : Key) :
    PendingBaseStrong { mutation, proposer, approvals := [] } := by
  simp [PendingBaseStrong]

private theorem commitBaseChange_state_eq
    {AppState AppEvent BaseProposal AppError : Type}
    {ig : Integration AppState AppEvent BaseProposal AppError}
    {pre post : GroupState AppState} {change : BaseChange}
    {result : IntegratedResult AppState}
    (hok : commitBaseChange ig pre post change = .ok result) :
    ∃ appState, result.state = { post with appFold := appState } := by
  unfold commitBaseChange at hok
  split at hok
  · next appState hhook =>
      cases hok
      exact ⟨appState, rfl⟩
  · exact Except.noConfusion hok

#print axioms base_change_runs_hook

private theorem integratedPropose_both
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (proposal : BaseProposal)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (result : IntegratedResult AppState)
    (hok : applyIntegratedEvent ig gs signer (.propose proposal) = .ok result) :
    WellFormed result.state ∧ StrongCoherent result.state ∧
      StrongBaseCoherent result.state := by
  cases hval : validateBaseMutation gs signer (ig.proposalMutation proposal) with
  | error err =>
      exfalso
      have heq : applyIntegratedEvent ig gs signer (.propose proposal) =
          .error (.validation err) := by
        simp [applyIntegratedEvent, hval]
      rw [heq] at hok
      exact Except.noConfusion hok
  | ok _ =>
      let pid := ig.digest proposal
      let pb : PendingBase :=
        { mutation := ig.proposalMutation proposal, proposer := signer,
          approvals := [] }
      let inserted : GroupState AppState :=
        { gs with pendingBase := assocInsert pid pb gs.pendingBase }
      have hroute2 : tryEnactBase ig inserted pid = .ok result := by
        simpa [applyIntegratedEvent, hval] using hok
      have hbase : PendingBaseWellFormed (adminCount inserted) pb :=
        baseEmptyWellFormed _ _ _
      have hbaseS : PendingBaseStrong pb := baseEmptyStrong _ _
      have hins : WellFormed inserted :=
        { memberKeys := h.memberKeys
          pendingKeys := h.pendingKeys
          membersCoherent := h.membersCoherent
          pendingCoherent := h.pendingCoherent
          basePendingCoherent := assocInsert_property pid pb gs.pendingBase
            (fun _ q => PendingBaseWellFormed (adminCount inserted) q) hbase
            (fun c o hm => baseStrong_to_indexed _ _ (hsb c o hm)) }
      have hinsS : StrongCoherent inserted := hs
      have hinsSB : StrongBaseCoherent inserted :=
        assocInsert_property pid pb gs.pendingBase
          (fun _ q => PendingBaseStrong q) hbaseS hsb
      cases hlk : lookupPendingBase pid inserted with
      | none =>
          have hres : result = { state := inserted, change := none } := by
            simp only [tryEnactBase, hlk] at hroute2
            exact (Except.ok.inj hroute2).symm
          subst hres
          exact ⟨hins, hinsS, hinsSB⟩
      | some pb' =>
          by_cases threshold : pb'.approvals.length ≥ majority inserted
          · have hok2 : commitBaseChange ig inserted (enactMutation { inserted with pendingBase := assocErase pid inserted.pendingBase } pb'.mutation) (mutationChange pb'.mutation) = .ok result := by
              simpa [tryEnactBase, hlk, threshold] using hroute2
            obtain ⟨appState, hstate⟩ := commitBaseChange_state_eq hok2
            have hcons : inserted.pendingBase =
                (pid, pb) :: assocErase pid gs.pendingBase := rfl
            cases hm : pb'.mutation with
            | removeMember key =>
                rw [hm] at hstate
                have hhist : result.state.pendingProposals = gs.pendingProposals := by
                  simp [hstate, enactMutation]
                  rfl
                have hbaseEq : result.state.pendingBase =
                    assocErase pid gs.pendingBase := by
                  simp [hstate, enactMutation, hcons, assocErase_cons_self]
                have hmemEq : result.state.members = assocErase key gs.members := by
                  simp [hstate, enactMutation, inserted]
                have hW : WellFormed result.state :=
                  { memberKeys := by
                      rw [hmemEq]
                      exact assocErase_keys_nodup key gs.members h.memberKeys,
                    pendingKeys := by
                      rw [hhist]
                      exact h.pendingKeys,
                    membersCoherent := by
                      intro k m hm
                      rw [hmemEq] at hm
                      exact assocErase_property key gs.members
                        (fun candidate member => member.key = candidate)
                        h.membersCoherent k m hm,
                    pendingCoherent := fun qid q hmem => by
                      rw [hhist] at hmem
                      exact strong_to_indexed _ _ (hs qid q hmem),
                    basePendingCoherent := fun qid q hmem => by
                      rw [hbaseEq] at hmem
                      exact baseStrong_to_indexed _ _ (hsb qid q
                        ((assocErase_sublist pid gs.pendingBase).mem hmem)) }
                have hS : StrongCoherent result.state :=
                  fun qid q hmem => by
                    rw [hhist] at hmem
                    exact hs qid q hmem
                have hSB : StrongBaseCoherent result.state :=
                  fun qid q hmem => by
                    rw [hbaseEq] at hmem
                    exact hsb qid q
                      ((assocErase_sublist pid gs.pendingBase).mem hmem)
                exact ⟨hW, hS, hSB⟩
            | changeRoles key roles =>
                rw [hm] at hstate
                have hhist : result.state.pendingProposals = gs.pendingProposals := by
                  simp [hstate, enactMutation]
                  rfl
                have hbaseEq : result.state.pendingBase =
                    assocErase pid gs.pendingBase := by
                  simp [hstate, enactMutation, hcons, assocErase_cons_self]
                have hmemEq : result.state.members =
                    assocAdjust key (fun member => { member with roles }) gs.members := by
                  simp [hstate, enactMutation, inserted]
                have hW : WellFormed result.state :=
                  { memberKeys := by
                      rw [hmemEq, assocAdjust_keys]
                      exact h.memberKeys,
                    pendingKeys := by
                      rw [hhist]
                      exact h.pendingKeys,
                    membersCoherent := by
                      intro k m hm
                      rw [hmemEq] at hm
                      exact assocAdjust_property key
                        (fun member => { member with roles }) gs.members
                        (fun candidate member => member.key = candidate)
                        (by intro member coherent; exact coherent)
                        h.membersCoherent k m hm,
                    pendingCoherent := fun qid q hmem => by
                      rw [hhist] at hmem
                      exact strong_to_indexed _ _ (hs qid q hmem),
                    basePendingCoherent := fun qid q hmem => by
                      rw [hbaseEq] at hmem
                      exact baseStrong_to_indexed _ _ (hsb qid q
                        ((assocErase_sublist pid gs.pendingBase).mem hmem)) }
                have hS : StrongCoherent result.state :=
                  fun qid q hmem => by
                    rw [hhist] at hmem
                    exact hs qid q hmem
                have hSB : StrongBaseCoherent result.state :=
                  fun qid q hmem => by
                    rw [hbaseEq] at hmem
                    exact hsb qid q
                      ((assocErase_sublist pid gs.pendingBase).mem hmem)
                exact ⟨hW, hS, hSB⟩
          · have hres : result = { state := inserted, change := none } := by
              simp only [tryEnactBase, hlk, if_neg threshold] at hroute2
              exact (Except.ok.inj hroute2).symm
            subst hres
            exact ⟨hins, hinsS, hinsSB⟩

private theorem integratedApprove_both
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (proposalId : ProposalId)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (result : IntegratedResult AppState)
    (hok : applyIntegratedEvent ig gs signer (.approve proposalId) = .ok result) :
    WellFormed result.state ∧ StrongCoherent result.state ∧
      StrongBaseCoherent result.state := by
  cases hval : validateBaseApproval gs signer proposalId with
  | error err =>
      exfalso
      have heq : applyIntegratedEvent ig gs signer (.approve proposalId) =
          .error (.validation err) := by
        simp [applyIntegratedEvent, hval]
      rw [heq] at hok
      exact Except.noConfusion hok
  | ok _ =>
      obtain ⟨pb0, hlk0, hself, _hfresh, hadmin⟩ := validateBaseApproval_ok hval
      cases hlk : lookupPendingBase proposalId gs with
      | none =>
          exfalso
          have heq : applyIntegratedEvent ig gs signer (.approve proposalId) =
              .error (.validation (.proposalNotFound proposalId)) := by
            simp [applyIntegratedEvent, hval, hlk]
          rw [heq] at hok
          exact Except.noConfusion hok
      | some pending =>
          have hsame : pb0 = pending := Option.some.inj (hlk0.symm.trans hlk)
          subst hsame
          let approved : PendingBase :=
            { pb0 with approvals := setInsert signer pb0.approvals }
          let updated : GroupState AppState :=
            { gs with pendingBase := assocInsert proposalId approved gs.pendingBase }
          have hroute2 : tryEnactBase ig updated proposalId = .ok result := by
            simpa [applyIntegratedEvent, hval, hlk] using hok
          have hmem0 : (proposalId, pb0) ∈ gs.pendingBase :=
            assocLookup_some_mem proposalId pb0 gs.pendingBase hlk0
          have hstrong0 : PendingBaseStrong pb0 := hsb proposalId pb0 hmem0
          have hceq : adminCount updated = adminCount gs := rfl
          have hself' : approved.proposer = signer → ¬ 1 < adminCount updated := by
            intro he hlt
            have he0 : pb0.proposer = signer :=
              (rfl : approved.proposer = pb0.proposer).symm.trans he
            rw [hceq] at hlt
            exact hself he0 hlt
          have happ : PendingBaseWellFormed (adminCount updated) approved :=
            approveBasePending_wellFormed (adminCount updated) signer pb0
              (baseStrong_to_indexed _ _ hstrong0) hself'
          have hupd : WellFormed updated :=
            { memberKeys := h.memberKeys
              pendingKeys := h.pendingKeys
              membersCoherent := h.membersCoherent
              pendingCoherent := h.pendingCoherent
              basePendingCoherent := assocInsert_property proposalId approved
                gs.pendingBase
                (fun _ q => PendingBaseWellFormed (adminCount updated) q) happ
                (fun c o hm => baseStrong_to_indexed _ _ (hsb c o hm)) }
          have hlkU : lookupPendingBase proposalId updated = some approved :=
            assocLookup_insert_self proposalId approved gs.pendingBase
          by_cases threshold : approved.approvals.length ≥ majority updated
          · have hok2 : commitBaseChange ig updated (enactMutation { updated with pendingBase := assocErase proposalId updated.pendingBase } approved.mutation) (mutationChange approved.mutation) = .ok result := by
              simpa [tryEnactBase, hlkU, threshold] using hroute2
            obtain ⟨appState, hstate⟩ := commitBaseChange_state_eq hok2
            have hcons : updated.pendingBase =
                (proposalId, approved) :: assocErase proposalId gs.pendingBase := rfl
            cases hm : approved.mutation with
            | removeMember key =>
                rw [hm] at hstate
                have hhist : result.state.pendingProposals = gs.pendingProposals := by
                  rw [hstate]; rfl
                have hbaseEq : result.state.pendingBase =
                    assocErase proposalId gs.pendingBase := by
                  rw [hstate]
                  show assocErase proposalId updated.pendingBase = _
                  rw [hcons, assocErase_cons_self]
                have hmemEq : result.state.members = assocErase key gs.members := by
                  simp [hstate, enactMutation, updated]
                have hW : WellFormed result.state :=
                  { memberKeys := by
                      rw [hmemEq]
                      exact assocErase_keys_nodup key gs.members h.memberKeys,
                    pendingKeys := by
                      rw [hhist]
                      exact h.pendingKeys,
                    membersCoherent := by
                      intro k m hm
                      rw [hmemEq] at hm
                      exact assocErase_property key gs.members
                        (fun candidate member => member.key = candidate)
                        h.membersCoherent k m hm,
                    pendingCoherent := fun qid q hmem => by
                      rw [hhist] at hmem
                      exact strong_to_indexed _ _ (hs qid q hmem),
                    basePendingCoherent := fun qid q hmem => by
                      rw [hbaseEq] at hmem
                      exact baseStrong_to_indexed _ _ (hsb qid q
                        ((assocErase_sublist proposalId gs.pendingBase).mem hmem)) }
                have hS : StrongCoherent result.state :=
                  fun qid q hmem => by
                    rw [hhist] at hmem
                    exact hs qid q hmem
                have hSB : StrongBaseCoherent result.state :=
                  fun qid q hmem => by
                    rw [hbaseEq] at hmem
                    exact hsb qid q
                      ((assocErase_sublist proposalId gs.pendingBase).mem hmem)
                exact ⟨hW, hS, hSB⟩
            | changeRoles key roles =>
                rw [hm] at hstate
                have hhist : result.state.pendingProposals = gs.pendingProposals := by
                  rw [hstate]; rfl
                have hbaseEq : result.state.pendingBase =
                    assocErase proposalId gs.pendingBase := by
                  rw [hstate]
                  show assocErase proposalId updated.pendingBase = _
                  rw [hcons, assocErase_cons_self]
                have hmemEq : result.state.members =
                    assocAdjust key (fun member => { member with roles }) gs.members := by
                  simp [hstate, enactMutation, updated]
                have hW : WellFormed result.state :=
                  { memberKeys := by
                      rw [hmemEq, assocAdjust_keys]
                      exact h.memberKeys,
                    pendingKeys := by
                      rw [hhist]
                      exact h.pendingKeys,
                    membersCoherent := by
                      intro k m hm
                      rw [hmemEq] at hm
                      exact assocAdjust_property key
                        (fun member => { member with roles }) gs.members
                        (fun candidate member => member.key = candidate)
                        (by intro member coherent; exact coherent)
                        h.membersCoherent k m hm,
                    pendingCoherent := fun qid q hmem => by
                      rw [hhist] at hmem
                      exact strong_to_indexed _ _ (hs qid q hmem),
                    basePendingCoherent := fun qid q hmem => by
                      rw [hbaseEq] at hmem
                      exact baseStrong_to_indexed _ _ (hsb qid q
                        ((assocErase_sublist proposalId gs.pendingBase).mem hmem)) }
                have hS : StrongCoherent result.state :=
                  fun qid q hmem => by
                    rw [hhist] at hmem
                    exact hs qid q hmem
                have hSB : StrongBaseCoherent result.state :=
                  fun qid q hmem => by
                    rw [hbaseEq] at hmem
                    exact hsb qid q
                      ((assocErase_sublist proposalId gs.pendingBase).mem hmem)
                exact ⟨hW, hS, hSB⟩
          · have hres : result = { state := updated, change := none } := by
              simp only [tryEnactBase, hlkU, if_neg threshold] at hroute2
              exact (Except.ok.inj hroute2).symm
            subst hres
            have hsupdB : StrongBaseCoherent updated := by
              intro qid q hmem
              have hmemU : (qid, q) ∈ (proposalId, approved) ::
                  assocErase proposalId gs.pendingBase := hmem
              simp only [List.mem_cons] at hmemU
              rcases hmemU with heq | hold
              · simp only [Prod.mk.injEq] at heq
                obtain ⟨h1, h2⟩ := heq
                subst h1
                subst h2
                constructor
                · exact setInsert_nodup signer pb0.approvals hstrong0.1
                · by_cases heq2 : signer = pb0.proposer
                  · exfalso
                    have hone : adminCount gs = 1 := by
                      have hle : adminCount gs ≤ 1 :=
                        Classical.byContradiction fun hgt => hself heq2.symm (by omega)
                      have hge : 1 ≤ adminCount gs :=
                        adminCount_pos_of_isAdmin hadmin
                      omega
                    have hlen : 1 ≤ approved.approvals.length := by
                      show 1 ≤ (setInsert signer pb0.approvals).length
                      exact length_setInsert_pos signer pb0.approvals
                    have hmaj : majority updated = 1 := by
                      simp [majority, hceq, hone]
                    omega
                  · have hne : pb0.proposer ≠ signer := fun he => heq2 he.symm
                    show pb0.proposer ∉ setInsert signer pb0.approvals
                    exact setInsert_not_mem signer pb0.proposer pb0.approvals
                      hne hstrong0.2
              · exact hsb qid q
                  ((assocErase_sublist proposalId gs.pendingBase).mem hold)
            exact ⟨hupd, hs, hsupdB⟩

private theorem integratedDirect_both
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (command : DirectCommand)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (result : IntegratedResult AppState)
    (hok : applyIntegratedEvent ig gs signer (.direct command) = .ok result) :
    WellFormed result.state ∧ StrongCoherent result.state ∧
      StrongBaseCoherent result.state := by
  cases command with
  | admitMember key email roles =>
      cases hval : validateDirectAdmission ig.reserved gs signer key email roles with
      | error err =>
          exfalso
          have heq : applyIntegratedEvent ig gs signer
              (.direct (.admitMember key email roles)) =
              .error (.validation err) := by
            simp [applyIntegratedEvent, hval]
          rw [heq] at hok
          exact Except.noConfusion hok
      | ok _ =>
          have hok2 : commitBaseChange ig gs (admitMemberInto gs key email roles)
              (.memberAdmitted key) = .ok result := by
            simpa [applyIntegratedEvent, hval] using hok
          obtain ⟨appState, hstate⟩ := commitBaseChange_state_eq hok2
          have hmemEq : result.state.members =
              assocInsert key { key := key, email := email, roles := roles }
                gs.members := by
            rw [hstate]; rfl
          have hhist : result.state.pendingProposals = gs.pendingProposals := by
            rw [hstate]; rfl
          have hbase : result.state.pendingBase = gs.pendingBase := by
            rw [hstate]; rfl
          have hkeys := assocInsert_keys_nodup key
            { key := key, email := email, roles := roles } gs.members h.memberKeys
          have hcoh := assocInsert_property key
            { key := key, email := email, roles := roles } gs.members
            (fun candidate member => member.key = candidate) rfl h.membersCoherent
          have hW : WellFormed result.state :=
            { memberKeys := by rw [hmemEq]; exact hkeys,
              pendingKeys := by rw [hhist]; exact h.pendingKeys,
              membersCoherent := by
                intro k m hm
                rw [hmemEq] at hm
                exact hcoh k m hm,
              pendingCoherent := fun qid q hmem => by
                rw [hhist] at hmem
                exact strong_to_indexed _ _ (hs qid q hmem),
              basePendingCoherent := fun qid q hmem => by
                rw [hbase] at hmem
                exact baseStrong_to_indexed _ _ (hsb qid q hmem) }
          exact ⟨hW,
            fun qid q hmem => by
              rw [hhist] at hmem
              exact hs qid q hmem,
            fun qid q hmem => by
              rw [hbase] at hmem
              exact hsb qid q hmem⟩

private theorem integratedApp_both
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key) (event : AppEvent)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (result : IntegratedResult AppState)
    (hok : applyIntegratedEvent ig gs signer (.app event) = .ok result) :
    WellFormed result.state ∧ StrongCoherent result.state ∧
      StrongBaseCoherent result.state := by
  by_cases hmemB : GroupView.isMember signer (groupView gs) = true
  · cases happ : ig.appFold signer (groupView gs) (groupView gs)
        gs.appFold event with
    | error err =>
        exfalso
        have heq : applyIntegratedEvent ig gs signer (.app event) =
            .error (.app err) := by
          simp [applyIntegratedEvent, hmemB, happ]
        rw [heq] at hok
        exact Except.noConfusion hok
    | ok appState =>
        have heq : applyIntegratedEvent ig gs signer (.app event) =
            .ok ({ state := { gs with appFold := appState }, change := none } :
              IntegratedResult AppState) := by
          simp [applyIntegratedEvent, hmemB, happ]
        rw [heq] at hok
        have hres : result =
            { state := { gs with appFold := appState }, change := none } :=
          (Except.ok.inj hok).symm
        subst hres
        have hW : WellFormed
            ({ state := { gs with appFold := appState }, change := none } :
              IntegratedResult AppState).state :=
          { memberKeys := h.memberKeys,
            pendingKeys := h.pendingKeys,
            membersCoherent := h.membersCoherent,
            pendingCoherent := h.pendingCoherent,
            basePendingCoherent := h.basePendingCoherent }
        exact ⟨hW, hs, hsb⟩
  · exfalso
    have heq : applyIntegratedEvent ig gs signer (.app event) =
        .error (.validation (.notAMember signer)) := by
      simp [applyIntegratedEvent, hmemB]
    rw [heq] at hok
    exact Except.noConfusion hok

private theorem applyIntegratedEvent_both
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key)
    (event : IntegratedEvent BaseProposal AppEvent)
    (h : WellFormed gs) (hs : StrongCoherent gs) (hsb : StrongBaseCoherent gs)
    (result : IntegratedResult AppState)
    (hok : applyIntegratedEvent ig gs signer event = .ok result) :
    WellFormed result.state ∧ StrongCoherent result.state ∧
      StrongBaseCoherent result.state := by
  cases event with
  | direct command =>
      exact integratedDirect_both ig gs signer command h hs hsb result hok
  | propose proposal =>
      exact integratedPropose_both ig gs signer proposal h hs hsb result hok
  | approve proposalId =>
      exact integratedApprove_both ig gs signer proposalId h hs hsb result hok
  | app event =>
      exact integratedApp_both ig gs signer event h hs hsb result hok

private theorem foldIntegrated_all
    {AppState AppEvent BaseProposal AppError : Type}
    (ig : Integration AppState AppEvent BaseProposal AppError)
    (evs : List (Key × IntegratedEvent BaseProposal AppEvent))
    (gs : GroupState AppState) (h : WellFormed gs) (hs : StrongCoherent gs)
    (hsb : StrongBaseCoherent gs) :
    WellFormed (foldIntegrated ig gs evs) ∧
    StrongCoherent (foldIntegrated ig gs evs) ∧
    StrongBaseCoherent (foldIntegrated ig gs evs) := by
  induction evs generalizing gs with
  | nil => exact ⟨h, hs, hsb⟩
  | cons hd tl ih =>
      obtain ⟨signer, event⟩ := hd
      cases hstep : applyIntegratedEvent ig gs signer event with
      | error _ =>
          have hsame : foldIntegrated ig gs ((signer, event) :: tl) =
              foldIntegrated ig gs tl := by
            simp [foldIntegrated, hstep]
          rw [hsame]
          exact ih gs h hs hsb
      | ok result =>
          have hnext : foldIntegrated ig gs ((signer, event) :: tl) =
              foldIntegrated ig result.state tl := by
            simp [foldIntegrated, hstep]
          rw [hnext]
          obtain ⟨h1, hs1, hsb1⟩ :=
            applyIntegratedEvent_both ig gs signer event h hs hsb result hstep
          exact ih result.state h1 hs1 hsb1

end KelGroups

/- The frozen gate prints these mandated names unqualified from the root
module. Keep root theorem aliases while the portable implementation remains in
the `KelGroups` namespace. -/
theorem approvals_nodup {α : Type} {gs : KelGroups.GroupState α}
    (h : KelGroups.WellFormed gs)
    (entry : KelGroups.ProposalId × KelGroups.PendingProposal)
    (hentry : entry ∈ gs.pendingProposals) : entry.2.approvals.Nodup :=
  KelGroups.approvals_nodup h entry hentry

/- V-2: the old root `proposer_mem_approvals` alias stated the superseded
proposer-credit regime and is retired without replacement. The
KelGroups-namespace ruled pair (`proposer_absent_above_one`,
`sole_admin_self_approval_ok`) is the replacement; retiring the alias
keeps the frozen 163-theorem pin at zero net change. -/

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
