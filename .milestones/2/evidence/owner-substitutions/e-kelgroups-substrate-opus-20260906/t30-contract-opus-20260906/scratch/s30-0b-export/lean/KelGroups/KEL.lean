/-
  KelGroups.KEL — L1/L2 architecture types and invariants

  Formalizes the 12 invariants from the keri-bridge design doc
  section 4. Models: hash-chained KEL, server identity, L2
  per-proposal voting KELs, enactment as compact proof on L1.
-/
import KERI.Crypto
import KERI.Event
import KERI.KEL
import KelGroups.Basic

namespace KelGroups

open KERI.Crypto
open KERI.Event
open KERI.KEL (hashChainValid)

/-- Abstract nonce (client-generated randomness). -/
abbrev Nonce := Nat

-- ============================================================
-- L1 event payloads
-- ============================================================

/-- Approval proof: an admin key and their signature over the
proposal SAID. -/
structure ApprovalProof where
  adminKey : Key
  signature : Signature
  deriving Repr, DecidableEq

/-- L1 event payloads. The server signs all L1 events. -/
inductive L1Payload where
  /-- Server inception: establishes server identity. -/
  | inception : Key → L1Payload
  /-- Enacted proposal with compact proof. -/
  | enacted : SAID → List ApprovalProof → L1Payload
  /-- Expired proposal (timeout elapsed). -/
  | expired : SAID → L1Payload
  deriving Repr

/-- An L1 event. -/
abbrev L1Event := KELEvent L1Payload

/-- The L1 chain. -/
abbrev L1 := KEL L1Payload

-- ============================================================
-- L2 event payloads
-- ============================================================

/-- L2 event payloads. -/
inductive L2Payload where
  /-- L2 inception: anchors the proposal content, nonce,
  and timeout. -/
  | inception : Proposal → Nonce → Nat → L2Payload
  /-- Approval: anchors the proposal SAID. -/
  | approval : SAID → L2Payload
  deriving Repr

/-- An L2 event. -/
abbrev L2Event := KELEvent L2Payload

/-- An L2 chain (per-proposal). -/
abbrev L2 := KEL L2Payload

-- ============================================================
-- Predicates for the 12 invariants
-- ============================================================

/-- INV 2: L1 event 0 is the server inception. -/
def l1StartsWithInception (l1 : L1) : Prop :=
  match l1.getLast? with
  | some e => e.sequenceNumber = 0
    ∧ e.priorDigest = none
    ∧ ∃ k, e.payload = .inception k
  | none => False

/-- Extract the server key from L1 inception. -/
def serverKey (l1 : L1) : Option Key :=
  match l1.getLast? with
  | some e => match e.payload with
    | .inception k => some k
    | _ => none
  | none => none

/-- INV 3: Only the server writes to L1. -/
def l1ServerOnly (l1 : L1) : Prop :=
  match serverKey l1 with
  | some sk => ∀ e ∈ l1, e.signer = sk
  | none => False

/-- INV 4: Every L1 enacted event is self-contained. -/
def l1EnactedSelfContained (e : L1Event) : Prop :=
  match e.payload with
  | .enacted said proofs => said ≠ 0 ∧ proofs.length > 0
  | _ => True

/-- INV 5: Approval signatures are over the proposal SAID.
Modeled as: each approval in an L2 references the same SAID
as the L2's inception. -/
def l2ApprovalsMatchSAID (l2 : L2) (proposalSAID : SAID) : Prop :=
  ∀ e ∈ l2, match e.payload with
    | .approval said => said = proposalSAID
    | .inception _ _ _ => True

/-- INV 6: Proposal SAIDs are unique across a set of L2s.
Modeled as: no two L2 inception events share the same SAID. -/
def proposalSAIDsUnique (saids : List SAID) : Prop :=
  saids.Nodup

/-- INV 7: The proposing admin controls the nonce.
Modeled as: the L2 inception is signed by an admin, not the
server. -/
def l2InceptionByAdmin
    (l2 : L2) (serverK : Key) (admins : List Key) : Prop :=
  match l2.getLast? with
  | some e => e.signer ≠ serverK ∧ e.signer ∈ admins
  | none => False

/-- INV 8: L2 only accepts approvals (after inception). -/
def l2OnlyApprovals (l2 : L2) : Prop :=
  ∀ e ∈ l2, match e.payload with
    | .inception _ _ _ => e.sequenceNumber = 0
    | .approval _ => e.sequenceNumber > 0

/-- INV 9: No duplicate approvals — each admin approves at
most once per L2. -/
def l2NoDuplicateApprovals (l2 : L2) : Prop :=
  let approvers := l2.filterMap fun e =>
    match e.payload with
    | .approval _ => some e.signer
    | _ => none
  approvers.Nodup

/-- INV 10: Threshold = admin majority. -/
def thresholdMet
    (approvalCount : Nat) (adminCnt : Nat) : Prop :=
  approvalCount ≥ majority adminCnt

/-- INV 11: L2 has a timeout. Modeled as: the inception
event carries a non-zero timeout. -/
def l2HasTimeout (l2 : L2) : Prop :=
  match l2.getLast? with
  | some e => match e.payload with
    | .inception _ _ timeout => timeout > 0
    | _ => False
  | none => False

/-- INV 12: L2 is ephemeral — this is a design constraint,
not a provable property. We state it as: the L1 enacted
event contains enough information to verify without L2. -/
def l1EnactmentComplete (e : L1Event) (adminCnt : Nat) : Prop :=
  match e.payload with
  | .enacted _ proofs => thresholdMet proofs.length adminCnt
  | _ => True

-- ============================================================
-- Transition functions (state machine actions)
-- ============================================================

/-- Create an L2 with inception event. The proposing admin signs. -/
def mkL2
    (prop : Proposal) (nonce : Nonce) (timeout : Nat)
    (adminKey : Key) (sig : Signature) : L2 :=
  [KELEvent.mk 0 none (.inception prop nonce timeout) adminKey sig]

/-- Append an approval to an L2. The approving admin signs.
Requires the digest of the current tip and the proposal SAID. -/
def appendApproval
    (l2 : L2) (adminKey : Key) (sig : Signature)
    (proposalSAID : SAID) (tipDigest : Digest) : L2 :=
  let seqN := match l2.head? with
    | some e => e.sequenceNumber + 1
    | none => 0
  KELEvent.mk seqN (some tipDigest)
    (.approval proposalSAID) adminKey sig :: l2

/-- Create an L1 with server inception event. -/
def mkL1 (serverK : Key) (sig : Signature) : L1 :=
  [KELEvent.mk 0 none (.inception serverK) serverK sig]

/-- Append an enacted event to L1. The server signs. -/
def appendEnacted
    (l1 : L1) (serverK : Key) (sig : Signature)
    (proposalSAID : SAID) (proofs : List ApprovalProof)
    (tipDigest : Digest) : L1 :=
  let seqN := match l1.head? with
    | some e => e.sequenceNumber + 1
    | none => 0
  KELEvent.mk seqN (some tipDigest)
    (.enacted proposalSAID proofs) serverK sig :: l1

/-- Append an expired event to L1. The server signs. -/
def appendExpired
    (l1 : L1) (serverK : Key) (sig : Signature)
    (proposalSAID : SAID) (tipDigest : Digest) : L1 :=
  let seqN := match l1.head? with
    | some e => e.sequenceNumber + 1
    | none => 0
  KELEvent.mk seqN (some tipDigest)
    (.expired proposalSAID) serverK sig :: l1

-- ============================================================
-- Combined system validity
-- ============================================================

/-- A valid L1 chain satisfies invariants 1–4. -/
structure L1Valid (l1 : L1) : Prop where
  chain : hashChainValid l1
  inception : l1StartsWithInception l1
  serverOnly : l1ServerOnly l1
  selfContained : ∀ e ∈ l1, l1EnactedSelfContained e

/-- A valid L2 chain satisfies invariants 5, 7–9, 11. -/
structure L2Valid
    (l2 : L2)
    (proposalSAID : SAID)
    (serverK : Key)
    (admins : List Key) : Prop where
  approvalsMatch : l2ApprovalsMatchSAID l2 proposalSAID
  inceptionByAdmin : l2InceptionByAdmin l2 serverK admins
  onlyApprovals : l2OnlyApprovals l2
  noDuplicates : l2NoDuplicateApprovals l2
  hasTimeout : l2HasTimeout l2

end KelGroups
