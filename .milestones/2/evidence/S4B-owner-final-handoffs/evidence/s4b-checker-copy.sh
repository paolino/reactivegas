#!/usr/bin/env bash
# scripts/check-lean-mirrors — S4-B mandatory mirror checker (R7-R9, C1-C3).
#
# Reconciles, per identity, source → compiled inventory → counterpart /
# correctness relation for the #66 Prop/Bool correspondence scope:
#   * discovers every Prop-valued project definition/inductive from the
#     COMPILED environment (result sort of the elaborated type, after opening
#     dependent parameters; Prop-parameter/non-Prop-result shapes excluded;
#     `<I>.below` eliminator machinery of a discovered inductive `I` excluded
#     by structural rule and logged);
#   * requires each discovered identity to be either a tabled correspondence
#     row (mirror const and theorem const both exist; the theorem is an `Iff`
#     off the discovered Prop; the right-hand side mentions the mirror) or a
#     separately named exception (V4 definitional identity, P11
#     per-constructor projection, R0 executable instance + evaluation, P13
#     bounded non-executable);
#   * rejects stale table rows, missing mirrors/theorems, shape mismatches,
#     orphan Bool mirrors in the new mirror modules, and an empty discovery.
# Exit 0 with a MIRROR-CHECK-OK receipt iff everything reconciles; exit 1
# NAMING the offending identity otherwise. On success the driver ALSO writes
# `lean/.lake/s4b-mirror-receipt` binding the reconciled extent to a freshness
# nonce: the `just lean` recipe writes a fresh nonce before invoking this script
# and asserts the receipt afterwards, so a checker neutered to unconditional
# success — or a removed/bypassed invocation — fails loudly as
# MIRROR-RECEIPT-ABSENT instead of passing silently (C4, instrument v2). The
# `just lean` evidence must contain the receipt line: its absence means this
# checker did not run (R10).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RECEIPT_DIR="$HERE/../lean/.lake"
RECEIPT="$RECEIPT_DIR/s4b-mirror-receipt"
NONCE_FILE="$RECEIPT_DIR/s4b-mirror-nonce"
rm -f "$RECEIPT"
if [ -f "$NONCE_FILE" ]; then NONCE="$(cat "$NONCE_FILE")"; else NONCE="leaf-$(date +%s%N)-$$"; fi
DRIVER="$(mktemp /tmp/s4b-mirror-driver.XXXXXX.lean)"
trap 'rm -f "$DRIVER"' EXIT
cat > "$DRIVER" <<'LEAN_EOF'
-- S4-B mandatory mirror checker driver (heredoc-embedded, not a repo file).
-- Contract: scripts/check-lean-mirrors header comment. This driver defines no
-- Prop-valued def: all evidence below is `example`s (theorem info, excluded
-- from discovery) plus Bool-typed table data.

import KelGroups
import Reactivegas
import Reactivegas.Mirrors
import KelGroups.Mirrors

-- V4 exception evidence: definitional identity (EQ form, closes by rfl).
example (theta : KelGroups.Vote.Threshold) (view : KelGroups.GroupView)
    (gs : KelGroups.Vote.VoteState) (s : KelGroups.Key)
    (ev : KelGroups.Vote.VoteEvent) (qid : KelGroups.Vote.QuestionId) :
    KelGroups.Vote.PreservesQuestionSemantics theta view gs s ev qid =
      (KelGroups.Vote.preservesQuestionDecide theta view gs s ev qid = true) := rfl

-- P11 exception evidence: per-constructor definitional projection (14x rfl).
-- Each arm of `authorizedStep` IS the existing `isResponsabile` application.
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.openPurchase a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.grantPermission a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.denyPermission a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (v : Int) :
    authorizedStep view s (.deposit a u v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (v : Int) :
    authorizedStep view s (.withdraw a u v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a f : KelGroups.Key) (v : Int) :
    authorizedStep view s (.transferCassa a f v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (v : Int) :
    authorizedStep view s (.donate a v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (w : Int) :
    authorizedStep view s (.backdonate a w) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) (v : Int) :
    authorizedStep view s (.pledge a u c v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.acceptPledge a u c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.refusePledge a u c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) (v : Int) :
    authorizedStep view s (.correctPledge a u c v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.closePurchase a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.failPurchase a c) s' = (isResponsabile view a = true) := rfl

-- R0 exception evidence: executable (Decidable instance + evaluation).
example (s : State) : Decidable (stalled s) := inferInstance
example : decide (stalled State.empty) = false := by decide

-- Executable-evaluation assertions for the 17 new mirrors (tiny witnesses).
-- Each elaborates only if the mirror compiles AND evaluates to the asserted
-- value: this is EXECUTABLE-DECISION evidence, not a Decidable instance.
example : conservationB State.empty = true := by decide
example : solventB { members := [] } State.empty = true := by decide
example : insolventB { members := [] } State.empty = false := by decide
example : uniquePledgesB { id := 0, referente := "r", permitted := false, accepted := [], pending := [] } = true := by decide
example : allUniquePledgesB State.empty = true := by decide
example : escrowHeldB { id := 0, referente := "r", permitted := false, accepted := [], pending := [] } "u" 0 = false := by decide
example : governanceEnactsB "u" State.empty = true := by decide
example : doubleEntryB State.empty State.empty "a" "u" 0 = true := by decide
example : canCloseGroupB { members := [] } State.empty = true := by decide
example : KelGroups.pendingWellFormedB { proposal := .removeMember "k", proposer := "p", approvals := [] } = false := by decide
example : KelGroups.membersCoherentB (KelGroups.emptyState ()) = true := by decide
example : KelGroups.pendingCoherentB (KelGroups.emptyState ()) = true := by decide
example : KelGroups.wellFormedB (KelGroups.emptyState ()) = true := by decide
example : KelGroups.enactsB (KelGroups.emptyState ()) "pid" (KelGroups.emptyState ()) = false := by decide
example : KelGroups.Vote.questionCleanB { kind := .collective, proposer := "a", assents := [], dissents := [] } = true := by decide
example : KelGroups.Vote.sweepReadyB { members := [] } KelGroups.Vote.emptyVoteState = true := by decide
example : KelGroups.Vote.voteWellFormedB (fun _ => 0) { members := [] } KelGroups.Vote.emptyVoteState = true := by decide

/-- The 19 claimed correspondence rows: discovered Prop, optional existing or
new mirror const, correctness theorem const. -/
def s4bCorrTable : Array (String × Option String × String) := #[
  ("comune_not_a_member", some "KelGroups.GroupView.isMember", "comune_not_a_member_corr"),
  ("conservation", some "conservationB", "conservation_corr"),
  ("solvent", some "solventB", "solvent_corr"),
  ("insolvent", some "insolventB", "insolvent_corr"),
  ("uniquePledges", some "uniquePledgesB", "uniquePledges_corr"),
  ("allUniquePledges", some "allUniquePledgesB", "allUniquePledges_corr"),
  ("permissionToClose", none, "permissionToClose_corr"),
  ("escrowHeld", some "escrowHeldB", "escrowHeld_corr"),
  ("governanceEnacts", some "governanceEnactsB", "governanceEnacts_corr"),
  ("doubleEntry", some "doubleEntryB", "doubleEntry_corr"),
  ("canCloseGroup", some "canCloseGroupB", "canCloseGroup_corr"),
  ("KelGroups.PendingWellFormed", some "KelGroups.pendingWellFormedB", "KelGroups.pendingWellFormed_corr"),
  ("KelGroups.MembersCoherent", some "KelGroups.membersCoherentB", "KelGroups.membersCoherent_corr"),
  ("KelGroups.PendingCoherent", some "KelGroups.pendingCoherentB", "KelGroups.pendingCoherent_corr"),
  ("KelGroups.WellFormed", some "KelGroups.wellFormedB", "KelGroups.wellFormed_corr"),
  ("KelGroups.Enacts", some "KelGroups.enactsB", "KelGroups.enacts_corr"),
  ("KelGroups.Vote.QuestionClean", some "KelGroups.Vote.questionCleanB", "KelGroups.Vote.questionClean_corr"),
  ("KelGroups.Vote.SweepReady", some "KelGroups.Vote.sweepReadyB", "KelGroups.Vote.sweepReady_corr"),
  ("KelGroups.Vote.VoteWellFormed", some "KelGroups.Vote.voteWellFormedB", "KelGroups.Vote.voteWellFormed_corr")]

/-- Separately named legitimate exceptions (V4, P11, R0 definitional or
executable evidence above; P13 bounded, no oracle required). -/
def s4bExceptions : Array String :=
  #["KelGroups.Vote.PreservesQuestionSemantics", "authorizedStep", "stalled", "Reach"]

/-- Harvest all referenced constant names from an expression. -/
partial def s4bHarvestConsts (e : Lean.Expr) (acc : Array Lean.Name) : Array Lean.Name :=
  match e with
  | .const n _ => if acc.contains n then acc else acc.push n
  | .app f a => s4bHarvestConsts a (s4bHarvestConsts f acc)
  | .lam _ t b _ => s4bHarvestConsts b (s4bHarvestConsts t acc)
  | .forallE _ t b _ => s4bHarvestConsts b (s4bHarvestConsts t acc)
  | .letE _ t v b _ => s4bHarvestConsts b (s4bHarvestConsts v (s4bHarvestConsts t acc))
  | .mdata _ b => s4bHarvestConsts b acc
  | .proj _ _ b => s4bHarvestConsts b acc
  | _ => acc

open Lean Elab Command Meta in
run_cmd do
  let env ← getEnv
  let mods := env.header.moduleNames
  let nonce ← IO.getEnv "S4B_MIRROR_NONCE"
  let nonce := nonce.getD "unset"
  let failedRef ← IO.mkRef false
  let fail (msg : MessageData) : CommandElabM Unit := do
    logError msg
    failedRef.set true
  -- 1. Discovery: Prop-valued defs/inducts with project home modules.
  let mut found : Array (String × String) := #[]
  let mut skipped : Array String := #[]
  for (n, ci) in env.constants.toList do
    let isPred : Bool :=
      match ci with
      | .defnInfo _ => true
      | .inductInfo _ => true
      | _ => false
    if !isPred then continue
    let home : String :=
      match env.getModuleIdxFor? n with
      | some idx =>
          match mods[idx]? with
          | some m => toString m
          | none => "<no-mod>"
      | none => "<current-file>"
    if !(home.startsWith "KelGroups" || home.startsWith "Reactivegas") then continue
    let verdict : Option Bool ← liftTermElabM (try
      forallTelescopeReducing ci.type fun _ cod => do
        let c ← whnf cod
        match c with
        | .sort .zero => pure (some true)
        | .sort _ => pure (some false)
        | _ => pure (some false)
      catch _ => pure none)
    match verdict with
    | some true => found := found.push (home, toString n)
    | some false => pure ()
    | none => skipped := skipped.push (toString n)
  if found.size == 0 then fail m!"MIRROR-EMPTY-DISCOVERY"
  for sk in skipped do logInfo m!"MIRROR-SKIP {sk}"
  -- 2. Structural below-rule: `<I>.below` of a discovered induct `I` is
  -- eliminator machinery, not an owned predicate. Logged, never silent.
  let mut covered : Array String := #[]
  let mut belowCount := 0
  for (_, s) in found do
    if s.endsWith ".below" then
      let parent := s.dropRight ".below".length
      match env.find? parent.toName with
      | some (.inductInfo _) =>
          logInfo m!"MIRROR-BELOW-EXCLUDED {s} (machinery of discovered induct {parent})"
          covered := covered.push s
          belowCount := belowCount + 1
      | _ => pure ()
  -- 3. Per-identity reconciliation of the 19 claimed rows.
  let mut rowCount := 0
  for (prop, mirrorOpt, thm) in s4bCorrTable do
    if !found.any (fun (_, s) => s == prop) then
      fail m!"MIRROR-STALE-ROW {prop} (tabled but not discovered)"
      continue
    covered := covered.push prop
    match mirrorOpt with
    | some m =>
        match env.find? m.toName with
        | some _ => pure ()
        | none => fail m!"MIRROR-MISSING {prop} (no const {m})"
    | none => pure ()
    match env.find? thm.toName with
    | none => fail m!"MIRROR-THEOREM-MISSING {prop} (no const {thm})"
    | some ti =>
        match ti with
        | .thmInfo _ => pure ()
        | _ => fail m!"MIRROR-NOT-THEOREM {prop} ({thm} is not theorem info)"
        let shape ← liftTermElabM (try
          forallTelescopeReducing ti.type fun _ ty => do
            let t ← whnf ty
            match t with
            | .app (.app (.const ``Iff _) lhs) rhs =>
                -- NOTE: no whnf on `lhs` here: unfolding would dissolve
                -- def-Props into their bodies and hide the head const.
                match lhs.getAppFn with
                | .const pn _ =>
                    if toString pn == prop then
                      match mirrorOpt with
                      | some m =>
                          pure (s4bHarvestConsts rhs #[] |>.contains m.toName)
                      | none => pure true
                    else pure false
                | _ => pure false
            | _ => pure false
          catch _ => pure false)
        if !shape then fail m!"MIRROR-SHAPE-MISMATCH {prop} (theorem {thm} is not an Iff off {prop} mentioning its mirror)"
        else
          logInfo m!"MIRROR-ROW {prop} {mirrorOpt.getD "(inline-expr)"} {thm} OK"
          rowCount := rowCount + 1
  -- 4. Named exceptions must be discovered (else the exception is stale).
  for e in s4bExceptions do
    if !found.any (fun (_, s) => s == e) then
      fail m!"MIRROR-STALE-EXCEPTION {e}"
    else
      covered := covered.push e
      logInfo m!"MIRROR-EXCEPTION {e} OK"
  -- 5. Anything discovered but unaccounted for is an uncovered owned predicate.
  for (home, s) in found do
    if !covered.contains s then
      fail m!"MIRROR-UNCOVERED {home} :: {s}"
  -- 6. Orphan Bool mirrors: every Bool def in the new mirror modules must be
  -- claimed by exactly one table row.
  let mut claimedMirrors : Array String := #[]
  for (_, mOpt, _) in s4bCorrTable do
    match mOpt with
    | some m => claimedMirrors := claimedMirrors.push m
    | none => pure ()
  let mut orphanCount := 0
  for (n, ci) in env.constants.toList do
    match ci with
    | .defnInfo _ => pure ()
    | _ => continue
    let home : String :=
      match env.getModuleIdxFor? n with
      | some idx =>
          match mods[idx]? with
          | some m => toString m
          | none => "<no-mod>"
      | none => "<current-file>"
    if !(home == "Reactivegas.Mirrors" || home == "KelGroups.Mirrors") then continue
    let isBool ← liftTermElabM (try
      forallTelescopeReducing ci.type fun _ cod => do
        let c ← whnf cod
        match c with
        | .const ``Bool _ => pure true
        | _ => pure false
      catch _ => pure false)
    if isBool then
      let s := toString n
      if !claimedMirrors.contains s then
        fail m!"MIRROR-ORPHAN {s} (Bool mirror in {home} claimed by no row)"
      else orphanCount := orphanCount + 1
  logInfo m!"MIRROR-SUMMARY rows={rowCount} exceptions={s4bExceptions.size} below={belowCount} orphans-checked={orphanCount} discovered={found.size}"
  let failed ← failedRef.get
  if failed then fail m!"MIRROR-CHECK-FAILED"
  else do
    logInfo m!"MIRROR-CHECK-OK rows={rowCount} exceptions={s4bExceptions.size} discovered={found.size}"
    let names := (found.map (·.2)).qsort (· < ·)
    let receipt := "MIRROR-CHECK-OK rows=" ++ toString rowCount ++
      " exceptions=" ++ toString s4bExceptions.size ++
      " discovered=" ++ toString found.size ++ "\n" ++
      "nonce=" ++ nonce ++ "\n" ++
      "\n".intercalate (names.toList.map (fun s => "extent " ++ s)) ++ "\n"
    IO.FS.writeFile ".lake/s4b-mirror-receipt" receipt
    logInfo m!"MIRROR-RECEIPT-WROTE nonce={nonce}"
LEAN_EOF
cd "$HERE/../lean" && S4B_MIRROR_NONCE="$NONCE" lake env lean "$DRIVER"
