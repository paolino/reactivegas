-- S2-census.lean — compiled-identity census, before/after classifier rules.
-- OT4 INSTRUMENT (submission 2; path chosen here, bound at handoff — the sheet
-- froze OT4 as a description, not a command). Single-file elaboration
-- importing ROOTS (full env via current oleans; kind census is unaffected by
-- the pending visibility promotion, which changes no kinds or counts).
-- Computes BOTH the old-rule view (defn/induct + home + sort: what v2
-- admitted) and the new-rule total census (every kind classified per the F01
-- repair, mechanically mirroring the repaired checker). Prints BOTH sorted
-- identity sets (names, never counts alone), the per-kind table, and the
-- named below-exclusion; asserts set relations, never a hardcoded extent:
-- new-found ⊇ old-found (no regression), symmetric difference EMPTY on the
-- clean tree (any delta appears BY NAME — room for legitimate new discovery,
-- not a quota), unclassified == 0, opaque-pred == 0 on this tree
-- (hole-absence baseline for this exact tree, not a future constant), and a
-- nonzero theorem-exclusion sanity. Sort-elaboration failures land in their
-- OWN named bucket (never "non-predicate": an error is not a classification).
-- S2-CENSUS-OK is printed ONLY with zero errors; otherwise S2-CENSUS-FAILED.
-- O1 cross-checks agreement with the repaired checker itself.
-- Expect: exit 0.
import KelGroups
import Reactivegas

open Lean Elab Command Meta in
run_cmd do
  let env ← getEnv
  let mods := env.header.moduleNames
  let failedRef ← IO.mkRef false
  let fail (msg : MessageData) : CommandElabM Unit := do
    logError msg
    failedRef.set true
  let mut oldFound : Array String := #[]
  let mut newFound : Array String := #[]
  let mut sortUndecided : Array String := #[]
  let mut belowExcluded : Array String := #[]
  let mut kindCounts : Array (String × Nat) :=
    #[("defn-pred", 0), ("defn-nonpred", 0), ("induct-pred", 0),
      ("induct-nonpred", 0), ("opaque-pred", 0), ("opaque-nonpred", 0),
      ("thm-excluded", 0), ("axiom-excluded", 0), ("ctor-rec-excluded", 0),
      ("unclassified", 0), ("nonowned", 0)]
  for (n, ci) in env.constants.toList do
    let home : String :=
      match env.getModuleIdxFor? n with
      | some idx =>
          match mods[idx]? with
          | some m => toString m
          | none => "<no-mod>"
      | none => "<current-file>"
    let owned := home.startsWith "KelGroups" || home.startsWith "Reactivegas"
    -- OLD rule view (v2 admitted behavior): defn/induct + home + sort.
    match ci with
    | .defnInfo _ | .inductInfo _ =>
        if owned then
          let verdict : Option Bool ← liftTermElabM (try
            forallTelescopeReducing ci.type fun _ cod => do
              let c ← whnf cod
              match c with
              | .sort .zero => pure (some true)
              | _ => pure (some false)
            catch _ => pure none)
          match verdict with
          | some true => oldFound := oldFound.push (toString n)
          | _ => pure ()
        else pure ()
    | _ => pure ()
    -- NEW rule view (F01 total classification, mirroring the repaired checker).
    if !owned then
      kindCounts := kindCounts.map (fun p => if p.1 == "nonowned" then (p.1, p.2 + 1) else p)
      continue
    match ci with
    | .defnInfo _ | .inductInfo _ | .opaqueInfo _ =>
        let kindTag : String :=
          match ci with
          | .defnInfo _ => "defn"
          | .inductInfo _ => "induct"
          | .opaqueInfo _ => "opaque"
          | _ => "UNREACHABLE-INNER"
        if kindTag == "UNREACHABLE-INNER" then
          kindCounts := kindCounts.map (fun p => if p.1 == "unclassified" then (p.1, p.2 + 1) else p)
          continue
        let verdict : Option Bool ← liftTermElabM (try
          forallTelescopeReducing ci.type fun _ cod => do
            let c ← whnf cod
            match c with
            | .sort .zero => pure (some true)
            | _ => pure (some false)
          catch _ => pure none)
        match verdict with
        | some true =>
            newFound := newFound.push (toString n)
            kindCounts := kindCounts.map (fun p => if p.1 == kindTag ++ "-pred" then (p.1, p.2 + 1) else p)
        | some false =>
            kindCounts := kindCounts.map (fun p => if p.1 == kindTag ++ "-nonpred" then (p.1, p.2 + 1) else p)
        | none =>
            sortUndecided := sortUndecided.push s!"{home} :: {toString n} :: {kindTag}"
    | .thmInfo _ =>
        kindCounts := kindCounts.map (fun p => if p.1 == "thm-excluded" then (p.1, p.2 + 1) else p)
    | .axiomInfo _ =>
        kindCounts := kindCounts.map (fun p => if p.1 == "axiom-excluded" then (p.1, p.2 + 1) else p)
    | .ctorInfo _ | .recInfo _ =>
        kindCounts := kindCounts.map (fun p => if p.1 == "ctor-rec-excluded" then (p.1, p.2 + 1) else p)
    | _ =>
        kindCounts := kindCounts.map (fun p => if p.1 == "unclassified" then (p.1, p.2 + 1) else p)
  -- Structural below-rule, same convention as the checker (logged, never silent).
  for s in newFound do
    if s.endsWith ".below" then
      belowExcluded := belowExcluded.push s
  let get := fun (t : String) => (kindCounts.find? (fun p => p.1 == t)).map (·.2) |>.getD 0
  for p in kindCounts do logInfo m!"S2-CENSUS {p.1}={p.2}"
  for sk in sortUndecided do logInfo m!"S2-CENSUS-SORT-UNDECIDED {sk}"
  for b in belowExcluded do logInfo m!"S2-CENSUS-BELOW-EXCLUDED {b}"
  let oldSorted := oldFound.qsort (· < ·)
  let newSorted := newFound.qsort (· < ·)
  for s in oldSorted do logInfo m!"S2-CENSUS-OLD {s}"
  for s in newSorted do logInfo m!"S2-CENSUS-NEW {s}"
  -- Identity reconciliation (sets, not counts): every old-rule identity must
  -- appear under the new rule; any symmetric difference is printed BY NAME.
  for s in oldSorted do
    if !newSorted.contains s then fail m!"S2-CENSUS-REGRESSION {s} (old-rule identity absent under new rule)"
  for s in newSorted do
    if !oldSorted.contains s then fail m!"S2-CENSUS-DELTA {s} (new-rule identity outside old rule: investigate, do not assume)"
  if get "unclassified" != 0 then fail m!"S2-CENSUS-UNCLASSIFIED-NONZERO"
  if get "opaque-pred" != 0 then fail m!"S2-CENSUS-OPAQUE-PRED-NONZERO (clean-tree baseline for this exact tree)"
  if get "thm-excluded" == 0 then fail m!"S2-CENSUS-THM-EXCLUDED-ZERO (sanity: theorems exist)"
  if !sortUndecided.isEmpty then fail m!"S2-CENSUS-SORT-UNDECIDED-NONZERO (sort-elaboration failures are not a classification)"
  let failed ← failedRef.get
  if failed then fail m!"S2-CENSUS-FAILED"
  else logInfo m!"S2-CENSUS-OK"
