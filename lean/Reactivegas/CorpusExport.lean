import Reactivegas.Trace
import KelGroups.Mirrors
import Reactivegas.VoteCorpus
import Reactivegas.ReplayContext
import Reactivegas.PolicyCheck

/-!
# Corpus exporter (`reactivegas#74`, slice S74)

The additive Lean end of the frozen corpus oracle: a `lean_exe` entry point
that calls the two existing corpus definitions and writes both wrapper files.

Dependency direction is one-way, mirroring the `Trace.lean` header rule: this
module imports `Reactivegas.Trace` (hence `Invariants`, hence the machine) and
`KelGroups.Mirrors` (to decide the integrated seed well formed),
and no existing module imports it back.

Call, do not restate: the wrappers reference `seedCorpus` and
`Reactivegas.emitIntegratedCorpus` directly. No corpus content, `seedView`,
`corpusInitial`, or `seedAuth` is restated here. Each economic `Trace`
already carries its own `initial` (`State.empty`), so the economic wrapper
does not repeat it.

Wrapper bound (NOTE-001): each file carries the `GroupView` plus the
authorization identity, and nothing else. The integrated file's `initial`
(`corpusInitial`) is the same shape of need as `GroupView`, not a third
thing. A third field needs a filed question, not an implementation decision.

Live binding (finding 1 property class + R86-C): `check` mode reads two wrapper
files back from bytes and compares their `traces`/`steps` element for
element against the live `seedCorpus`/`emitIntegratedCorpus` evaluated
through this call site, requiring a nonzero extent derived from the data.
Counts alone are not sufficient: a same-size swap must fail here.
The wrapper context is bound the same way: economic `view` against live
`seedView`, integrated `initial` against live `corpusInitial`, and each
`auth` string against its identity def, with nonzero member extents.
Bounded claim: this live-call/derived-ToJson method does not establish
serializer-instance independence (an independent encoder could differ;
none is required here).
-/

/-- JSON projection of `KelGroups.GroupView`. This is the only `ToJson`
instance for `GroupView` in the tree: it lives here, never in an existing
module. Total over `members`; each entry keeps the association pair shape
`(key, member)` of the canonical relation. -/
instance : Lean.ToJson KelGroups.GroupView where
  toJson v :=
    Lean.Json.mkObj
      [ ("members",
         Lean.Json.arr
           (v.members.map (fun entry =>
             Lean.Json.mkObj
               [ ("key", Lean.Json.str entry.1)
               , ("member", Lean.toJson entry.2) ])).toArray) ]

/-- Authorization identity the economic corpus was evaluated under: the
refusing probe `seedAuth` (`fun _ _ => false`). Rendered as an explicit
identity string the replayer can match, never as an opaque closure. The
corpus contains no backdonate event, so backdonation is not evaluated here. -/
def econAuthIdentity : String :=
  "seedAuth:refusing-probe(fun _ _ => false);corpus-contains-no-backdonate-event"

/-- Authorization identity the integrated corpus was evaluated under: the
refusing probe `probeAuth` (`fun _ _ => false`). Same rendering rule as
`econAuthIdentity`. -/
def intAuthIdentity : String :=
  "probeAuth:refusing-probe(fun _ _ => false);corpus-contains-no-backdonate-event"

/-- F74-ECONWRAP: the D74-ECONWRAP wrapper value, from (`seedView`,
`seedAuth`-identity, `seedCorpus`). Takes no event-list argument: a list
argument would be a second corpus. Adds no field beyond view/auth/traces. -/
def econWrapperJson : Lean.Json :=
  Lean.Json.mkObj
    [ ("view", Lean.toJson seedView)
    , ("auth", Lean.Json.str econAuthIdentity)
    , ("traces", Lean.toJson seedCorpus) ]

/-- F74-INTWRAP: the D74-INTWRAP wrapper value, from (`corpusInitial`,
auth-identity, `emitIntegratedCorpus`). Takes no step-list argument. Adds
no field beyond initial/auth/steps. -/
def intWrapperJson : Lean.Json :=
  Lean.Json.mkObj
    [ ("initial", Lean.toJson Reactivegas.corpusInitial)
    , ("auth", Lean.Json.str intAuthIdentity)
    , ("steps", Lean.toJson Reactivegas.emitIntegratedCorpus) ]

/- The frozen integrated corpus starts from a state the current base-channel
regime can reach: well formed at its admin count (no proposer among the
approvals above one admin) and free of proposer credit at any count, in both
pending stores. The economic traces start from `State.empty`, which carries no
group state. Evaluated on the very value `intWrapperJson` exports. -/
#guard KelGroups.wellFormedB Reactivegas.corpusInitial
  && KelGroups.strongCoherentB Reactivegas.corpusInitial
  && KelGroups.strongBaseCoherentB Reactivegas.corpusInitial

/-- Repair 1 core: bind one file array to its live value, element for
element. The extent is derived from the data: zero elements fail, and
every element must equal the live one, so a same-size swap fails too.
Both sides share the derived `ToJson` instance (call-site independence,
not instance independence) — see the receipt residual. -/
def checkLiveArray (name : String) (live file : Lean.Json) : Except String Nat := do
  let liveArr ← live.getArr?.mapError (fun e => s!"{name}: live value is not an array: {e}")
  let fileArr ← file.getArr?.mapError (fun e => s!"{name}: file value is not an array: {e}")
  if fileArr.size == 0 then throw s!"{name}: zero extent (no elements to bind)"
  if liveArr.size != fileArr.size then
    throw s!"{name}: extent differs (live {liveArr.size}, file {fileArr.size})"
  for i in [:fileArr.size] do
    if liveArr[i]! != fileArr[i]! then throw s!"{name}: element {i} differs from live value"
  return fileArr.size

/-- Bind the economic wrapper context: `view` equals live `seedView`
(value equality through the same call-site `ToJson`), with a nonzero member
extent read from the bytes; `auth` equals the refusing-probe identity.
Kills: `.view.members[0].key` → `"ZZZ"`, permissive-`auth` swaps. -/
def checkEconContext (j : Lean.Json) : Except String Nat := do
  let view ← (j.getObjVal? "view").mapError (fun e => s!"economic: no view key: {e}")
  if view != Lean.toJson seedView then throw "economic: view differs from live seedView"
  let members ← (view.getObjVal? "members").mapError (fun e => s!"economic: view without members: {e}")
  let marr ← members.getArr?.mapError (fun e => s!"economic: view members not an array: {e}")
  if marr.size == 0 then throw "economic: zero view members"
  let auth ← (j.getObjVal? "auth").mapError (fun e => s!"economic: no auth key: {e}")
  if auth != Lean.Json.str econAuthIdentity then
    throw "economic: auth differs from live econAuthIdentity"
  return marr.size

/-- Bind the integrated wrapper context: `initial` equals live
`corpusInitial`, with a nonzero member extent from the bytes; `auth`
equals the refusing-probe identity. Kills: `.initial.members` → `[]`,
permissive-`auth` swaps. -/
def checkIntContext (j : Lean.Json) : Except String Nat := do
  let initial ← (j.getObjVal? "initial").mapError (fun e => s!"integrated: no initial key: {e}")
  if initial != Lean.toJson Reactivegas.corpusInitial then
    throw "integrated: initial differs from live corpusInitial"
  let members ← (initial.getObjVal? "members").mapError (fun e => s!"integrated: initial without members: {e}")
  let marr ← members.getArr?.mapError (fun e => s!"integrated: initial members not an array: {e}")
  if marr.size == 0 then throw "integrated: zero initial members"
  let auth ← (j.getObjVal? "auth").mapError (fun e => s!"integrated: no auth key: {e}")
  if auth != Lean.Json.str intAuthIdentity then
    throw "integrated: auth differs from live intAuthIdentity"
  return marr.size

/-- Bind an economic wrapper file: context (`view`/`auth`) plus `traces`
element for element, with nonzero trace and event extents from the bytes. -/
def checkEconFile (j : Lean.Json) : Except String (Nat × Nat) := do
  let _ ← checkEconContext j
  let traces ← (j.getObjVal? "traces").mapError (fun e => s!"economic: no traces key: {e}")
  let ntraces ← checkLiveArray "economic.traces" (Lean.toJson seedCorpus) traces
  let arr ← traces.getArr?.mapError (fun e => s!"economic: traces not an array: {e}")
  let mut nevents := 0
  for t in arr do
    let steps ← (t.getObjVal? "steps").mapError (fun e => s!"economic: trace without steps: {e}")
    let sarr ← steps.getArr?.mapError (fun e => s!"economic: steps not an array: {e}")
    nevents := nevents + sarr.size
  if nevents == 0 then throw "economic: zero events"
  return (ntraces, nevents)

/-- Bind an integrated wrapper file: `steps` equals live
`emitIntegratedCorpus` element for element, with nonzero extent. -/
def checkIntFile (j : Lean.Json) : Except String Nat := do
  let _ ← checkIntContext j
  let steps ← (j.getObjVal? "steps").mapError (fun e => s!"integrated: no steps key: {e}")
  checkLiveArray "integrated.steps" (Lean.toJson Reactivegas.emitIntegratedCorpus) steps

/-! ## The vote corpus (`reactivegas#75`)

The third wrapper. Same discipline as the integrated one — `initial`, `auth`,
`steps` and nothing else — with the replay threshold deliberately absent: it
lives in the sidecar. Generation binds `legacyThreshold` as test input; it is
not a shipped default. -/

/-- Authorization identity the vote corpus was evaluated under: the same
refusing probe as the integrated corpus. -/
def voteAuthIdentity : String := intAuthIdentity

def voteWrapperJson : Lean.Json :=
  Lean.Json.mkObj
    [ ("initial", Lean.toJson Reactivegas.voteCorpusInitial)
    , ("auth", Lean.Json.str voteAuthIdentity)
    , ("steps",
        Lean.toJson (Reactivegas.emitVoteCorpus KelGroups.Vote.legacyThreshold)) ]

/-- The exact bytes of `lean/corpus/vote.json`. -/
def voteFileBytes : String := voteWrapperJson.compress ++ "\n"

/-- One tagged line per check; a check that raised prints its reason. -/
def voteTag (name : String) (r : Except String String) : IO Bool :=
  match r with
  | .ok detail => do IO.println s!"VOTE-CHECK {name} PASS {detail}"; return true
  | .error e => do IO.println s!"VOTE-CHECK {name} FAIL {e}"; return false

def boolCheck (what : String) (b : Bool) (detail : String) : Except String String :=
  if b then .ok detail else .error what

/-- Wrapper context of the vote file, bound to the live definitions. -/
def checkVoteContext (bytes : String) : Except String String := do
  let j ← Lean.Json.parse bytes
  let initial ← j.getObjVal? "initial"
  if initial != Lean.toJson Reactivegas.voteCorpusInitial then
    throw "initial differs from live voteCorpusInitial"
  let members ← initial.getObjVal? "members"
  if (← members.getArr?).size == 0 then throw "zero initial members"
  let auth ← j.getObjVal? "auth"
  if auth != Lean.Json.str voteAuthIdentity then
    throw "auth differs from live voteAuthIdentity"
  return s!"initial members and auth identity equal the live values"

/-- Steps equal the live emission element for element, nonzero extent. -/
def checkVoteLive (bytes : String) : Except String Nat := do
  let j ← Lean.Json.parse bytes
  let steps ← j.getObjVal? "steps"
  checkLiveArray "vote.steps"
    (Lean.toJson (Reactivegas.emitVoteCorpus KelGroups.Vote.legacyThreshold)) steps

def extentSummary (steps : List Reactivegas.VoteTraceStep) : String :=
  ", ".intercalate (Reactivegas.discoveredExtents.map (fun (ty, names) =>
    let seen := Reactivegas.seenNames steps ty
    s!"{ty} {(names.filter seen.contains).length}/{names.length}"))

/-- `vote check <vote.json>`: every live check of the vote corpus, one tagged
line each. Exit 0 only when all pass. -/
def voteCheck (path : String) : IO UInt32 := do
  let bytes ← IO.FS.readFile path
  match Reactivegas.decodeVoteFile bytes with
  | .error e => do
    IO.println s!"VOTE-CHECK decode FAIL {e}"
    return 1
  | .ok (initial, steps) => do
    let θ := KelGroups.Vote.legacyThreshold
    let okRef ← IO.mkRef true
    let record := fun (b : Bool) => okRef.modify (· && b)
    record (← voteTag "live-binding"
      ((checkVoteLive bytes).map (fun n => s!"{n} steps equal the live emission")))
    record (← voteTag "wrapper-context" (checkVoteContext bytes))
    record (← voteTag "nonzero-extent"
      (boolCheck "zero steps or zero events" (steps.length > 0 && initial.members.length > 0)
        s!"{steps.length} steps, {initial.members.length} initial members"))
    record (← voteTag "V1-vote-route-observable"
      (boolCheck "no accepted vote-routed .app step" (Reactivegas.voteRouteObservable steps)
        s!"{(steps.filter (fun st => st.accepted && (match Reactivegas.appOf st.event with
            | some e => Reactivegas.isVoteRouted e | none => false))).length} accepted vote-routed .app steps"))
    record (← voteTag "V2-closure-with-economics"
      (boolCheck "no closure with an economic effect in the file"
        (Reactivegas.closureWithEconomics steps initial)
        s!"minted-and-spent={Reactivegas.mintedAndSpent (Reactivegas.withPre initial steps)} refund-on-negative={Reactivegas.refundOnNegative (Reactivegas.withPre initial steps)}"))
    record (← voteTag "V3-franchise-closure-while-active"
      (boolCheck "no franchiseChange closure at an economically active payload"
        (Reactivegas.franchiseClosureWhileActive steps initial) "franchiseChange closure with active payload present"))
    record (← voteTag "journey-lifted"
      (let items := Reactivegas.journeyItems steps initial
       let missing := items.filter (fun it => !it.2) |>.map (·.1)
       if missing.isEmpty then .ok s!"{items.length}/{items.length} journey items present"
       else .error s!"journey items missing: {missing}"))
    record (← voteTag "lifecycle-rows"
      (boolCheck "renounced / proposerDeparted closure records missing"
        (Reactivegas.lifecycleRows steps initial) "renounced and proposerDeparted closures, both negative, present in the stored state"))
    record (← voteTag "vote-error-identity-observed"
      (boolCheck "a stored voteError differs from the vote machine's refusal on the stored predecessor"
        (Reactivegas.voteErrorIdentityObserved θ steps initial)
        s!"{(steps.filter (fun st => st.voteError.isSome)).length} refused steps carry their VoteError, {(steps.filter (fun st => !st.accepted && st.voteError.isNone)).length} refusals never reach the vote machine"))
    record (← voteTag "coverage-discovered"
      ((Reactivegas.voteCorpusCoversDiscovered steps).map (fun _ => extentSummary steps)))
    return (if (← okRef.get) then 0 else 1)

/-- `vote coverage-control <vote.json>`: the coverage check rejects an extent that
gained a constructor the corpus does not show, for every discovered type, and
still accepts the unextended extents. -/
def voteCoverageControl (path : String) : IO UInt32 := do
  match Reactivegas.decodeVoteFile (← IO.FS.readFile path) with
  | .error e => do
    IO.println s!"VOTE-CHECK coverage-added-constructor-control FAIL decode: {e}"
    return 1
  | .ok (_, steps) => do
    let baseline := Reactivegas.voteCorpusCoversDiscovered steps
    let mut rejected := 0
    let mut failures : List String := []
    for (ty, _) in Reactivegas.discoveredExtents do
      let fresh := s!"AddedConstructor{ty}"
      let extended := Reactivegas.discoveredExtents.map (fun (t, ns) =>
        if t == ty then (t, ns ++ [fresh]) else (t, ns))
      match Reactivegas.coverageAgainst extended steps with
      | .error msg =>
        if (msg.splitOn fresh).length > 1 then rejected := rejected + 1
        else failures := s!"{ty}: rejected without naming {fresh}" :: failures
      | .ok _ => failures := s!"{ty}: accepted an uncovered constructor" :: failures
    let rejects := fun (extents : List (String × List String)) =>
      match Reactivegas.coverageAgainst extents steps with
      | .error _ => true
      | .ok _ => false
    let emptyRefused := rejects [] && rejects [("VoteError", [])]
    let n := Reactivegas.discoveredExtents.length
    match baseline with
    | .ok _ =>
      if rejected == n && failures.isEmpty && emptyRefused then
        IO.println s!"VOTE-CHECK coverage-added-constructor-control PASS {rejected}/{n} extended extents rejected naming the added constructor; the discovered extents accepted; an empty extent list and an empty extent refused"
        return 0
      else
        IO.println s!"VOTE-CHECK coverage-added-constructor-control FAIL {failures} emptyRefused={emptyRefused}"
        return 1
    | .error e =>
      IO.println s!"VOTE-CHECK coverage-added-constructor-control FAIL the discovered extents are not covered: {e}"
      return 1

/-- The replayer's three failure outcomes and its success are distinct in both the
exit code and the tagged first word of the report. -/
def replayExit : Reactivegas.ReplayOutcome → IO UInt32
  | .ok steps domain => do
      IO.println s!"REPLAY OK steps={steps} domain=0..{domain}"
      return 0
  | .refusal rules detail => do
      IO.println s!"PRE-REPLAY REFUSAL rule={",".intercalate rules} {detail}"
      return 10
  | .abort n domain step => do
      IO.println s!"RUNTIME ABORT threshold queried at n={n}, outside the table's domain 0..{domain}, at step {step}; the {step} steps before it are a diagnostic prefix only and no full-conformance claim is made"
      return 11
  | .mismatch step field => do
      IO.println s!"CONFORMANCE MISMATCH step={step} field={field}"
      return 12

def voteUsage : IO UInt32 := do
  IO.eprintln "usage: corpusExport vote emit <vote.json> | emit-context <vote.json> <sidecar> <root> | check <vote.json> | coverage-control <vote.json> | witness <vote.json> | replay <vote.json> <sidecar> | policy <sidecar> | digest <root>"
  return 1

def voteMain : List String → IO UInt32
  | ["emit", votePath] => do
    IO.FS.writeFile votePath voteFileBytes
    return 0
  | ["emit-context", votePath, ctxPath, root] => do
    IO.FS.writeFile ctxPath (← Reactivegas.contextBytes root (← IO.FS.readFile votePath))
    return 0
  | ["check", votePath] => voteCheck votePath
  | ["coverage-control", votePath] => voteCoverageControl votePath
  | ["witness", votePath] => do
    match ← Reactivegas.policyWitness (← IO.FS.readFile votePath) with
    | .ok (step, n, legacy, zero, late) => do
      IO.println s!"POLICY-WITNESS step={step} n={n} legacy={legacy} zero={zero}"
      match late with
      | some (lstep, ln) =>
        IO.println s!"POLICY-LATE-QUERY step={lstep} n={ln} larger than every size queried at the {lstep} earlier steps"
      | none => IO.println "POLICY-LATE-QUERY-ABSENT no query size first reached after step 0"
      return 0
    | .error e => do
      IO.println s!"POLICY-WITNESS-INADEQUATE {e}"
      return 1
  | ["replay", votePath, ctxPath] => do
    replayExit (← Reactivegas.replayVoteCorpus (← IO.FS.readFile votePath) (← IO.FS.readFile ctxPath))
  | ["policy", ctxPath] => do
    match Reactivegas.PolicyCheck.checkPolicyAgainstDeclaration (← IO.FS.readFile ctxPath) with
    | .ok points => do
      IO.println s!"POLICY-REDERIVED declaration=KelGroups.Vote.legacyThreshold domain=0..{points - 1} points={points}"
      return 0
    | .error e => do
      IO.println (if e.startsWith "POLICY-" then e else s!"POLICY-REFUSED {e}")
      return 1
  | ["digest", root] => do
    IO.println (← Reactivegas.sourceInputDigest root)
    return 0
  | _ => voteUsage

/-- F74-MAIN: `lean_exe` entry point. The sole writer of the frozen files:
writes the economic wrapper to the first path argument and the integrated
wrapper to the second, as exact bytes the verify target compares. Exits
non-zero on wrong arity or any write failure. -/
def main (args : List String) : IO UInt32 := do
  match args with
  | "vote" :: rest => voteMain rest
  | ["check", econPath, intPath] => do
    let econBytes ← IO.FS.readFile econPath
    let intBytes ← IO.FS.readFile intPath
    match Lean.Json.parse econBytes, Lean.Json.parse intBytes with
    | .ok ej, .ok ij =>
      match checkEconFile ej, checkIntFile ij with
      | .ok (nt, nev), .ok ns => do
        IO.println s!"corpus-check: ntraces={nt} nevents={nev} nsteps={ns} live-bound"
        return 0
      | .error e, _ => do
        IO.eprintln s!"corpus-check FAIL economic: {e}"
        return 1
      | _, .error e => do
        IO.eprintln s!"corpus-check FAIL integrated: {e}"
        return 1
    | .error e, _ => do
      IO.eprintln s!"corpus-check FAIL economic parse: {e}"
      return 1
    | _, .error e => do
      IO.eprintln s!"corpus-check FAIL integrated parse: {e}"
      return 1
  | [econPath, intPath] => do
    if econPath == "check" then do
      IO.eprintln "usage: corpusExport <economic.json> <integrated.json> | corpusExport check <economic.json> <integrated.json>"
      return 1
    IO.FS.writeFile econPath (econWrapperJson.compress ++ "\n")
    IO.FS.writeFile intPath (intWrapperJson.compress ++ "\n")
    return 0
  | _ => do
    IO.eprintln "usage: corpusExport <economic.json> <integrated.json> | corpusExport check <economic.json> <integrated.json>"
    return 1
