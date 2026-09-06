import Reactivegas.Trace

/-!
# Corpus exporter (`reactivegas#74`, slice S74)

The additive Lean end of the frozen corpus oracle: a `lean_exe` entry point
that calls the two existing corpus definitions and writes both wrapper files.

Dependency direction is one-way, mirroring the `Trace.lean` header rule: this
module imports `Reactivegas.Trace` (hence `Invariants`, hence the machine),
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

/-- F74-MAIN: `lean_exe` entry point. The sole writer of the frozen files:
writes the economic wrapper to the first path argument and the integrated
wrapper to the second, as exact bytes the verify target compares. Exits
non-zero on wrong arity or any write failure. -/
def main (args : List String) : IO UInt32 := do
  match args with
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
