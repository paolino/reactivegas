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

/-- F74-MAIN: `lean_exe` entry point. The sole writer of the frozen files:
writes the economic wrapper to the first path argument and the integrated
wrapper to the second, as exact bytes the verify target compares. Exits
non-zero on wrong arity or any write failure. -/
def main (args : List String) : IO UInt32 := do
  match args with
  | [econPath, intPath] => do
    IO.FS.writeFile econPath (econWrapperJson.compress ++ "\n")
    IO.FS.writeFile intPath (intWrapperJson.compress ++ "\n")
    return 0
  | _ => do
    IO.eprintln "usage: corpusExport <economic.json> <integrated.json>"
    return 1
