import Lean
import Reactivegas.VoteCorpus
import Reactivegas.Digest

/-!
# Replay context for the vote corpus (`reactivegas#75`, contract R3.1)

A sidecar beside the corpus names the threshold policy the corpus was generated
under as a *finite table*, so a replayer needs no Lean and no declaration.

* **Authority.** `policies.threshold.table` is the sole authority for replay.
  `label`, `source` and `generatedFrom` are provenance with no operative force:
  nothing here resolves them.
* **Two moments, three outcomes.** A *pre-replay refusal* is found before any
  step runs (schema, version, corpus digest, table rules, auth semantics). A
  *runtime abort* is a threshold query outside the table's domain, found only
  when the query is made; it makes no full-conformance claim. A *conformance
  mismatch* is a step that disagrees under a valid, sufficient context.
* **Point of query.** The table lookup records the first out-of-domain query
  at the moment it is made. Nothing predicts which sizes will be queried; the
  replay stops at the end of the step that made the query and reports it.
* **Source inputs.** `sourceInputDigest` binds the sidecar to the bytes of the
  files generation actually reads: pins and every `.lean` file in the exporter's
  import closure, never `lean/corpus/**`.

The policy values a sidecar carries are checked against the live declaration by
`Reactivegas.PolicyCheck`, a module this one does not import.
-/

namespace Reactivegas

open Lean (Json)

/-! ## Pre-replay validation -/

/-- A pre-replay refusal: the rule names broken, and a detail. A plain pair, so
it is not a refusal vocabulary of the mutant ledger's census. -/
abbrev ContextRefusal := List String × String

def refuse {α : Type} (rules : List String) (detail : String) : Except ContextRefusal α :=
  .error (rules, detail)

/-- A table number is exact when it is a non-negative integer written with no
fraction. -/
def natExact? : Json → Option Nat
  | .num n => if n.exponent == 0 && n.mantissa ≥ 0 then some n.mantissa.toNat else none
  | _ => none

/-- The raw text of the sidecar's `"table":` value (outer brackets included). -/
def tableText (bytes : String) : Option String :=
  match bytes.splitOn "\"table\":" with
  | _ :: rest :: _ =>
      let chars := rest.trimLeft.toList
      let rec go (cs : List Char) (depth : Nat) (acc : List Char) : List Char :=
        match cs with
        | [] => acc.reverse
        | c :: tl =>
            let depth' := if c == '[' then depth + 1 else if c == ']' then depth - 1 else depth
            if c == ']' && depth' == 0 then (c :: acc).reverse else go tl depth' (c :: acc)
      some (String.mk (go chars 0 []))
  | _ => none

/-- Canonical table validation (R3.1 §3): every key and value a non-negative
integer written exactly (no floats, even `2.0`), keys strictly increasing, keys
contiguous from `0`, no duplicate key, non-empty. Every violated rule is named.
No monotonicity is assumed. -/
def validateTable (raw : Option String) (table : Json) : Except ContextRefusal (Array Nat) := do
  let entries ← match table.getArr? with
    | .ok a => pure a
    | .error _ => refuse ["non-integer"] "table is not an array"
  let textExact := (raw.map (·.all (fun c => c.isDigit || c == ',' || c == '[' || c == ']' || c.isWhitespace))).getD false
  let parsed := entries.toList.map (fun e =>
    match e.getArr? with
    | .ok pair =>
        if pair.size == 2 then (do let k ← natExact? pair[0]!; let v ← natExact? pair[1]!; some (k, v))
        else none
    | .error _ => none)
  if !textExact || parsed.any Option.isNone then
    refuse ["non-integer"] "a table entry is not a pair of exactly written non-negative integers"
  let kvs := parsed.filterMap id
  let keys := kvs.map (·.1)
  let dup := keys.any (fun k => (keys.filter (· == k)).length > 1)
  let increasing := (keys.zip keys.tail).all (fun (a, b) => a < b)
  let rules :=
    (if kvs.isEmpty then ["empty"] else [])
      ++ (if dup then ["duplicate-key"] else [])
      ++ (if increasing then [] else ["not-strictly-increasing"])
      ++ (if keys == List.range keys.length then [] else ["not-contiguous-from-zero"])
  unless rules.isEmpty do refuse rules s!"table keys {keys}"
  return (kvs.map (·.2)).toArray

/-- `e` succeeded with exactly `a`. -/
def okEq {α : Type} [BEq α] (e : Except String α) (a : α) : Bool :=
  match e with
  | .ok x => x == a
  | .error _ => false

/-- The validated context: the threshold table, indexed by size. -/
def parseContext (corpusBytes sidecarBytes : String) : Except ContextRefusal (Array Nat) := do
  let j ← (Json.parse sidecarBytes).mapError (fun e => (["malformed"], e))
  unless okEq (j.getObjValAs? String "schema") "reactivegas.replay-context" do
    refuse ["schema"] "unrecognised schema"
  unless okEq (j.getObjValAs? Nat "version") 1 do refuse ["version"] "unrecognised version"
  let want := (do let c ← j.getObjVal? "corpus"; c.getObjValAs? String "contentSha256")
  unless okEq want (Digest.sha256 corpusBytes.toUTF8) do
    refuse ["digest"] "corpus.contentSha256 is not the SHA-256 of the corpus bytes"
  let policies ← (j.getObjVal? "policies").mapError (fun e => (["malformed"], e))
  let semantics := (do let b ← policies.getObjVal? "backdonateAuth"; b.getObjValAs? String "semantics")
  unless okEq semantics "always-refuse" do
    refuse ["semantics"] "an authorization semantics token this replayer does not implement"
  let table ← (do let t ← policies.getObjVal? "threshold"; t.getObjVal? "table").mapError
    (fun e => (["malformed"], e))
  validateTable (tableText sidecarBytes) table

/-! ## Threshold lookups with an effect at the point of query

Both are `unsafe` implementations behind an opaque pure signature: the vote
machine takes a pure `Threshold`, and the lookup has to leave a trace when it is
applied. The replay evaluates each step inside an action (`IO.lazyPure`), so the
trace is read after, never before, the step's queries. -/

unsafe def tableThresholdUnsafe (table : Array Nat) (firstOut : IO.Ref (Option Nat)) :
    Nat → Nat :=
  fun n => unsafeBaseIO (do
    match table[n]? with
    | some v => pure v
    | none => do
        if (← firstOut.get).isNone then firstOut.set (some n)
        pure 0)

/-- The table as a threshold. Applied outside its domain it answers `0` and
records the first such size in `firstOut`; the replayer treats a recorded size
as an abort and never uses what the step computed after it. -/
@[implemented_by tableThresholdUnsafe]
opaque tableThreshold (table : Array Nat) (firstOut : IO.Ref (Option Nat)) : Nat → Nat

unsafe def recordingThresholdUnsafe (base : Nat → Nat) (log : IO.Ref (Array Nat)) :
    Nat → Nat :=
  fun n => unsafeBaseIO (do log.modify (·.push n); pure (base n))

/-- A threshold that logs every size it is applied to. -/
@[implemented_by recordingThresholdUnsafe]
opaque recordingThreshold (base : Nat → Nat) (log : IO.Ref (Array Nat)) : Nat → Nat

/-! ## Replay -/

inductive ReplayOutcome where
  | ok (steps domain : Nat)
  | refusal (rules : List String) (detail : String)
  /-- `n` queried outside `0..domain` at step `step`; `step` steps preceded it. -/
  | abort (n domain step : Nat)
  | mismatch (step : Nat) (field : String)

def firstDiff (got stored : VoteTraceStep) : String :=
  if got.accepted != stored.accepted then "accepted"
  else if got.voteError != stored.voteError then "voteError"
  else if got.change != stored.change then "change"
  else if got.state != stored.state then "state"
  else "event"

/-- Replay the corpus under the context's table alone. The context is validated
first; each step is then recomputed with the table-backed threshold, and the
first out-of-domain query ends the replay as an abort before any comparison of
that step. -/
def replayVoteCorpus (corpusBytes sidecarBytes : String) : IO ReplayOutcome := do
  match parseContext corpusBytes sidecarBytes with
  | .error r => return .refusal r.1 r.2
  | .ok table =>
    match decodeVoteFile corpusBytes with
    | .error e => return .refusal ["corpus"] e
    | .ok (initial, steps) =>
      let domain := table.size - 1
      IO.println s!"CONTEXT VALID domain=0..{domain} steps={steps.length}"
      let mut gs := initial
      let mut i := 0
      for stored in steps do
        let firstOut ← IO.mkRef (none : Option Nat)
        let θ := tableThreshold table firstOut
        let got ← IO.lazyPure (fun _ => snapshotVoteStep θ gs stored.signer stored.event)
        if let some n ← firstOut.get then return .abort n domain i
        if got != stored then return .mismatch i (firstDiff got stored)
        gs := got.state
        i := i + 1
      return .ok steps.length domain

/-! ## Policy witness -/

def stepSummary (st : VoteTraceStep) : String :=
  s!"accepted={st.accepted},closed={st.state.appFold.votes.closed.length},open={st.state.appFold.votes.openQuestions.length}"

/-- The first step, replayed from its stored predecessor, whose outcome differs
between `legacyThreshold` and `zeroThreshold`, together with a size the
threshold was queried at during it (`n >= 1`). Only collective questions query
the threshold (`verdictOf`'s permission arm never does), so any recorded size is
a collective-question query. The last component is the first query (step, size)
that is larger than every size queried at any earlier step, at a step after the
first: a table truncated just below it is valid for every earlier step and
first runs short there, which a check that only looked at the initial franchise
would not. `none` for the witness means the corpus cannot tell the two policies
apart. -/
def policyWitness (corpusBytes : String) :
    IO (Except String (Nat × Nat × String × String × Option (Nat × Nat))) := do
  match decodeVoteFile corpusBytes with
  | .error e => return .error s!"corpus does not decode: {e}"
  | .ok (initial, steps) =>
    let mut pre := initial
    let mut i := 0
    let mut witness : Option (Nat × Nat × String × String) := none
    let mut late : Option (Nat × Nat) := none
    let mut seenMax := 0
    for stored in steps do
      let log ← IO.mkRef (#[] : Array Nat)
      let legacy ← IO.lazyPure (fun _ =>
        snapshotVoteStep (recordingThreshold KelGroups.Vote.legacyThreshold log) pre stored.signer stored.event)
      let zero ← IO.lazyPure (fun _ =>
        snapshotVoteStep KelGroups.Vote.zeroThreshold pre stored.signer stored.event)
      let queried ← log.get
      if witness.isNone && legacy != zero then
        if let some n := queried.find? (· ≥ 1) then
          witness := some (i, n, stepSummary legacy, stepSummary zero)
      if late.isNone && i > 0 then
        if let some n := queried.find? (fun q => q ≥ 1 && q > seenMax) then
          late := some (i, n)
      seenMax := queried.foldl max seenMax
      pre := stored.state
      i := i + 1
    match witness with
    | some (step, n, legacy, zero) => return .ok (step, n, legacy, zero, late)
    | none => return .error "no collective-question threshold query at n >= 1 at which legacyThreshold and zeroThreshold give different step outcomes: the corpus is inadequate as a policy witness"

/-! ## Generation: the sidecar's domain, table and digest -/

/-- The sizes the generating run applied the threshold to. -/
def generationQueries : IO (Array Nat) := do
  let log ← IO.mkRef (#[] : Array Nat)
  let _ ← IO.lazyPure (fun _ =>
    (emitVoteCorpus (recordingThreshold KelGroups.Vote.legacyThreshold log)).length)
  log.get

/-- Import lines of a Lean source: `import <Module>` at the start of a line. -/
def importsOf (text : String) : List String :=
  (text.splitOn "\n").filterMap (fun line =>
    if line.startsWith "import" then
      let rest := line.drop 6
      if rest.isEmpty || !rest.front.isWhitespace then none
      else
        let name := rest.trimLeft.takeWhile (fun c => c.isAlphanum || c == '_' || c == '.')
        if name.isEmpty then none else some name
    else none)

/-- The source-input projection: the pins and the `.lean` file of every module
in the transitive import closure of the exporter (the exporter included);
`Init`, `Lean` and `Std` are toolchain-pinned and excluded. Repository-relative,
sorted bytewise. -/
def sourceProjection (root : String) : IO (List String) := do
  let mut todo := ["Reactivegas.CorpusExport"]
  let mut seen : List String := []
  let mut files : List String := []
  while !todo.isEmpty do
    let m := todo.head!
    todo := todo.tail
    if seen.contains m then continue
    seen := m :: seen
    let top := (m.splitOn ".").head!
    if top == "Init" || top == "Lean" || top == "Std" then continue
    let f := "lean/" ++ m.replace "." "/" ++ ".lean"
    unless ← System.FilePath.pathExists (root ++ "/" ++ f) do
      throw (IO.userError s!"projection: module {m} has no file {f}")
    files := f :: files
    todo := todo ++ importsOf (← IO.FS.readFile (root ++ "/" ++ f))
  let all := files ++ ["lean/lakefile.lean", "lean/lean-toolchain", "lean/lake-manifest.json"]
  return (all.eraseDups.toArray.qsort (· < ·)).toList

/-- SHA-256 of the records `<path> 0x00 <hex SHA-256 of the file> 0x0A`, one per
projected path in order, read from the working tree under `root`. -/
def sourceInputDigest (root : String) : IO String := do
  let mut buf := ByteArray.empty
  for p in ← sourceProjection root do
    let bytes ← IO.FS.readBinFile (root ++ "/" ++ p)
    buf := buf ++ p.toUTF8 ++ ByteArray.mk #[0] ++ (Digest.sha256 bytes).toUTF8 ++ ByteArray.mk #[10]
  return Digest.sha256 buf

/-- Provenance only: where a declaration lives and the Git blob id of that file. -/
def sourceRecord (root declaration path : String) : IO Json := do
  let bytes ← IO.FS.readBinFile (root ++ "/" ++ path)
  return Json.mkObj
    [ ("declaration", Json.str declaration), ("path", Json.str path)
    , ("gitBlobOid", Json.str (Digest.gitBlobOid bytes)) ]

/-- The exact bytes of `lean/corpus/vote.context.json` for a corpus: the table
`n ↦ legacyThreshold n` over `0..N`, `N` the largest size generation applied the
threshold to. -/
def contextBytes (root corpusBytes : String) : IO String := do
  let queries ← generationQueries
  let domain := queries.foldl max 0
  let table := (List.range (domain + 1)).map (fun n =>
    Json.arr #[Lean.toJson n, Lean.toJson (KelGroups.Vote.legacyThreshold n)])
  let thresholdSource ← sourceRecord root "KelGroups.Vote.legacyThreshold" "lean/KelGroups/Vote/Types.lean"
  let authSource ← sourceRecord root "Reactivegas.probeAuth" "lean/Reactivegas/Step.lean"
  let digest ← sourceInputDigest root
  let j := Json.mkObj
    [ ("schema", Json.str "reactivegas.replay-context")
    , ("version", Lean.toJson (1 : Nat))
    , ("corpus", Json.mkObj
        [ ("path", Json.str "corpus/vote.json")
        , ("contentSha256", Json.str (Digest.sha256 corpusBytes.toUTF8)) ])
    , ("policies", Json.mkObj
        [ ("threshold", Json.mkObj
            [ ("table", Json.arr table.toArray)
            , ("label", Json.str "legacyThreshold")
            , ("source", thresholdSource) ])
        , ("backdonateAuth", Json.mkObj
            [ ("semantics", Json.str "always-refuse")
            , ("label", Json.str "probeAuth")
            , ("source", authSource) ]) ])
    , ("generatedFrom", Json.mkObj
        [ ("sourceInputDigest", Json.str digest)
        , ("exporter", Json.str "Reactivegas.CorpusExport") ]) ]
  return j.compress ++ "\n"

end Reactivegas
