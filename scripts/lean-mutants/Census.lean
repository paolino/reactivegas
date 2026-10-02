/-
Lean mutant ledger — compiled census (M-1).

The runner (`scripts/lean-mutants/run`) prepends `import Lean` and one
`import` per tracked Lean module, then runs this file with `lake env lean`.
Every fact the runner judges with comes from here, read off the elaborated
environment: the module extent and import graph, the theorem extent and its
exclusion rule, declaration spans, statement closures, the production rule,
and the refusal vocabularies with their emitting definitions. Nothing here
reads Lean source text.

Environment:
  CENSUS_MODULES  newline-separated tracked module names (the discovered extent)
  CENSUS_ROOT     the Lean package directory the imports were built in
  CENSUS_OUT      path of the JSON census to write
-/

open Lean Meta

namespace MutantCensus

/-- The single production rule, stated once (data model: a non-theorem
constant defined in a library module that is not a test, fixture, corpus,
oracle or checker module). A module's role is read from the last component of
its name. -/
def moduleRole (m : Name) : String :=
  let last := match m with
    | .str _ s => s
    | _ => ""
  if last.endsWith "Tests" then "test"
  else if last.startsWith "Corpus" then "corpus"
  else if last == "Mirrors" then "checker"
  else if last == "Invariants" || last == "Lifecycle" then "fixture"
  else if last == "Predicates" then "oracle"
  else "production"

def productionRuleText : String :=
  "a module is production unless the last component of its name ends with " ++
  "`Tests` (test), starts with `Corpus` (corpus), is `Mirrors` (checker), " ++
  "is `Invariants` or `Lifecycle` (proof helpers, fixtures, witnesses and " ++
  "mutation-only inversions) or is `Predicates` (law " ++
  "oracle); a production definition is a non-theorem constant with a source " ++
  "declaration range in a production module"

def exclusionRuleText : String :=
  "a theorem constant is EXCLUDED exactly when the elaborator, not a " ++
  "`theorem` command, produced it: the environment records no source " ++
  "declaration range of its own for it (equation, injectivity, sizeOf, " ++
  "auxiliary-proof and simp lemmas), or it is a structure projection " ++
  "(a `Prop` field of a structure); every other theorem constant is AUTHORED"

/-- Refusal vocabularies named by the mandate (R-9). Further ones are
discovered: see `discoveredVocabularies`. -/
def namedVocabularies : List Name :=
  [`GuardId, `KelGroups.Vote.VoteError, `KelGroups.ValidationError]

def moduleFile (m : Name) : String :=
  (m.components.map (·.toString)).foldl
    (fun acc s => if acc.isEmpty then s else acc ++ "/" ++ s) "" ++ ".lean"

/-- Stable identity: the declared name, with `private` mangling resolved to
the user name plus its home module. -/
def identity (env : Environment) (n : Name) : String :=
  match privateToUserName? n with
  | some u =>
    let home := match env.getModuleIdxFor? n with
      | some i => (env.header.moduleNames[i.toNat]?).getD .anonymous
      | none => .anonymous
    s!"{u}@{home}"
  | none => n.toString

def homeModule (env : Environment) (n : Name) : Option Name := do
  let i ← env.getModuleIdxFor? n
  env.header.moduleNames[i.toNat]?

def kindOf : ConstantInfo → String
  | .defnInfo _ => "def"
  | .thmInfo _ => "theorem"
  | .opaqueInfo _ => "opaque"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .recInfo _ => "recursor"
  | .axiomInfo _ => "axiom"
  | .quotInfo _ => "quot"

/-- Constants of a definition body that statement closure walks through.
Theorem proofs are never walked (ownership is by statement only). -/
def bodyOf : ConstantInfo → Option Expr
  | .defnInfo v => some v.value
  | .opaqueInfo v => some v.value
  | _ => none

structure Span where
  l1 : Nat
  c1 : Nat
  l2 : Nat
  c2 : Nat

def spanJson (s : Span) : Json := Json.arr #[s.l1, s.c1, s.l2, s.c2]

def fail (msg : String) : MetaM α := throwError s!"CENSUS-FAIL {msg}"

def run : MetaM Unit := do
  let env ← getEnv
  let sRaw := (← IO.getEnv "CENSUS_MODULES").getD ""
  let tracked : List Name := ((sRaw.splitOn "\n").filter (· != "")).map (·.toName)
  let out := (← IO.getEnv "CENSUS_OUT").getD ""
  let root := (← IO.getEnv "CENSUS_ROOT").getD ""
  if tracked.isEmpty then fail "CENSUS-ENV no tracked modules supplied (CENSUS_MODULES empty)"
  if out.isEmpty then fail "CENSUS-ENV CENSUS_OUT unset"
  if root.isEmpty then fail "CENSUS-ENV CENSUS_ROOT unset"
  -- R-1 extent: every tracked module is in the elaborated environment, and
  -- every environment module built inside the package is tracked.
  let envMods := env.header.moduleNames
  for m in tracked do
    unless envMods.contains m do fail s!"CENSUS-MODULE-GAP {m}: tracked but absent from the environment"
  let rootCanon := (← IO.FS.realPath root).toString ++ "/"
  let sp ← searchPathRef.get
  for m in envMods do
    match ← sp.findModuleWithExt "olean" m with
    | some p =>
      let r := (← IO.FS.realPath p).toString
      if rootCanon.isPrefixOf r && !tracked.contains m then
        fail s!"CENSUS-MODULE-GAP {m}: built in the package but not in the census extent"
    | none => fail s!"CENSUS-MODULE-GAP {m}: no loadable olean"
  let isProject (n : Name) : Bool :=
    match homeModule env n with
    | some m => tracked.contains m
    | none => false
  -- every project constant, with its own declaration range when it has one
  let mut consts : Array (Name × ConstantInfo) := #[]
  for (n, ci) in env.constants.toList do
    if isProject n then consts := consts.push (n, ci)
  if consts.isEmpty then fail "CENSUS-EMPTY zero project constants"
  let mut ranges : Std.HashMap Name Span := {}
  for (n, _) in consts do
    if let some r ← findDeclarationRangesCore? n then
      ranges := ranges.insert n
        ⟨r.range.pos.line, r.range.pos.column, r.range.endPos.line, r.range.endPos.column⟩
  let isProduction (n : Name) (ci : ConstantInfo) : Bool :=
    match ci with
    | .thmInfo _ => false
    | _ =>
      ranges.contains n &&
        (match homeModule env n with
         | some m => moduleRole m == "production"
         | none => false)
  -- owner: the nearest enclosing name with a source range (aux defs such as
  -- `f.match_1` belong to `f`).
  -- Constructors belong to their inductive and structure projections to
  -- their structure: a mutant edits the type declaration, not either.
  let owner (n : Name) : Option Name := Id.run do
    let mut cur := n
    match env.find? n with
    | some (.ctorInfo cv) => cur := cv.induct
    | _ =>
      if let some pi := env.getProjectionFnInfo? n then
        if let some (.ctorInfo cv) := env.find? pi.ctorName then cur := cv.induct
    for _ in [0:16] do
      if ranges.contains cur then return some cur
      match cur with
      | .anonymous => return none
      | _ => cur := cur.getPrefix
    return none
  -- statement closure: walk the theorem type, then the bodies of every
  -- project definition reached; never a theorem's proof.
  let closureOf (start : Expr) : Std.HashSet Name := Id.run do
    let mut seen : Std.HashSet Name := {}
    let mut todo : Array Name := start.getUsedConstants
    while !todo.isEmpty do
      let n := todo.back!
      todo := todo.pop
      if seen.contains n then continue
      seen := seen.insert n
      if !isProject n then continue
      match env.find? n with
      | some ci =>
        if let some b := bodyOf ci then todo := todo ++ b.getUsedConstants
        match ci with
        | .thmInfo _ => pure ()
        -- an inductive is defined by its constructors (a structure by its
        -- fields): their types are part of what a statement naming it means
        | .inductInfo iv => todo := todo ++ ci.type.getUsedConstants ++ iv.ctors.toArray
        | _ => todo := todo ++ ci.type.getUsedConstants
      | none => pure ()
    return seen
  -- proof dependencies: the authored theorems a theorem's proof reaches,
  -- through other theorems' proofs and definition bodies. Used only to explain
  -- an OPEN row (a killed dependency is admitted by Lean, so the error stays
  -- in the dependency's own declaration); never to credit a kill.
  let isAuthoredThm (n : Name) : Bool :=
    match env.find? n with
    | some (.thmInfo _) => ranges.contains n && (env.getProjectionFnInfo? n).isNone
    | _ => false
  let proofDepsOf (self : Name) (proof : Expr) : Array Name := Id.run do
    let mut seen : Std.HashSet Name := {}
    let mut acc : Array Name := #[]
    let mut todo : Array Name := proof.getUsedConstants
    while !todo.isEmpty do
      let n := todo.back!
      todo := todo.pop
      if seen.contains n then continue
      seen := seen.insert n
      if !isProject n then continue
      match env.find? n with
      | some (.thmInfo tv) =>
        if n != self && isAuthoredThm n then acc := acc.push n
        todo := todo ++ tv.value.getUsedConstants
      | some ci =>
        if let some b := bodyOf ci then todo := todo ++ b.getUsedConstants
      | none => pure ()
    return acc
  -- modules
  let mut modsJson : Array Json := #[]
  for m in tracked.toArray.qsort (·.toString < ·.toString) do
    let some i := envMods.idxOf? m | fail s!"CENSUS-MODULE-GAP {m}"
    let imps := (env.header.moduleData[i]!.imports.map (·.module)).filter (tracked.contains ·)
    modsJson := modsJson.push <| Json.mkObj [
      ("name", toJson m.toString), ("file", toJson (moduleFile m)),
      ("role", toJson (moduleRole m)),
      ("imports", toJson ((imps.map (·.toString)).qsort (· < ·)))]
  -- declarations with ranges
  let sorted := consts.qsort (fun a b => a.1.toString < b.1.toString)
  let mut declsJson : Array Json := #[]
  let mut ids : Std.HashSet String := {}
  for (n, ci) in sorted do
    let some sp := ranges.get? n | continue
    let some m := homeModule env n | continue
    let id := identity env n
    -- constructors and recursors share their inductive's range; they are
    -- not separate declarations for span purposes.
    match ci with
    | .ctorInfo _ | .recInfo _ => continue
    | _ => pure ()
    if ids.contains id then fail s!"CENSUS-DUP-ID {id}"
    ids := ids.insert id
    -- a type declaration: an inductive, or a definition whose value is a
    -- type (its type ends in a sort), e.g. `abbrev Key := String`
    let typeDecl ← match ci with
      | .inductInfo _ => pure true
      | .defnInfo _ | .opaqueInfo _ =>
        try forallTelescopeReducing ci.type fun _ body => pure body.isSort
        catch _ => pure false
      | _ => pure false
    declsJson := declsJson.push <| Json.mkObj [
      ("id", toJson id), ("module", toJson m.toString), ("kind", toJson (kindOf ci)),
      ("private", toJson (isPrivateName n)), ("production", toJson (isProduction n ci)),
      ("typeDecl", toJson typeDecl), ("span", spanJson sp)]
  -- theorems
  let mut thmsJson : Array Json := #[]
  let mut nAuthored := 0
  let mut nExcluded := 0
  for (n, ci) in sorted do
    let .thmInfo tv := ci | continue
    let some m := homeModule env n | continue
    let id := identity env n
    let generated := !ranges.contains n || (env.getProjectionFnInfo? n).isSome
    match (if generated then none else ranges.get? n) with
    | none =>
      nExcluded := nExcluded + 1
      thmsJson := thmsJson.push <| Json.mkObj [
        ("id", toJson id), ("module", toJson m.toString), ("class", toJson "EXCLUDED")]
    | some sp =>
      nAuthored := nAuthored + 1
      let cl := closureOf tv.type
      let mut prod : Array String := #[]
      let mut other : Array String := #[]
      for c in cl.toList do
        let some c' := owner c | continue
        if c' != c && cl.contains c' then continue
        match env.find? c' with
        | some ci' =>
          if isProduction c' ci' then prod := prod.push (identity env c')
          else if isProject c' then other := other.push (identity env c')
        | none => pure ()
      thmsJson := thmsJson.push <| Json.mkObj [
        ("id", toJson id), ("module", toJson m.toString), ("class", toJson "AUTHORED"),
        ("span", spanJson sp),
        ("closure", toJson (prod.toList.eraseDups.toArray.qsort (· < ·))),
        ("specClosure", toJson (other.toList.eraseDups.toArray.qsort (· < ·))),
        ("proofDeps", toJson (((proofDepsOf n tv.value).map (identity env)).qsort (· < ·)))]
  if nAuthored == 0 then fail "CENSUS-EMPTY zero authored theorems"
  -- refusal vocabularies: the named ones plus every inductive of a
  -- production module that is the error type of an `Except` returned by a
  -- production definition.
  let mut discovered : Array Name := #[]
  for (n, ci) in sorted do
    unless isProduction n ci do continue
    match ci with
    | .defnInfo _ | .opaqueInfo _ =>
      let eTy? ← try
          forallTelescopeReducing ci.type fun _ body => do
            let body ← whnf body
            if body.isAppOfArity ``Except 2 then
              return (body.getArg! 0).getAppFn.constName?
            else return none
        catch _ => pure none
      if let some e := eTy? then
        if isProject e then
          match env.find? e with
          | some (.inductInfo _) =>
            if !discovered.contains e then discovered := discovered.push e
          | _ => pure ()
    | _ => pure ()
  let vocab := namedVocabularies.toArray ++
    (discovered.filter (!namedVocabularies.contains ·)).qsort (·.toString < ·.toString)
  -- direct emitters of each constructor, closed over classifier functions
  -- whose result type is the vocabulary itself (e.g. `guardOf : Event → GuardId`).
  let mentions (needle : Name) : Array Name := Id.run do
    let mut acc := #[]
    for (n, ci) in sorted do
      if let some b := bodyOf ci then
        if b.getUsedConstants.contains needle then
          if let some o := owner n then
            if !acc.contains o then acc := acc.push o
    return acc
  let mut vocabJson : Array Json := #[]
  for v in vocab do
    let some (.inductInfo iv) := env.find? v
      | fail s!"CENSUS-VOCABULARY {v}: not an inductive in the environment"
    let mut ctorsJson : Array Json := #[]
    for k in iv.ctors do
      -- an emitter is a production definition (not an instance, not the
      -- vocabulary's own declaration) whose body names the constructor
      let isEmitter (e : Name) : Bool :=
        e != v && !isInstanceCore env e && !isInstanceCore env e.getPrefix &&
        match env.find? e with
        | some ci@(.defnInfo _) | some ci@(.opaqueInfo _) => isProduction e ci
        | _ => false
      let mut emitters : Array Name := (mentions k).filter isEmitter
      -- classifier closure: a production function returning `v` itself
      let mut frontier := emitters
      while !frontier.isEmpty do
        let e := frontier.back!
        frontier := frontier.pop
        let some ci := env.find? e | continue
        let ret ← forallTelescopeReducing ci.type fun _ body => pure body.getAppFn.constName?
        if ret == some v then
          for c in mentions e do
            if isEmitter c && !emitters.contains c then
              emitters := emitters.push c
              frontier := frontier.push c
      let mut scope : Std.HashSet String := {}
      for e in emitters do
        scope := scope.insert (identity env e)
        let some ci := env.find? e | continue
        if let some b := bodyOf ci then
          for c in (closureOf b).toList do
            let some c' := owner c | continue
            match env.find? c' with
            | some ci' => if isProduction c' ci' then scope := scope.insert (identity env c')
            | none => pure ()
      ctorsJson := ctorsJson.push <| Json.mkObj [
        ("id", toJson (identity env k)),
        ("emitters", toJson ((emitters.map (identity env)).qsort (· < ·))),
        ("scope", toJson (scope.toArray.qsort (· < ·)))]
    vocabJson := vocabJson.push <| Json.mkObj [
      ("type", toJson (identity env v)),
      ("origin", toJson (if namedVocabularies.contains v then "named" else "discovered")),
      ("module", toJson ((homeModule env v).getD .anonymous).toString),
      ("constructors", Json.arr ctorsJson)]
  let census := Json.mkObj [
    ("schema", toJson "lean-mutants-census/1"),
    ("productionRule", toJson productionRuleText),
    ("exclusionRule", toJson exclusionRuleText),
    ("modules", Json.arr modsJson),
    ("declarations", Json.arr declsJson),
    ("theorems", Json.arr thmsJson),
    ("vocabularies", Json.arr vocabJson)]
  IO.FS.writeFile out (census.pretty ++ "\n")
  IO.println s!"CENSUS-OK modules={tracked.length} authored={nAuthored} excluded={nExcluded} vocabularies={vocab.size}"

end MutantCensus

#eval MutantCensus.run
