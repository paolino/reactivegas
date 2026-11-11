import Lean
-- @IMPORTS@

/-!
# Design-record citation checker — the Lean side

Generated into a temporary file by `scripts/design-citations/check`, which
replaces the `@IMPORTS@` line above with one `import` per Lean module found on
disk under the library directory. Nothing under `lean/` imports this file.

Inputs (environment): `DESIGN_RECORD_FILES` — one `display<TAB>absolute` pair
per Markdown file of the record, found by scanning; `DESIGN_PROJECT_MODULES` —
the library module names found by scanning. The set of known declarations is
read from the Lean environment these imports produced: every constant defined by
one of those modules, minus private and compiler-internal ones.

The code-span rule (stated in the record, section "How to cite this page"):

* a code span `lean:Name` is a citation; `Name` must be a declaration;
* any other code span names a declaration when one of its identifier tokens
  does. A token is a maximal dotted run of identifier characters, or one of the
  dot-separated components of such a run; it names a declaration when it equals a
  trailing run of the components of some declaration's name;
* fenced blocks are not scanned, except that a block tagged `lean` is refused;
* `lean:` followed by an identifier outside a code span is refused.
-/

open Lean

namespace DesignCitations

structure Finding where
  where_ : String
  tag : String
  detail : String

def Finding.render (f : Finding) : String := s!"{f.where_}: {f.tag} {f.detail}"

/-- Identifier shape: dot-separated, non-empty components, Lean identifier
characters. -/
def isDottedIdent (s : String) : Bool :=
  let parts := s.splitOn "."
  !parts.isEmpty && parts.all fun p =>
    match p.toList with
    | [] => false
    | c :: rest => isIdFirst c && rest.all isIdRest

def nameOfDotted (s : String) : Name :=
  (s.splitOn ".").foldl (fun n c => Name.str n c) Name.anonymous

/-- All trailing component runs of a name, as dotted strings
(`A.B.c` gives `c`, `B.c`, `A.B.c`). -/
def suffixes (n : Name) : List String :=
  let comps := n.components.map (·.toString)
  (List.range comps.length).map fun i => ".".intercalate (comps.drop i)

/-- Maximal runs of identifier characters and dots inside a span, with the
leading and trailing dots trimmed. -/
def identRuns (s : String) : List String := Id.run do
  let mut runs : List String := []
  let mut cur : List Char := []
  for c in s.toList do
    if isIdRest c || c == '.' then cur := c :: cur
    else
      if !cur.isEmpty then runs := String.mk cur.reverse :: runs
      cur := []
  if !cur.isEmpty then runs := String.mk cur.reverse :: runs
  return (runs.map fun r => (r.dropWhile (· == '.')).dropRightWhile (· == '.')).reverse

/-- The identifier tokens of a span: every maximal dotted run, and every
dot-separated component of a run. A token is a candidate name; whether it names
a declaration is for the environment to say. -/
def spanTokens (s : String) : List String := Id.run do
  let mut out : List String := []
  for r in identRuns s do
    for t in r :: r.splitOn "." do
      if isDottedIdent t && !out.contains t then out := out ++ [t]
  return out

/-! ## Scanning -/

structure Paragraph where
  line : Nat
  text : String

/-- Paragraphs outside fenced blocks, plus the lines of fenced blocks tagged
`lean`. A table row and a heading are paragraphs of their own, so an unmatched
backtick cannot pair with a span in another row. -/
def paragraphs (lines : Array String) : Array Paragraph × Array Nat := Id.run do
  let mut paras : Array Paragraph := #[]
  let mut leanFences : Array Nat := #[]
  let mut fence : Option Char := none
  let mut curStart := 0
  let mut cur : List String := []
  let flush (paras : Array Paragraph) (curStart : Nat) (cur : List String) : Array Paragraph :=
    if cur.isEmpty then paras else paras.push ⟨curStart, "\n".intercalate cur.reverse⟩
  for i in [0:lines.size] do
    let line := lines[i]!
    let t := line.trimLeft
    let isFence := t.startsWith "```" || t.startsWith "~~~"
    match fence with
    | some fc =>
      if isFence && t.front == fc then fence := none
    | none =>
      if isFence then
        paras := flush paras curStart cur
        cur := []
        fence := some (t.front)
        let info := (t.dropWhile (· == t.front)).trim
        if (info.splitOn " ").head! == "lean" then leanFences := leanFences.push (i + 1)
      else if t.isEmpty then
        paras := flush paras curStart cur
        cur := []
      else if t.startsWith "|" || t.startsWith "#" then
        paras := flush paras curStart cur
        paras := paras.push ⟨i + 1, line⟩
        cur := []
      else
        if cur.isEmpty then curStart := i + 1
        cur := line :: cur
  paras := flush paras curStart cur
  return (paras, leanFences)

structure Scanned where
  /-- (line, span text) -/
  spans : Array (Nat × String) := #[]
  /-- prose characters with their line, a space standing where a span was -/
  prose : Array (Char × Nat) := #[]

def scanParagraph (p : Paragraph) : Scanned := Id.run do
  let cs := p.text.toList.toArray
  let n := cs.size
  let mut out : Scanned := {}
  let mut line := p.line
  let mut i := 0
  while i < n do
    let c := cs[i]!
    if c == '`' then
      let mut run := 0
      while i + run < n && cs[i + run]! == '`' do run := run + 1
      let mut j := i + run
      let mut found : Option Nat := none
      while j < n && found.isNone do
        if cs[j]! == '`' then
          let mut k := 0
          while j + k < n && cs[j + k]! == '`' do k := k + 1
          if k == run then found := some j else j := j + k
        else j := j + 1
      match found with
      | some close =>
        let inner := String.mk (cs.extract (i + run) close).toList
        let text := (inner.map fun ch => if ch == '\n' then ' ' else ch).trim
        out := { out with spans := out.spans.push (line, text), prose := out.prose.push (' ', line) }
        for ch in (cs.extract i (close + run)).toList do
          if ch == '\n' then line := line + 1
        i := close + run
      | none =>
        for _ in [0:run] do out := { out with prose := out.prose.push ('`', line) }
        i := i + run
    else
      out := { out with prose := out.prose.push (c, line) }
      if c == '\n' then line := line + 1
      i := i + 1
  return out

/-- Lines where `lean:` begins an identifier outside any code span. -/
def bareCitations (prose : Array (Char × Nat)) : List (Nat × String) := Id.run do
  let mut hits : List (Nat × String) := []
  let n := prose.size
  for i in [0:n] do
    if i + 5 < n && prose[i]!.1 == 'l' && prose[i+1]!.1 == 'e' && prose[i+2]!.1 == 'a'
        && prose[i+3]!.1 == 'n' && prose[i+4]!.1 == ':' && isIdFirst prose[i+5]!.1
        && (i == 0 || !(isIdRest prose[i-1]!.1 || prose[i-1]!.1 == '.')) then
      let mut j := i + 5
      let mut ident : List Char := []
      while j < n && (isIdRest prose[j]!.1 || prose[j]!.1 == '.') do
        ident := prose[j]!.1 :: ident
        j := j + 1
      hits := (prose[i]!.2, "lean:" ++ String.mk ident.reverse) :: hits
  return hits.reverse

/-! ## Environment -/

abbrev Known := Std.HashSet String

structure Extent where
  full : Known
  trailing : Known
  count : Nat

def collectExtent (env : Environment) (projectModules : Array Name) : CoreM Extent := do
  let mut full : Known := {}
  let mut trailing : Known := {}
  let mut count := 0
  for (n, _) in env.constants.fold (fun acc n ci => acc.push (n, ci)) #[] do
    if n.isInternalDetail || isPrivateName n || n.isAnonymous then continue
    match env.getModuleIdxFor? n with
    | none => continue
    | some idx =>
      let some m := env.header.moduleNames[idx.toNat]? | continue
      if !projectModules.contains m then continue
      -- Only what an author wrote: a declaration with a source range, or a
      -- constructor or projection, minus the recursors and eliminators the
      -- elaborator derives for every inductive type.
      let written := (← findDeclarationRanges? n).isSome || env.isConstructor n || env.isProjectionFn n
      let generated := isAuxRecursor env n || isNoConfusion env n || (match env.find? n with | some (.recInfo _) => true | _ => false)
      if !written || generated then continue
      count := count + 1
      full := full.insert n.toString
      for s in suffixes n do trailing := trailing.insert s
  return { full, trailing, count }

def ownerOf (env : Environment) (n : Name) : Option Name :=
  (env.getModuleIdxFor? n).bind fun idx => env.header.moduleNames[idx.toNat]?

/-! ## The check -/

def checkFile (env : Environment) (ext : Extent) (display abs : String) :
    IO (Array Finding × Nat × Nat) := do
  let lines ← IO.FS.lines abs
  let (paras, leanFences) := paragraphs lines
  let mut findings : Array Finding := #[]
  let mut citations := 0
  let mut spans := 0
  for l in leanFences do
    findings := findings.push
      ⟨s!"{display}:{l}", "LEAN-FENCE", "a fenced block tagged lean is not scanned; cite the declaration by name instead"⟩
  for p in paras do
    let sc := scanParagraph p
    for (line, name) in bareCitations sc.prose do
      findings := findings.push
        ⟨s!"{display}:{line}", "BARE-CITATION", s!"`{name}` stands outside a code span, so it is not checked"⟩
    for (line, text) in sc.spans do
      spans := spans + 1
      let where_ := s!"{display}:{line}"
      if text.startsWith "lean:" then
        citations := citations + 1
        let nm := text.drop 5
        if !isDottedIdent nm then
          findings := findings.push ⟨where_, "MALFORMED-CITATION", s!"`{text}`: not `lean:` followed by one Lean name"⟩
        else if !ext.full.contains nm then
          let n := nameOfDotted nm
          let why :=
            if env.contains n then
              match ownerOf env n with
              | some m => s!"{nm} is defined by {m}, which is outside the Lean library"
              | none => s!"{nm} is not defined by the Lean library"
            else s!"no declaration {nm} in the built Lean library"
          findings := findings.push ⟨where_, "UNRESOLVED-CITATION", s!"lean:{nm} — {why}"⟩
      else
        let named := (spanTokens text).filter ext.trailing.contains
        unless named.isEmpty do
          findings := findings.push
            ⟨where_, "UNCITED-DECLARATION", s!"`{text}` names {", ".intercalate named} of the Lean library without the citation marker (write `lean:<full name>`)"⟩
  return (findings, citations, spans)

def main : CoreM Unit := do
  let env ← getEnv
  let some filesRaw ← IO.getEnv "DESIGN_RECORD_FILES" | throwError "design-citations: DESIGN_RECORD_FILES unset"
  let some modsRaw ← IO.getEnv "DESIGN_PROJECT_MODULES" | throwError "design-citations: DESIGN_PROJECT_MODULES unset"
  let files := (filesRaw.splitOn "\n").filter (· != "")
  let mods := ((modsRaw.splitOn "\n").filter (· != "")).map nameOfDotted |>.toArray
  let mut findings : Array Finding := #[]
  if mods.isEmpty then
    findings := findings.push ⟨"lean", "NO-LIBRARY", "no Lean module was discovered"⟩
  for m in mods do
    unless env.header.moduleNames.contains m do
      findings := findings.push ⟨"lean", "MODULE-NOT-LOADED", s!"{m} was discovered on disk but is absent from the loaded environment"⟩
  let ext ← collectExtent env mods
  if ext.count == 0 then
    findings := findings.push ⟨"lean", "NO-LIBRARY", "the loaded environment holds no declaration of the library"⟩
  if files.isEmpty then
    findings := findings.push ⟨"record", "NO-RECORD-FILES", "no Markdown file was discovered in the record directory"⟩
  if (← IO.getEnv "DESIGN_CITATIONS_LIST").isSome then
    for s in ext.full.toList.toArray.qsort (· < ·) do IO.println s!"declaration {s}"
  let mut citations := 0
  let mut spans := 0
  for f in files do
    match f.splitOn "\t" with
    | [display, abs] =>
      let (fs, c, s) ← checkFile env ext display abs
      findings := findings ++ fs
      citations := citations + c
      spans := spans + s
    | _ => findings := findings.push ⟨f, "BAD-INPUT", "record file entry is not display<TAB>absolute"⟩
  if citations == 0 then
    findings := findings.push ⟨"record", "NO-CITATIONS", "the record carries no citation, so nothing was resolved"⟩
  for f in findings do IO.eprintln f.render
  if findings.isEmpty then
    IO.println s!"design-citations: ok files={files.length} citations={citations} spans={spans} declarations={ext.count} modules={mods.size}"
  else
    throwError s!"design-citations: FAILED {findings.size} finding(s)"

end DesignCitations

#eval DesignCitations.main
