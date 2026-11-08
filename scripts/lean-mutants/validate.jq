# Lean mutant ledger — validator.
#
# Input: the run bundle assembled by `scripts/lean-mutants/run`:
#   .census        the compiled census (Census.lean), facts about the head
#   .catalogue     lean/mutants/catalogue.json (tracked)
#   .ledger        lean/mutants/ledger.json (tracked)
#   .sourceLines   { mutant id: text of its patch line at the head, or null }
#   .observations  { baseline: RUN, mutants: { id: RUN } }
#       RUN = { applied, chain: [module], status: { module: ok|missing },
#               unparseable: [ {module, text} ], diags: { module: [ {severity, l, c} ] } }
#
# Output: { violations: [ {rule, id, detail} ], theorems, mutants, guards, counts }.
# Every judgement compares a tracked claim with a census fact or an observed
# diagnostic; nothing here is derived from the claim it checks.

def v(rule; id; detail): {rule: rule, id: id, detail: detail};

# position (line, column) inside a span [l1, c1, l2, c2], inclusive
def inSpan($s): (.l > $s[0] or (.l == $s[0] and .c >= $s[1]))
  and (.l < $s[2] or (.l == $s[2] and .c <= $s[3]));

def errorsOf($run; $module): ($run.diags[$module] // []) | map(select(.severity == "error"));

. as $b
| ($b.census.theorems | map(select(.class == "AUTHORED"))) as $authored
| ($authored | map({key: .id, value: .}) | from_entries) as $thm
| ($b.census.declarations | map({key: .id, value: .}) | from_entries) as $decl
| ($b.catalogue.mutants // []) as $mutants
| ($mutants | map(select(.decoy != true))) as $real
| ($mutants | map(select(.decoy == true))) as $decoys
| ($b.ledger.rows // []) as $rows
| ($b.observations.mutants // {}) as $obs
| ([$b.census.vocabularies[] | .constructors[] | {key: .id, value: .}] | from_entries) as $ctor

# ---- observed kills, per mutant (R-7) ------------------------------------
| ($mutants | map(
    . as $m
    | ($obs[$m.id] // null) as $run
    | {
        id: $m.id,
        target: $m.target,
        ran: ($run != null and $run.applied == true),
        kills: (if $run == null then [] else
          [ $authored[] | select(.closure | index($m.target))
            | . as $t
            | select([ errorsOf($run; $t.module)[] | select(inSpan($t.span)) ] | length > 0)
            | .id ] | sort end),
        proofOnly: (if $run == null then [] else
          [ $authored[] | select((.closure | index($m.target)) | not)
            | . as $t
            | select([ errorsOf($run; $t.module)[] | select(inSpan($t.span)) ] | length > 0)
            | .id ] | sort end),
        errors: (if $run == null then 0 else [ $run.diags[][] | select(.severity == "error") ] | length end),
        targetErrors: (if $run == null or ($decl[$m.target] == null) then 0 else
          [ errorsOf($run; $decl[$m.target].module)[] | select(inSpan($decl[$m.target].span)) ] | length end)
      })
  ) as $mres
| ($mres | map({key: .id, value: .}) | from_entries) as $mby

# ---- per theorem: computed class, killers, attempted ------------------------
| ($authored | map(
    . as $t
    | ([ $real[] | select(.target as $tg | $t.closure | index($tg)) | .id ] | sort) as $att
    | ([ $mres[] | select(.kills | index($t.id)) | .id ] | sort) as $killers
    | {
        id: $t.id, module: $t.module,
        class: (if ($t.closure | length) == 0 then "HELPER"
                elif ($killers | length) > 0 then "KILLED" else "OPEN" end),
        killedBy: $killers,
        attempted: ($att - $killers),
        helperFact: ("statement closure holds no production definition"
          + (if ($t.specClosure | length) > 0
             then " (it names only: " + ($t.specClosure | join(", ")) + ")" else "" end)),
        # why an attempted-but-unkilled theorem is OPEN, computed: a theorem its
        # proof uses is killed (Lean admits it, so the error stays there), or
        # nothing it rests on fails under any attempted mutant
        openFact: (($att - $killers) as $a
          | ($t.proofDeps // []) as $deps
          | ([ $a[] as $m | $mby[$m].kills[] | select(. as $k | $deps | index($k)) | {m: $m, l: .} ]
             | unique_by(.l)) as $masks
          | if ($a | length) == 0 then null
            elif ($masks | length) > 0 then
              "masked: its proof uses "
              + ([ $masks[:4][] | "`\(.l)` (killed by `\(.m)`)" ] | join(", "))
              + (if ($masks | length) > 4 then " and \(($masks | length) - 4) more" else "" end)
              + "; Lean admits a killed theorem, so the error stays in that theorem's own declaration"
            else
              "unconstrained: under none of its \($a | length) attempted mutants does Lean report an error in this theorem or in a theorem its proof uses"
            end)
      })
  ) as $tres
| ($tres | map({key: .id, value: .}) | from_entries) as $tby

# ---- guards (R-9, E-4) ------------------------------------------------------
| ([ $b.census.vocabularies[] as $voc | $voc.constructors[] as $k
    | ([ $real[] | select(.guard == $k.id) | select(.target as $tg | $k.scope | index($tg)) | .id ] | sort) as $gm
    | {
        type: $voc.type, module: $voc.module, id: $k.id, emitters: $k.emitters,
        mutants: $gm,
        kills: ([ $gm[] as $g | $mby[$g].kills[] ] | unique),
        outcome: (if ($k.emitters | length) == 0 then "UNEMITTED"
                  elif ($gm | length) == 0 then "MISSING"
                  elif ([ $gm[] as $g | $mby[$g].kills[] ] | length) == 0 then "SURVIVED"
                  else "KILLS" end)
      } ]) as $gres

| {
  violations: (
    # extent guards: never quantify over an empty set
    (if ($authored | length) == 0 then [v("EMPTY-EXTENT"; "census"; "zero authored theorems")] else [] end)
    + (if ($real | length) == 0 then [v("EMPTY-CATALOGUE"; "catalogue"; "zero non-decoy mutants")] else [] end)
    + (if ($b.observations.baseline.chain // [] | length) == 0
       then [v("OBSERVATION-MISSING"; "baseline"; "no module elaborated")] else [] end)

    # diagnostics are parsed fail-closed
    + [ ($b.observations.baseline | .unparseable[]? | v("DIAG-UNPARSEABLE"; "baseline"; "\(.module): \(.text)")) ]
    + [ $obs | to_entries[] | .key as $id | .value.unparseable[]? | v("DIAG-UNPARSEABLE"; $id; "\(.module): \(.text)") ]
    + [ $b.observations.baseline.status | to_entries[] | select(.value != "ok")
        | v("OBSERVATION-MISSING"; "baseline"; .key) ]
    + [ $obs | to_entries[] | .key as $id | .value.status | to_entries[] | select(.value != "ok")
        | v("OBSERVATION-MISSING"; $id; .key) ]

    # R-8 baseline: no error inside any authored theorem span on the head
    + [ $authored[] as $t
        | errorsOf($b.observations.baseline; $t.module)[] | select(inSpan($t.span))
        | v("BASELINE-ERROR"; $t.id; "error at \($t.module):\(.l):\(.c)") ]
    + [ $b.census.modules[] | .name as $n
        | select(($b.observations.baseline.chain // []) | index($n) | not)
        | v("OBSERVATION-MISSING"; "baseline"; "module \($n) not elaborated") ]

    # R-6 mutants
    + [ $mutants | group_by(.id)[] | select(length > 1) | v("DUPLICATE-MUTANT"; .[0].id; "\(length) entries") ]
    + [ $mutants[] as $m
        | ($decl[$m.target]) as $d
        | if $d == null then v("TARGET-UNKNOWN"; $m.id; $m.target)
          elif $d.production != true then v("TARGET-NOT-PRODUCTION"; $m.id; "\($m.target) (\($d.kind) in \($d.module))")
          else
            ( if ($b.sourceLines[$m.id]) != $m.patch.before
              then v("PATCH-NOT-APPLIED"; $m.id; "line \($m.patch.line) of \($d.module) does not read as the patch's `before`")
              else empty end ),
            ( if $m.patch.before == $m.patch.after then v("PATCH-NOOP"; $m.id; "before == after") else empty end ),
            # the edited segment (the line minus the common prefix and suffix
            # of `before` and `after`) must lie inside the target, and the
            # target must be among the innermost declarations containing it
            ( ($m.patch.before | explode) as $x | ($m.patch.after | explode) as $y
              | ([ $x, $y ] | map(length) | min) as $n
              | ([ range(0; $n) | select($x[.] != $y[.]) ][0] // $n) as $p
              | ([ range(0; $n - $p) | select($x[($x | length) - 1 - .] != $y[($y | length) - 1 - .]) ][0] // ($n - $p)) as $s
              | ([$p, ($x | length) - $s] | max) as $e
              | $m.patch.line as $l
              | [ $b.census.declarations[]
                  | select(.module == $d.module)
                  | select((.span[0] < $l or (.span[0] == $l and .span[1] <= $p))
                       and (.span[2] > $l or (.span[2] == $l and .span[3] >= $e))) ]
              | (map((.span[2] - .span[0]) * 100000 + (if .span[0] == .span[2] then .span[3] - .span[1] else 0 end)) | min) as $least
              | map(select((.span[2] - .span[0]) * 100000 + (if .span[0] == .span[2] then .span[3] - .span[1] else 0 end) == $least)) as $inner
              | if ($inner | map(.id) | index($m.target)) == null
                then v("PATCH-OUTSIDE-TARGET"; $m.id;
                       "the edit at line \($l), columns \($p)-\($e) lies in \(($inner | map(.id) | join(", ")) | if . == "" then "no declaration" else . end), not in \($m.target)")
                else empty end ),
            ( if ($obs[$m.id] // null) == null or ($obs[$m.id].applied != true)
              then v("MUTANT-NOT-ELABORATED"; $m.id; "no observation under this mutant")
              else empty end ),
            ( if $mby[$m.id].targetErrors > 0
              then v("MUTANT-TARGET-ERROR"; $m.id; "\($mby[$m.id].targetErrors) error(s) inside \($m.target) itself")
              else empty end )
          end ]

    # R-7 kill claims: in the statement closure, and observed, both ways
    + [ $real[] as $m | ($m.claimedKills // [])[] as $t
        | if $thm[$t] == null then v("CLAIM-UNKNOWN-THEOREM"; $m.id; $t)
          elif ($thm[$t].closure | index($m.target)) == null
            then v("CLAIM-OUTSIDE-CLOSURE"; $m.id; "\($t): \($m.target) is not in its statement closure")
          elif ($mby[$m.id].kills | index($t)) == null
            then v("CLAIM-NOT-OBSERVED"; $m.id; "\($t): no error inside its span under this mutant")
          else empty end ]
    + [ $real[] as $m | $mby[$m.id].kills[] as $t
        | select((($m.claimedKills // []) | index($t)) == null)
        | v("KILL-UNCLAIMED"; $m.id; "\($t) is killed but not claimed") ]

    # R-10 decoy: present, claim-free, and neutral (no error anywhere)
    + (if ($decoys | length) == 0 then [v("DECOY-MISSING"; "catalogue"; "no neutral decoy mutant")] else [] end)
    + [ $decoys[] | select(((.claimedKills // []) | length) > 0 or .guard != null)
        | v("DECOY-HAS-CLAIMS"; .id; "a decoy claims no kill and no guard") ]
    + [ $decoys[] | . as $m | $mby[$m.id] | select(.errors > 0 or (.kills | length) > 0)
        | v("DECOY-NOT-NEUTRAL"; $m.id; "\(.errors) error(s), kills: \(.kills | join(", "))") ]

    # R-3 ledger extent: one row per authored theorem, no extras
    + [ $rows | group_by(.theorem)[] | select(length > 1) | v("DUPLICATE-ROW"; .[0].theorem; "\(length) rows") ]
    + [ $authored[] | .id as $id | select(([ $rows[] | select(.theorem == $id) ] | length) == 0)
        | v("THEOREM-WITHOUT-ROW"; $id; "authored theorem has no ledger row") ]
    + [ $rows[] | select($thm[.theorem] == null)
        | v("ROW-ABSENT-THEOREM"; .theorem; "no authored theorem of this identity in the census") ]

    # R-4 / R-5 / R-7 per row
    + [ $rows[] | select($thm[.theorem] != null) | . as $r | $tby[$r.theorem] as $c
        | if ($r.class | IN("KILLED", "HELPER", "OPEN") | not) then v("CLASS-INVALID"; $r.theorem; "\($r.class)")
          elif ($r.class == "HELPER" or $c.class == "HELPER") and $r.class != $c.class
            then v("HELPER-MISMATCH"; $r.theorem; "ledger \($r.class), computed \($c.class)")
          elif $r.class != $c.class then v("CLASS-MISMATCH"; $r.theorem; "ledger \($r.class), observed \($c.class)")
          elif $r.class == "HELPER" and $r.reason != $c.helperFact
            then v("HELPER-REASON"; $r.theorem; "reason must be the computed fact")
          elif $r.class == "KILLED" and (($r.killedBy // []) | sort) != $c.killedBy
            then v("KILLEDBY-MISMATCH"; $r.theorem; "ledger \(($r.killedBy // []) | join(",")), observed \($c.killedBy | join(","))")
          elif $r.class == "OPEN" then
            ( if (($r.reason // "") | test("\\S") | not) then v("OPEN-NO-REASON"; $r.theorem; "an OPEN row needs its reason") else empty end ),
            ( if (($r.attempted // []) | sort) != $c.attempted
              then v("OPEN-ATTEMPTED-MISMATCH"; $r.theorem; "ledger \(($r.attempted // []) | join(",")), computed \($c.attempted | join(","))")
              else empty end ),
            ( if ($c.attempted | length) == 0 and (($r.reason // "") | startswith("inexpressible:") | not)
              then v("OPEN-NO-ATTEMPT"; $r.theorem; "no mutant attempted and the reason does not state `inexpressible:`")
              else empty end ),
            # `inexpressible:` stands only when the computed statement closure
            # holds type declarations alone; any production definition with a
            # computational body is a mutation target that was not attempted
            ( if ($c.attempted | length) == 0 and (($r.reason // "") | startswith("inexpressible:"))
              then ([ $thm[$r.theorem].closure[] | select(($decl[.].typeDecl // false) | not) ]) as $bodies
                | if ($bodies | length) > 0
                  then v("INEXPRESSIBLE-REJECTED"; $r.theorem; "its statement closure holds \($bodies | join(", ")), which no mutant attempts")
                  else empty end
              else empty end ),
            ( if $c.openFact != null and (($r.reason // "") | test("\\S")) and $r.reason != $c.openFact
              then v("OPEN-REASON-STALE"; $r.theorem; "reason must be the computed fact: \($c.openFact)")
              else empty end )
          else empty end ]

    # R-9 guards
    + [ $real[] | select(.guard != null) | . as $m
        | if $ctor[$m.guard] == null then v("GUARD-UNKNOWN"; $m.id; $m.guard)
          elif ($ctor[$m.guard].scope | index($m.target)) == null
            then v("GUARD-OUT-OF-SCOPE"; $m.id; "\($m.target) neither emits \($m.guard) nor is reached by an emitter")
          else empty end ]
    + [ $gres[] | select(.outcome == "MISSING") | v("GUARD-MISSING"; .id; "emitted by \(.emitters | join(", ")) but no guard mutant") ]
  ),
  theorems: $tres,
  mutants: $mres,
  guards: $gres,
  counts: {
    discovered: ($b.census.theorems | length),
    excluded: ([ $b.census.theorems[] | select(.class == "EXCLUDED") ] | length),
    authored: ($authored | length),
    helper: ([ $tres[] | select(.class == "HELPER") ] | length),
    required: ([ $tres[] | select(.class != "HELPER") ] | length),
    executed: ([ $mres[] | select(.ran) ] | length),
    killed: ([ $tres[] | select(.class == "KILLED") ] | length),
    open: ([ $tres[] | select(.class == "OPEN") ] | length),
    survived: ([ $gres[] | select(.outcome == "SURVIVED") ] | length),
    unemitted: ([ $gres[] | select(.outcome == "UNEMITTED") ] | length)
  }
}
