# Lean mutant ledger — fail-closed reader of the elaboration driver's stdout.
#
#   jq -R -s --argjson chain '[modules…]' --argjson applied true -f parse.jq <raw>
#
# Every non-empty line must be one JSON object: a driver marker
# ({"elab": "begin"|"end", "module": …}) or a frontend message (has
# "severity" and "pos"). Anything else is reported as unparseable, never
# skipped. A module of the chain without its "end" marker is `missing`.

(split("\n") | map(select(length > 0))) as $lines
| reduce $lines[] as $raw (
    {cur: null, ended: [], diags: {}, unparseable: []};
    ($raw | try fromjson catch null) as $j
    | if ($j | type) != "object" then
        .unparseable += [{module: (.cur // "-"), text: ($raw | .[0:160])}]
      elif $j.elab == "begin" then .cur = $j.module
      elif $j.elab == "end" then .ended += [$j.module] | .cur = null
      elif ($j.severity | type) == "string" and ($j.pos | type) == "object" and .cur != null then
        .diags[.cur] += [{severity: $j.severity, l: $j.pos.line, c: $j.pos.column,
                          text: ($j.data // "" | tostring | .[0:200])}]
      else
        .unparseable += [{module: (.cur // "-"), text: ($raw | .[0:160])}]
      end)
| {
    applied: $applied,
    chain: $chain,
    status: ([ $chain[] as $m | {key: $m, value: (if (.ended | index($m)) then "ok" else "missing" end)} ] | from_entries),
    unparseable: .unparseable,
    diags: .diags
  }
