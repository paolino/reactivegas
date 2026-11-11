#!/usr/bin/env bash
# D-4 identity normalizer (F-A redesign). Derives ONE EXACT IDENTITY PER LINE
# from a real `ghc --show-iface` dump, so the join keeps whole-line matching
# (grep -qxF) and therefore cannot match Foo inside FooBar.
#
# WHY THIS EXISTS. The r9 leg matched `grep -qxF "<name>"` against the RAW
# dump. That worked on the synthetic fixture, whose dump was five bare lines,
# and it can NEVER match a real dump: real export lines are two-space indented
# with the member set in braces, e.g. `  GroupEvent{App Base}`. Verified on
# real bytes: `grep -qxF -e GroupEvent` on s30-0a-Event.dump is ABSENT.
#
# WHAT IS AND IS NOT DISCARDED. This is a DERIVED VIEW for identity matching
# only. The raw dump is retained and hashed separately; nothing is normalized
# away to make a signature appear, and this view is never used as the pin.
#
# GRAMMAR, read from real dumps (s30-0a-Event.dump, s30-0a-JSON.dump):
#   exports:
#     TypeName{Member Member ...}      -- members are constructors AND record
#     BareName                            selectors; a value export has no braces
#   <first non-two-space-indented line ends the block>
set -u
dump="${1:?dump path required}"
[ -r "$dump" ] || { printf '%s\n' "D4-IDENT-REFUSE: unreadable dump: $dump" >&2; exit 3; }
awk '
  /^exports:$/ { inblock=1; next }
  inblock && !/^  / { exit }
  inblock {
    line = substr($0, 3)
    b = index(line, "{")
    if (b == 0) { if (length(line)) print line; next }
    head = substr(line, 1, b-1)
    if (length(head)) print head
    rest = substr(line, b+1)
    e = index(rest, "}")
    if (e > 0) rest = substr(rest, 1, e-1)
    n = split(rest, parts, " ")
    for (i = 1; i <= n; i++) if (length(parts[i])) print parts[i]
  }
' "$dump" | sort -u
