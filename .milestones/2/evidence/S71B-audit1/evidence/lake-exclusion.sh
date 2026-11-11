#!/usr/bin/env bash
set -euo pipefail
cd /code/reactivegas-issue-71-audit-b1
probe=/tmp/reactivegas/ms2/t71-design-record/audit-b1/evidence/lake-probe
mkdir -p "$probe/lean/.lake/poison" "$probe/lean/Model"
printf 'def canCloseGroup := true\n' > "$probe/lean/Model/Seed.lean"
printf 'def canCloseGroup := false\ndef PoisonDeclaration := 9\n' > "$probe/lean/.lake/poison/Bad.lean"
# These are the exact source exclusion patterns in frozen v6, tested both ways.
count() { grep -rn --include='*.lean' --exclude-dir=.lake 'canCloseGroup' "$1" | wc -l; }
discover() { find "$1" -name '*.lean' -not -path '*/.lake/*' | sort; }
test "$(count "$probe/lean")" -eq 1
test "$(grep -r --include='*.lean' 'canCloseGroup' "$probe/lean" | wc -l)" -eq 2
test "$(discover "$probe/lean" | wc -l)" -eq 1
printf 'def secondUse := canCloseGroup\n' > "$probe/lean/Model/Extra.lean"
test "$(count "$probe/lean")" -eq 2
test "$(discover "$probe/lean" | wc -l)" -eq 2
test -d lean/.lake
test "$(count lean/)" -eq 1
printf 'synthetic .lake poison excluded; second real source included; real post-build canCloseGroup count=1\n'
printf 'real post-build source files='
discover lean/ | wc -l
test -z "$(git status --porcelain=v1 --untracked-files=no)"
