#!/usr/bin/env bash
# Preflight runner r8 — NOTE-009 fixed campaign fixtures + suite.
# TAXONOMY-v1 (exit-status taxonomy — bound IDENTICALLY here, in the leg
# script header, and in contract §8; never reclassified after the fact —
# any change requires re-freeze + new campaign binding):
#   exit 0 PASS — all gates green on resolved inputs.
#   exit 1 RED — verdict rendered: >=1 subject check failed on resolved
#     inputs.
#   exit 3 REFUSAL — no verdict possible: unbound config, unknown mode,
#     missing/unreadable frozen inputs, unresolvable artifacts, no producer
#     evidence, broken tools. Fail-fast; setup/config failures are NEVER
#     domain kills.
# TOOL AVAILABILITY (presence only — grants no authority; S0 never gates
# domain verdicts): bash, git (synthetic fixture repos ONLY), coreutils,
# grep, sed, diff, tar. NEVER: lean/lake/ghc(real)/cabal/nix/cargo.
# Bash-only constructs used (labeled — never POSIX-sh promises from a Bash
# invocation): `local`, `${//}`, `${##}`, `$(( ))`. ABSENT (author-audited,
# preflight-verifiable by grep): `[[`, `((`, `function`, `source`/`.`,
# `pipefail`, `&>`, `printf --`, leading-dash formats.
# PORTABILITY RULE: no format may begin with `-` (heredocs and `%s`-arg
# forms only); every generated fixture asserted non-empty immediately
# (`test -s`, else SETUP-FAILED loudly); tool-availability self-check first.
# STUBBED metadata (ghc shim serving fixture bytes; stub dumps/leg4-log/
# receipt fixtures) proves PLUMBING ONLY — never compiler-output
# compatibility, never semantic coverage. Stated in every verdict line.
# Idempotent: wipes and rebuilds CASES on every run. Copy convention: every
# branch copies template CONTENTS (`cp -r tmpl/. case`, never into-existing
# nesting) — see contract branch-fix table; structural assert below guards
# the whole class.
set -u
ROOT="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/handoffs/T30-DRIFT-LEG-r8.sh"
FX="$ROOT/fx"; EV="$ROOT/ev"; CASES="$ROOT/cases"
SETUP_FAILS=0
MISPREDICTS=0
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null

# S0: tool-availability self-check + printf sanity (fail-fast, diagnosed).
for t in bash git find sort diff sha256sum cat mkdir rm cp tar grep sed touch chmod printf; do
  command -v "$t" >/dev/null 2>&1 || { printf '%s\n' "S0-ABORT: tool missing: $t"; exit 3; }
done
[ "$(printf '%s\n' probe)" = "probe" ] || { printf '%s\n' "S0-ABORT: printf sanity failed"; exit 3; }
printf '%s\n' "S0: tool self-check passed"

git_env() { GIT_AUTHOR_NAME=fx GIT_AUTHOR_EMAIL=fx@fx GIT_COMMITTER_NAME=fx GIT_COMMITTER_EMAIL=fx@fx "$@"; }
must_nonempty() { # $1=path: fixture smoke assertion (silent corruption impossible)
  test -s "$1" || { printf '%s\n' "SETUP-FAILED: fixture empty/missing: $1"; return 1; }
}

setup_tree() { # $1=dest : pristine fixture repos + frozen values + stubs
  local d="$1" f
  rm -rf "$d"
  mkdir -p "$d/lean/lean/KelGroups/Vote" "$d/lean/lean/KelGroups"
  mkdir -p "$d/hs/lib/KelGroups/Vote" "$d/hs/dist-newstyle/b1/KelGroups/Vote"
  mkdir -p "$d/frozen" "$d/stubbin"
  for f in Types State Event Validate Fold Invariants Tests; do
    cat > "$d/lean/lean/KelGroups/Vote/$f.lean" <<EOF
fixture $f
inductive Fx$f where
  | mkA
  | mkB
EOF
    must_nonempty "$d/lean/lean/KelGroups/Vote/$f.lean" || return 1
  done
  for f in Integration State Validate Event Types; do
    cat > "$d/lean/lean/KelGroups/$f.lean" <<EOF
fixture base $f
def fxBase$f : Nat := 1
EOF
    must_nonempty "$d/lean/lean/KelGroups/$f.lean" || return 1
  done
  ( cd "$d/lean" && git init -q . && git add -A && git_env git commit -qm frozen ) || return 1
  cat > "$d/hs/lib/KelGroups/Vote/Types.hs" <<'EOF'
module KelGroups.Vote.Types (Foo (..), Bar (..)) where
data Foo = FooA | FooB deriving (Eq, Show)
data Bar = BarA deriving (Eq, Show)
helperOld :: Int
helperOld = 1
EOF
  must_nonempty "$d/hs/lib/KelGroups/Vote/Types.hs" || return 1
  cat > "$d/hs/lib/KelGroups/Vote/State.hs" <<'EOF'
module KelGroups.Vote.State (Baz (..)) where
data Baz = BazA deriving (Eq, Show)
EOF
  must_nonempty "$d/hs/lib/KelGroups/Vote/State.hs" || return 1
  ( cd "$d/hs" && git init -q . && git add -A && git_env git commit -qm frozen ) || return 1
  ( cd "$d/lean" && git ls-files 'lean/KelGroups/Vote' 'lean/KelGroups/Integration.lean' 'lean/KelGroups/State.lean' 'lean/KelGroups/Validate.lean' 'lean/KelGroups/Event.lean' 'lean/KelGroups/Types.lean' | sort ) > "$d/frozen/LEAN_FILES" || return 1
  must_nonempty "$d/frozen/LEAN_FILES" || return 1
  ( cd "$d/lean" && git rev-parse HEAD ) > "$d/frozen/LEAN_HEAD" || return 1
  ( cd "$d/hs" && git rev-parse HEAD ) > "$d/frozen/HS_BASE" || return 1
  ( cd "$d/hs" && git ls-files 'lib' | sort ) > "$d/frozen/HS_FILES" || return 1
  must_nonempty "$d/frozen/HS_FILES" || return 1
  : > "$d/frozen/LEAN_HASHES"
  while IFS= read -r p; do
    [ -z "$p" ] && continue
    h="$(git -C "$d/lean" show "HEAD:$p" | sha256sum | cut -d' ' -f1)" || return 1
    printf '%s  %s\n' "$h" "$p" >> "$d/frozen/LEAN_HASHES"
  done < "$d/frozen/LEAN_FILES"
  must_nonempty "$d/frozen/LEAN_HASHES" || return 1
  : > "$d/frozen/HS_SOURCE_HASHES"
  while IFS= read -r p; do
    [ -z "$p" ] && continue
    h="$(git -C "$d/hs" show "HEAD:$p" | sha256sum | cut -d' ' -f1)" || return 1
    printf '%s  %s\n' "$h" "$p" >> "$d/frozen/HS_SOURCE_HASHES"
  done < "$d/frozen/HS_FILES"
  must_nonempty "$d/frozen/HS_SOURCE_HASHES" || return 1
  cat > "$d/frozen/JOIN_ROWS" <<'EOF'
Verdict|KelGroups.Vote.Types|Verdict|REQ-A
Ballot|KelGroups.Vote.Types|Ballot|REQ-B
Foo|KelGroups.Vote.Types|Foo|REQ-C
SideProp|KelGroups.Vote.Types||
EOF
  must_nonempty "$d/frozen/JOIN_ROWS" || return 1
  printf '%s\n' "4" > "$d/frozen/FROZEN_ROWS"
  : > "$d/frozen/IDENTITY_MAP"
  while IFS= read -r p; do
    [ -z "$p" ] && continue
    printf '%s rows: present\n' "$p" >> "$d/frozen/IDENTITY_MAP"
  done < "$d/frozen/LEAN_FILES"
  printf '%s\n' "lean/KelGroups/Vote/Invariants.lean expected-empty (proof-only)" >> "$d/frozen/IDENTITY_MAP"
  printf '%s\n' "lean/KelGroups/Vote/Tests.lean expected-empty (witness-only)" >> "$d/frozen/IDENTITY_MAP"
  must_nonempty "$d/frozen/IDENTITY_MAP" || return 1
  cat > "$d/frozen/LEG4_LOG" <<'EOF'
PASS: REQ-A OK
PASS: REQ-B OK
PASS: REQ-C OK
EOF
  must_nonempty "$d/frozen/LEG4_LOG" || return 1
  printf '%s\n' "KelGroups.Vote.Types" > "$d/frozen/HS_MODULES"
  touch "$d/frozen/BUILD_MARKER" || return 1
  printf '%s\n' "exit=0" > "$d/frozen/BUILD_RECEIPT" || return 1
  printf '%s\n' "id=fx-build-1" >> "$d/frozen/BUILD_RECEIPT" || return 1
  cat > "$d/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi" <<'EOF'
Verdict
Ballot
Foo
FooBar
Foo2
EOF
  must_nonempty "$d/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi" || return 1
  ( cd "$d/hs/dist-newstyle/b1/KelGroups/Vote" && sha256sum Types.hi | sed 's| .*||' > "$d/frozen/hi-sha" ) || return 1
  mkdir -p "$d/frozen/hi"
  cp "$d/frozen/hi-sha" "$d/frozen/hi/KelGroups_Vote_Types.dump.sha256" || return 1
  cat > "$d/stubbin/ghc" <<'EOF'
#!/usr/bin/env bash
# STUBBED ghc shim — plumbing only (serves fixture bytes for --show-iface;
# proves selection/freshness/hash path logic, NEVER compiler-output
# compatibility). Real ghc behavior = prerequisite P1 at freeze.
if [ "${1:-}" = "--show-iface" ] && [ -r "${2:-}" ]; then cat "$2"; exit 0; fi
printf '%s\n' "stub-ghc: refused ($*)" >&2; exit 99
EOF
  chmod +x "$d/stubbin/ghc" || return 1
}

case_env() { # $1=case dir : export frozen config for the script under test
  unset OVERLAY_LEAN OVERLAY_HS OVERLAY_BASE_OID OVERLAY_EXPORT_DIFF CASE_ENV_OK
  local d="$1"
  FROZEN_LEAN_HEAD="$(cat "$d/frozen/LEAN_HEAD")"
  FROZEN_HS_BASE="$(cat "$d/frozen/HS_BASE")"
  REACTIVE_GAS="$d/lean"; KELGROUPS_WORKTREE="$d/hs"; EVIDENCE_DIR="$d/ev"
  IDENTITY_MAP="$d/frozen/IDENTITY_MAP"; JOIN_ROWS="$d/frozen/JOIN_ROWS"
  LEG4_LOG="$d/frozen/LEG4_LOG"; BUILD_MARKER="$d/frozen/BUILD_MARKER"
  BUILD_RECEIPT="$d/frozen/BUILD_RECEIPT"
  LEAN_FILES="$(cat "$d/frozen/LEAN_FILES")"; LEAN_HASHES="$(cat "$d/frozen/LEAN_HASHES")"
  LEAN_SCOPE="lean/KelGroups/Vote lean/KelGroups/Integration.lean lean/KelGroups/State.lean lean/KelGroups/Validate.lean lean/KelGroups/Event.lean lean/KelGroups/Types.lean"
  HS_FILES="$(cat "$d/frozen/HS_FILES")"; HS_SCOPE="lib"
  HS_SOURCE_HASHES="$(cat "$d/frozen/HS_SOURCE_HASHES")"
  HS_MODULES="$(cat "$d/frozen/HS_MODULES")"; FROZEN_HI_DIR="$d/frozen/hi"
  FROZEN_ROWS="$(cat "$d/frozen/FROZEN_ROWS")"
  export FROZEN_LEAN_HEAD FROZEN_HS_BASE REACTIVE_GAS KELGROUPS_WORKTREE EVIDENCE_DIR IDENTITY_MAP JOIN_ROWS LEG4_LOG BUILD_MARKER BUILD_RECEIPT LEAN_FILES LEAN_HASHES LEAN_SCOPE HS_FILES HS_SCOPE HS_SOURCE_HASHES HS_MODULES FROZEN_HI_DIR FROZEN_ROWS
  export PATH="$d/stubbin:$ORIG_PATH"
  mkdir -p "$EVIDENCE_DIR" || return 1
  export CASE_ENV_OK=1
}

run_case() { # $1=name $2=want-exit(0|1|3) $3...=required substrings in stdout+stderr
  local name="$1" want="$2"; shift 2
  local cdir="$CASES/$name"; rm -rf "$cdir"; mkdir -p "$cdir/ev"
  unset CASE_ENV_OK
  eval "$SETUP_FN" >"$cdir/setup.log" 2>&1
  setup_rc=$?
  if [ "$setup_rc" -ne 0 ]; then printf '%s\n' "CASE $name: SETUP-FAILED (rc=$setup_rc, see setup.log)"; SETUP_FAILS=$((SETUP_FAILS+1)); return; fi
  if [ "${CASE_ENV_OK:-}" != "1" ]; then printf '%s\n' "CASE $name: SETUP-FAILED (case_env did not complete; structural)"; SETUP_FAILS=$((SETUP_FAILS+1)); return; fi
  if [ ! -f "$cdir/frozen/LEAN_FILES" ] || [ ! -d "$cdir/ev" ]; then printf '%s\n' "CASE $name: SETUP-FAILED (structural: case root malformed — nested-copy class)"; SETUP_FAILS=$((SETUP_FAILS+1)); return; fi
  MODE="$CMODE" OVERLAY_LEAN="${OVERLAY_LEAN:-}" OVERLAY_HS="${OVERLAY_HS:-}" OVERLAY_BASE_OID="${OVERLAY_BASE_OID:-}" OVERLAY_EXPORT_DIFF="${OVERLAY_EXPORT_DIFF:-}" \
    bash "$SCRIPT" >"$cdir/stdout" 2>"$cdir/stderr"; printf '%s' "$?" >"$cdir/exit"
  local got; got="$(cat "$cdir/exit")"
  local ok=1 pat
  [ "$got" = "$want" ] || ok=0
  for pat in "$@"; do grep -qF -e "$pat" "$cdir/stdout" "$cdir/stderr" || ok=0; done
  if [ "$ok" -eq 1 ]; then printf '%s\n' "CASE $name: AS-PREDICTED (exit=$got)"; else printf '%s\n' "CASE $name: MISPREDICT (exit=$got; see $name/stdout+stderr)"; MISPREDICTS=$((MISPREDICTS+1)); fi
}

assert_absent() { # $1=case $2...=patterns that must NOT appear in stdout+stderr
  local name="$1"; shift
  local cdir="$CASES/$name" pat bad=0
  for pat in "$@"; do
    if grep -qF -e "$pat" "$cdir/stdout" "$cdir/stderr"; then
      printf '%s\n' "CASE $name: MISPREDICT (forbidden pattern present: $pat)"; bad=1
    fi
  done
  [ "$bad" -eq 0 ] || MISPREDICTS=$((MISPREDICTS+1))
}

ORIG_PATH="$PATH"
rm -rf "$FX" "$EV" "$CASES"; mkdir -p "$FX" "$EV" "$CASES"
setup_tree "$FX/tmpl" > "$FX/template-setup.log" 2>&1 || { printf '%s\n' "TEMPLATE-FAILED: fixture freeze broken (see fx/template-setup.log)"; exit 3; }

# A1 baseline GREEN (GREEN-path evidence only — PASS lines never cited as firing)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A1"; case_env "$CASES/A1" >/dev/null'
CMODE=live; run_case A1 0 "FINAL: PASS" "traversed=4 frozen=4" "3-pinned KelGroups.Vote.Types" "4-type Foo exact"
assert_absent A1 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
# A2 deleted-mapping -> count RED (mapping integrity)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A2"; grep -v "^Ballot|" "$CASES/A2/frozen/JOIN_ROWS" > "$CASES/A2/frozen/JOIN_ROWS.tmp"; mv "$CASES/A2/frozen/JOIN_ROWS.tmp" "$CASES/A2/frozen/JOIN_ROWS"; case_env "$CASES/A2" >/dev/null'
CMODE=live; run_case A2 1 "4-count" "traversed=3 frozen=4"
# A3 empty mapping -> vacuity REFUSAL (no verdict definable on zero rows)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A3"; : > "$CASES/A3/frozen/JOIN_ROWS"; case_env "$CASES/A3" >/dev/null'
CMODE=live; run_case A3 3 "ZERO data rows" "vacuous pass REFUSED"
# A4 comments-only mapping -> row_count 0 (parsed-construct proof: a
# literal-paren reading would count >=1 and differ here)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A4"; printf "%s\n" "# mapping under review" "" "   # indented comment" "" > "$CASES/A4/frozen/JOIN_ROWS"; case_env "$CASES/A4" >/dev/null'
CMODE=live; run_case A4 3 "ZERO data rows"
# A5 lose-one + duplicate-one (same-size) -> UNIQUENESS fires, count passes
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A5"; grep -v "^Ballot|" "$CASES/A5/frozen/JOIN_ROWS" > "$CASES/A5/frozen/JOIN_ROWS.tmp"; printf "%s\n" "Verdict|KelGroups.Vote.Types|Verdict|REQ-A" >> "$CASES/A5/frozen/JOIN_ROWS.tmp"; mv "$CASES/A5/frozen/JOIN_ROWS.tmp" "$CASES/A5/frozen/JOIN_ROWS"; case_env "$CASES/A5" >/dev/null'
CMODE=live; run_case A5 1 "duplicate mapping rows" "traversed=4 frozen=4"
assert_absent A5 "4-count"
# A6 duplicate-row addition (count+1) -> count RED
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A6"; printf "%s\n" "Verdict|KelGroups.Vote.Types|Verdict|REQ-A" >> "$CASES/A6/frozen/JOIN_ROWS"; case_env "$CASES/A6" >/dev/null'
CMODE=live; run_case A6 1 "traversed=5 frozen=4"
# A7 empty dump -> RED (emission ran per receipt, product empty)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A7"; : > "$CASES/A7/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/A7" >/dev/null'
CMODE=live; run_case A7 1 "empty dump" "FINAL: RED"
# A8 missing .hi -> REFUSAL (expected artifact unresolvable; unfounded run)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A8"; rm "$CASES/A8/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/A8" >/dev/null'
CMODE=live; run_case A8 3 "ZERO .hi candidates"
# A9 duplicate .hi -> REFUSAL (ambiguous selection; never silent pick)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A9"; mkdir -p "$CASES/A9/hs/dist-newstyle/b2/KelGroups/Vote"; cp "$CASES/A9/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi" "$CASES/A9/hs/dist-newstyle/b2/KelGroups/Vote/Types.hi"; case_env "$CASES/A9" >/dev/null'
CMODE=live; run_case A9 3 "ambiguous selection REFUSED"
# A10 stale .hi -> RED (resolved artifact, old — subject-state failure)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A10"; touch -d "1999-01-01" "$CASES/A10/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/A10" >/dev/null'
CMODE=live; run_case A10 1 "stale inheritance refused"
# A11 poisoned-preseed dump -> emission OVERWRITES -> GREEN (no-inheritance
# proof). Pre-asserted: poison present before run (vacuity impossible).
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A11"; case_env "$CASES/A11" >/dev/null; printf "%s\n" "POISON" > "$CASES/A11/ev/hi-KelGroups_Vote_Types.dump"; grep -q POISON "$CASES/A11/ev/hi-KelGroups_Vote_Types.dump" || return 1'
CMODE=live; run_case A11 0 "FINAL: PASS"
assert_absent A11 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
# A12 receipt exit=1 -> REFUSAL (no producer to attribute to)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A12"; printf "%s\n" "exit=1" > "$CASES/A12/frozen/BUILD_RECEIPT"; printf "%s\n" "id=fx-bad" >> "$CASES/A12/frozen/BUILD_RECEIPT"; case_env "$CASES/A12" >/dev/null'
CMODE=live; run_case A12 3 "no producer evidence"
# A13 receipt missing -> REFUSAL (unfounded run)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A13"; rm "$CASES/A13/frozen/BUILD_RECEIPT"; case_env "$CASES/A13" >/dev/null'
CMODE=live; run_case A13 3 "BUILD_RECEIPT absent"
# A14 unknown MODE -> REFUSAL (no silent live-fallthrough)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A14"; case_env "$CASES/A14" >/dev/null'
CMODE=banana; run_case A14 3 "unknown MODE"
# A15 adversarial leg4 (PASS-A + FAILED-B + SKIPPED-C + bare-D... D has no
# row here: mapping needs A/B/C; C appears ONLY bare) -> RED naming B and C
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A15"; printf "%s\n" "PASS: REQ-A OK" "FAILED: REQ-B at step 2" "SKIPPED: REQ-C holiday" "note REQ-C seen in plan" > "$CASES/A15/frozen/LEG4_LOG"; case_env "$CASES/A15" >/dev/null'
CMODE=live; run_case A15 1 "REQ-B has NO successful" "REQ-C has NO successful"
# A16 leg4 zero-success -> RED (evidence of execution with nothing passing)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A16"; printf "%s\n" "FAILED: REQ-A x" "FAILED: REQ-B y" > "$CASES/A16/frozen/LEG4_LOG"; case_env "$CASES/A16" >/dev/null'
CMODE=live; run_case A16 1 "ZERO successful execution records"
# A17 dirty tree -> clean-sample RED (subject state)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A17"; printf "%s\n" "-- dirt" >> "$CASES/A17/lean/lean/KelGroups/Vote/Types.lean"; case_env "$CASES/A17" >/dev/null'
CMODE=live; run_case A17 1 "1-clean" "uncommitted bytes"
# A18 committed change -> pin-lean RED (reference moved) + frozen-oid reads
# PASS (old-bytes-equal is CORRECT — reference-vs-content separation made
# visible; the two layers report exactly what each sees)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A18"; printf "%s\n" "-- moved" >> "$CASES/A18/lean/lean/KelGroups/Vote/Types.lean"; ( cd "$CASES/A18/lean" && git add -A && git_env git commit -qm moved ); case_env "$CASES/A18" >/dev/null'
CMODE=live; run_case A18 1 "1-position-lean" "rebind procedure"
assert_absent A18 "differs from frozen bytes" "ZERO data rows" "unbound config"
# A19 short-oid config -> mismatch RED (full-oid exactness)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A19"; case_env "$CASES/A19" >/dev/null; FROZEN_LEAN_HEAD="$(printf "%s" "$FROZEN_LEAN_HEAD" | cut -c1-7)"; export FROZEN_LEAN_HEAD'
CMODE=live; run_case A19 1 "1-position-lean"
# A20 overlay edited (lean) -> hash-trigger RED + bound diff; metadata
# constant (overlay-base PASS). Pre-asserted: export diff nonempty.
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A20"; mkdir -p "$CASES/A20/ovl" "$CASES/A20/ovl-hs" "$CASES/A20/ovl-base"; ( cd "$CASES/A20/lean" && git archive HEAD | tar -x -C "$CASES/A20/ovl-base" ); ( cd "$CASES/A20/lean" && git archive HEAD | tar -x -C "$CASES/A20/ovl" ); ( cd "$CASES/A20/hs" && git archive HEAD | tar -x -C "$CASES/A20/ovl-hs" ); printf "%s\n" "-- overlay edit" >> "$CASES/A20/ovl/lean/KelGroups/Vote/Types.lean"; diff -r "$CASES/A20/ovl-base" "$CASES/A20/ovl" > "$CASES/A20/export.diff" || true; test -s "$CASES/A20/export.diff" || return 1; printf "%s\n" "Verdict" "Ballot" "Foo" "FooBar" "Foo2" > "$CASES/A20/ev-staged-hi.dump"; case_env "$CASES/A20" >/dev/null; cp "$CASES/A20/ev-staged-hi.dump" "$CASES/A20/ev/hi-KelGroups_Vote_Types.dump"; export OVERLAY_LEAN="$CASES/A20/ovl" OVERLAY_HS="$CASES/A20/ovl-hs" OVERLAY_BASE_OID="$(cat "$CASES/A20/frozen/LEAN_HEAD")" OVERLAY_EXPORT_DIFF="$CASES/A20/export.diff"'
CMODE=overlay; run_case A20 1 "1-hash lean/KelGroups/Vote/Types.lean" "re-review required" "0-overlay-base"
assert_absent A20 "1-pin-lean" "FINAL: PASS"
# A21 overlay clean -> GREEN (overlay-GREEN path incl. staged-dump join)
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A21"; mkdir -p "$CASES/A21/ovl" "$CASES/A21/ovl-hs"; ( cd "$CASES/A21/lean" && git archive HEAD | tar -x -C "$CASES/A21/ovl" ); ( cd "$CASES/A21/hs" && git archive HEAD | tar -x -C "$CASES/A21/ovl-hs" ); diff -r "$CASES/A21/ovl" "$CASES/A21/ovl" > "$CASES/A21/export.diff" || true; case_env "$CASES/A21" >/dev/null; printf "%s\n" "Verdict" "Ballot" "Foo" "FooBar" "Foo2" > "$CASES/A21/ev/hi-KelGroups_Vote_Types.dump"; export OVERLAY_LEAN="$CASES/A21/ovl" OVERLAY_HS="$CASES/A21/ovl-hs" OVERLAY_BASE_OID="$(cat "$CASES/A21/frozen/LEAN_HEAD")" OVERLAY_EXPORT_DIFF="$CASES/A21/export.diff"'
CMODE=overlay; run_case A21 0 "FINAL: PASS" "0-overlay-base"
assert_absent A21 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
# A22 source-add-helper on the BOUND overlay, metadata UNCHANGED: overlay HS
# tree gains one unexported helper line; source-hash channel FIRES with file
# attribution while pins/file-sets/dumps stay silent. Independence here is
# ceteris-paribus design (single edited layer fires, reference layers
# silent), never line-count. Pre-asserted: export diff nonempty.
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A22"; mkdir -p "$CASES/A22/ovl" "$CASES/A22/ovl-hs" "$CASES/A22/ovl-hs-base"; ( cd "$CASES/A22/lean" && git archive HEAD | tar -x -C "$CASES/A22/ovl" ); ( cd "$CASES/A22/hs" && git archive HEAD | tar -x -C "$CASES/A22/ovl-hs-base" ); ( cd "$CASES/A22/hs" && git archive HEAD | tar -x -C "$CASES/A22/ovl-hs" ); cat >> "$CASES/A22/ovl-hs/lib/KelGroups/Vote/Types.hs" <<EOF
helperNew :: Int
helperNew = 2
EOF
diff -r "$CASES/A22/ovl-hs-base" "$CASES/A22/ovl-hs" > "$CASES/A22/export-hs.diff" || true; test -s "$CASES/A22/export-hs.diff" || return 1; printf "%s\n" "Verdict" "Ballot" "Foo" "FooBar" "Foo2" > "$CASES/A22/ev-staged-hi.dump"; case_env "$CASES/A22" >/dev/null; cp "$CASES/A22/ev-staged-hi.dump" "$CASES/A22/ev/hi-KelGroups_Vote_Types.dump"; export OVERLAY_LEAN="$CASES/A22/ovl" OVERLAY_HS="$CASES/A22/ovl-hs" OVERLAY_BASE_OID="$(cat "$CASES/A22/frozen/LEAN_HEAD")" OVERLAY_EXPORT_DIFF="$CASES/A22/export-hs.diff"'
CMODE=overlay; run_case A22 1 "1-hash-hs: lib/KelGroups/Vote/Types.hs differs" "0-overlay-base" "FINAL: RED"
assert_absent A22 "1-pin-lean" "rebind procedure" "FINAL: PASS"
# A23a/b/c/d setup failures -> REFUSAL (unfounded runs) AND no FINAL PASS
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A23a"; rm "$CASES/A23a/frozen/BUILD_MARKER"; case_env "$CASES/A23a" >/dev/null'
CMODE=live; run_case A23a 3 "BUILD_MARKER absent"
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A23b"; rm "$CASES/A23b/frozen/JOIN_ROWS"; mkdir "$CASES/A23b/frozen/JOIN_ROWS"; case_env "$CASES/A23b" >/dev/null'
CMODE=live; run_case A23b 3 "not a regular file"
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A23c"; rm "$CASES/A23c/frozen/LEG4_LOG"; case_env "$CASES/A23c" >/dev/null'
CMODE=live; run_case A23c 3 "not a regular file"
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A23d"; case_env "$CASES/A23d" >/dev/null; unset FROZEN_ROWS LEG4_LOG'
CMODE=live; run_case A23d 3 "unbound config:" "FROZEN_ROWS" "LEG4_LOG"
if grep -l "FINAL: PASS" "$CASES"/A23*/stdout 2>/dev/null | grep -q .; then printf '%s\n' "CASE A23e: MISPREDICT (setup failure printed PASS)"; MISPREDICTS=$((MISPREDICTS+1)); else printf '%s\n' "CASE A23e: AS-PREDICTED (no setup failure prints PASS)"; fi
# A24 exact-substring need (`Fo`: substring-present, line-absent) -> RED
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A24"; printf "%s\n" "Verdict|KelGroups.Vote.Types|Verdict|REQ-A" "Ballot|KelGroups.Vote.Types|Ballot|REQ-B" "Foo|KelGroups.Vote.Types|Foo|REQ-C" "SideProp|KelGroups.Vote.Types||" "FoNeed|KelGroups.Vote.Types|Fo|REQ-D" > "$CASES/A24/frozen/JOIN_ROWS"; printf "%s\n" "5" > "$CASES/A24/frozen/FROZEN_ROWS"; case_env "$CASES/A24" >/dev/null'
CMODE=live; run_case A24 1 "expected exact line [Fo] ABSENT"
# A25 multi-missing config -> SINGLE refusal naming ALL
SETUP_FN='cp -r "$FX/tmpl/." "$CASES/A25"; case_env "$CASES/A25" >/dev/null; unset JOIN_ROWS BUILD_RECEIPT'
CMODE=live; run_case A25 3 "unbound config:" "JOIN_ROWS" "BUILD_RECEIPT"

printf '%s\n' "===== SUITE: $([ "$SETUP_FAILS" -eq 0 ] && [ "$MISPREDICTS" -eq 0 ] && echo PASS || echo FAIL) (setup-failures=$SETUP_FAILS mispredicts=$MISPREDICTS) =====" | tee "$EV/SUITE.log"
[ "$SETUP_FAILS" -eq 0 ] && [ "$MISPREDICTS" -eq 0 ]
