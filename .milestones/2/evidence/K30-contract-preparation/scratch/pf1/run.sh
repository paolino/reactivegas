#!/usr/bin/env bash
# Preflight runner — NOTE-007 script-plumbing campaign, invocation 1 (or rerun).
# TOOL ALLOWLIST (desk grant): bash, git (synthetic fixture repos ONLY),
# coreutils (touch/find/sort/diff/sha256sum/cat/mkdir/rm/cp/tar), grep, sed.
# NEVER invoked: lean/lake/ghc(real)/cabal/nix (a PATH shim named `ghc`
# serves fixture bytes for --show-iface plumbing; STUBBED metadata proves
# PLUMBING ONLY — never compiler-output compatibility, never coverage).
# Idempotent: wipes and rebuilds CASES on every run. Suite verdict =
# every case behaves AS PREDICTED; evidence per case (stdout/stderr/exit).
set -u
ROOT="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/handoffs/T30-DRIFT-LEG-r6.sh"
FX="$ROOT/fx"; EV="$ROOT/ev"; CASES="$ROOT/cases"
SUITE_FAIL=0
ORIG_PATH="$PATH"  # captured BEFORE any case_env prepend (NixOS has no /usr/bin coreutils)
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null
git_env() { GIT_AUTHOR_NAME=fx GIT_AUTHOR_EMAIL=fx@fx GIT_COMMITTER_NAME=fx GIT_COMMITTER_EMAIL=fx@fx "$@"; }

setup_tree() { # $1=dest : pristine fixture repos + frozen values + stubs
  local d="$1"; rm -rf "$d"; mkdir -p "$d/lean/lean/KelGroups/Vote" "$d/lean/lean/KelGroups" "$d/hs/dist-newstyle/b1/KelGroups/Vote" "$d/frozen" "$d/stubbin"
  local i f
  for f in Types State Event Validate Fold Invariants Tests; do
    printf '-- fixture %s\ninductive Fx%s where\n  | mkA\n  | mkB\n' "$f" "$f" > "$d/lean/lean/KelGroups/Vote/$f.lean"
  done
  for f in Integration State Validate Event Types; do
    printf -- '-- fixture base %s\ndef fxBase%s : Nat := 1\n' "$f" "$f" > "$d/lean/lean/KelGroups/$f.lean"
  done
  ( cd "$d/lean" && git init -q . && git add -A && git_env git commit -qm frozen )
  mkdir -p "$d/hs/lib"
  ( cd "$d/hs" && git init -q . && git add -A 2>/dev/null; git_env git commit -qm frozen --allow-empty )
  # frozen extent (12 repo-relative paths) + pins + hashes (THE freeze step)
  ( cd "$d/lean" && git ls-files 'lean/KelGroups/Vote' 'lean/KelGroups/Integration.lean' 'lean/KelGroups/State.lean' 'lean/KelGroups/Validate.lean' 'lean/KelGroups/Event.lean' 'lean/KelGroups/Types.lean' | sort ) > "$d/frozen/LEAN_FILES"
  ( cd "$d/lean" && git rev-parse HEAD ) > "$d/frozen/LEAN_HEAD"
  ( cd "$d/hs" && git rev-parse HEAD ) > "$d/frozen/HS_BASE"
  : > "$d/frozen/LEAN_HASHES"
  while IFS= read -r p; do
    [ -z "$p" ] && continue
    h="$(git -C "$d/lean" show "HEAD:$p" | sha256sum | cut -d' ' -f1)"
    printf '%s  %s\n' "$h" "$p" >> "$d/frozen/LEAN_HASHES"
  done < "$d/frozen/LEAN_FILES"
  # mapping + identity excerpt + leg4 log + dumps (STUBBED compiler output)
  cat > "$d/frozen/JOIN_ROWS" <<'EOF'
Verdict|KelGroups.Vote.Types|Verdict|REQ-A
Ballot|KelGroups.Vote.Types|Ballot|REQ-B
Foo|KelGroups.Vote.Types|Foo|REQ-C
SideProp|KelGroups.Vote.Types||
EOF
  printf '4\n' > "$d/frozen/FROZEN_ROWS"
  : > "$d/frozen/IDENTITY_MAP"
  while IFS= read -r p; do [ -z "$p" ] && continue; printf '%s rows: present\n' "$p" >> "$d/frozen/IDENTITY_MAP"; done < "$d/frozen/LEAN_FILES"
  printf 'lean/KelGroups/Vote/Invariants.lean expected-empty (proof-only)\nlean/KelGroups/Tests.lean expected-empty (witness-only)\n' >> "$d/frozen/IDENTITY_MAP"
  printf 'oneAdmin ran REQ-A green\nballot REQ-B recorded\nroundtrip REQ-C ok\n' > "$d/frozen/LEG4_LOG"
  printf 'KelGroups.Vote.Types\n' > "$d/frozen/HS_MODULES"
  touch "$d/frozen/BUILD_MARKER"  # marker BEFORE emission (ordering rule)
  printf 'Verdict\nBallot\nFoo\nFooBar\nFoo2\n' > "$d/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"
  ( cd "$d/hs/dist-newstyle/b1/KelGroups/Vote" && sha256sum Types.hi > "$d/frozen/Types.hi.dump.sha256" 2>/dev/null; cp Types.hi "$d/frozen/Types.hi.dump.golden" )
  mkdir -p "$d/frozen/hi"
  cp "$d/frozen/Types.hi.dump.sha256" "$d/frozen/hi/KelGroups_Vote_Types.dump.sha256"
  # PATH shim: serves fixture bytes for `ghc --show-iface <path>` (STUBBED).
  cat > "$d/stubbin/ghc" <<'EOF'
#!/usr/bin/env bash
# STUBBED ghc shim — plumbing only (serves fixture bytes; proves path/
# selection/freshness/hash logic, NEVER compiler-output compatibility).
if [ "${1:-}" = "--show-iface" ] && [ -r "${2:-}" ]; then cat "$2"; exit 0; fi
echo "stub-ghc: refused ($*)" >&2; exit 99
EOF
  chmod +x "$d/stubbin/ghc"
}

# base env for a case dir $1 (paths absolute)
case_env() {
  unset OVERLAY_LEAN OVERLAY_BASE_OID OVERLAY_EXPORT_DIFF
  local d="$1"
  FROZEN_LEAN_HEAD="$(cat "$d/frozen/LEAN_HEAD")"
  FROZEN_HS_BASE="$(cat "$d/frozen/HS_BASE")"
  REACTIVE_GAS="$d/lean"; KELGROUPS_WORKTREE="$d/hs"; EVIDENCE_DIR="$d/ev"
  IDENTITY_MAP="$d/frozen/IDENTITY_MAP"; JOIN_ROWS="$d/frozen/JOIN_ROWS"
  LEG4_LOG="$d/frozen/LEG4_LOG"; BUILD_MARKER="$d/frozen/BUILD_MARKER"
  LEAN_FILES="$(cat "$d/frozen/LEAN_FILES")"; LEAN_HASHES="$(cat "$d/frozen/LEAN_HASHES")"
  LEAN_SCOPE="lean/KelGroups/Vote lean/KelGroups/Integration.lean lean/KelGroups/State.lean lean/KelGroups/Validate.lean lean/KelGroups/Event.lean lean/KelGroups/Types.lean"
  HS_MODULES="$(cat "$d/frozen/HS_MODULES")"; FROZEN_HI_DIR="$d/frozen/hi"
  FROZEN_ROWS="$(cat "$d/frozen/FROZEN_ROWS")"
  export FROZEN_LEAN_HEAD FROZEN_HS_BASE REACTIVE_GAS KELGROUPS_WORKTREE EVIDENCE_DIR IDENTITY_MAP JOIN_ROWS LEG4_LOG BUILD_MARKER LEAN_FILES LEAN_HASHES LEAN_SCOPE HS_MODULES FROZEN_HI_DIR FROZEN_ROWS
  export PATH="$d/stubbin:$ORIG_PATH"
  mkdir -p "$EVIDENCE_DIR"
}

run_case() { # $1=name $2=expected exit(0|non0) $3... = required stdout patterns (each grep -q); env PRESET via current shell vars + $SETUP_FN
  local name="$1" want="$2"; shift 2
  local cdir="$CASES/$name"; rm -rf "$cdir"; mkdir -p "$cdir/ev"
  eval "$SETUP_FN" >"$cdir/setup.log" 2>&1
  setup_rc=$?
  if [ "$setup_rc" -ne 0 ]; then echo "CASE $name: SETUP-FAILED (rc=$setup_rc, see setup.log)"; SUITE_FAIL=1; return; fi
  MODE="$CMODE" OVERLAY_LEAN="${OVERLAY_LEAN:-}" OVERLAY_BASE_OID="${OVERLAY_BASE_OID:-}" OVERLAY_EXPORT_DIFF="${OVERLAY_EXPORT_DIFF:-}" \
    bash "$SCRIPT" >"$cdir/stdout" 2>"$cdir/stderr"; echo "$?" >"$cdir/exit"
  local got; got="$(cat "$cdir/exit")"
  local ok=1 pat
  if [ "$want" = "0" ]; then [ "$got" -eq 0 ] || ok=0; else [ "$got" -ne 0 ] || ok=0; fi
  for pat in "$@"; do grep -qF "$pat" "$cdir/stdout" "$cdir/stderr" || ok=0; done
  if [ "$ok" -eq 1 ]; then echo "CASE $name: AS-PREDICTED (exit=$got)"; else echo "CASE $name: MISPREDICT (exit=$got; see $name/stdout)"; SUITE_FAIL=1; fi
}

rm -rf "$FX" "$EV" "$CASES"; mkdir -p "$FX" "$EV" "$CASES"
# ---- template freeze (the honest freeze simulation) ----
setup_tree "$FX/tmpl"

# P1 baseline GREEN
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P1"; EVIDENCE_DIR="$CASES/P1/ev"; mkdir -p "$EVIDENCE_DIR"; case_env "$CASES/P1" >/dev/null'
CMODE=live; run_case P1 0 "FINAL: PASS" "traversed=4 frozen=4" "3-pinned KelGroups.Vote.Types" "4-type Foo exact"
# P2 deleted-mapping (JOIN_ROWS 4->3, FROZEN_ROWS stays 4) -> count RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P2"; grep -v "^Ballot|" "$CASES/P2/frozen/JOIN_ROWS" > "$CASES/P2/frozen/JOIN_ROWS.tmp"; mv "$CASES/P2/frozen/JOIN_ROWS.tmp" "$CASES/P2/frozen/JOIN_ROWS"; case_env "$CASES/P2" >/dev/null'
CMODE=live; run_case P2 non0 "4-count" "traversed=3 frozen=4"
# P3 empty mapping -> vacuity REFUSED (never pass)
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P3"; : > "$CASES/P3/frozen/JOIN_ROWS"; case_env "$CASES/P3" >/dev/null'
CMODE=live; run_case P3 non0 "ZERO rows" "vacuous pass REFUSED"
# P4 empty live dump -> RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P4"; : > "$CASES/P4/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/P4" >/dev/null'
CMODE=live; run_case P4 non0 "empty dump" "FINAL: RED"
# P5a missing .hi artifact -> selection ZERO RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P5a"; rm "$CASES/P5a/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/P5a" >/dev/null'
CMODE=live; run_case P5a non0 "ZERO .hi candidates"
# P5b unreadable JOIN_ROWS (directory as file) -> RED recorded
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P5b"; rm "$CASES/P5b/frozen/JOIN_ROWS"; mkdir "$CASES/P5b/frozen/JOIN_ROWS"; case_env "$CASES/P5b" >/dev/null'
CMODE=live; run_case P5b non0 "not a regular file"
# P6 exact-vs-substring: need `Fo` (substring-present, line-absent) -> RED; Foo-row GREEN in P1 already proves the other direction
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P6"; printf "Verdict|KelGroups.Vote.Types|Verdict|REQ-A\nBallot|KelGroups.Vote.Types|Ballot|REQ-B\nFoo|KelGroups.Vote.Types|Foo|REQ-C\nSideProp|KelGroups.Vote.Types||\nFoNeed|KelGroups.Vote.Types|Fo|REQ-D\n" > "$CASES/P6/frozen/JOIN_ROWS"; printf "5\n" > "$CASES/P6/frozen/FROZEN_ROWS"; case_env "$CASES/P6" >/dev/null'
CMODE=live; run_case P6 non0 "expected exact line [Fo] ABSENT"
# P7 live-side deletion (dump minus Ballot line, frozen hash kept) -> tripwire RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P7"; grep -v "^Ballot$" "$CASES/P7/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi" > "$CASES/P7/hs/dist-newstyle/b1/Types.hi.tmp"; mv "$CASES/P7/hs/dist-newstyle/b1/Types.hi.tmp" "$CASES/P7/hs/dist-newstyle/b1/Types.hi"; case_env "$CASES/P7" >/dev/null'
CMODE=live; run_case P7 non0 "interface bytes drifted"
# P8a duplicate .hi candidates -> ambiguity RED (never silent pick)
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P8a"; mkdir -p "$CASES/P8a/hs/dist-newstyle/b2/KelGroups/Vote"; cp "$CASES/P8a/hs/dist-newstyle/b1/Types.hi" "$CASES/P8a/hs/dist-newstyle/b2/KelGroups/Vote/Types.hi"; case_env "$CASES/P8a" >/dev/null'
CMODE=live; run_case P8a non0 "ambiguous selection REFUSED"
# P8b stale .hi (older than marker) -> freshness RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P8b"; touch -d "1999-01-01" "$CASES/P8b/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/P8b" >/dev/null'
CMODE=live; run_case P8b non0 "stale inheritance refused"
# P9a/b/c setup failures: missing marker / JOIN_ROWS dir-missing handled / LEG4_LOG deleted -> exit!=0 AND no FINAL PASS
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P9a"; rm "$CASES/P9a/frozen/BUILD_MARKER"; case_env "$CASES/P9a" >/dev/null'
CMODE=live; run_case P9a non0 "BUILD_MARKER absent"
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P9b"; rm "$CASES/P9b/frozen/LEG4_LOG"; case_env "$CASES/P9b" >/dev/null'
CMODE=live; run_case P9b non0 "not a regular file"
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P9c"; case_env "$CASES/P9c" >/dev/null; unset FROZEN_ROWS'
CMODE=live; run_case P9c non0 "FROZEN_ROWS"
# P9d: assert NONE of P9a/b/c printed FINAL PASS
if grep -l "FINAL: PASS" "$CASES"/P9*/stdout 2>/dev/null | grep -q .; then echo "CASE P9d: MISPREDICT (setup failure printed PASS)"; SUITE_FAIL=1; else echo "CASE P9d: AS-PREDICTED (no setup failure prints PASS)"; fi
# P10b dirty tree -> clean-sample RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P10b"; echo "-- dirt" >> "$CASES/P10b/lean/lean/KelGroups/Vote/Types.lean"; case_env "$CASES/P10b" >/dev/null'
CMODE=live; run_case P10b non0 "1-clean" "uncommitted bytes"
# P10c committed change -> pin RED + hash REDs
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P10c"; echo "-- moved" >> "$CASES/P10c/lean/lean/KelGroups/Vote/Types.lean"; ( cd "$CASES/P10c/lean" && git add -A && git_env git commit -qm moved ); case_env "$CASES/P10c" >/dev/null'
CMODE=live; run_case P10c non0 "1-pin-lean" "rebind procedure"
# P10d short-oid config -> mismatch RED (full-oid exactness)
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P10d"; case_env "$CASES/P10d" >/dev/null; FROZEN_LEAN_HEAD="$(cat "$CASES/P10d/frozen/LEAN_HEAD" | cut -c1-7)"; export FROZEN_LEAN_HEAD'
CMODE=live; run_case P10d non0 "1-pin-lean"
# P11 overlay executable: clean overlay GREEN; edited overlay RED with bound diff
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P11"; mkdir -p "$CASES/P11/ovl-clean" "$CASES/P11/ovl-edit"; ( cd "$CASES/P11/lean" && git archive HEAD | tar -x -C "$CASES/P11/ovl-clean" ); ( cd "$CASES/P11/lean" && git archive HEAD | tar -x -C "$CASES/P11/ovl-edit" ); printf -- "-- overlay edit\n" >> "$CASES/P11/ovl-edit/lean/KelGroups/Vote/Types.lean"; diff -r "$CASES/P11/ovl-clean" "$CASES/P11/ovl-edit" > "$CASES/P11/export.diff" || true; case_env "$CASES/P11" >/dev/null; printf 'Verdict\nBallot\nFoo\nFooBar\nFoo2\n' > "$CASES/P11/ev/hi-KelGroups_Vote_Types.dump"; export OVERLAY_LEAN="$CASES/P11/ovl-edit" OVERLAY_BASE_OID="$(cat "$CASES/P11/frozen/LEAN_HEAD")" OVERLAY_EXPORT_DIFF="$CASES/P11/export.diff"'
CMODE=overlay; run_case P11 non0 "1-hash" "re-review required"
# P11b overlay clean tree -> GREEN (overlay can pass when unchanged)
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P11b"; mkdir -p "$CASES/P11b/ovl-clean"; ( cd "$CASES/P11b/lean" && git archive HEAD | tar -x -C "$CASES/P11b/ovl-clean" ); diff -r "$CASES/P11b/ovl-clean" "$CASES/P11b/ovl-clean" > "$CASES/P11b/export.diff" || true; case_env "$CASES/P11b" >/dev/null; printf 'Verdict\nBallot\nFoo\nFooBar\nFoo2\n' > "$CASES/P11b/ev/hi-KelGroups_Vote_Types.dump"; export OVERLAY_LEAN="$CASES/P11b/ovl-clean" OVERLAY_BASE_OID="$(cat "$CASES/P11b/frozen/LEAN_HEAD")" OVERLAY_EXPORT_DIFF="$CASES/P11b/export.diff"'
CMODE=overlay; run_case P11b 0 "FINAL: PASS"
# P12 unmapped-live-addition (dump + Sneaky line, frozen hash kept) -> tripwire RED
SETUP_FN='cp -r "$FX/tmpl" "$CASES/P12"; printf "Sneaky\n" >> "$CASES/P12/hs/dist-newstyle/b1/KelGroups/Vote/Types.hi"; case_env "$CASES/P12" >/dev/null'
CMODE=live; run_case P12 non0 "interface bytes drifted"

echo "===== SUITE: $([ "$SUITE_FAIL" -eq 0 ] && echo PASS || echo FAIL) =====" | tee "$EV/SUITE.log"
exit "$SUITE_FAIL"
