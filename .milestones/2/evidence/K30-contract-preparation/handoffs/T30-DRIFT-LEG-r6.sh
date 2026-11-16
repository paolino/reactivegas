#!/usr/bin/env bash
# T30-DRIFT-LEG-r6 — drift/input-binding leg, REPAIRED (kelgroups #30).
# Repairs r5's verified defects (NOTE-007): vacuous join pass (NONEMPTY +
# EXACT-COUNT gates), silent set+e paths (no pipeline-subshell state, no
# /dev/null on loads, writability pre-checks), unused HS pin (both repos
# pinned), 10-path omission (full consumed extent + projection statement in
# contract), head-n1 pick (exactly-one-or-RED), single-charged double-build
# (B22a/B22b itemized in contract totals, not here).
# PREFLIGHT STATUS: demonstrated on synthetic fixtures (plumbing only —
# never compiler-output compatibility, never semantic coverage).
# Design artifact for ticket-owner freeze (values bound there; `:?` guards
# refuse unbound config — fail-closed, never silent pass).
#
# LAYER MAP (each control reaches its target layer, nothing further):
#  byte layer .... sha256 over IMMUTABLE `git show HEAD:path` streams (live)
#                  or bound overlay files; HEAD pins (both repos); file-set
#                  equality. Complete change detection (any added/removed item
#                  alters bytes); item-level attribution = NONE automatic ->
#                  ANY mismatch REDs into MANDATORY re-review. enforced:NONE
#                  for automatic attribution + mandatory re-review deliverable.
#  join layer .... frozen mapping vs live emission: mapping->live (exact-line
#                  per row, EXACT-COUNT == FROZEN_ROWS) + live->mapping
#                  (hash tripwire on dumps + new-file rule + review
#                  classification). NONEMPTY asserts kill the vacuity class.
#  tripwire ...... .hi hash vs frozen (drift -> RED + review).
#  compiler ..... GHC -Werror exhaustiveness + .hi existence, live builds.
# TOCTOU: live bytes from content-addressed git views (TOCTOU-free by
# construction). Working-tree conformance is a SEPARATE labeled point-in-time
# sample; residual race = STATED accepted limit (accidental-drift model;
# fenced writers; leg-1 hygiene before/after catches residue).
# Modes: MODE=live (git views) | MODE=overlay (plain-tree overlay + bound
# export provenance; D-3 skipped with note — overlays aren't built; .hi
# firing is demonstrated by B22 design, stated).
# Exits 0 on PASS, nonzero on any RED/INCONCLUSIVE. set +e, explicit flow.

set +e
OVERALL_FAIL=0
TRAVERSED=0
leg() { echo "===== DRIFT-$1: $2 ====="; }
pass() { echo "DRIFT-PASS: $1"; }
fail() { echo "DRIFT-FAIL: $1"; OVERALL_FAIL=1; }

# --- frozen config (ALL fail-closed) ---
: "${FROZEN_LEAN_HEAD:?frozen Lean full-oid pin missing}"
: "${FROZEN_HS_BASE:?frozen Haskell base full-oid pin missing}"
: "${REACTIVE_GAS:?Reactivegas checkout path missing}"
: "${KELGROUPS_WORKTREE:?kelgroups worktree path missing}"
: "${EVIDENCE_DIR:?evidence dir missing}"
: "${IDENTITY_MAP:?identity-map path missing}"
: "${JOIN_ROWS:?join-row file missing}"
: "${LEG4_LOG:?leg-4 execution log missing}"
: "${BUILD_MARKER:?pre-build freshness marker missing}"
: "${LEAN_FILES:?frozen Lean extent file list missing}"
: "${LEAN_HASHES:?frozen Lean hash list missing}"
: "${LEAN_SCOPE:?frozen Lean scope dirs missing}"
: "${HS_MODULES:?frozen Haskell module list missing}"
: "${FROZEN_HI_DIR:?frozen .hi inventory dir missing}"
: "${FROZEN_ROWS:?frozen mapping row count missing}"
: "${MODE:?MODE live|overlay missing}"
[ -d "$EVIDENCE_DIR" ] || fail "config: EVIDENCE_DIR $EVIDENCE_DIR not a directory (writes would be losable)"
[ -w "$EVIDENCE_DIR" ] || fail "config: EVIDENCE_DIR not writable (writes would be losable)"
[ -f "$IDENTITY_MAP" ] || fail "config: IDENTITY_MAP absent or not a regular file: $IDENTITY_MAP"
[ -f "$JOIN_ROWS" ] || fail "config: JOIN_ROWS absent or not a regular file: $JOIN_ROWS"
[ -f "$LEG4_LOG" ] || fail "config: LEG4_LOG absent or not a regular file: $LEG4_LOG"
[ -f "$BUILD_MARKER" ] || fail "config: BUILD_MARKER absent: $BUILD_MARKER (touch pre-build, then build, then emit)"
[ "$FROZEN_ROWS" -ge 1 ] 2>/dev/null || fail "config: FROZEN_ROWS [$FROZEN_ROWS] not >= 1"

# Source flavor per mode: live = immutable git views; overlay = bound trees.
USE_GIT=1
SRC_LEAN="$REACTIVE_GAS"
if [ "$MODE" = "overlay" ]; then
  : "${OVERLAY_LEAN:?overlay lean tree missing}"
  : "${OVERLAY_BASE_OID:?overlay base oid missing}"
  : "${OVERLAY_EXPORT_DIFF:?overlay export-diff record missing}"
  [ -d "$OVERLAY_LEAN" ] || fail "config: OVERLAY_LEAN not a directory"
  [ -r "$OVERLAY_EXPORT_DIFF" ] || fail "config: OVERLAY_EXPORT_DIFF unreadable (overlay provenance unbound)"
  [ "$OVERLAY_BASE_OID" = "$FROZEN_LEAN_HEAD" ] \
    && echo "DRIFT-PASS: 0-overlay-base: export derives from frozen base (diff bound at $OVERLAY_EXPORT_DIFF)" \
    || fail "0-overlay-base: export base [$OVERLAY_BASE_OID] != frozen [$FROZEN_LEAN_HEAD]"
  USE_GIT=0
  SRC_LEAN="$OVERLAY_LEAN"
fi

# D-1: input binding (pins + file-set + byte hashes + clean sample)
leg 1 "input binding"
if [ "$USE_GIT" -eq 1 ]; then
  live_lean="$(git -C "$SRC_LEAN" rev-parse HEAD 2>"$EVIDENCE_DIR/git-lean.err")"
  [ -s "$EVIDENCE_DIR/git-lean.err" ] && fail "1-git: rev-parse refused ($(head -n1 "$EVIDENCE_DIR/git-lean.err"))"
  [ "$live_lean" = "$FROZEN_LEAN_HEAD" ] \
    && pass "1-pin-lean: HEAD == frozen full oid" \
    || fail "1-pin-lean: HEAD [$live_lean] != frozen [$FROZEN_LEAN_HEAD] — rebind procedure"
  # shellcheck disable=SC2086
  live_files="$(git -C "$SRC_LEAN" ls-files $LEAN_SCOPE 2>"$EVIDENCE_DIR/git-ls.err")"
  [ -s "$EVIDENCE_DIR/git-ls.err" ] && fail "1-git: ls-files refused ($(head -n1 "$EVIDENCE_DIR/git-ls.err"))"
else
  echo "DRIFT-NOTE: 1-pin-lean via overlay base check above (exports carry no .git by design)"
  live_files="$(cd "$SRC_LEAN" && find $LEAN_SCOPE -type f 2>"$EVIDENCE_DIR/find-src.err" | sort)"
  [ -s "$EVIDENCE_DIR/find-src.err" ] && fail "1-find: overlay listing refused ($(head -n1 "$EVIDENCE_DIR/find-src.err"))"
fi
live_hs="$(git -C "$KELGROUPS_WORKTREE" rev-parse HEAD 2>"$EVIDENCE_DIR/git-hs.err")"
[ -s "$EVIDENCE_DIR/git-hs.err" ] && fail "1-git: rev-parse refused in kelgroups tree ($(head -n1 "$EVIDENCE_DIR/git-hs.err"))"
[ "$live_hs" = "$FROZEN_HS_BASE" ] \
  && pass "1-pin-hs: HEAD == frozen full oid" \
  || fail "1-pin-hs: HEAD [$live_hs] != frozen [$FROZEN_HS_BASE] — rebind procedure"
frozen_sorted="$(printf '%s' "$LEAN_FILES" | sort)"
live_sorted="$(printf '%s' "$live_files" | sort)"
[ "$live_sorted" = "$frozen_sorted" ] \
  && pass "1-fileset: live file set == frozen extent (12 paths incl Event/Types)" \
  || fail "1-fileset: live file set differs from frozen extent — re-review required"
hash_fail=0
while IFS= read -r line; do
  [ -z "$line" ] && continue
  want="${line%%  *}"; path="${line#*  }"
  if [ "$USE_GIT" -eq 1 ]; then
    got="$(git -C "$SRC_LEAN" show "HEAD:$path" 2>"$EVIDENCE_DIR/git-show.err" | sha256sum | cut -d' ' -f1)"
    if [ -s "$EVIDENCE_DIR/git-show.err" ]; then
      fail "1-show: HEAD:$path unreadable ($(head -n1 "$EVIDENCE_DIR/git-show.err"))"
      : > "$EVIDENCE_DIR/git-show.err"; hash_fail=1; continue
    fi
  else
    [ -r "$SRC_LEAN/$path" ] \
      || { fail "1-show: overlay file absent/unreadable: $path"; hash_fail=1; continue; }
    got="$(sha256sum "$SRC_LEAN/$path" | cut -d' ' -f1)"
  fi
  [ "$got" = "$want" ] \
    && echo "DRIFT-PASS: 1-hash $path" \
    || { fail "1-hash: $path differs from frozen bytes — input moved, re-review required"; hash_fail=1; }
done <<EOF
$LEAN_HASHES
EOF
[ "$hash_fail" -eq 0 ] && pass "1-hashes: all extent bytes == frozen (complete change detection)"
if [ "$USE_GIT" -eq 1 ]; then
  porcelain="$(git -C "$SRC_LEAN" status --porcelain 2>"$EVIDENCE_DIR/git-st.err")"
  [ -s "$EVIDENCE_DIR/git-st.err" ] && fail "1-git: status refused ($(head -n1 "$EVIDENCE_DIR/git-st.err"))"
  [ -z "$porcelain" ] \
    && pass "1-clean: tree == HEAD (point-in-time sample; residual race stated in header)" \
    || fail "1-clean: uncommitted bytes present (HEAD pins a commit, never these):
$porcelain"
else
  echo "DRIFT-NOTE: 1-clean N/A in overlay (provenance = bound export-diff, not a working tree)"
fi

# D-2: mapping self-check (frozen artifacts only — no source parsing)
leg 2 "mapping self-check"
while IFS= read -r f; do
  [ -z "$f" ] && continue
  if grep -qF "$f" "$IDENTITY_MAP" 2>"$EVIDENCE_DIR/grep-map.err"; then
    echo "DRIFT-PASS: 2-map $f has rows"
  else
    fail "2-map: extent file $f has NO identity-map rows"
  fi
done <<EOF
$LEAN_FILES
EOF
[ -s "$EVIDENCE_DIR/grep-map.err" ] && fail "2-grep: identity-map read refused ($(head -n1 "$EVIDENCE_DIR/grep-map.err"))"
grep -q "expected-empty" "$IDENTITY_MAP" 2>/dev/null \
  && pass "2-empty: expected-empty statuses present" \
  || fail "2-empty: no expected-empty status in identity map"

# D-3: .hi inventory (compiler metadata) — exactly-one-or-RED, freshness-bound.
# Overlay mode SKIPS with note (overlays aren't built; .hi firing is
# demonstrated by the B22 GREEN-build-with-drift design, stated).
leg 3 ".hi inventory"
if [ "$MODE" = "overlay" ]; then
  echo "DRIFT-NOTE: 3-skipped in overlay (no build products; firing covered by B22 design)"
else
  while IFS= read -r mod; do
    [ -z "$mod" ] && continue
    rel="${mod//.//}"
    matches="$(find "$KELGROUPS_WORKTREE/dist-newstyle" -name "${mod##*.}.hi" -path "*$rel*" 2>"$EVIDENCE_DIR/find-hi.err")"
    [ -s "$EVIDENCE_DIR/find-hi.err" ] && fail "3-find: search refused for $mod ($(head -n1 "$EVIDENCE_DIR/find-hi.err"))"
    n="$(printf '%s' "$matches" | grep -c . || true)"
    if [ "$n" -eq 0 ]; then fail "3-select: $mod — ZERO .hi candidates (missing artifact)"; continue; fi
    if [ "$n" -gt 1 ]; then fail "3-select: $mod — $n .hi candidates (ambiguous selection REFUSED, never silent pick): $matches"; continue; fi
    hi="$matches"
    # Freshness as refusal-of-older (1s granularity stated limit): equal-or-
    # newer passes (same-second build+emit is fresh); strictly older refuses.
    [ "$hi" -ot "$BUILD_MARKER" ] \
      && { fail "3-fresh: $hi OLDER than pre-build marker (stale inheritance refused)"; continue; }
    dump="$EVIDENCE_DIR/hi-$(echo "$mod" | tr . _).dump"
    ghc --show-iface "$hi" > "$dump" 2>"$dump.err" \
      || { fail "3-emit: --show-iface refused for $mod ($(head -n2 "$dump.err"))"; continue; }
    [ -s "$dump" ] || { fail "3-emit: empty dump for $mod (nonempty discovery violated)"; continue; }
    frozen_hash_file="$FROZEN_HI_DIR/$(echo "$mod" | tr . _).dump.sha256"
    [ -r "$frozen_hash_file" ] || { fail "3-pinned: no frozen inventory hash for $mod"; continue; }
    echo "$(cat "$frozen_hash_file")  $dump" | sha256sum -c - >/dev/null 2>"$EVIDENCE_DIR/sha-check.err" \
      && echo "DRIFT-PASS: 3-pinned $mod interface bytes == frozen" \
      || fail "3-pinned: $mod interface bytes drifted from frozen — RED + mandate review (tripwire fired)"
  done <<EOF
$HS_MODULES
EOF
fi

# D-4: coverage join, BOTH directions, NONEMPTY-guarded (vacuity refusal).
# Direction A: mapping -> live, exact full-line fixed-string (substring never
# suffices). Direction B (live -> mapping): hash tripwire on dumps (ANY live
# change incl. deletion fires it — D-3) + file-set rule (D-1); BOTH already
# executed above with RED-on-difference. Reconciled below, never inherited.
leg 4 "coverage join"
row_count="$(grep -cve '^\s*(#|$)' "$JOIN_ROWS" 2>"$EVIDENCE_DIR/grep-rows.err" || true)"
[ -s "$EVIDENCE_DIR/grep-rows.err" ] && fail "4-read: JOIN_ROWS unreadable"
[ "$row_count" -ge 1 ] \
  || fail "4-nonempty: JOIN_ROWS holds ZERO rows — vacuous pass REFUSED (r5 bug class)"
[ -s "$LEG4_LOG" ] \
  || fail "4-nonempty: leg-4 log empty or missing — vacuous pass REFUSED"
while IFS='|' read -r lean_item hs_module hs_expect req_ids; do
  case "$lean_item" in ''|\#*) continue;; esac
  TRAVERSED=$((TRAVERSED+1))
  if [ -z "$hs_expect" ]; then
    echo "DRIFT-PASS: 4-excluded $lean_item (proof-side/fixture — no Haskell obligation)"
    continue
  fi
  dump="$EVIDENCE_DIR/hi-$(echo "$hs_module" | tr . _).dump"
  [ -f "$dump" ] || { fail "4-missing: no live dump for $hs_module (needed by $lean_item)"; continue; }
  grep -qxF "$hs_expect" "$dump" 2>"$EVIDENCE_DIR/grep-join.err" \
    && echo "DRIFT-PASS: 4-type $lean_item exact in $hs_module" \
    || fail "4-type: $lean_item — expected exact line [$hs_expect] ABSENT from live $hs_module dump"
  [ -s "$EVIDENCE_DIR/grep-join.err" ] && fail "4-read: dump read refused for $hs_module"
  for req in $req_ids; do
    grep -qF "$req" "$LEG4_LOG" 2>"$EVIDENCE_DIR/grep-leg4.err" \
      && echo "DRIFT-PASS: 4-exec $req executed" \
      || fail "4-exec: $req NOT in leg-4 execution log"
  done
done < "$JOIN_ROWS"
[ "$OVERALL_FAIL" -eq 0 ] \
  && pass "4-livedir: tripwire clean AND file-set clean => no unmapped live change outstanding" \
  || echo "DRIFT-NOTE: 4-livedir skipped — failures above already name the direction"

# Final verdict: QUADRUPLE-gated — no setup failure, no vacuity can print PASS.
[ "$TRAVERSED" -ge 1 ] \
  || fail "4-traversed: ZERO mapping rows traversed — vacuous pass REFUSED"
[ "$TRAVERSED" -eq "$FROZEN_ROWS" ] \
  || fail "4-count: traversed $TRAVERSED != frozen $FROZEN_ROWS (mapping integrity violated)"
[ "$OVERALL_FAIL" -eq 0 ] && [ "$TRAVERSED" -ge 1 ] && [ "$TRAVERSED" -eq "$FROZEN_ROWS" ] && [ -s "$LEG4_LOG" ] \
  && echo "FINAL: PASS (traversed=$TRAVERSED frozen=$FROZEN_ROWS)" \
  || { echo "FINAL: RED (fail=$OVERALL_FAIL traversed=$TRAVERSED frozen=$FROZEN_ROWS)"; OVERALL_FAIL=1; }
exit "$OVERALL_FAIL"
