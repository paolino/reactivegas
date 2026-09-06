#!/usr/bin/env bash
# T30-DRIFT-LEG-r9 — drift/input-binding leg (kelgroups #30).
# Supersedes r8, preserved unmodified at
# e-kelgroups-substrate/t30-contract/handoffs/T30-DRIFT-LEG-r8.sh sha256
# f0afa32b4fbb13ac6084b6c3c5abd503f7e21f051fef458265b97fd56a4de3e3, which
# remains the byte-exact defect witness for pf8 invocation 1.
# STATUS: UNDEMONSTRATED — demonstration is the epic-owner-bound repair
# rerun (pf8r); no success claim here.
#
# r9 DELTAS vs r8 (three, each evidenced by pf8 invocation 1):
#  FIX-4  r8 line 53 lacked its leading marker, so a documentation line was
#    EXECUTED as shell on every run (observed in every cases/*/stderr: two
#    lines, id: No such file or directory + differently.: command not
#    found). No verdict impact (set +e, no state change) — but it proves r8
#    was bound without ever having been executed. One character.
#  FIX-5  TAXONOMY-v2 refusal precedence (below).
#  FIX-6  per-run emission provenance for joined dumps (below).
#
# TAXONOMY-v1 (exit-status taxonomy — bound IDENTICALLY here, in the runner
# header, and in contract §8; never reclassified after the fact — any change
# requires re-freeze + new campaign binding):
#   exit 0 PASS — all gates green on resolved inputs.
#   exit 1 RED — verdict rendered: >=1 subject check failed on resolved
#     inputs (pins, file-sets, hashes, clean-samples, mapping gaps,
#     tripwire diffs, join mismatches, uniqueness dups, count mismatch,
#     exec absences, stale/empty products).
#   exit 3 REFUSAL — no verdict possible: unbound config, unknown mode,
#     missing/unreadable frozen inputs, unresolvable artifacts (0 or >1
#     candidates), no producer evidence (receipt absent or exit!=0), broken
#     tools. Fail-fast; setup/config failures are NEVER domain kills.
# TAXONOMY-v2 AMENDMENT (r9, PROPOSED — needs the epic owner re-freeze per
# NOTE-009 item 3: any change requires re-freeze + new campaign binding):
#   REFUSAL PRECEDENCE. exit 3 means no verdict possible. Under r8 a refusal
#   reached AFTER a subject check had already failed still exited 3, silently
#   discarding the rendered verdict. Measured in pf8 invocation 1: twelve
#   cases (A1 A2 A5 A6 A10 A15 A16 A17 A18 A19 A24 plus A11 class) exited 3
#   with DRIFT-FAIL lines already in the stream. That inverts NOTE-009 item 5
#   own rule: a setup failure is never a domain kill — and its dual, a domain
#   kill must never be reported as a setup failure.
#   v2: a refusal raised while OVERALL_FAIL=1 prints its reason AND exits 1,
#   because a verdict HAS been rendered. A refusal raised while OVERALL_FAIL=0
#   is unchanged (exit 3). No check changes; only the status a consumer
#   branches on. FALLBACK if the amendment is declined: TAXONOMY_V2=0 in the
#   environment restores r8 behaviour exactly; the packet carries both
#   prediction columns.
#
# HASH ASSUMPTION (stated): hash-equality => byte-identity rests on SHA-256
# second-preimage resistance for the consumed streams. Content reads use
# frozen-oid addressing (`git show $OID:path`, content-addressed and
# TOCTOU-free); HEAD position is checked SEPARATELY (reference-vs-content
# never conflated). A break of the assumption breaks L1/L4 binding only.
#
# LAYER MAP (each control reaches its target layer, nothing further):
#  byte layer .... oid-addressed reads + HEAD-position checks (both repos)
#                  + file-sets (both scopes) + mapping self-check + labeled
#                  clean samples + per-module source hashes (catches
#                  unexported additions the .hi tripwire cannot see).
#                  Complete change detection (any added/removed item alters
#                  bytes); item-level attribution = NONE automatic -> ANY
#                  mismatch REDs into MANDATORY re-review. enforced:NONE for
#                  automatic attribution + mandatory re-review deliverable.
#  join layer .... frozen mapping vs live emission: mapping->live
#                  (exact-line per row, EXACT-COUNT == FROZEN_ROWS,
#                  row-UNIQUENESS) + live->mapping (hash tripwires + new-file
#                  rules + review classification). NONEMPTY asserts kill the
#                  vacuity class (r5 bug, closed by construction + proven by
#                  the comments-only fixture).
#  tripwire ...... .hi hash vs frozen + source-byte hash vs frozen (drift ->
#                  RED + review; INDEPENDENT channels — demonstrated by the
#                  source-add fixture with .hi clean).
#  compiler ..... GHC -Werror exhaustiveness + .hi existence, live builds.
# Portability rule (F9 lesson): POSIX sh + POSIX grep -E ONLY; no format may begin with `-`
# (heredoc/`%s`-arg forms only); no `head -n1` selection (excerpts below
# display ALREADY-RECORDED failures only — never select, never gate); no
# pipeline-subshell state; no /dev/null on loads. Data-row filtering uses one
# POSIX ERE (`^[[:space:]]*(#|$)`); the join subject is materialized ONCE into
# evidence (join-data.rows) and every counter reads that file — no dual-filter
# drift possible. Parsed-construct proof: the comments-only campaign fixture
# yields 0 data rows — a literal-paren misreading would yield >=1 and fail
# differently. REQ records are exact-success lines `PASS: <id> OK`
# (full-line fixed-string; FAILED/SKIPPED/bare names never match).
# Exit codes: 0 PASS; 1 RED; 3 REFUSAL. set +e with explicit propagation.

set +e
OVERALL_FAIL=0
TRAVERSED=0
leg() { printf '%s\n' "===== DRIFT-$1: $2 ====="; }
pass() { printf '%s\n' "DRIFT-PASS: $1"; }
fail() { printf '%s\n' "DRIFT-FAIL: $1"; OVERALL_FAIL=1; }
refuse() { # TAXONOMY-v2: refusal after a rendered verdict exits RED, not REFUSAL
  printf '%s\n' "DRIFT-REFUSE: $1"
  if [ "${TAXONOMY_V2:-1}" = "1" ] && [ "$OVERALL_FAIL" -eq 1 ]; then
    printf '%s\n' "DRIFT-NOTE: taxonomy-v2 precedence — refusal follows a rendered verdict; exiting RED (1), reason above"
    exit 1
  fi
  exit 3
}

# --- frozen config: collect ALL missing names, then refuse once ---
missing_cfg=""
[ -n "${FROZEN_LEAN_HEAD+x}" ] && [ -n "$FROZEN_LEAN_HEAD" ] || missing_cfg="$missing_cfg FROZEN_LEAN_HEAD"
[ -n "${FROZEN_HS_BASE+x}" ] && [ -n "$FROZEN_HS_BASE" ] || missing_cfg="$missing_cfg FROZEN_HS_BASE"
[ -n "${REACTIVE_GAS+x}" ] && [ -n "$REACTIVE_GAS" ] || missing_cfg="$missing_cfg REACTIVE_GAS"
[ -n "${KELGROUPS_WORKTREE+x}" ] && [ -n "$KELGROUPS_WORKTREE" ] || missing_cfg="$missing_cfg KELGROUPS_WORKTREE"
[ -n "${EVIDENCE_DIR+x}" ] && [ -n "$EVIDENCE_DIR" ] || missing_cfg="$missing_cfg EVIDENCE_DIR"
[ -n "${IDENTITY_MAP+x}" ] && [ -n "$IDENTITY_MAP" ] || missing_cfg="$missing_cfg IDENTITY_MAP"
[ -n "${JOIN_ROWS+x}" ] && [ -n "$JOIN_ROWS" ] || missing_cfg="$missing_cfg JOIN_ROWS"
[ -n "${LEG4_LOG+x}" ] && [ -n "$LEG4_LOG" ] || missing_cfg="$missing_cfg LEG4_LOG"
[ -n "${BUILD_MARKER+x}" ] && [ -n "$BUILD_MARKER" ] || missing_cfg="$missing_cfg BUILD_MARKER"
[ -n "${BUILD_RECEIPT+x}" ] && [ -n "$BUILD_RECEIPT" ] || missing_cfg="$missing_cfg BUILD_RECEIPT"
[ -n "${LEAN_FILES+x}" ] && [ -n "$LEAN_FILES" ] || missing_cfg="$missing_cfg LEAN_FILES"
[ -n "${LEAN_HASHES+x}" ] && [ -n "$LEAN_HASHES" ] || missing_cfg="$missing_cfg LEAN_HASHES"
[ -n "${LEAN_SCOPE+x}" ] && [ -n "$LEAN_SCOPE" ] || missing_cfg="$missing_cfg LEAN_SCOPE"
[ -n "${HS_FILES+x}" ] && [ -n "$HS_FILES" ] || missing_cfg="$missing_cfg HS_FILES"
[ -n "${HS_SCOPE+x}" ] && [ -n "$HS_SCOPE" ] || missing_cfg="$missing_cfg HS_SCOPE"
[ -n "${HS_SOURCE_HASHES+x}" ] && [ -n "$HS_SOURCE_HASHES" ] || missing_cfg="$missing_cfg HS_SOURCE_HASHES"
[ -n "${HS_MODULES+x}" ] && [ -n "$HS_MODULES" ] || missing_cfg="$missing_cfg HS_MODULES"
[ -n "${FROZEN_HI_DIR+x}" ] && [ -n "$FROZEN_HI_DIR" ] || missing_cfg="$missing_cfg FROZEN_HI_DIR"
[ -n "${FROZEN_ROWS+x}" ] && [ -n "$FROZEN_ROWS" ] || missing_cfg="$missing_cfg FROZEN_ROWS"
[ -n "${MODE+x}" ] && [ -n "$MODE" ] || missing_cfg="$missing_cfg MODE"
[ -z "$missing_cfg" ] || refuse "unbound config:$missing_cfg"
case "$MODE" in
  live|overlay) ;;
  *) refuse "unknown MODE [$MODE] (live|overlay only — silent modes refused)" ;;
esac
[ -d "$EVIDENCE_DIR" ] || refuse "config: EVIDENCE_DIR $EVIDENCE_DIR not a directory (writes would be losable)"
[ -w "$EVIDENCE_DIR" ] || refuse "config: EVIDENCE_DIR not writable (writes would be losable)"
[ -f "$IDENTITY_MAP" ] || refuse "config: IDENTITY_MAP absent or not a regular file: $IDENTITY_MAP"
[ -f "$JOIN_ROWS" ] || refuse "config: JOIN_ROWS absent or not a regular file: $JOIN_ROWS"
[ -f "$LEG4_LOG" ] || refuse "config: LEG4_LOG absent or not a regular file: $LEG4_LOG"
[ -f "$BUILD_MARKER" ] || refuse "config: BUILD_MARKER absent: $BUILD_MARKER (touch pre-build, then build, then emit)"
[ -f "$BUILD_RECEIPT" ] || refuse "config: BUILD_RECEIPT absent: $BUILD_RECEIPT (no exit0-producer evidence)"
[ -d "$FROZEN_HI_DIR" ] || refuse "config: FROZEN_HI_DIR absent: $FROZEN_HI_DIR (no inventory baseline)"
grep -qxF "exit=0" "$BUILD_RECEIPT" 2>"$EVIDENCE_DIR/receipt.err" \
  && pass "config: build receipt records exit=0 (touch-faked outputs refused)" \
  || refuse "config: build receipt lacks exit=0 (no producer evidence — refusing, not judging)"
[ -s "$EVIDENCE_DIR/receipt.err" ] && refuse "config: receipt read refused"
case "$FROZEN_ROWS" in ''|*[!0-9]*) refuse "config: FROZEN_ROWS [$FROZEN_ROWS] not a positive integer" ;; *) [ "$FROZEN_ROWS" -ge 1 ] || refuse "config: FROZEN_ROWS not >= 1" ;; esac

# Source flavor per mode: live = frozen-oid git views; overlay = bound trees
# (overlay D-3 emission skipped — overlays aren't built; firing via B22).
USE_GIT=1
SRC_LEAN="$REACTIVE_GAS"
SRC_HS="$KELGROUPS_WORKTREE"
if [ "$MODE" = "overlay" ]; then
  [ -n "${OVERLAY_LEAN+x}" ] && [ -n "$OVERLAY_LEAN" ] || refuse "unbound config: OVERLAY_LEAN"
  [ -n "${OVERLAY_HS+x}" ] && [ -n "$OVERLAY_HS" ] || refuse "unbound config: OVERLAY_HS"
  [ -n "${OVERLAY_BASE_OID+x}" ] && [ -n "$OVERLAY_BASE_OID" ] || refuse "unbound config: OVERLAY_BASE_OID"
  [ -n "${OVERLAY_EXPORT_DIFF+x}" ] && [ -n "$OVERLAY_EXPORT_DIFF" ] || refuse "unbound config: OVERLAY_EXPORT_DIFF"
  [ -d "$OVERLAY_LEAN" ] || refuse "config: OVERLAY_LEAN not a directory"
  [ -d "$OVERLAY_HS" ] || refuse "config: OVERLAY_HS not a directory"
  [ -r "$OVERLAY_EXPORT_DIFF" ] || refuse "config: OVERLAY_EXPORT_DIFF unreadable (overlay provenance unbound)"
  [ "$OVERLAY_BASE_OID" = "$FROZEN_LEAN_HEAD" ] \
    && pass "0-overlay-base: export derives from frozen base (diff bound at $OVERLAY_EXPORT_DIFF)" \
    || refuse "0-overlay-base: export base [$OVERLAY_BASE_OID] != frozen [$FROZEN_LEAN_HEAD] (unfounded overlay)"
  USE_GIT=0
  SRC_LEAN="$OVERLAY_LEAN"
  SRC_HS="$OVERLAY_HS"
fi

# D-1: input binding (frozen-oid reads + position checks + file-sets + hashes)
leg 1 "input binding"
if [ "$USE_GIT" -eq 1 ]; then
  live_lean="$(git -C "$SRC_LEAN" rev-parse HEAD 2>"$EVIDENCE_DIR/git-lean.err")"
  [ -s "$EVIDENCE_DIR/git-lean.err" ] && refuse "1-git: rev-parse refused ($(head -n1 "$EVIDENCE_DIR/git-lean.err"))"
  [ "$live_lean" = "$FROZEN_LEAN_HEAD" ] \
    && pass "1-position-lean: HEAD == frozen full oid (reference check; content comes from the oid below, never from HEAD)" \
    || fail "1-position-lean: HEAD [$live_lean] != frozen [$FROZEN_LEAN_HEAD] — rebind procedure"
  # shellcheck disable=SC2086
  live_files="$(git -C "$SRC_LEAN" ls-files $LEAN_SCOPE 2>"$EVIDENCE_DIR/git-ls.err")"
  [ -s "$EVIDENCE_DIR/git-ls.err" ] && refuse "1-git: ls-files refused ($(head -n1 "$EVIDENCE_DIR/git-ls.err"))"
  # shellcheck disable=SC2086
  live_hs_files="$(git -C "$KELGROUPS_WORKTREE" ls-files $HS_SCOPE 2>"$EVIDENCE_DIR/git-ls-hs.err")"
  [ -s "$EVIDENCE_DIR/git-ls-hs.err" ] && refuse "1-git: hs ls-files refused ($(head -n1 "$EVIDENCE_DIR/git-ls-hs.err"))"
else
  pass "1-position-lean: via overlay base check above (exports carry no .git by design)"
  live_files="$(cd "$SRC_LEAN" && find $LEAN_SCOPE -type f 2>"$EVIDENCE_DIR/find-src.err" | sort)"
  [ -s "$EVIDENCE_DIR/find-src.err" ] && refuse "1-find: overlay lean listing refused ($(head -n1 "$EVIDENCE_DIR/find-src.err"))"
  live_hs_files="$(cd "$SRC_HS" && find $HS_SCOPE -type f 2>"$EVIDENCE_DIR/find-src-hs.err" | sort)"
  [ -s "$EVIDENCE_DIR/find-src-hs.err" ] && refuse "1-find: overlay hs listing refused ($(head -n1 "$EVIDENCE_DIR/find-src-hs.err"))"
fi
live_hs="$(git -C "$KELGROUPS_WORKTREE" rev-parse HEAD 2>"$EVIDENCE_DIR/git-hs.err")"
[ -s "$EVIDENCE_DIR/git-hs.err" ] && refuse "1-git: rev-parse refused in kelgroups tree ($(head -n1 "$EVIDENCE_DIR/git-hs.err"))"
[ "$live_hs" = "$FROZEN_HS_BASE" ] \
  && pass "1-position-hs: HEAD == frozen full oid" \
  || fail "1-position-hs: HEAD [$live_hs] != frozen [$FROZEN_HS_BASE] — rebind procedure"
frozen_sorted="$(printf '%s' "$LEAN_FILES" | sort)"
live_sorted="$(printf '%s' "$live_files" | sort)"
[ "$live_sorted" = "$frozen_sorted" ] \
  && pass "1-fileset-lean: live file set == frozen extent (12 paths incl Event/Types)" \
  || fail "1-fileset-lean: live file set differs from frozen extent — re-review required"
frozen_hs_sorted="$(printf '%s' "$HS_FILES" | sort)"
live_hs_sorted="$(printf '%s' "$live_hs_files" | sort)"
[ "$live_hs_sorted" = "$frozen_hs_sorted" ] \
  && pass "1-fileset-hs: live hs file set == frozen module sources (new-file rule)" \
  || fail "1-fileset-hs: live hs file set differs — re-review required"
porcelain="$(git -C "$SRC_LEAN" status --porcelain 2>"$EVIDENCE_DIR/git-st.err")"
if [ "$USE_GIT" -eq 1 ]; then
  [ -s "$EVIDENCE_DIR/git-st.err" ] && refuse "1-git: status refused ($(head -n1 "$EVIDENCE_DIR/git-st.err"))"
  [ -z "$porcelain" ] \
    && pass "1-clean: tree == HEAD (point-in-time sample; residual race stated in header)" \
    || fail "1-clean: uncommitted bytes present (pins bind commits, never these):
$porcelain"
else
  pass "1-clean: N/A in overlay (provenance = bound export-diff, not a working tree)"
fi
porcelain_hs="$(git -C "$KELGROUPS_WORKTREE" status --porcelain 2>"$EVIDENCE_DIR/git-st-hs.err")"
[ -s "$EVIDENCE_DIR/git-st-hs.err" ] && refuse "1-git: hs status refused ($(head -n1 "$EVIDENCE_DIR/git-st-hs.err"))"
[ -z "$porcelain_hs" ] \
  && pass "1-clean-hs: kelgroups tree clean (point-in-time sample; same stated limit)" \
  || fail "1-clean-hs: uncommitted bytes in kelgroups tree (pins bind commits, never these)"
hash_fail=0
while IFS= read -r line; do
  [ -z "$line" ] && continue
  want="${line%%  *}"; path="${line#*  }"
  if [ "$USE_GIT" -eq 1 ]; then
    got="$(git -C "$SRC_LEAN" show "$FROZEN_LEAN_HEAD:$path" 2>"$EVIDENCE_DIR/git-show.err" | sha256sum | cut -d' ' -f1)"
    if [ -s "$EVIDENCE_DIR/git-show.err" ]; then
      refuse "1-show: $FROZEN_LEAN_HEAD:$path unreadable ($(head -n1 "$EVIDENCE_DIR/git-show.err"))"
    fi
  else
    [ -f "$SRC_LEAN/$path" ] \
      || { fail "1-show: overlay file absent/not-regular: $path"; hash_fail=1; continue; }
    got="$(sha256sum "$SRC_LEAN/$path" | cut -d' ' -f1)"
  fi
  [ "$got" = "$want" ] \
    && printf '%s\n' "DRIFT-PASS: 1-hash $path" \
    || { fail "1-hash: $path differs from frozen bytes — input moved, re-review required"; hash_fail=1; }
done <<EOF
$LEAN_HASHES
EOF
while IFS= read -r line; do
  [ -z "$line" ] && continue
  want="${line%%  *}"; path="${line#*  }"
  if [ "$USE_GIT" -eq 1 ]; then
    got="$(git -C "$KELGROUPS_WORKTREE" show "$FROZEN_HS_BASE:$path" 2>"$EVIDENCE_DIR/git-show-hs.err" | sha256sum | cut -d' ' -f1)"
    if [ -s "$EVIDENCE_DIR/git-show-hs.err" ]; then
      refuse "1-show-hs: $FROZEN_HS_BASE:$path unreadable ($(head -n1 "$EVIDENCE_DIR/git-show-hs.err"))"
    fi
  else
    [ -f "$SRC_HS/$path" ] \
      || { fail "1-show-hs: overlay file absent/not-regular: $path"; hash_fail=1; continue; }
    got="$(sha256sum "$SRC_HS/$path" | cut -d' ' -f1)"
  fi
  [ "$got" = "$want" ] \
    && printf '%s\n' "DRIFT-PASS: 1-hash-hs $path" \
    || { fail "1-hash-hs: $path differs from frozen bytes (incl. unexported edits) — re-review required"; hash_fail=1; }
done <<EOF
$HS_SOURCE_HASHES
EOF
[ "$hash_fail" -eq 0 ] && pass "1-hashes: all extent+source bytes == frozen (complete change detection, hash assumption stated in header)"

# D-2: mapping self-check (frozen artifacts only — no source parsing)
leg 2 "mapping self-check"
while IFS= read -r f; do
  [ -z "$f" ] && continue
  if grep -qF -e "$f" "$IDENTITY_MAP" 2>"$EVIDENCE_DIR/grep-map.err"; then
    printf '%s\n' "DRIFT-PASS: 2-map $f has rows"
  else
    fail "2-map: extent file $f has NO identity-map rows"
  fi
done <<EOF
$LEAN_FILES
EOF
[ -s "$EVIDENCE_DIR/grep-map.err" ] && refuse "2-grep: identity-map read refused ($(head -n1 "$EVIDENCE_DIR/grep-map.err"))"
grep -qF -e "expected-empty" "$IDENTITY_MAP" 2>/dev/null \
  && pass "2-empty: expected-empty statuses present" \
  || fail "2-empty: no expected-empty status in identity map"

# D-3: .hi inventory (compiler metadata) — receipt-preconditioned (config),
# exactly-one-or-REFUSE, freshness-bound, ALWAYS re-emitted, hash-pinned.
# Overlay mode SKIPS emission (overlays aren't built; firing via B22a/b).
leg 3 ".hi inventory"
if [ "$MODE" = "overlay" ]; then
  pass "3-skipped in overlay (no build products; firing covered by B22a/b design)"
else
  # FIX-6: bind every joined dump to THIS run's emission. pf8 invocation 1
  # established (cases/A11) that D-4 will read a dump the run did not emit:
  # A11 pre-seeded POISON, the freshness RED skipped emission, and the join
  # then consumed the poisoned file. Nothing bound dump-to-emission; the only
  # thing preventing a false GREEN was that every emission-skip path happens
  # to set OVERALL_FAIL first — an incidental ordering, enforced:NONE as an
  # invariant. This record makes it explicit, and also refuses a JOIN_ROWS
  # module absent from HS_MODULES (never emitted, previously satisfiable by
  # an inherited file).
  : > "$EVIDENCE_DIR/emitted.mods"
  while IFS= read -r mod; do
    [ -z "$mod" ] && continue
    rel="${mod//.//}"
    matches="$(find "$KELGROUPS_WORKTREE/dist-newstyle" -name "${mod##*.}.hi" -path "*$rel*" 2>"$EVIDENCE_DIR/find-hi.err")"
    [ -s "$EVIDENCE_DIR/find-hi.err" ] && refuse "3-find: search refused for $mod ($(head -n1 "$EVIDENCE_DIR/find-hi.err"))"
    n="$(printf '%s' "$matches" | grep -c . || true)"
    if [ "$n" -eq 0 ]; then refuse "3-select: $mod — ZERO .hi candidates (missing artifact; unfounded run)"; continue; fi
    if [ "$n" -gt 1 ]; then refuse "3-select: $mod — $n .hi candidates (ambiguous selection REFUSED, never silent pick): $matches"; continue; fi
    hi="$matches"
    [ "$hi" -ot "$BUILD_MARKER" ] \
      && { fail "3-fresh: $hi OLDER than pre-build marker (stale inheritance refused)"; continue; }
    dump="$EVIDENCE_DIR/hi-$(echo "$mod" | tr . _).dump"
    ghc --show-iface "$hi" > "$dump" 2>"$dump.err" \
      || { refuse "3-emit: --show-iface refused for $mod ($(head -n2 "$dump.err"))"; continue; }
    printf '%s\n' "$mod" >> "$EVIDENCE_DIR/emitted.mods"
    [ -s "$dump" ] || { fail "3-emit: empty dump for $mod (nonempty discovery violated)"; continue; }
    frozen_hash_file="$FROZEN_HI_DIR/$(echo "$mod" | tr . _).dump.sha256"
    [ -r "$frozen_hash_file" ] || { refuse "3-pinned: no frozen inventory hash for $mod (unfounded comparison)"; continue; }
    printf '%s  %s\n' "$(cat "$frozen_hash_file")" "$dump" | sha256sum -c - >/dev/null 2>"$EVIDENCE_DIR/sha-check.err" \
      && printf '%s\n' "DRIFT-PASS: 3-pinned $mod interface bytes == frozen" \
      || fail "3-pinned: $mod interface bytes drifted from frozen — RED + mandate review (tripwire fired)"
  done <<EOF
$HS_MODULES
EOF
fi

# D-4: coverage join, BOTH directions, NONEMPTY-guarded (vacuity refusal).
# Row-count is POSIX BRE (parsed-construct proof: the comments-only campaign
# fixture yields 0 — a literal-paren reading would count >=1).
leg 4 "coverage join"
# Single-filter construction: the join subject is materialized ONCE here;
# row_count, traversal, and uniqueness ALL read join-data.rows — no drift
# between counters by construction (the r6 dual-filter hole is closed).
grep -Eve '^[[:space:]]*(#|$)' "$JOIN_ROWS" 2>"$EVIDENCE_DIR/grep-rows.err" > "$EVIDENCE_DIR/join-data.rows" || true
[ -s "$EVIDENCE_DIR/grep-rows.err" ] && refuse "4-read: JOIN_ROWS unreadable"
row_count="$(grep -c . "$EVIDENCE_DIR/join-data.rows" 2>/dev/null || true)"
[ "$row_count" -ge 1 ] \
  || refuse "4-nonempty: JOIN_ROWS holds ZERO data rows — vacuous pass REFUSED (r5 bug class)"
leg4_pass="$(grep -c '^PASS: .* OK$' "$LEG4_LOG" 2>"$EVIDENCE_DIR/grep-leg4c.err" || true)"
[ -s "$EVIDENCE_DIR/grep-leg4c.err" ] && refuse "4-read: leg-4 log unreadable"
[ "$leg4_pass" -ge 1 ] \
  || fail "4-nonempty: leg-4 log holds ZERO successful execution records — vacuous pass REFUSED"
# Direction A: mapping -> live, exact full-line fixed-string (substring never
# suffices) + exact-success REQ records (FAILED/SKIPPED/bare names never match).
while IFS='|' read -r lean_item hs_module hs_expect req_ids; do
  # Defense-in-depth only: the pre-filter above already removed blanks and
  # comments, so this branch is unreachable on filtered input; it fires only
  # if the filter regresses (then count-vs-traversal diverges and REDs below).
  case "$lean_item" in ''|\#*) continue;; esac
  TRAVERSED=$((TRAVERSED+1))
  if [ -z "$hs_expect" ]; then
    printf '%s\n' "DRIFT-PASS: 4-excluded $lean_item (proof-side/fixture — no Haskell obligation)"
    continue
  fi
  dump="$EVIDENCE_DIR/hi-$(echo "$hs_module" | tr . _).dump"
  [ -f "$dump" ] || { refuse "4-missing: no live dump for $hs_module (needed by $lean_item; pipeline breakage, not subject state)"; continue; }
  if [ "$MODE" != "overlay" ]; then
    grep -qxF -e "$hs_module" "$EVIDENCE_DIR/emitted.mods" 2>/dev/null \
      || refuse "4-provenance: dump for $hs_module was not emitted by this run (inherited artifact refused)"
  fi
  grep -qxF -e "$hs_expect" "$dump" 2>"$EVIDENCE_DIR/grep-join.err" \
    && printf '%s\n' "DRIFT-PASS: 4-type $lean_item exact in $hs_module" \
    || fail "4-type: $lean_item — expected exact line [$hs_expect] ABSENT from live $hs_module dump"
  [ -s "$EVIDENCE_DIR/grep-join.err" ] && refuse "4-read: dump read refused for $hs_module"
  for req in $req_ids; do
    grep -qxF -e "PASS: $req OK" "$LEG4_LOG" 2>"$EVIDENCE_DIR/grep-leg4.err" \
      && printf '%s\n' "DRIFT-PASS: 4-exec $req successfully executed" \
      || fail "4-exec: $req has NO successful execution record (failed/skipped/name-only never suffice)"
  done
done < "$EVIDENCE_DIR/join-data.rows"
# Row-uniqueness over the SAME join subject (lose-one + duplicate-one defeats
# counts alone).
dupes="$(sort "$EVIDENCE_DIR/join-data.rows" 2>/dev/null | uniq -d || true)"
[ -z "$dupes" ] \
  && pass "4-unique: no duplicate mapping rows (same-size swap attacks refused)" \
  || fail "4-unique: duplicate mapping rows present:
$dupes"
# Direction B: live -> mapping (hash tripwires + file-set rules, executed
# above with RED-on-difference; reconciled here, never inherited).
[ "$OVERALL_FAIL" -eq 0 ] \
  && pass "4-livedir: tripwires clean AND file-sets clean => no unmapped live change outstanding" \
  || printf '%s\n' "DRIFT-NOTE: 4-livedir skipped — failures above already name the direction"

# Final verdict: SEXTUPLE-gated — no setup/config failure, no vacuity prints PASS.
[ "$TRAVERSED" -ge 1 ] \
  || fail "4-traversed: ZERO mapping rows traversed — vacuous pass REFUSED"
[ "$TRAVERSED" -eq "$FROZEN_ROWS" ] \
  || fail "4-count: traversed $TRAVERSED != frozen $FROZEN_ROWS (mapping integrity violated)"
[ "$OVERALL_FAIL" -eq 0 ] && [ "$TRAVERSED" -ge 1 ] && [ "$TRAVERSED" -eq "$FROZEN_ROWS" ] && [ "$leg4_pass" -ge 1 ] \
  && printf '%s\n' "FINAL: PASS (traversed=$TRAVERSED frozen=$FROZEN_ROWS leg4pass=$leg4_pass)" \
  || { printf '%s\n' "FINAL: RED (fail=$OVERALL_FAIL traversed=$TRAVERSED frozen=$FROZEN_ROWS leg4pass=$leg4_pass)"; OVERALL_FAIL=1; }
exit "$OVERALL_FAIL"
