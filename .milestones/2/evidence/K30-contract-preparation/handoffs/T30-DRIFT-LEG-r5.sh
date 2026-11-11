#!/usr/bin/env bash
# T30-DRIFT-LEG-r5 — drift/input-binding leg DESIGN ARTIFACT (kelgroups #30).
# Status: frozen LOGIC for ticket-owner freeze. Every FROZEN_* value is bound
# at freeze; `:?` guards make unbound config REFUSE (fail-closed), never pass.
# No regex output is cited as semantic inventory anywhere in this leg.
# Conventions: set +e (S28 gate shape); every failing command captured
# diagnosably; OVERALL_FAIL accumulates; evidence dir per campaign.
#
# LAYER MAP (each control reaches its target layer, nothing further):
#  L1 input binding ..... sha256sum + HEAD pin + empty-porcelain (byte layer)
#  L3 coverage join ..... frozen mapping vs live Haskell emission (join layer)
#  L4 .hi tripwire ....... interface-byte diff (tripwire + review)
#  L5 arm totality ....... GHC -Werror, live every build (compiler)
#  Item-level attribution: NONE automatic — ANY mismatch REDs into MANDATORY
#  re-review (diff review vs T30-IDENTITY-MAP-r5 + sign-off). enforced:NONE
#  for automatic attribution + mandatory re-review deliverable (NOTE-006-2a).
# TOCTOU (NOTE-006-2b): drift inputs consumed from IMMUTABLE git views
# (`git show HEAD:path` — content-addressed, TOCTOU-free by construction);
# working-tree conformance (`status --porcelain` + `diff HEAD` empty) is a
# SEPARATE labeled point-in-time sample. Residual race (write landing strictly
# between sample and build) is a STATED accepted limit: threat model is
# accidental drift, writers are fenced workers, leg-1-after catches residue.

set +e
OVERALL_FAIL=0
leg() { echo "===== DRIFT-$1: $2 ====="; }
pass() { echo "DRIFT-PASS: $1"; }
fail() { echo "DRIFT-FAIL: $1"; OVERALL_FAIL=1; }

# --- frozen config (bound at ticket freeze; unbound = REFUSE) ---
: "${FROZEN_LEAN_HEAD:?frozen Lean pin missing}"
: "${FROZEN_HS_BASE:?frozen Haskell base SHA missing}"
: "${REACTIVE_GAS:?frozen Reactivegas checkout path missing}"
: "${EVIDENCE_DIR:?evidence dir missing}"
: "${IDENTITY_MAP:?frozen identity-map path missing}"
: "${FROZEN_HI_DIR:?frozen .hi inventory dir missing}"
: "${BUILD_MARKER:?pre-build freshness marker path missing}"
: "${LEG4_LOG:?leg-4 execution log path missing}"
: "${JOIN_ROWS:?frozen join-row file missing}"
: "${KELGROUPS_WORKTREE:?frozen kelgroups worktree path missing}"
cd "$KELGROUPS_WORKTREE" || { echo "DRIFT-FAIL: cannot enter $KELGROUPS_WORKTREE"; exit 3; }
# Frozen extent file lists (newline-separated, bound at freeze):
#   LEAN_FILES: 7 Vote files + Integration/State/Validate (10 paths total)
#   LEAN_HASHES: "<sha256>  <path>" per extent file (from `git show` bytes)
#   HS_MODULES: frozen Haskell module list (Vote.* + Event/State/Types/Fold
#     as mapped) for .hi emission + mapping self-check
: "${LEAN_FILES:?frozen Lean file list missing}"
: "${LEAN_HASHES:?frozen Lean hash list missing}"
: "${HS_MODULES:?frozen Haskell module list missing}"

# D-1: Lean pin + immutable-view byte binding + clean-tree sample
leg 1 "Lean input binding (immutable views)"
live_head="$(git -C "$REACTIVE_GAS" rev-parse HEAD 2>&1)"
[ "$live_head" = "$FROZEN_LEAN_HEAD" ] \
  && pass "1-pin: HEAD == frozen $FROZEN_LEAN_HEAD" \
  || fail "1-pin: HEAD [$live_head] != frozen [$FROZEN_LEAN_HEAD] — rebind procedure, no silent pass"
live_files="$(git -C "$REACTIVE_GAS" ls-files lean/KelGroups/Vote/ 2>&1 | sort)"
[ "$live_files" = "$(printf '%s' "$LEAN_FILES" | grep '^lean/KelGroups/Vote/' | sort)" ] \
  && pass "1-fileset: live Vote dir == frozen 7-file list" \
  || fail "1-fileset: live Vote file set differs from frozen list (added/renamed file?) — re-review required"
echo "$LEAN_HASHES" | while IFS= read -r line; do
  [ -z "$line" ] && continue
  want="${line%%  *}"; path="${line#*  }"
  got="$(git -C "$REACTIVE_GAS" show "HEAD:$path" 2>/dev/null | sha256sum | cut -d' ' -f1)"
  [ "$got" = "$want" ] \
    && echo "DRIFT-PASS: 1-hash $path" \
    || { echo "DRIFT-FAIL: 1-hash $path (want $want got $got) — input moved, re-review required"; echo HIT >> "$EVIDENCE_DIR/failflag"; }
done
[ -f "$EVIDENCE_DIR/failflag" ] && { fail "1-hashes: one or more extent files differ from frozen bytes"; rm -f "$EVIDENCE_DIR/failflag"; } || pass "1-hashes: all extent bytes == frozen"
porcelain="$(git -C "$REACTIVE_GAS" status --porcelain 2>&1 || echo PORCELAIN-FAILED)"
[ -z "$porcelain" ] \
  && pass "1-clean: working tree == HEAD (point-in-time sample; residual race stated in header)" \
  || fail "1-clean: uncommitted bytes present (HEAD pins a commit, never these bytes):
$porcelain"

# D-2: mapping self-check (frozen artifacts only — no source parsing)
leg 2 "mapping self-check (extent files resolve in identity map)"
while IFS= read -r f; do
  [ -z "$f" ] && continue
  if grep -qF "$f" "$IDENTITY_MAP"; then
    echo "DRIFT-PASS: 2-map $f has rows"
  else
    fail "2-map: extent file $f has NO identity-map rows"
  fi
done <<EOF
$LEAN_FILES
EOF
grep -q "expected-empty" "$IDENTITY_MAP" \
  && pass "2-empty: expected-empty statuses present (Invariants/Tests named)" \
  || fail "2-empty: no expected-empty status in identity map"

# D-3: Haskell live emission (.hi inventory — compiler metadata, NOT regex)
# PRECONDITION (ordering contract, enforced here): the calling gate runs this
# leg ONLY after an exit-0 build in the same campaign; additionally every
# consumed .hi must be newer than BUILD_MARKER (touched pre-build) —
# stale .hi never inherited (NOTE-006-2c).
leg 3 "Haskell .hi inventory (freshness-bound, hash-pinned)"
[ -f "$BUILD_MARKER" ] \
  || { fail "3-marker: no pre-build freshness marker — emission REFUSED"; }
if [ "$OVERALL_FAIL" -eq 0 ]; then
  while IFS= read -r mod; do
    [ -z "$mod" ] && continue
    hi="$(find dist-newstyle -name "${mod##*.}.hi" -path "*${mod//.//}*" 2>/dev/null | head -n 1)"
    [ -n "$hi" ] && [ "$hi" -nt "$BUILD_MARKER" ] \
      || { fail "3-fresh: no post-build .hi for $mod (stale inheritance refused)"; continue; }
    dump="$EVIDENCE_DIR/hi-$(echo "$mod" | tr . _).dump"
    ghc --show-iface "$hi" > "$dump" 2>"$dump.err" \
      || { fail "3-emit: --show-iface refused for $mod ($(head -n2 "$dump.err"))"; continue; }
    if [ -f "$FROZEN_HI_DIR/$(echo "$mod" | tr . _).dump.sha256" ]; then
      echo "$(cat "$FROZEN_HI_DIR/$(echo "$mod" | tr . _).dump.sha256")  $dump" | sha256sum -c - >/dev/null 2>&1 \
        && echo "DRIFT-PASS: 3-pinned $mod interface bytes == frozen" \
        || fail "3-pinned: $mod interface bytes drifted from frozen — RED + mandate review (tripwire fired; classification is review, not auto)"
    else
      fail "3-pinned: no frozen inventory hash for $mod"
    fi
  done <<EOF
$HS_MODULES
EOF
fi

# D-4: coverage join (frozen mapping vs LIVE .hi dumps + leg-4 log)
# Each mapping row carries: Lean item | Haskell type (+module) | REQ-IDs.
# Row file format (frozen at freeze): LEAN_ITEM|HS_MODULE|HS_TYPE|REQ_IDS
# (HS_TYPE empty for proof-side/excluded rows — join skips them by rule.)
leg 4 "coverage join (mapped Haskell types present; REQ-IDs executed)"
if [ "$OVERALL_FAIL" -eq 0 ]; then
  while IFS='|' read -r lean_item hs_module hs_type req_ids; do
    case "$lean_item" in ''|\#*) continue;; esac
    if [ -z "$hs_type" ]; then
      echo "DRIFT-PASS: 4-excluded $lean_item (proof-side/fixture — no Haskell obligation)"
      continue
    fi
    dump="$EVIDENCE_DIR/hi-$(echo "$hs_module" | tr . _).dump"
    grep -qF "$hs_type" "$dump" 2>/dev/null \
      && echo "DRIFT-PASS: 4-type $lean_item -> $hs_module.$hs_type present" \
      || fail "4-type: $lean_item -> $hs_module.$hs_type ABSENT from live .hi dump"
    for req in $req_ids; do
      grep -qF "$req" "$LEG4_LOG" 2>/dev/null \
        && echo "DRIFT-PASS: 4-exec $req executed" \
        || fail "4-exec: $req NOT in leg-4 execution log"
    done
  done < "$JOIN_ROWS"
  # Known-absent control: a fictitious token must NOT occur (discrimination).
  if grep -rqF "ZZZ-NoSuchType" "$EVIDENCE_DIR"/hi-*.dump 2>/dev/null; then
    fail "4-control: fictitious token PRESENT — presence check is broken"
  else
    pass "4-control: fictitious token absent (presence check discriminates)"
  fi
fi

# D-5: overlay procedure (directional trigger demonstration; each a counted
# PROBE in the campaign plan — P-DRIFT-ADD/SRCOMIT/MAPOMIT/FILEADD).
# overlay_demo <name> <edit-description>: scratch `git archive` export of the
# frozen base + ONE intentional edit; export-diff bound in evidence; the SAME
# D-1/D-4 checks run against the overlay and must RED with the predicted
# direction message. Labeled: trigger discrimination on source-shaped bytes,
# NOT review correctness. (GREEN path P-DRIFT-GREEN = this leg on live
# inputs, above.)
overlay_demo() {
  echo "DRIFT-OVERLAY: $1 ($2) — see campaign evidence for bound export-diff"
  echo "DRIFT-OVERLAY: rule — predicted direction message required, else INCONCLUSIVE"
}

echo "===== DRIFT LEG COMPLETE: OVERALL_FAIL=$OVERALL_FAIL ====="
exit "$OVERALL_FAIL"
