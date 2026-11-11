#!/usr/bin/env bash
# ISOLATED M8 runner (S28-R2, NOTE-040 §3): gate v10.2 M8 leg (lines 292-304) verbatim + gate-identical setup. 1 substantive build (M8 test). NOT replacing M8 in full gate.
# Source gate norm 12f392b6fe691230269a70bf9588fa4c25f71330639d0b6a464ceb8c532d67b0. Candidate 3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4 (precondition-enforced).
set +e
cd /code/kelgroups-issue-28
CAND_EXPECT=3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4
[ "$(git rev-parse HEAD)" = "$CAND_EXPECT" ] || { echo "ABORT: HEAD mismatch"; exit 3; }
[ -z "$(git status --porcelain | grep -v "^??" || true)" ] || { echo "ABORT: tracked drift"; exit 3; }
NIX="nix develop .#ci --quiet -c"
OVERALL_FAIL=0
EVIDENCE_DIR="${G28_EVIDENCE_DIR:-/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/handoffs/evidence}"
mkdir -p "$EVIDENCE_DIR" 2>/dev/null
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
SHORT="$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
abort3() { echo "ABORT(exit 3): $1"; echo "RECOVERY: inspect 'git status'/'git diff', restore by hand, re-run gate"; sleep 1; exit 3; }
pass() { echo "LEG-PASS: $1"; }
fail() { echo "LEG-FAIL: $1"; OVERALL_FAIL=1; }
evlog() { echo "$EVIDENCE_DIR/$STAMP-$SHORT-$1"; }
sha_of() { sha256sum "$1" | cut -d' ' -f1; }
LEG5_OK=1
ISO_LOG="$EVIDENCE_DIR/$STAMP-$SHORT-isolated-M8.log"; mkdir -p "$EVIDENCE_DIR"; exec > >(tee "$ISO_LOG") 2>&1
echo "ISOLATED-M8 gate=v10.2 candidate=$CAND_EXPECT"
revert_all() { git checkout -- lib/KelGroups/Event.hs lib/KelGroups/Fold.hs lib/KelGroups/Types.hs lib/KelGroups/Store.hs test/S28DemoApp.hs 2>/dev/null; }
  H_Event=$(git hash-object lib/KelGroups/Event.hs); H_Fold=$(git hash-object lib/KelGroups/Fold.hs); H_Types=$(git hash-object lib/KelGroups/Types.hs); H_Store=$(git hash-object lib/KelGroups/Store.hs); H_Demo=$(git hash-object test/S28DemoApp.hs)
  echo "entry hashes: Event=$H_Event Fold=$H_Fold Types=$H_Types Store=$H_Store Demo=$H_Demo"
  verify_restore() { # $1=file $2=prehash $3=mutant
    post=$(git hash-object "$1")
    if [ "$post" = "$2" ]; then echo "restored byte-exact: $1 ($3)"; else abort3 "restoration FAILED for $1 ($3): pre=$2 post=$post"; fi
  }
  mutant_diff_hash() { git diff -- lib test | sha256sum | cut -d' ' -f1; }
  kill_check() { # $1=log $2=row-slug $3=rownum $4=label
    fsec=$(awk '/^Failures:/,0' "$1")
    if [ -z "$fsec" ]; then echo "MUTANT-INCONCLUSIVE($4): nonzero exit with EMPTY Failures section (crash/timeout/infra — never a kill)"; return 1; fi
    while IFS= read -r nm; do
      [ -z "$nm" ] && continue
      if echo "$fsec" | grep -qF "$nm"; then echo "KILL-QUOTE($4): failing example: $nm"; echo "$fsec" | grep -B1 -A12 -F "$nm" | head -n 20; return 0; fi
    done < "$REGDIR/row$3.txt"
    echo "MUTANT-FAILURE($4): Failures section names NONE of the row's registered examples:"; echo "$fsec" | head -n 15; return 1
  }
REGDIR="$EVIDENCE_DIR/$STAMP-$SHORT-registered"; mkdir -p "$REGDIR"
slug="S28-1 rejecting step before append"
    awk -v slug="$slug" 'index($0, "describe \"" slug "\""){inb=1; next} inb && /^    describe "/{inb=0; next} !inb{next} /^[ \t]*(it|prop)[ \t]*$/{want=1; next} want==1{if (match($0, /^[ \t]*"[^"]*"/)) { s=substr($0, RSTART, RLENGTH); sub(/^[ \t]*"/, "", s); sub(/"$/, "", s); print s } else { print "EXTRACT-FAIL: " $0 } want=0; next} { if (match($0, /^[ \t]*(it|prop) "[^"]*"/)) { s=substr($0, RSTART, RLENGTH); sub(/^[^"]*"/, "", s); sub(/"$/, "", s); print s } }' test/S28AppApiSpec.hs > "$REGDIR/row2.txt" || true
  # M8 (test): F3 encode-first reorder kill for permanent faulting-codec controls (bound-faithful v10.2: lets+force hoisted pre-decision, scope-walked identifiers).
  echo "--- M8 F3-reorder (expect TEST red quoting a faulting-codec refused control) ---"
  if grep -q '^appendIntegratedEvent' lib/KelGroups/Store.hs && [ "$(grep -cF 'case applyIntegratedEvent integration gs signer event of' lib/KelGroups/Store.hs)" -eq 1 ] && [ "$(grep -cF '                let payloadJson = encode event' lib/KelGroups/Store.hs)" -eq 1 ] && [ "$(grep -c '_ <- evaluate payloadText' lib/KelGroups/Store.hs)" -eq 1 ] && ! grep -q 'MUTANT-M8' lib/KelGroups/Store.hs; then
    awk 'BEGIN{inb=0} /^                let payloadJson = encode event$/{inb=1; print "                -- MUTANT-M8-NOFORCE: encode+forcing hoisted pre-decision"; next} /^                _ <- evaluate payloadText$/{if (inb==1) {inb=0; next}} inb==1{next} {print}' lib/KelGroups/Store.hs > /tmp/g28m8a.hs && mv /tmp/g28m8a.hs lib/KelGroups/Store.hs
    perl -pi -e 'if (/^        case applyIntegratedEvent integration gs signer event of$/) { $_ = "        let payloadJson = encode event\n            payloadText = TE.decodeUtf8 (LBS.toStrict payloadJson)\n            noEnvelope = T.empty\n        _ <- evaluate payloadText -- MUTANT-M8-PREFORCE: encode forced pre-decision\n" . $_ }' lib/KelGroups/Store.hs
    [ "$(grep -c 'MUTANT-M8' lib/KelGroups/Store.hs)" -eq 2 ] || { fail "5-M8: splice count != 2 (noforce+preforce)"; LEG5_OK=0; }
    [ "$(grep -c '_ <- evaluate payloadText' lib/KelGroups/Store.hs)" -eq 1 ] || { fail "5-M8: forcing count != 1"; LEG5_OK=0; }
    [ "$(grep -cF 'let payloadJson = encode event' lib/KelGroups/Store.hs)" -eq 1 ] || { fail "5-M8: hoisted-let count != 1"; LEG5_OK=0; }
    echo "M8 diff sha256=$(mutant_diff_hash)"
    $NIX cabal test all -O0 --test-show-details=direct 2>&1 | tee "$(evlog leg5-M8-test.log)"; m8_exit=${PIPESTATUS[0]}; echo "M8 test exit=$m8_exit sha256=$(sha_of "$(evlog leg5-M8-test.log)")"; echo "M8 GHC-errors=$(grep -c 'error:' "$(evlog leg5-M8-test.log)" || true)"
    if [ "$m8_exit" -ne 0 ] && kill_check "$(evlog leg5-M8-test.log)" "S28-1 rejecting step before append" 2 "M8"; then pass "5-M8: test RED quoting faulting-codec refused control"; else fail "5-M8: expected witness-quoted RED (exit=$m8_exit)"; LEG5_OK=0; fi
    revert_all; verify_restore lib/KelGroups/Store.hs "$H_Store" M8
  else fail "5-M8: PRECONDITION missing (decision-first + encode-block x1 + force x1 + fresh sentinel in Store.hs)"; LEG5_OK=0; fi
echo "ISOLATED-M8-RESULT LEG5_OK=$LEG5_OK OVERALL_FAIL=$OVERALL_FAIL"
echo "post-HEAD=$(git rev-parse --short HEAD) post-clean=$(git status --porcelain | grep -vc "^??" || true)"
[ "$LEG5_OK" = "1" ] && [ "$OVERALL_FAIL" = "0" ] && exit 0 || exit 1
