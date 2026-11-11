#!/usr/bin/env bash
# Slice gate v4 — #71 design record rewrite.
# v3 -> v4: desk NOTE-006 review (controls tested wrong failure classes) +
#   submission-1 findings (R71-08 count, R71-11 vacuity). All changes static.
#   D1: C3 resolves the stripped SM copy ALONE (VM no longer masks zero).
#   D2: C4 DELETES the marker text (no lean:REMOVED substitute), asserts the
#       mutation applied (before>=1, after==0, total still >0); stale
#       Types.lean alternative gone (canCloseGroup lives in Predicates.lean).
#   D3: every control asserts its INTENDED reason (expect_red) plus a genuinely
#       GREEN subject: SYNTH, generated FROM the required table, greens the
#       resolver; C4m/C7m mutate SYNTH itself. Mutant->predicate attribution in
#       evidence/gate-v4-attribution.log (auditor's exact edits replayed R1-R27).
#   D4: required anchors are file|symbol|line|section|R71 (mandate-derived
#       MINIMUM, no count asserted): git-show PIN line association + docs
#       citation + claim-section binding (cross-section C7). Row-level AUTH and
#       pending-table truth is submission-2 human-audit duty, labeled as such.
#   D5: leg 0c source-blob freshness (git diff PIN -- lean/ empty) + per-anchor
#       git-show-at-PIN line checks; C8 wrong-file / C9 wrong-line (+positives).
#   D6: leg 7 needs the DOTTED lean:KelGroups.majority_table marker + census
#       phrase + law/witness HEADING (bare token insufficient); theta sentence
#       verbatim; open-operator-question anchored. Row-level table truth is
#       explicitly submission-2 duty, not a gate claim.
# Control-only knobs (documented, fail-closed, never used on acceptance path):
#   DOC_SM_OVERRIDE (redirect shipped docs to a mutant copy), SKIP_CI=1 (skip
#   leg 12 in replay runs; leg 12 proven by real GREEN runs), REPLAYS=0
#   (inner replay runs do not recurse), REQUIRE_ANCHORS=0 (C5b only).
# Base: origin/master 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af (S1 #79 landed).
# Writable fence: docs/en/design/*.md (docs only).
#   Forbidden: lean/**, lakefile, justfile, nix/**, specs/, gate.sh itself,
#   Haskell/simulator sources, all production code/proofs/semantics.
#   Preserved headings (owner: keep verbatim; renames need T.O. ruling):
#   ## State, ## Events, ## Step, ## Route and the sealed base hook,
#   ### AUTH — Only entitled actors can act (law),
#   ### L1 — Governance enacts removal (law, restated against the hook),
#   ### L2 — Closure needs permission (law, with a stated limit),
#   ### L3 — Escrow at pledge (law), ### L6 — Conservation (flagship law),
#   ### L7 — Solvency is enforced, insolvency unreachable (law, scoped),
#   ## Laws versus finite witnesses, ## Unimplemented runtime composition,
#   ## Vote-lifecycle limits, ## Group closure: classification, not a theorem,
#   ## Voci non-goal, ## Dated operator authority,
#   ## Current versus ruled (pending merges), ## Reconciliation hook, ## Verifying.
# Budget: owner 2/6 spent; repair full run = next open spend. v4 battery spends
#   0 builds (static discovery only). Full CI leg 12 counts openly.
# Falsification (preserved): v1 BASE-RED (evidence/gate-v1.sh, base-red-gate.log);
#   v2 boundary probes (evidence/gate-v2-falsification.log);
#   v4 battery (evidence/gate-v4-falsification.log + attribution log).
#   C1-C9 + R1-R27 run every invocation (self-falsifying gate).
set -euo pipefail
cd "$(dirname "$0")"
PIN=4a6cd87fcbc3e4a536bbc9f240f5efe5704022af
DOC_SM=${DOC_SM_OVERRIDE:-docs/en/design/state-machine.md}
DOC_VM=docs/en/design/kelgroups-vote-machine.md
REPLAYS=${REPLAYS:-1}

echo "== leg 0: tracked hygiene + base + source-blob freshness =="
git diff --check
git merge-base --is-ancestor "$PIN" HEAD || { echo "BASE-RED: HEAD does not descend from PIN $PIN (rebase onto origin/master first)"; exit 1; }
git diff --quiet "$PIN" -- lean/ || { echo "SOURCE-DRIFT-RED: working-tree lean/ differs from PIN $PIN blobs (citations verified against other source than claimed)"; exit 1; }

echo "== leg 1: prerequisites fail-closed =="
for f in lean/Reactivegas/State.lean lean/Reactivegas/Types.lean lean/Reactivegas/Step.lean lean/Reactivegas/Composition.lean lean/Reactivegas/Predicates.lean "$DOC_SM" "$DOC_VM"; do
  if [[ ! -f "$f" ]]; then echo "PREREQ-RED: missing $f (fail-closed)"; exit 1; fi
done
echo "prereqs present at pin $PIN"

echo "== leg 2: State mapping =="
if ! grep -q "conti.*: List" lean/Reactivegas/State.lean; then echo "STATE-RED: conti missing in source"; exit 1; fi
if ! grep -q "casse.*: List" lean/Reactivegas/State.lean; then echo "STATE-RED: casse missing in source"; exit 1; fi
if ! grep -q "collections.*: List Collection" lean/Reactivegas/State.lean; then echo "STATE-RED: collections missing in source"; exit 1; fi
if ! grep -q "votes.*: KelGroups.Vote.VoteState" lean/Reactivegas/State.lean; then echo "STATE-RED: votes missing in source"; exit 1; fi
if grep -qE '^\| *`users` *\|' "$DOC_SM"; then echo "STATE-RED: docs still list users as State field"; exit 1; fi
if grep -qE '^\| *`responsabili` *\|' "$DOC_SM"; then echo "STATE-RED: docs still list responsabili as State field"; exit 1; fi
if ! grep -q "conti" "$DOC_SM"; then echo "STATE-RED: docs omit conti"; exit 1; fi
if ! grep -q "GroupView" "$DOC_SM"; then echo "STATE-RED: docs omit GroupView (substance of #62)"; exit 1; fi
echo "state green"

echo "== leg 3: Event count 14 =="
evt_count=$(awk '/inductive Event where/,/^deriving/ {if ($1=="|") c++} END{print c+0}' lean/Reactivegas/Types.lean)
echo "discovered Event constructors: $evt_count"
if [[ "$evt_count" != "14" ]]; then echo "EVENT-RED: source Event count is $evt_count, expected 14 at $PIN"; exit 1; fi
if grep -q "15 events" "$DOC_SM"; then echo "EVENT-RED: docs still claim 15 events"; exit 1; fi
if grep -q "all 15 events" "$DOC_SM"; then echo "EVENT-RED: docs still claim all-15"; exit 1; fi
if ! grep -qE "14 (event constructors|constructors|events)" "$DOC_SM"; then echo "EVENT-RED: docs do not state 14"; exit 1; fi
echo "event green"

echo "== leg 4: step signature with GroupView =="
if ! grep -q "def step (view : KelGroups.GroupView)" lean/Reactivegas/Step.lean; then echo "STEP-RED: source step signature drifted"; exit 1; fi
if ! grep -q "def stepEvent (view : KelGroups.GroupView)" lean/Reactivegas/Step.lean; then echo "STEP-RED: source stepEvent signature drifted"; exit 1; fi
if grep -q 'step : State → Event → Option State' "$DOC_SM"; then echo "STEP-RED: docs still carry stale 2-arg step signature"; exit 1; fi
if ! grep -q "GroupView" "$DOC_SM"; then echo "STEP-RED: docs omit GroupView arg"; exit 1; fi
echo "step green"

echo "== leg 5: Route 11 direct + 0 baseEnacted + 3 appDecided =="
if ! grep -q "11 \`direct\`, 0 \`baseEnacted\`, 3 \`appDecided\`" lean/Reactivegas/Composition.lean; then echo "ROUTE-RED: source route inventory drifted"; exit 1; fi
if ! grep -q "appDecided" "$DOC_SM"; then echo "ROUTE-RED: docs omit appDecided route"; exit 1; fi
if ! grep -qE "0 baseEnacted|baseEnacted.*(unpopulated|no event|zero)" "$DOC_SM"; then echo "ROUTE-RED: docs do not state baseEnacted arm unpopulated"; exit 1; fi
echo "route green"

echo "== leg 6: L1 deleted transition + canCloseGroup classification =="
if grep -q "removeUser" "$DOC_SM"; then echo "L1-RED: docs still cite deleted removeUser"; exit 1; fi
if grep -q "governance_enacts_remove" "$DOC_SM"; then echo "L1-RED: docs still cite deleted governance_enacts_remove"; exit 1; fi
if ! grep -q "canCloseGroup" lean/Reactivegas/Predicates.lean; then echo "CLOSE-RED: source canCloseGroup missing"; exit 1; fi
usages=$(grep -rn --include='*.lean' --exclude-dir=.lake "canCloseGroup" lean/ | wc -l | tr -d ' ' || true)
echo "canCloseGroup source usages (discovered extent): $usages"
if [[ "$usages" != "1" ]]; then echo "CLOSE-RED: canCloseGroup usages=$usages, expected 1 (orphan definition only) at $PIN — re-derive, do not trust"; exit 1; fi
if ! grep -q "canCloseGroup" "$DOC_SM"; then echo "CLOSE-RED: docs do not classify canCloseGroup"; exit 1; fi
if grep -qE "theorem.*(group_closure|close_group|canCloseGroup).*(proved|holds|preserved)" "$DOC_SM"; then echo "CLOSE-RED: docs invent a group-closure theorem"; exit 1; fi
echo "close green"

echo "== leg 7: law-vs-finite-witness distinction (heading + dotted marker + caveat) =="
if ! grep -qiE '^#+.*law.*witness|^#+.*witness.*law' "$DOC_SM"; then echo "WITNESS-RED: no law/witness heading (a leftover word is not a section)"; exit 1; fi
if ! grep -qF '`lean:KelGroups.majority_table`' "$DOC_SM"; then echo "WITNESS-RED: dotted lean:KelGroups.majority_table marker missing (bare token does not cite)"; exit 1; fi
if grep -qiE '29[^0-9A-Za-z]{0,3}of[^0-9A-Za-z]{0,3}224|29-of-224|Twenty-nine' "$DOC_SM" && ! grep -qiE "not.*(census|total)|syntactic categor" "$DOC_SM"; then echo "WITNESS-RED: docs cite 29 without not-a-census caveat"; exit 1; fi
echo "witness green"

echo "== leg 8: unimplemented composition + vote-lifecycle limits + theta sentence =="
if ! grep -qiE "PROVED-IN-MODEL|nothing.*consumes.*route|composition.*(leaf|not.*runtime|unimplemented)" "$DOC_SM"; then echo "COMP-RED: docs omit unimplemented runtime composition"; exit 1; fi
if ! grep -qiE "renounce.*(no-op|identity|succeeds and changes nothing)|notDesignee.*notProposer|vote.*lifecycle|appFold|voteApply" "$DOC_SM"; then echo "COMP-RED: docs omit vote-lifecycle limits"; exit 1; fi
if ! grep -q "Reachability" "$DOC_SM" || ! grep -q "Target" "$DOC_SM" || ! grep -q "Polarity" "$DOC_SM"; then echo "COMP-RED: unbound reachability/target/polarity block missing (M-06 class)"; exit 1; fi
if ! grep -q "exhibit" "$DOC_SM" || ! grep -q "s62bThreshold" "$DOC_SM"; then echo "COMP-RED: theta-exhibits disclosure missing (F-07 class)"; exit 1; fi
if ! grep -qF "The vote threshold policy θ is open" "$DOC_SM"; then echo "COMP-RED: theta-open sentence missing (F-07 class)"; exit 1; fi
echo "composition green"

echo "== leg 9: Voci non-goal (21, pair, open question, stale-twenty RED) =="
if ! grep -qiE "Voci" "$DOC_SM"; then echo "VOCI-RED: docs omit Voci catalogue non-goal"; exit 1; fi
if ! grep -qiE "ImpegnoVincolato|order-bound|product catalogue" "$DOC_SM"; then echo "VOCI-RED: docs omit order-bound pledge fact"; exit 1; fi
if ! grep -qiE "out of scope|non-goal" "$DOC_SM"; then echo "VOCI-RED: docs omit non-goal ruling"; exit 1; fi
if grep -qE 'twenty `Voci/' "$DOC_SM"; then echo "VOCI-RED: stale twenty-modules count (discovered extent is 21)"; exit 1; fi
if ! grep -qE "21 .{0,40}Voci|Voci.{0,40}21" "$DOC_SM"; then echo "VOCI-RED: docs do not state the 21-module extent"; exit 1; fi
if ! grep -q "Quantità" "$DOC_SM"; then echo "VOCI-RED: docs omit the distinct Quantita/Quantità pair"; exit 1; fi
if ! grep -q "Open operator question" "$DOC_SM"; then echo "VOCI-RED: open-operator-question paragraph missing (M-08 class)"; exit 1; fi
echo "voci green"

echo "== leg 10: dated operator authority + honesty + pending anchors =="
if ! grep -qE "2026-08-2[67]|2026-09-05" "$DOC_SM"; then echo "AUTH-RED: docs lack dated operator rulings"; exit 1; fi
if ! grep -qiE "V-2|pledge.*(pending|referente)|V-1|Q-001|NOTE-016|A-Q001" "$DOC_SM"; then echo "AUTH-RED: docs omit required ruling keys"; exit 1; fi
if ! grep -qiE "tension" "$DOC_SM"; then echo "AUTH-RED: honesty-tension paragraph missing (M-04 class)"; exit 1; fi
for k in "#68" "#69" "#81" "S1"; do
  if ! grep -qF "$k" "$DOC_SM"; then echo "AUTH-RED: pending anchor $k missing (M-10 class)"; exit 1; fi
done
if ! grep -qiE "re-pin|reconciliation hook" "$DOC_SM"; then echo "AUTH-RED: re-pin hook missing (M-10 class)"; exit 1; fi
if ! grep -q "^## Reconciliation hook" "$DOC_SM"; then echo "AUTH-RED: reconciliation hook section missing (M-10 class)"; exit 1; fi
if ! grep -q "4a6cd87f" "$DOC_SM"; then echo "AUTH-RED: S1 landed pin 4a6cd87f not cited (S1 class)"; exit 1; fi
if ! grep -q "check-trace-coverage-agreement" "$DOC_SM"; then echo "AUTH-RED: S1 agreement script not cited (S1 class)"; exit 1; fi
echo "authority green"

echo "== leg 11: citation resolver over the discovered qualified extent =="
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT
# Required anchors: file|symbol|line|section|mandate-row. Mandate-derived MINIMUM
# (spec R71-01..12) — explicitly NOT complete per-claim coverage: row-level AUTH
# and pending-table truth is submission-2 human-audit duty. Lines read at PIN.
REQUIRED_ANCHORS="Reactivegas/State.lean|State|23|## State|R71-01 Reactivegas/State.lean|Collection|13|## State|R71-01 Reactivegas/State.lean|bal|39|## State|R71-01 Reactivegas/State.lean|bump|62|## State|R71-01 Reactivegas/State.lean|comuneBal|47|## State|R71-01 Reactivegas/State.lean|stalled|55|## State|R71-01 KelGroups/Types.lean|GroupView|134|## State|R71-01 Reactivegas/Types.lean|Pledge|26|## State|R71-01 Reactivegas/Types.lean|Event|41|## Events|R71-02 Reactivegas/Types.lean|AppEvent|75|## Events|R71-02 Reactivegas/Types.lean|Proposal|119|## Events|R71-02 Reactivegas/Step.lean|step|44|## Step|R71-03 Reactivegas/Step.lean|stepEvent|147|## Step|R71-03 Reactivegas/Step.lean|BackdonateAuth|41|## Step|R71-03 Reactivegas/Step.lean|absorbConto|245|## Route and the sealed base hook|R71-03 Reactivegas/Step.lean|windUpAdmin|254|## Route and the sealed base hook|R71-03 Reactivegas/Step.lean|baseHook|298|## Route and the sealed base hook|R71-03 Reactivegas/Composition.lean|route|47|## Route and the sealed base hook|R71-03 Reactivegas/Composition.lean|Route|38|## Route and the sealed base hook|R71-03 Reactivegas/Predicates.lean|authorizedStep|74|### AUTH — Only entitled actors can act (law)|R71-04 Reactivegas/Predicates.lean|governanceEnacts|62|### L1 — Governance enacts removal (law, restated against the hook)|R71-04 Reactivegas/Predicates.lean|permissionToClose|50|### L2 — Closure needs permission (law, with a stated limit)|R71-04 Reactivegas/Predicates.lean|escrowHeld|55|### L3 — Escrow at pledge (law)|R71-04 Reactivegas/Predicates.lean|conservation|22|### L6 — Conservation (flagship law)|R71-05 Reactivegas/Predicates.lean|solvent|30|### L7 — Solvency is enforced, insolvency unreachable (law, scoped)|R71-05 Reactivegas/Predicates.lean|canCloseGroup|85|## Group closure: classification, not a theorem|R71-12 Reactivegas/Invariants.lean|conservation_preserved|430|### L6 — Conservation (flagship law)|R71-05 Reactivegas/Invariants.lean|solvent_preserved|1164|### L7 — Solvency is enforced, insolvency unreachable (law, scoped)|R71-05 Reactivegas/Invariants.lean|s62bThreshold|1302|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Types.lean|legacyThreshold|44|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Types.lean|zeroThreshold|48|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Validate.lean|VoteError|38|## Vote-lifecycle limits|R71-07 KelGroups/Vote/State.lean|closureCause|109|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Fold.lean|effectedState|87|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Fold.lean|sweepClosures|74|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Validate.lean|validateVoteEvent|54|## Vote-lifecycle limits|R71-07 KelGroups/Vote/Invariants.lean|open_questions_are_open|810|## Laws versus finite witnesses|R71-07 KelGroups/Invariants.lean|KelGroups.majority_table|450|## Laws versus finite witnesses|R71-05 Reactivegas/Step.lean|voteApply|172|## Vote-lifecycle limits|R71-07 Reactivegas/Step.lean|appFold|181|## Vote-lifecycle limits|R71-07 Reactivegas/Invariants.lean|checkAdminDepartureCleanup|1437|### L1 — Governance enacts removal (law, restated against the hook)|R71-04"
resolve_docs() {
  docs_list=$1; leandir=$2
  if [[ ! -d "$leandir" ]]; then echo "CITE-RED: lean dir $leandir absent (fail-closed)"; return 1; fi
  disc=$tmpdir/disc.txt; : > "$disc"
  while IFS= read -r lf; do
    awk -v file="$lf" '
      /^namespace[ \t]+[A-Za-z0-9_.]+/ { n=split($2,p,/\./); for(i=1;i<=n;i++) st[++d]=p[i]; next }
      /^end([ \t]+[A-Za-z0-9_.]+)?[ \t]*$/ { pop=($2=="")?1:split($2,t,/\./); while(pop-->0 && d>0) delete st[d--]; next }
      match($0, /^(private[ \t]+)?(noncomputable[ \t]+)?(def|theorem|inductive|structure|abbrev|instance|class|opaque)[ \t]+([A-Za-z0-9_.?!]+)/, m) {
        q=m[4]; for(i=d;i>=1;i--) q=st[i]"."q; print q"|"file"|"m[4] }
    ' "$lf" >> "$disc"
  done < <(find "$leandir" -name '*.lean' -not -path '*/.lake/*' | sort)
  if [[ ! -s "$disc" ]]; then echo "CITE-RED: discovered Lean extent is empty (fail-closed)"; return 1; fi
  raw=$tmpdir/raw.txt; : > "$raw"
  # shellcheck disable=SC2086
  grep -ohE $'lean:[^] \t`)\'",;}>]*' $docs_list 2>/dev/null | sort -u > "$raw" || true
  mcount=$(grep -c . "$raw" || true)
  if [[ "$mcount" -eq 0 ]]; then echo "CITE-RED: zero lean: markers in docs (nonzero extent required)"; return 1; fi
  echo "discovered markers: $mcount; discovered declarations: $(wc -l < "$disc" | tr -d ' ')"
  basenames=$tmpdir/bases.txt; : > "$basenames"
  occs=$tmpdir/occs.txt; : > "$occs"
  awk '/^##+ /{h2="";h3=""; if($0 ~ /^## /){h2=$0} else {h3=$0; }} /lean:/{while(match($0,/lean:[A-Za-z0-9_./:-]+/)){print substr($0,RSTART,RLENGTH)"|"h2"|"h3; $0=substr($0,RSTART+RLENGTH)}}' $docs_list 2>/dev/null | sort -u > "$occs" || true
  while IFS= read -r tok; do
    if [[ "$tok" =~ ^lean:[A-Za-z0-9_./-]+\.lean:[A-Za-z0-9_.]+$ ]]; then
      f=${tok#lean:}; f=${f%:*}; sym=${tok##*:}
      if [[ ! -f "$leandir/$f" ]]; then echo "CITE-RED: marker $tok cites missing file $f"; return 1; fi
      if ! grep -qE "^[ \t]*(private[ \t]+)?(noncomputable[ \t]+)?(def|theorem|inductive|structure|abbrev|instance|class|opaque)[ \t]+$sym([ \t]|$)" "$leandir/$f"; then
        echo "CITE-RED: marker $tok symbol $sym not declared in $f"; return 1
      fi
      echo "$sym" >> "$basenames"
    elif [[ "$tok" =~ ^lean:[A-Za-z0-9_.]+$ ]]; then
      base=${tok#lean:}
      if [[ "$base" == *.* ]]; then
        if awk -F'|' -v q="$base" '$1==q {found=1} END{exit !found}' "$disc"; then echo "$base" >> "$basenames"; echo "${base##*.}" >> "$basenames";
        else echo "CITE-RED: marker $tok matches no qualified declaration in discovered extent"; return 1; fi
      else
        exp=$(awk -F'|' -v b="$base" '$3==b {print $1}' "$disc" | sort -u)
        n=$(echo "$exp" | grep -c . || true)
        if [[ "$n" -eq 0 ]]; then echo "CITE-RED: marker $tok resolves nowhere in discovered extent"; return 1; fi
        if [[ "$n" -gt 1 ]]; then echo "CITE-RED: marker $tok is ambiguous ($(echo "$exp" | tr '\n' ' ')) — file-qualify it"; return 1; fi
        echo "$base" >> "$basenames"
      fi
    else
      echo "CITE-RED: malformed marker $tok"; return 1
    fi
  done < "$raw"
  : > "$tmpdir/sec.txt"
  while IFS= read -r tok; do
    if [[ "$tok" =~ ^lean:[A-Za-z0-9_./-]+\.lean:[A-Za-z0-9_.]+$ ]]; then
      sym=${tok##*:}
      grep -F "$tok" "$occs" | while IFS='|' read -r _ o2 o3; do printf '%s|%s|%s\n' "$sym" "$o2" "$o3"; done >> "$tmpdir/sec.txt" || true
    elif [[ "$tok" =~ ^lean:[A-Za-z0-9_.]+$ ]]; then
      base=${tok#lean:}; key=${base##*.}
      grep -F "$tok" "$occs" | while IFS='|' read -r _ o2 o3; do printf '%s|%s|%s\n' "$key" "$o2" "$o3"; done >> "$tmpdir/sec.txt" || true
    fi
  done < "$raw"
  if [[ "${REQUIRE_ANCHORS:-1}" == "1" ]]; then
  while IFS= read -r triple; do
    f=${triple%%|*}; rest=${triple#*|}; sym=${rest%%|*}; tail=${rest#*|}; line=${tail%%|*}; rest2=${tail#*|}; sec=${rest2%%|*}; row=${rest2##*|}
    if ! git show "$PIN:lean/$f" 2>/dev/null | sed -n "${line}p" | grep -qE "^[ \t]*(private[ \t]+)?(noncomputable[ \t]+)?(def|theorem|inductive|structure|abbrev|instance|class|opaque)[ \t]+$(echo "$sym" | sed 's/.*\.//')([ \t]|$)"; then
      echo "CITE-RED: required anchor $sym ($row) not at $f:$line of PIN $PIN — re-derive, do not trust"; return 1
    fi
    key=$(echo "$sym" | sed 's/.*\.//')
    if ! grep -qxF "$key" "$basenames"; then
      echo "CITE-RED: required anchor $sym ($row) cited nowhere in docs (removal fails)"; return 1
    fi
    if ! awk -F'|' -v k="$key" -v s="$sec" '$1==k && ($2==s || $3==s) {found=1} END{exit !found}' "$tmpdir/sec.txt"; then
      echo "CITE-RED: required anchor $sym ($row) cited outside its claim section $sec (cross-section satisfaction fails)"; return 1
    fi
  done < <(printf '%s' "$REQUIRED_ANCHORS" | sed 's/|R71-\([0-9][0-9]*\) /|R71-\1\n/g')
  fi
  echo "citation resolution green"
}
expect_red() {
  name=$1; want=$2; shift 2
  set +e
  out=$("$@" 2>&1)
  rc=$?
  set -e
  if [[ $rc -eq 0 ]]; then echo "NEG-RED: control $name did not RED"; return 1; fi
  if ! echo "$out" | grep -qF "$want"; then echo "NEG-RED: control $name RED for wrong reason (want [$want], got [$(echo "$out" | tail -n 1)])"; return 1; fi
  echo "$name green ($(echo "$out" | tail -n 1 | cut -c1-110))"
}
echo "-- resolver on shipped docs --"
resolve_docs "$DOC_SM $DOC_VM" lean || exit 1
echo "-- control C1 malformed (must RED malformed) --"
cp "$DOC_SM" "$tmpdir/c1.md"; printf '`lean:!!bogus`\n' >> "$tmpdir/c1.md"
expect_red C1 "malformed marker" resolve_docs "$tmpdir/c1.md $DOC_VM" lean || exit 1
echo "-- control C2 unknown (must RED resolves nowhere) --"
cp "$DOC_SM" "$tmpdir/c2.md"; printf '\n`lean:NoSuchDeclXYZ`\n' >> "$tmpdir/c2.md"
expect_red C2 "resolves nowhere" resolve_docs "$tmpdir/c2.md $DOC_VM" lean || exit 1
echo "-- control C3 zero markers, SM alone (must RED zero) --"
sed 's/lean:/zz:/g' "$DOC_SM" > "$tmpdir/c3.md"
expect_red C3 "zero lean: markers" resolve_docs "$tmpdir/c3.md" lean || exit 1
echo "-- control C4 canCloseGroup marker DELETED, rest remain (must RED required anchor) --"
before=$(grep -o 'lean:[^ `]*canCloseGroup' "$DOC_SM" | wc -l | tr -d ' ' || true)
sed 's/`lean:\(Reactivegas\/Predicates\.lean:\)\?canCloseGroup`//g' "$DOC_SM" > "$tmpdir/c4.md"
after=$(grep -o 'lean:[^ `]*canCloseGroup' "$tmpdir/c4.md" | wc -l | tr -d ' ' || true)
total=$(grep -c 'lean:' "$tmpdir/c4.md" || true)
if [[ "$before" -lt 1 || "$after" -ne 0 || "$total" -lt 1 ]]; then echo "NEG-RED: C4 mutation did not apply as designed (before=$before after=$after total=$total)"; exit 1; fi
expect_red C4 "required anchor canCloseGroup" resolve_docs "$tmpdir/c4.md $DOC_VM" lean || exit 1
echo "-- control C5 ambiguity fires on a real second declaration (must RED ambiguous) --"
mkdir -p "$tmpdir/ln/A" "$tmpdir/ln/B"
printf 'namespace Q1\ndef SameName := 1\nend Q1\n' > "$tmpdir/ln/A/M.lean"
printf 'namespace Q2\ndef SameName := 2\nend Q2\n' > "$tmpdir/ln/B/N.lean"
printf 'see `lean:SameName`\n' > "$tmpdir/c5.md"
expect_red C5 "is ambiguous" resolve_docs "$tmpdir/c5.md" "$tmpdir/ln" || exit 1
printf 'see `lean:Q1.SameName`\n' > "$tmpdir/c5b.md"
if ! REQUIRE_ANCHORS=0 resolve_docs "$tmpdir/c5b.md" "$tmpdir/ln" >/dev/null 2>&1; then echo "NEG-RED: qualified form should resolve"; exit 1; fi
echo "C5b green (qualified resolves; required-presence proven by C4, not waived)"
echo "-- control C6 missing Lean (must RED fail-closed) --"
expect_red C6 "absent (fail-closed)" resolve_docs "$DOC_SM $DOC_VM" /nonexistent-lean-dir-71 || exit 1
echo "-- control C7 cross-section: claim-site citation moved away (must RED section) --"
sed 's/`lean:\(Reactivegas\/Predicates\.lean:\)\?canCloseGroup`//g' "$DOC_SM" > "$tmpdir/c7a.md"
awk '/^## Group closure: classification, not a theorem$/{found=1} END{if(!found) exit 1}' "$tmpdir/c7a.md" || { echo "NEG-RED: C7 setup broken (claim heading absent)"; exit 1; }
awk '/^## State$/{print; print "`lean:Reactivegas/Predicates.lean:canCloseGroup`"; next}1' "$tmpdir/c7a.md" > "$tmpdir/c7.md"
c7moved=$(grep -c 'lean:Reactivegas/Predicates.lean:canCloseGroup' "$tmpdir/c7.md" || true)
c7orig=$(grep -o 'lean:[^ `]*canCloseGroup' "$DOC_SM" | wc -l | tr -d ' ' || true)
if [[ "$c7moved" -ne "$c7orig" ]]; then echo "NEG-RED: C7 setup broken (moved=$c7moved orig=$c7orig)"; exit 1; fi
expect_red C7 "outside its claim section" resolve_docs "$tmpdir/c7.md $DOC_VM" lean || exit 1
echo "-- control C8 wrong file location (must RED file/pin/symbol/line) --"
if git show "$PIN:lean/Reactivegas/Types.lean" 2>/dev/null | sed -n '85p' | grep -qE "^[ \t]*(def|theorem|inductive|structure|abbrev)[ \t]+canCloseGroup([ \t]|$)"; then echo "NEG-RED: C8 setup broken (unexpected match)"; exit 1; fi
if ! git show "$PIN:lean/Reactivegas/Predicates.lean" 2>/dev/null | sed -n '85p' | grep -qE "^[ \t]*(def|theorem|inductive|structure|abbrev)[ \t]+canCloseGroup([ \t]|$)"; then echo "NEG-RED: C8 positive broken (right file+line must match)"; exit 1; fi
echo "C8 green (wrong-file REDs, right-file GREENs)"
echo "-- control C9 wrong line (must RED file/pin/symbol/line) --"
if git show "$PIN:lean/Reactivegas/Predicates.lean" 2>/dev/null | sed -n '86p' | grep -qE "^[ \t]*(def|theorem|inductive|structure|abbrev)[ \t]+canCloseGroup([ \t]|$)"; then echo "NEG-RED: C9 setup broken (line 86 also matches)"; exit 1; fi
echo "C9 green (wrong-line REDs; right-line proven by C8 positive)"
echo "-- SYNTH: table-generated compliant docset must GREEN the resolver --"
{
echo "# Synth compliant subject (generated from REQUIRED_ANCHORS; proves the table satisfiable)"
prev=""
while IFS= read -r triple; do
  f=${triple%%|*}; rest=${triple#*|}; sym=${rest%%|*}; tail=${rest#*|}; line=${tail%%|*}; rest2=${tail#*|}; sec=${rest2%%|*}; row=${rest2##*|}
  if [[ "$sec" != "$prev" ]]; then echo; echo "$sec"; prev="$sec"; fi
  key=$(echo "$sym" | sed 's/.*\.//')
  case "$sym" in
    Proposal) echo "text \`lean:$f:$sym\` tail" ;;
    checkAdminDepartureCleanup) echo "text \`lean:$f:$sym\` tail" ;;
    *.*) echo "text \`lean:$sym\` tail" ;;
    *) echo "text \`lean:$sym\` tail" ;;
  esac
done < <(printf '%s' "$REQUIRED_ANCHORS" | sed 's/|R71-\([0-9][0-9]*\) /|R71-\1\n/g')
} > "$tmpdir/synth.md"
if ! resolve_docs "$tmpdir/synth.md" lean >/dev/null 2>&1; then echo "NEG-RED: SYNTH subject did not GREEN (table unsatisfiable as generated)"; resolve_docs "$tmpdir/synth.md" lean 2>&1 | tail -n 2; exit 1; fi
echo "SYNTH green (table satisfiable; SYNTH GREEN is not document correctness)"
echo "-- control C4m removal on SYNTH (must RED required anchor) --"
sed 's/`lean:\(Reactivegas\/Predicates\.lean:\)\?canCloseGroup`//g' "$tmpdir/synth.md" > "$tmpdir/synth-m.md"
expect_red C4m "required anchor canCloseGroup" resolve_docs "$tmpdir/synth-m.md" lean || exit 1
echo "-- control C7m cross-section on SYNTH (must RED section) --"
sed 's/`lean:\(Reactivegas\/Predicates\.lean:\)\?canCloseGroup`//g' "$tmpdir/synth.md" > "$tmpdir/synth-m2a.md"
awk '/^## State$/{print; print "`lean:Reactivegas/Predicates.lean:canCloseGroup`"; next}1' "$tmpdir/synth-m2a.md" > "$tmpdir/synth-m2.md"
expect_red C7m "outside its claim section" resolve_docs "$tmpdir/synth-m2.md" lean || exit 1

echo "== leg 12: full local CI (budgeted) =="
if [[ "${SKIP_CI:-0}" == "1" ]]; then echo "leg 12 skipped (SKIP_CI control-only run)"; else nix develop --quiet -c just ci; fi

echo "== replays: auditor-exact mutants must RED the intended leg (SKIP_CI, DOC override) =="
if [[ "${REPLAYS:-1}" == "1" ]]; then
perl_edit() { f=$1; code=$2; PERL_CODE="$code" perl -0777 -i -e 'eval $ENV{PERL_CODE}; die $@ if $@; print' "$f"; }
cut_between() {
  f=$1; start=$2; end=$3
  perl_edit "$f" "
    \$_=<>;
    my \$i = index(\$_, q{$start});
    my \$j = index(\$_, q{$end});
    die \"missing $start / $end\" if \$i < 0 || \$j < \$i;
    substr(\$_, \$i, \$j-\$i) = '';
  "
}
replay_gate() {
  id=$1; want=$2; mut=$3
  out=$(SKIP_CI=1 REPLAYS=0 DOC_SM_OVERRIDE="$mut" ./gate.sh 2>&1); rc=$?
  if [[ $rc -eq 0 ]]; then echo "NEG-RED: replay $id did not RED"; return 1; fi
  if ! echo "$out" | grep -qF "$want"; then echo "NEG-RED: replay $id RED for wrong reason (want [$want], got [$(echo "$out" | grep -E -- '-RED' | tail -n 1)])"; return 1; fi
  echo "$id green ($(echo "$out" | grep -E -- '-RED' | tail -n 1 | cut -c1-100))"
}
mkcopy() { cp "$DOC_SM" "$tmpdir/$1.md"; }
# --- R1-R3 state ---
needles() { grep -qF "$1" "$tmpdir/$2" || { echo "NEG-RED: replay $3 setup broken (needle [$1] absent)"; return 1; }; }
mkcopy R1x
perl_edit "$tmpdir/R1x.md" '$_=<>; die "needle missing" unless s/\Q| field | contents |\E/| field | contents |\n| `users` | stale membership list |/;'
needles '| `users` |' R1x.md R1 || exit 1
replay_gate R1-M-01-USERS "still list users" "$tmpdir/R1x.md" || exit 1
mkcopy R2x
perl_edit "$tmpdir/R2x.md" '$_=<>; die "needle missing" unless s/\Q| field | contents |\E/| field | contents |\n| `responsabili` | stale admin list |/;'
needles '| `responsabili` |' R2x.md R2 || exit 1
replay_gate R2-M-01-RESP "still list responsabili" "$tmpdir/R2x.md" || exit 1
mkcopy R3x
perl_edit "$tmpdir/R3x.md" '$_=<>; s/conti/XXXXCONTIXXXX/g;'
if grep -q conti "$tmpdir/R3x.md"; then echo "NEG-RED: replay R3 setup broken (conti remains)"; exit 1; fi
replay_gate R3-M-01-NOCONTI "docs omit conti" "$tmpdir/R3x.md" || exit 1
# --- R4-R5 events ---
mkcopy R4x
perl_edit "$tmpdir/R4x.md" '$_=<>; die "needle" unless s/14 event constructors/15 events/;'
needles '15 events' R4x.md R4 || exit 1
replay_gate R4-M-02-15 "still claim 15 events" "$tmpdir/R4x.md" || exit 1
mkcopy R5x
perl_edit "$tmpdir/R5x.md" '$_=<>; s/14 (event constructors|constructors|events)/N event constructors/g;'
if grep -qE "14 (event constructors|constructors|events)" "$tmpdir/R5x.md"; then echo "NEG-RED: replay R5 setup broken"; exit 1; fi
replay_gate R5-M-02-NO14 "do not state 14" "$tmpdir/R5x.md" || exit 1
# --- R6-R8 route/step ---
mkcopy R6x
perl_edit "$tmpdir/R6x.md" '$_=<>; s/appDecided/XXXXAPPXXXX/g;'
if grep -q appDecided "$tmpdir/R6x.md"; then echo "NEG-RED: replay R6 setup broken"; exit 1; fi
replay_gate R6-M-03-NOAPP "omit appDecided" "$tmpdir/R6x.md" || exit 1
mkcopy R7x
perl_edit "$tmpdir/R7x.md" '$_=<>; s/0 `baseEnacted` \(unpopulated — no event\nroutes to it\)/baseEnacted still in the vocabulary/; s/0 baseEnacted/baseEnacted present/g; s/baseEnacted.{0,80}(unpopulated|no event|zero)/baseEnacted remains/sg;'
replay_gate R7-M-03-NOBASE "baseEnacted arm unpopulated" "$tmpdir/R7x.md" || exit 1
mkcopy R8x
perl_edit "$tmpdir/R8x.md" '$_=<>; die "needle" unless s/def step      \(view : GroupView\) \(s : State\) \(signer : Key\) \(app : AppEvent\)/step : State → Event → Option State/;'
needles 'step : State → Event → Option State' R8x.md R8 || exit 1
replay_gate R8-M-03-STALESTEP "stale 2-arg" "$tmpdir/R8x.md" || exit 1
# --- R9 honesty ---
mkcopy R9x
cut_between "$tmpdir/R9x.md" "Two honest tensions the reader must not miss." "## Route and the sealed base hook"
if grep -q "Two honest tensions" "$tmpdir/R9x.md"; then echo "NEG-RED: replay R9 setup broken"; exit 1; fi
replay_gate R9-M-04-NOTENSION "honesty-tension" "$tmpdir/R9x.md" || exit 1
# --- R10-R11 witness ---
mkcopy R10x
perl_edit "$tmpdir/R10x.md" '$_=<>; for my $w ("universally quantified", "finite oracle", "Laws versus", "law versus", "law-vs", "Witness", "witness") { my $q = quotemeta($w); s/$q/XXXX/g; }'
replay_gate R10-M-05-NOWITNESS "no law/witness heading" "$tmpdir/R10x.md" || exit 1
mkcopy R11x
perl_edit "$tmpdir/R11x.md" '$_=<>; s/not a total census/COMPLETE CENSUS/g; s/syntactic category/complete list/g; s/majority_table/XXXXTABLEXXXX/g; $_ = "Twenty-nine of 224 checks.\n" . $_;'
replay_gate R11-M-05-29NOCAVEAT "dotted lean:KelGroups.majority_table marker missing" "$tmpdir/R11x.md" || exit 1
# --- R12-R13 composition ---
mkcopy R12x
perl_edit "$tmpdir/R12x.md" '$_=<>; s/PROVED-IN-MODEL/XXXX/g; s/proved-in-model/XXXX/g; s/nothing in this repository consumes the route/XXXX/g; s/composition/XXXX/gi; s/Composition/XXXX/g;'
replay_gate R12-M-06-NOCOMP "omit unimplemented runtime composition" "$tmpdir/R12x.md" || exit 1
mkcopy R13x
cut_between "$tmpdir/R13x.md" "Three links" "Because these links are unbuilt"
perl_edit "$tmpdir/R13x.md" '$_=<>; s/Reachability/XXXX/g; s/Polarity/XXXX/g; s/unbound/XXXX/g;'
replay_gate R13-M-06-NOUNBOUND "unbound reachability" "$tmpdir/R13x.md" || exit 1
# --- R14-R15 vote/theta ---
mkcopy R14x
cut_between "$tmpdir/R14x.md" "## Vote-lifecycle limits" "## Group closure:"
perl_edit "$tmpdir/R14x.md" '$_=<>; for my $w ("Vote-lifecycle", "vote-lifecycle", "renounce", "notDesignee", "notProposer", "appFold", "voteApply") { my $q = quotemeta($w); s/$q/XXXX/g; }'
replay_gate R14-M-07-NOVOTE "omit vote-lifecycle limits" "$tmpdir/R14x.md" || exit 1
mkcopy R15x
perl_edit "$tmpdir/R15x.md" '$_=<>; die "needle" unless s/The vote threshold policy θ is open\.//;'
if grep -qF "The vote threshold policy θ is open" "$tmpdir/R15x.md"; then echo "NEG-RED: replay R15 setup broken (sentence remains)"; exit 1; fi
replay_gate R15-F-07-THETAONLY "theta-open sentence missing" "$tmpdir/R15x.md" || exit 1
mkcopy R15bx
perl_edit "$tmpdir/R15bx.md" '$_=<>; s/The vote threshold policy θ is open\.//; s/legacyThreshold/XXXX/g; s/zeroThreshold/XXXX/g; s/s62bThreshold/XXXX/g;'
replay_gate R15b-M-07-NOTHETA "theta-exhibits disclosure missing" "$tmpdir/R15bx.md" || exit 1
# --- R16-R17 voci ---
mkcopy R16x
cut_between "$tmpdir/R16x.md" "## Voci non-goal" "## Dated operator authority"
perl_edit "$tmpdir/R16x.md" '$_=<>; for my $w ("ImpegnoVincolato", "product catalogue", "out of scope", "order-bound", "non-goal", "Voci") { my $q = quotemeta($w); s/$q/XXXX/g; }'
replay_gate R16-M-08-NOVOCI "omit Voci catalogue non-goal" "$tmpdir/R16x.md" || exit 1
mkcopy R17x
perl_edit "$tmpdir/R17x.md" '$_=<>; die "needle" unless s/\*\*Open operator question\.\*\* Whether the group reaches its outcome test\nwithout a catalogue — inherited, undecided, carried up by the milestone\nowner\. This record does not pick a side\.\n//;'
replay_gate R17-M-08-NOOPENQ "open-operator-question paragraph missing" "$tmpdir/R17x.md" || exit 1
# --- R18 authority dates ---
mkcopy R18x
perl_edit "$tmpdir/R18x.md" '$_=<>; s/2026-08-26/DATE-REDACTED/g; s/2026-08-27/DATE-REDACTED/g; s/2026-09-05/DATE-REDACTED/g; s/2026-08-25/DATE-REDACTED/g;'
replay_gate R18-M-09-NODATES "lack dated operator rulings" "$tmpdir/R18x.md" || exit 1
# --- R19-R21 pending table ---
mkcopy R19x
cut_between "$tmpdir/R19x.md" "## Current versus ruled (pending merges)" "## Reconciliation hook"
if grep -q "#66 S1" "$tmpdir/R19x.md"; then echo "NEG-RED: replay R19 setup broken"; exit 1; fi
if grep -q "## Current versus ruled" "$tmpdir/R19x.md"; then echo "NEG-RED: replay R19 setup broken (table heading remains)"; exit 1; fi
replay_gate R19-M-10-NOTABLE "pending anchor #81 missing" "$tmpdir/R19x.md" || exit 1
# NOTE R19 vs R20 share the want string by gate order (#68/#69 survive elsewhere
# in authority prose; #81 dies first). Distinguished by setup: R19 removes the
# whole table incl. heading; R20 keeps table+hook and removes only the V-5 row.
mkcopy R20x
perl_edit "$tmpdir/R20x.md" '$_=<>; s/^\| V-5 lifecycle.*\n//m;'
if grep -qF "#81" "$tmpdir/R20x.md"; then echo "NEG-RED: replay R20 setup broken (#81 elsewhere)"; exit 1; fi
for k in "## Current versus ruled" "#68" "## Reconciliation hook"; do
  if ! grep -qF "$k" "$tmpdir/R20x.md"; then echo "NEG-RED: replay R20 setup broken ([$k] gone, not row-scoped)"; exit 1; fi
done
replay_gate R20-M-10-NOV5 "pending anchor #81" "$tmpdir/R20x.md" || exit 1
mkcopy R21x
cut_between "$tmpdir/R21x.md" "## Reconciliation hook" "## Verifying"
replay_gate R21-M-10-NOHOOK "reconciliation hook section missing" "$tmpdir/R21x.md" || exit 1
# --- R22-R23 closure ---
mkcopy R22x
perl_edit "$tmpdir/R22x.md" '$_=<>; s/canCloseGroup/XXXXCLOSEXXXX/g;'
replay_gate R22-M-12-NOCLOSE "do not classify canCloseGroup" "$tmpdir/R22x.md" || exit 1
mkcopy R23x
perl_edit "$tmpdir/R23x.md" '$_=<>; die "needle" unless s/\*\*Verdict: missing guarantee\.\*\*/theorem canCloseGroup_holds is proved and preserved.\n**Verdict: missing guarantee.**/;'
needles 'theorem canCloseGroup_holds is proved' R23x.md R23 || exit 1
replay_gate R23-M-12-INVENT "invent a group-closure theorem" "$tmpdir/R23x.md" || exit 1
# --- R24-R26 followups ---
mkcopy R24x
cut_between "$tmpdir/R24x.md" "## Laws versus finite witnesses" "## Unimplemented runtime composition"
replay_gate R24-F-05-SECTION "no law/witness heading" "$tmpdir/R24x.md" || exit 1
mkcopy R25x
cut_between "$tmpdir/R25x.md" "## Laws versus finite witnesses" "## Unimplemented runtime composition"
perl_edit "$tmpdir/R25x.md" '$_=<>; s/witnessed/XXXX/gi; s/witness/XXXX/gi; s/finite oracle/XXXX/gi; s/universally quantified/XXXX/gi; s/law-vs/XXXX/gi;'
replay_gate R25-F-05-SCRUB "no law/witness heading" "$tmpdir/R25x.md" || exit 1
mkcopy R26x
perl_edit "$tmpdir/R26x.md" '$_=<>; s/not a total census/COMPLETE CENSUS/g; s/syntactic category/complete list/g; $_ = "Twenty-nine of 224 checks.\n" . $_;'
replay_gate R26-F-05-29KEEP "without not-a-census caveat" "$tmpdir/R26x.md" || exit 1
# R27 M-11-BOGUS == C2, M-11-EMPTY == C3, M-11-MALFORMED == C1 (attributed, not duplicated)
fi
trap - EXIT
rm -rf "$tmpdir"

echo "GATE-v4-GREEN"
