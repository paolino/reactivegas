#!/usr/bin/env bash
# Successor acceptance gate (v16) — DRAFT, static preparation only.
# NOT FROZEN, NOT FALSIFIED. Do not run this as an acceptance instrument.
#
# v16 = v14 body entire + v15's invocation printing + a REWRITTEN step 8 that
# RETAINS per-suite evidence. v14 and v15 stay immutable; v15 remains the
# history of what actually certified ef773ec4.
#
# Why v16 exists. v15 discarded every nested suite's output: the ordinary,
# derive and vocab runs went to /dev/null, and the one log it did capture went
# to a mktemp deleted on every path including success. So a full v15 run
# charged FOUR substantive units while leaving only its own stage asserts
# behind. That breaks the exact condition under which nested executions may
# stand in for standalone suites — the auditor must be able to extract and
# retain "individual logs, exit codes and rejection stage for each". With v15
# it cannot, and would be pushed into duplicate standalone suites.
#
# This is the same defect class this campaign exists to end — evidence that
# cannot be independently inspected — and it was in the ticket owner's own
# instrument, written one turn after diagnosing it in the commit owner's
# receipts. Recorded here rather than quietly fixed.
#
# v16 therefore: writes each nested suite's stdout+stderr to a NAMED retained
# file, records its exit code, hashes it, and prints every path. Combined with
# the invocation receipt, each nested charge then names both how it was invoked
# and what it produced.
#
# Evidence goes OUTSIDE the repository on purpose: the gate asserts a clean
# worktree at the end, so writing receipts into the tree would make the
# instrument fail for its own side effect.
#
# Falsification and freezing happen only AFTER the accepted S2R base lands,
# because S2R changes the Invariants source/line identities and the justfile
# path this gate's sub-gates consume, and controls must be re-established
# against the input actually consumed.
#
# ---- v14's own header follows, kept as the record --------------------------
# Frozen NOTE-048 one-membership re-bind gate (v14). Ticket-owner authored.
#
# v12 supersedes v11 for ONE reason, on the A-002 ruling:
#
#   "quantify over the discovered extent, never over a list of members.
#    A freshness check must enumerate the sources actually cited and check
#    every one — a hand-maintained manifest is precisely the second list this
#    milestone keeps deleting."
#
# v11's step 1 iterated a hardcoded pair, Types.lean and Composition.lean.
# The transcription cites TWENTY-TWO Lean sources in CHECK_RECEIPT.sourcePins.
# So the gate written to catch the composition-pin drift was itself blind to
# twenty of the files it claims to stand over: it reproduced the exact defect
# it exists to catch. v12 derives the extent from the transcription instead.
#
# Each cited file is checked at ITS OWN declared pin against origin/master, so
# a citation added tomorrow cannot escape by not being listed in this file.
# The degenerate cases the old shape hid are now explicit REDs: an empty
# extent, an extent that collapses below two entries, and an extent missing
# either source the transcription cannot do without.
#
# v11 and v10 are kept as the record of what ran before each ruling.
#
# Falsified before freezing — see gate-v12-falsification.log:
#   A  stale pin on lean/KelGroups/Vote/State.lean, a file v11 never looked at
#   B  sourcePins emptied outright
#
# Earlier RED witnesses, pre-rebind tree (HEAD=dabfb860):
#   claim-red-pre-rebind.log  sha256 c9e8fc49244a8245a69657810911c518c5cdd27eed323283753df642cb6cc3ca
#   oracle-red-base.log       17 problems, exit 1
#   oracle-selftest-base.log  7/7 negative controls fired
set -euo pipefail

repo=${1:-/code/reactivegas-sim-fable}
# The #62 merge (PR #64). The brief pins against this commit, not against a
# floating branch tip.
lean_pin=934de7a8df136d86a8ad2caadbda99af60e58b59
root=/tmp/reactivegas/ms2/t-simulator-fable
oracle="$root/handoffs/oracle-one-membership-v2.mjs"
derive="$root/handoffs/derive-cited-sources.mjs"

cd "$repo"
core=economics-simulator-core.mjs
html=economics-simulator.html

red() { printf 'RED: %s\n' "$*" >&2; exit 1; }

# ---- invocation receipt -------------------------------------------------
printf 'INVOCATION: %s\n' "$0 $*"
printf 'ENV: RG_OMIT_NOOP=%s RG_CHROMIUM=%s RG_GATE_EVIDENCE=%s\n' "${RG_OMIT_NOOP-unset}" "${RG_CHROMIUM-unset}" "${RG_GATE_EVIDENCE-default}"
printf 'REPO: %s\n' "$repo"

# ---- 0. clean tree; the candidate is what is committed --------------------
test -z "$(git status --porcelain)" || red "worktree non pulito"

# ---- 1. the accepted pin is reachable ------------------------------------
git merge-base --is-ancestor "$lean_pin" origin/master \
  || red "il pin Lean $lean_pin non è raggiungibile da origin/master"

# ---- 1a. every CITED source is fresh, over the DERIVED extent -------------
cited=$(node "$derive" "$repo/$core")
test -n "$cited" \
  || red "estensione citata vuota: nessun sourcePins derivato dalla trascrizione"
cited_n=$(printf '%s\n' "$cited" | grep -c . || true)
test "$cited_n" -ge 2 \
  || red "estensione citata degenere ($cited_n file): il controllo si è ridotto a una lista"

stale_cited=""
while IFS=$'\t' read -r f pin; do
  test -n "$f" || continue
  a=$(git rev-parse --verify --quiet "$pin:$f" 2>/dev/null) || a=ASSENTE
  b=$(git rev-parse --verify --quiet "origin/master:$f" 2>/dev/null) || b=ASSENTE
  if test "$a" != "$b"; then
    stale_cited="${stale_cited}
  $f pin=$a origin/master=$b"
  fi
done <<< "$cited"
test -z "$stale_cited" \
  || red "sorgenti citate obsolete fra le $cited_n derivate:$stale_cited"

# the two the transcription cannot do without must be inside the derived set,
# so an extent that happens to pass cannot hide their absence
for must in lean/Reactivegas/Types.lean lean/Reactivegas/Composition.lean; do
  printf '%s\n' "$cited" | cut -f1 | grep -qx "$must" \
    || red "$must non compare fra le $cited_n sorgenti citate derivate"
done

# ---- 1b. the branch actually carries the merged Lean ---------------------
# The pre-rebind branch forked at ccdda830 (#55) and its worktree `lean/` was
# still the 15-constructor intermediate, so `lake build` compiled a Lean the
# transcription no longer claims to follow. Blob freshness alone cannot see
# that: it compares git objects at the pin, not the checkout.
git merge-base --is-ancestor origin/master HEAD \
  || red "il ramo non è ricostruito su origin/master: il lean/ del worktree non è quello fuso"
# v13 (NOTE-060): lean/lakefile.lean may now differ too, ONLY to register the
# two existing root drivers as build targets. scripts/check-reactivegas-inversion-coverage
# discovers every .lean under lean/ and builds it as a module, so an unregistered
# root driver is an "unknown target" and full CI is red. That failure predates
# C1 and arrived with the rebase onto master; the fence to fix it is scoped to
# these drivers and must not weaken master-side discovery or the axiom gate.
stray=$(git diff --name-only origin/master -- lean/ |
  grep -vxE 'lean/TraceDriverV1\.lean|lean/KelTraceDriverV1\.lean|lean/lakefile\.lean' || true)
test -z "$stray" || red "lean/ diverge da origin/master oltre i due produttori di tracce e il lakefile:
$stray"

# the lakefile delta is permitted only for driver registration: it must not
# touch the two default_target libraries master owns.
if ! git diff --quiet origin/master -- lean/lakefile.lean; then
  lakediff=$(git diff origin/master -- lean/lakefile.lean | grep -E '^[+-][^+-]' || true)
  printf '%s\n' "$lakediff" | grep -qE '^-' \
    && red "il lakefile RIMUOVE righe rispetto a origin/master: la registrazione dei driver e' additiva
$lakediff"
  printf '%s\n' "$lakediff" | grep -qiE 'default_target|lean_lib (Reactivegas|KelGroups)' \
    && red "la modifica al lakefile tocca le librerie default_target di master, oltre la deroga NOTE-060:
$lakediff"
  for d in TraceDriverV1 KelTraceDriverV1; do
    printf '%s\n' "$lakediff" | grep -q "$d" \
      || red "il lakefile diverge da master senza registrare $d"
  done
fi

# ---- 2. fourteen Event constructors, derived from the pinned Lean ---------
# Derived by parsing the pin, never from a remembered list.
lean_ctors=$(git show "$lean_pin:lean/Reactivegas/Types.lean" |
  awk '/^inductive Event where/{f=1;next} f&&/^inductive |^deriving |^structure |^abbrev /{f=0} f' |
  grep -E '^  \| [a-zA-Z]' | awk '{print $2}' | LC_ALL=C sort -u)
n=$(printf '%s\n' "$lean_ctors" | grep -c . || true)
test "$n" = 14 || red "costruttori Event al pin = $n, attesi 14:
$lean_ctors"
for r in addUser electResponsabile removeResponsabile removeMember; do
  printf '%s\n' "$lean_ctors" | grep -qx "$r" \
    && red "il pin Lean espone ancora il costruttore ritirato $r"
done

# ---- 3. the retired constructors are gone from the transcription ---------
# Absent, not renamed and not parked behind a `retired` marker: the marker
# existed only because the Lean still had them.
#
# Three of the four appear in NO vocabulary at the pin, so blanket is right.
# `removeMember` is both a retired `Reactivegas.Event` constructor and a live
# `KelGroups.Proposal` one — Composition.lean:89-99, `baseProposalFaithful`.
# Forcing the true claim that cites it to be reworded would be the gate
# deforming the product (Q-003 / A-003), so it is permitted only there.
#
# These greps read source text: they are a lead, not the evidence. The
# executable proof that the retired constructors are gone is the oracle's
# route inventory at step 5.
for ctor in addUser electResponsabile removeResponsabile; do
  if grep -n "$ctor" "$core" "$html" | grep -q .; then
    printf 'RED: costruttore ritirato %s ancora presente nella trascrizione:\n' "$ctor" >&2
    grep -n "$ctor" "$core" "$html" | cut -c1-160 | sed -n '1,5p' >&2
    exit 1
  fi
done

# v14 (NOTE-061): v13's inline block carried the SAME weakness in new clothes —
# it compared only EVENT_ROUTES/EV metadata and never the actual dispatch
# surface, silently skipped probes on a missing export, treated a thrown
# exception as a refusal, and exempted any line containing a second magic
# phrase. The check now lives in a separately falsifiable instrument that
# derives the dispatch extent from the machine, establishes the shape of a real
# refusal before believing one, requires a positive control through the same
# transport, and anchors the prose exemption to the parsed claim field/value.
node "$root/handoffs/retired-surface-probe.mjs" "$repo" "$(printf '%s' "$lean_ctors" | tr '\n' ',')"
node "$root/handoffs/retired-surface-probe.mjs" "$repo" "$(printf '%s' "$lean_ctors" | tr '\n' ',')" --selftest

grep -q 'retired-by-#62' "$core" "$html" \
  && red "marcatori retired-by-#62 superstiti: escono con i costruttori"

# ---- 4. no second membership store, no Nat↔Key bridge --------------------
grep -nE '\busers\s*:|\bresponsabili\s*:|\bresponsabili\b' "$core" | grep -q . \
  && red "users/responsabili sopravvivono nel core"
grep -nE '\bkgUid\b|\bkgKey\b|\bkgName\b|\bFOUNDER\b|COMUNE_ID' "$core" "$html" | grep -q . \
  && red "ponte Nat↔Key o sentinella numerica del comune superstite"

# ---- 5. behaviour: the frozen oracle, then its negative controls ---------
node "$oracle" "$repo"
node "$oracle" "$repo" --selftest

# ---- 6. the page is regenerated from the core, not forked ----------------
node economics-simulator-build.mjs --check

# ---- 7. everything already green stays green, controls still firing ------
git diff --check
node economics-simulator-claim-gate.mjs
node economics-simulator-claim-gate.mjs --selftest
node economics-simulator-trace-gate.mjs
node economics-simulator-trace-gate.mjs --selftest
node economics-simulator-vote-trace-gate.mjs
node economics-simulator-vote-trace-gate.mjs --selftest
node economics-simulator-scenario-gate.mjs
node economics-simulator-scenario-gate.mjs --selftest
node economics-simulator-teaching-gate.mjs
node economics-simulator-teaching-gate.mjs --selftest

test -z "$(git status --porcelain)" || red "i gate hanno sporcato il worktree"

# ---- 8. C-KEY / C-CHROME, with EVERY nested suite retained ---------------
ui=economics-simulator-ui-gate.mjs
test -f "$ui" || red "ui-gate assente: $ui"

# Receipts live OUTSIDE the repo on purpose: the gate asserts a clean worktree
# below, so writing them into the tree would fail the instrument for its own writes.
ev=${RG_GATE_EVIDENCE:-/tmp/reactivegas/ms2/t-simulator-fable/evidence/gate-v16-$(date -u +%Y%m%dT%H%M%SZ)-$$}
mkdir -p "$ev" || red "impossibile creare la directory delle ricevute: $ev"
printf 'EVIDENCE-DIR: %s\n' "$ev"

suite_rc=0
# run_suite NAME -- CMD... : retains stdout+stderr, the exit code, AND the
# invocation itself, so two suites with byte-identical output are still told
# apart by their own logs. That is precisely the S13/S15 defect this campaign
# hit, closed at the instrument instead of asked of the reader.
run_suite() {
  local name=$1; shift
  [ "$1" = "--" ] && shift
  local log="$ev/$name.log"
  {
    printf 'SUITE-INVOCATION: %s\n' "$*"
    printf 'SUITE-ENV-PARENT-SHELL: RG_OMIT_NOOP=%s\n' "${RG_OMIT_NOOP-unset}"
    printf 'SUITE-ENV-NOTE: parent-shell scope only. A child wrapped in "env VAR=.." takes its effective value from the SUITE-INVOCATION line above, not from this.\n'
  } > "$log"
  set +e
  "$@" >>"$log" 2>&1
  suite_rc=$?
  set -e
  printf 'SUITE-EXIT: %s\n' "$suite_rc" >> "$log"
  printf 'SUITE %s exit=%s sha256=%s log=%s\n' \
    "$name" "$suite_rc" "$(sha256sum "$log" | cut -d' ' -f1)" "$log"
}

run_suite ordinary -- node "$ui"
test "$suite_rc" -eq 0 || red "ui-gate: corsa ordinaria non verde (exit $suite_rc) — $ev/ordinary.log"

run_suite omit-K2 -- node "$ui" --omit K-2
test "$suite_rc" -ne 0 || red "controllo di omissione: --omit K-2 NON è rosso — $ev/omit-K2.log"
grep -q 'copertura incompleta' "$ev/omit-K2.log" \
  || red "--omit K-2 è rosso per la ragione sbagliata (attesa: copertura incompleta) — $ev/omit-K2.log"

# env, not a prefix assignment on a function call: a prefix would persist in the
# shell after the call and silently leak into the suites below.
run_suite omit-K2-noop -- env RG_OMIT_NOOP=1 node "$ui" --omit K-2
test "$suite_rc" -eq 0 \
  || red "scarto neutralizzato: con RG_OMIT_NOOP=1 la corsa --omit deve essere VERDE; se è rossa il controllo riporta il flag e non l'evidenza — $ev/omit-K2-noop.log"

run_suite derive-only -- node "$ui" --derive-only
test "$suite_rc" -eq 0 || red "derivazione: --derive-only non verde — $ev/derive-only.log"

run_suite vocab-only -- node "$ui" --vocab-only
test "$suite_rc" -eq 0 || red "vocabolario: parole vietate nel testo visibile — $ev/vocab-only.log"

run_suite vocab-expect-red-clean -- node "$ui" --vocab-only --expect-red pledge
test "$suite_rc" -ne 0 \
  || red "controllo del vocabolario: la pagina pulita non deve poter segnalare «pledge» — $ev/vocab-expect-red-clean.log"

test -z "$(git status --porcelain)" || red "la ui-gate ha sporcato il worktree"

printf 'RICEVUTE: %s\n' "$ev"
ls -1 "$ev"
printf 'GREEN: v16 — v14 integrale; C-KEY/C-CHROME con omissione rossa per la propria ragione, scarto neutralizzato VERDE, derivazione e vocabolario in entrambe le direzioni; ogni suite annidata ha invocazione, log, exit e sha256 ritenuti in %s\n' "$ev"
