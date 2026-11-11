#!/usr/bin/env bash
# S30-0 prerequisite probe. Runs INSIDE the ci dev shell. Frozen by
# T30-S30-0-FREEZE.md before execution. Reads artifacts; compiles nothing
# beyond the single declared `just build`.
set -u
EV="${EV:?evidence dir required}"
LABEL="${LABEL:?label required}"
mkdir -p "$EV"

printf '%s\n' "== toolchain =="
ghc --version; cabal --version | head -1; just --version 2>/dev/null || true
printf '%s\n' "== pre-build marker =="
touch "$EV/$LABEL-marker"
ls -l --time-style=full-iso "$EV/$LABEL-marker"

printf '%s\n' "== E1: build =="
just build; BUILD_RC=$?
printf 'build-exit=%s\n' "$BUILD_RC"
printf '%s\n' "exit=$BUILD_RC" > "$EV/$LABEL-build-receipt"
printf '%s\n' "id=$LABEL-$(date -u +%Y%m%dT%H%M%SZ)" >> "$EV/$LABEL-build-receipt"

printf '%s\n' "== E2: dist-newstyle present? =="
if [ -d dist-newstyle ]; then
  printf 'dist-newstyle: present; total .hi files=%s\n' "$(find dist-newstyle -name '*.hi' | grep -c . || true)"
else
  printf '%s\n' "dist-newstyle: ABSENT"
fi

for spec in "KelGroups.Event:Event:KelGroups/Event" "KelGroups.Server.JSON:JSON:KelGroups/Server/JSON"; do
  MOD="${spec%%:*}"; rest="${spec#*:}"; LEAF="${rest%%:*}"; REL="${rest#*:}"
  printf '%s\n' "== module $MOD (leaf=$LEAF rel=$REL) =="
  matches="$(find dist-newstyle -name "$LEAF.hi" -path "*$REL*" 2>"$EV/$LABEL-find-$LEAF.err")"
  printf '%s\n' "--- ALL candidates verbatim ---"
  printf '%s\n' "$matches"
  n="$(printf '%s' "$matches" | grep -c . || true)"
  printf 'E3/E4 candidate-count(%s)=%s\n' "$MOD" "$n"
  if [ "$n" -ne 1 ]; then
    printf '%s\n' "AMBIGUOUS-OR-ABSENT: $MOD selector returned $n candidates — charged and returned, no Vote claim"
    continue
  fi
  hi="$matches"
  ls -l --time-style=full-iso "$hi"
  if [ "$hi" -nt "$EV/$LABEL-marker" ]; then printf 'E5 freshness(%s)=NEWER-than-marker\n' "$MOD"
  else printf 'E5 freshness(%s)=NOT-newer-than-marker\n' "$MOD"; fi
  ghc --show-iface "$hi" > "$EV/$LABEL-$LEAF.dump" 2>"$EV/$LABEL-$LEAF.dump.err"; rc=$?
  printf 'E6 show-iface-exit(%s)=%s bytes=%s\n' "$MOD" "$rc" "$(wc -c < "$EV/$LABEL-$LEAF.dump")"
  if [ -s "$EV/$LABEL-$LEAF.dump.err" ]; then printf '%s\n' "show-iface stderr:"; head -5 "$EV/$LABEL-$LEAF.dump.err"; fi
  printf 'E6 dump-sha256(%s)=%s\n' "$MOD" "$(sha256sum "$EV/$LABEL-$LEAF.dump" | cut -d' ' -f1)"
done
printf '%s\n' "== probe complete =="
