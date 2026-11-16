# R5-ADDENDUM-Q006-GATESIDE — F1–F4 instrument repairs (versioned, NOT an overwrite)

Ticket owner `t28-app-api`. Authority: NOTE-009 (desk NOTE-011 via epic) as
refined by NOTE-010 (desk NOTE-012 via epic — 25-cap execution, no
gap-BLOCK). Lineage: r5 + gate v6 + BINDING(-v4) stand; this addendum + gate
v7 + BINDING-v5 carry the Q-006 repairs. Scope: instrument operations ONLY.
Splice semantics, kills, H-mandates, fences, objective all UNCHANGED. No
production bytes touched by these repairs (candidate `84a2dae` frozen).

## Exact changes (gate v6 → v7; nothing else in the file differs)

- F1 (M1 leg): `cabal build all -O0` → `cabal build all --enable-tests -O0`
  (all other tokens identical). Proof pre-freeze: `--dry-run -v1` exit 0
  with `kelgroups-0.1.0.0 (test:invariants)` in plan (quoted in BINDING-v5).
  (Record note: `cabal test all` also runs keri-hs suites — dependency-suite
  counts elsewhere are never slice witnesses.)
- F2 (M2 splice): line-start perl + marker-count check REPLACED by in-place
  `s/isMemberInView signer view/True/` (no line-start assumption, no
  emitted comment) + post-splice `grep -c … == 0` + `git diff --numstat`
  exactly 1 insertion / 1 deletion for the file. Synthetic mid-line proof
  (`in  if …` → `in  if True`, `then`-line intact, 1c1 diff) in BINDING-v5.
  Rationale recorded: code-side H1 reshape REJECTED (new commit would void
  identical-envelope SLIM + M4/M5 inheritance + full re-gate, all unfunded).
- F3 (M3 skip): `skip_m3==1 && /^[A-Za-z_]/ {…} skip_m3==1 {…}` REPLACED by
  blank-keeping + col-0-ends-and-prints + indented-deletes triple. Proven on
  a copy of the REAL `Fold.hs:315-352` region: equations stubbed, marker
  count 1, both `{- | -}` blocks + next def byte-identical, no stray
  (BINDING-v5 quotes the diff). H3 + single-equation record (committed
  bytes: exactly one `^commitBaseChange ` equation) stand.
- F4 (M6 backdoor+rewire): `:: TVar (GroupState s) -> STM ()` /
  `readTVar var >>= \gs -> writeTVar var (gs { appFold = newApp })` REPLACED
  by `:: TVar (GroupState s) -> GroupState s -> STM ()` /
  `writeTVar var newGs`; rewire passes `gs` (not `(appFold gs)`). Export +
  STM-import splices + count==4 + kill UNCHANGED. Construction review (mine,
  pre-binding): TVar/GroupState/writeTVar present in committed imports; STM
  via splice; `gs` bound at H5 site (`Store.hs:604`, enclosing scope of the
  627 STM write); zero `appFold` mentions (both NOTE-011 defects closed).
  Compile failure at re-proof = FAILED row, never a pass.
- Version strings: `G28-1 v7 (r5-Q006-addendum)` (title + `GATE_VERSION`).
  FROZEN_BASE stays RED `570fe4a…` (ancestry covers `84a2dae`).

## M4/M5 status (evidence, not acceptance)

LEG-PASS + KILL-QUOTE on `84a2dae` stand as evidence (full log `4405c545…`
verified); programs UNCHANGED in v7. Acceptance ONLY from the complete
corrected v7 run — never old excerpts + new rows concatenated (NOTE-010 §2).

## Spend (this instrument pass: 0/0; envelopes per NOTE-010 §4)

Reads, writes, hashes, dry-runs (no-compile), synthetic awk/perl fixtures
in /tmp, fourmolu-parse scans — no compilation anywhere. Standing: spent
RED 4 + failed GREEN 9 = 13/16 builds; probes 15/24. Authorized: ONE
complete corrected full gate v7 (9B) + final SLIM (3B) = 25 exact, ZERO
reserve. STOP on real defect / past-25 / inconclusive-never-kill.
