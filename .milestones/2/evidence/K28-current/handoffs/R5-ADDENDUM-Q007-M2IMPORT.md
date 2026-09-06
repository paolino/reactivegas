# R5-ADDENDUM-Q007-M2IMPORT — M2 import-step repair (versioned, NOT an overwrite)

Ticket owner `t28-app-api`. Authority: NOTE-012 (desk NOTE-014 via epic —
same D3 mechanical class, existing authority + explicit 34-cap funding).
Lineage: r5 + gates v6/v7 + BINDING(-v5) stand; this addendum + gate v8 +
BINDING-v6 carry the Q-007 repair. Scope: M2 splice ONLY (+ version
strings). Splice semantics (unconditional-True bypass), kill
(rejecting-step witness quote), H-mandates, fences, objective all UNCHANGED.
No production bytes touched by this repair (candidate `84a2dae` frozen).

## Exact change (gate v7 → v8; nothing else in the file differs — proven by diff)

- Preconditions ADD: `[ "$(grep -c '^    , isMemberInView$' …)" -eq 1 ]`
  (exactly-one import line targeted — NOTE-012 static-uniqueness rule).
- Splice ADDS (after the proven swap): `sed -i '/^    , isMemberInView$/d'`
  (import entry is LAST in its block — verified on committed bytes — so
  deletion leaves valid comma-first layout, no dangling comma).
- Post-checks become: full excision (`isMemberInView` count==0 file-wide)
  + diff-shape exactly 1 insertion / 2 deletions (was 1+/1-).
- Kill, test command, revert, restore-verify UNCHANGED.

## Proofs pre-freeze (charge-0; no compilation per NOTE-012 §2)

- H1 count==1 + import-line count==1 on committed `Fold.hs` (guard at :443,
  import at :62; no other use sites — text ops only).
- Dry-run on `/tmp` copy of committed bytes with EXACT frozen splice lines:
  full excision (count 0), diff 1+/2- (`62d61` import deletion +
  `443c442` guard change), `in  if True` + intact `then` line.
- fourmolu-parse of the mutated copy: zero parse errors (drift hunks are
  the /tmp no-config artifact, same protocol as prior proofs).
- bash -n + no-C-comments + version strings verified.

## Spend (this instrument pass: 0/0; 34-cap path per NOTE-012 §3)

Reads, writes, hashes, dry-runs/parse-scans/synthetic fixtures — no
compilation anywhere. Standing: spent RED 4 + v6-GREEN 9 + v7-GREEN 9 = 22
builds; probes 15/24. Authorized: ONE complete corrected final gate v8 (9B)
+ reserved SLIM (3B) = 34 exact, ZERO reserve. STOP on real survivor /
setup-cost gap; inconclusive never a kill. Freeze values: STATUS NOTE
GATE-FROZEN-v8.
