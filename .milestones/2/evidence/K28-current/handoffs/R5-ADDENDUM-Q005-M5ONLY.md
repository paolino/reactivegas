# R5-ADDENDUM-Q005-M5ONLY — M5 block-computation fix + closure (versioned, NOT an overwrite)

Ticket owner `t28-app-api`. Authority: NOTE-008 (desk NOTE-010 via epic —
same D3 mechanical class, existing authority, no desk round). Lineage: r5 +
gate v5 + BINDING(-v3) stand; this addendum + gate v6 + BINDING-v4 carry the
Q-005 fix. Scope: M5 block COMPUTATION only. Mutation text, H2/H2b
selection, kill, H-mandates, fences, budgets, objective all UNCHANGED.

## The fix (exact v6 text)

Old (v5): `m5block="$(sed -n '/^foldIntegrated/,/^[^ \t]/p' …)"` — range
starts at the bare signature-name line and ends at the equation head, so on
fourmolu-shaped bytes it spans signatures+heads ONLY (measured 0 arm-hits
on real bytes; line-count color child-8/epic-12/mine-irrelevant — invariant
agreed: 0 hits).
New (v6): `m5block="$(awk '/^foldIntegrated / && !inb { inb=1 } inb &&
/^[A-Za-z_]/ && $0 !~ /^foldIntegrated / { exit } inb { print }' …)"` —
M4-style inb-range FROM the equation head (trailing space: matches equation
heads, NOT the bare signature-name line and NOT `foldIntegratedFrom…`)
through the next top-level head. Triple-lock intent preserved (awk range +
`!done5` single-shot + splice-count==1).

## Proofs on ACTUAL GREEN bytes (dirty tree, reads only, pre-freeze)

- Extraction output: 10 lines, from `Fold.hs:463`
  (`foldIntegrated integration initial =`) through the body, ending before
  the next top-level head. `m5a=1` (arm at :468), `m5b=0` → H2 branch.
- From-counterexample: `Fold.hs:477` bare + `:482` equation + `:487` arm
  all present in FILE, `foldIntegratedFrom` appears 0 times in extraction
  output — provably uncounted.
- M5 mutation awk (UNCHANGED from v5) traced correct on these bytes by owner
  and verified by reading: `in_f` survives col-0 heads, indented arm 468
  reached, `!done5` contains the 487 arm, splice-count==1 enforced.
- Gate-wide scan for remaining `sed -n '/^…/,…/p'` range checks: NONE —
  class exhausted with this fix.
- Dirty Fold.hs hash `d76219e5…` (transfer check: committed bytes must hash
  equal at submission or re-bind; commit preserves bytes).

## Closure (NOTE-009 deliverable completed)

Owner's re-sweep adopted (M2/M3-v5/M4/M6/M1 layout-robust on the actual
tree); with this fix the precondition class is EMPTY — stated explicitly,
no further anchor drips expected. (A FUTURE conflict would arrive as a new
Q with freeze-defect evidence, never silent absorption.)

## Sequencing (desk-ordered NOTE-008 §2, overriding "commit after gate")

Commit GREEN first (local signed commit on RED `570fe4a`, journaled SHA —
commitment, not acceptance), THEN full gate on the committed candidate.
Clean-tree guard NOT weakened; mutant legs NEVER on uncommitted tree.
Narrow dev successes are NOT the full gate (only the 9-build envelope
counts).

## Spend

This pass: 0 builds / 0 probes (reads, writes, hashes, awk runs in-shell
over source bytes, greps — no compilation). Envelopes unchanged (RED sunk
4, GREEN 9, SLIM-final 3 = 16 exact; probes ≤19/24). Freeze values: STATUS
NOTE GATE-FROZEN-v6.
