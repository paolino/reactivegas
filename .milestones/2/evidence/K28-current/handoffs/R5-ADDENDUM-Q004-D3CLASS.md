# R5-ADDENDUM-Q004-D3CLASS — M3 precondition fix + class reconciliation (versioned, NOT an overwrite)

Ticket owner `t28-app-api`. Authority: NOTE-007 (desk NOTE-009 via epic —
existing A-001 §3 + D3 precedent, explicitly no desk round). Lineage: r5 +
gate v4 + BINDING(-v2) stand; this addendum + gate v5 + BINDING-v3 carry the
Q-004 fix. Scope: precondition-only. Splice text, kills, H-mandates,
fences, budgets, and objective all UNCHANGED.

## The fix (exact v5 text)

Old M3 first conjunct (`grep -q '^commitBaseChange ::'`, killed by 70-col
fourmolu splits — same class as D3/M1, proven by RED `S28DemoApp.hs:144-145`
precedent) replaced by:
`^commitBaseChange` prefix present + flattened (`tr '\n' ' '`)
`commitBaseChange +:: +Integration` signature-shape match + unchanged
`^commitBaseChange ` equation check (three conjuncts, fail-closed).
The M3 awk is UNTOUCHED — passthrough-correct on both layouts (bare name
line matches neither rule → printed; `::` continuation printed; equation
heads still trigger the stub; skip behavior identical). Synthetic
split-fixture proof (shapes only): preconditions `m3_pre=1`; awk output
keeps name+`::` lines byte-identical, stubs the equation, marker count 1,
next top-level intact.

## Class reconciliation (NOTE-009 deliverable, one pass — each anchor)

- H1 guard substring (~46ch, fits 70): STABLE single-line; count==1
  fail-closed; tail-preserving perl covers then-on-line/bare-guard both.
- H2 arm: short arms; `foldIntegratedFrom`-prefixed second block is
  textually inside the awk range BUT contained by triple lock
  (`!done5` single-shot + sed-range count==1 + splice-count==1) — synthetic
  two-block proof: line-4 arm replaced, line-8 arm intact. Constraint
  restated: owned-file helpers must not alter frozen semantics or decide
  anything (no second decision path).
- H4' arm: data-decl one-per-line; no `|` collision per scan; freshness
  runtime-checked.
- H5 write literal (~60ch + indent fits 70; `$`-shape distinct from
  historical): mandated single-line.
- M1 flattened + prefix: PROVEN on RED bytes (D3 record).
- All `^name` prefixes + closeKEL/writeTVar anchors: line-start/short,
  layout-free or RED-verified; ANCHOR-ATTEST re-verifies at GREEN.
- Two-layout it/prop rule: proven 19/19 on RED bytes.
- Gate-wide scan for remaining full-line single-line signature checks:
  NONE (M1 + M3 both flattened now) — class exhausted, quoted in BINDING-v3.
Conclusion: exactly ONE conflict existed (M3 signature conjunct) — fixed
above. No one-by-one drips on anchor mechanics after this (a FURTHER
conflict would arrive as a new Q with freeze-defect evidence, not silent
absorption — but the length analysis says the class is empty).

## E1-ADDITIONAL check (NOTE-007 §4)

Confirmed complete: `handoffs/FENCE-AMENDMENT-E1E2.md` exists with RED
hashes (InvariantsSpec `24d7d3db…`, StoreInvariantsSpec `f8978465…`,
ValidateSpec `400e8ba2…`, JSON.hs `87a611a1…`); STATUS journal line carries
bounds + hashes + TBD-at-submission. NOTHING missing — no completion work.

## Spend + freeze refs

This pass: 0 builds / 0 probes (reads, writes, hashes, synthetic awk runs
in /tmp, greps — no compilation). Envelopes unchanged (RED sunk 4, GREEN 9,
SLIM 3 = 16; probes ≤19/24). Freeze values: STATUS NOTE GATE-FROZEN-v5
(gate normalized + full + backup `gate-v5.sh.backup` + BINDING-v3 hash).
FROZEN_BASE stays RED `570fe4a…`; pins/ancestry unchanged.
