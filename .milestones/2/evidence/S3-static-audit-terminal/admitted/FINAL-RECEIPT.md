# FINAL-RECEIPT — Phase-1 counters and model-scope inventory reconciliation

Static record. OP-10 spent and closed (see `OP10-RESULT.md`); nothing else run
for this note.

## Counters (recorded, never invented)

| class | spent | ceiling | remaining |
|---|---|---|---|
| substantive builds | 5 (4 historical overrun + OP-10 granted) | 5 prospective | 0 |
| targeted (elaboration/probe) | 3 (2 pre-grant `lake env lean` + OP-10 driver) | 1-operation allowance (spent) + unmetered history | 0 granted |

No probes ever run. The 4v3 overrun stays an overrun in its original campaign;
OP-10 was separately granted and charged.

## Model-scope reconciliation (239 source ↔ 1213 compiled, neither a quota)

Source extent: 239 qualified identities (163 non-private + 76 private),
27 modules, guard/effect/error axis per R3/R5. Compiled extent (OP-10 retained
output): 1213 distinct identities over 27 built modules, gate verdict ok.

- 163/163 non-private source identities present VERBATIM in the compiled list.
- 76/76 private source identities present via `_private.<Module>.<idx>.<name>`
  mapping. Private-name mapping preserved in the retained stdout lines.
- Remainder 974 = compiler-generated/internal-detail census (patterns overlap):
  95 `.inj`, 95 `.injEq`, 133 `sizeOf_spec`, 9 `ofNat_ctorIdx`, 9 `.eq_def`,
  101 `match_N.eq_N`, 122 `.eq_1` (incl. the counted duplicate
  `KelGroups.setInsert.eq_1` behind walkOcc 1214 vs distinct 1213), 373
  `_proof_*`, 209 `_private` total (76 source-private + 133 generated-private),
  `inst*`/deriving outputs. The 15 short-name pairs resolve as distinct
  qualified identities in both inventories.
- Unexpected names: NONE. Missing source identities: NONE. Open findings of
  this kind: none. The composition above IS the account; nothing filtered away.

## Unfunded forward envelope (corrected method, no grant)

Per-op targeted counts: inversion/solvent 3 (closure rebuilds + check),
vote/substrate 2, witnesses 1, aliases 0 (static, C2), final acceptance 1 build
+ 2 elaborations. Envelope: 14×3 + 7×3 + 6×2 + 10×2 + 10×2 + 5×1 + (1 build + 2)
with helper rows at $0 — arithmetic, batching unsubtracted, authorization zero.

*End of FINAL-RECEIPT.*
