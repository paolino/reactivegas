# Tasks — #66 S3

## Slice S3

- [x] T301 Compiled census (M-1): extent, exclusion rule, statement closures, refusal vocabularies, spans (R-1, R-2, R-4, R-9)
- [x] T302 Runner normal mode (M-4/F-2): isolated application, positioned diagnostics, kill validation, baseline, restore (R-6, R-7, R-8, R-11)
- [x] T303 Runner negative-control mode (F-3) covering every R-10 control
- [x] T304 Mutant catalogue and ledger (M-2, M-3) covering every REQUIRED theorem and every refusal constructor (R-3, R-5, R-9)
- [x] T305 Rendered ledgers `lean/REACTIVEGAS-MUTANTS.md`, `lean/KELGROUPS-MUTANTS.md` with identity-level counts (R-12, R-13)
- [x] T306 `just lean-mutants` recipe and CI step on every PR and master push (R-15)
- [x] T307 `lean/LEAN-CLARITY.md` (R-14)
- [x] T308 OD74-S1-COMMENT: correct the stale doc comment above `seedDenyPermissionRefunds`

## Slice S3 — re-derivation on current master

- [ ] T309 Merge origin/master (2bd9a20: #81 V-5 lifecycle and S-12 refusals, #92 lake-root reach) and re-derive census, catalogue, ledger and renderings so R-1…R-15 hold on the merged tree
