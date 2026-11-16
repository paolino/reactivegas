# PACKET INDEX — one current packet (operative assessment + full history + evidence)

Base `3590c0015b84fd58004bf6fb44dd18b107304c48` (#66 OPEN). Static only; spend 5
substantive / 3 targeted, 0 remaining in any grant. No coverage claimed. No
Phase-2/audit commissioned by any label here.

## Operative assessment (current; read these)

| document | sha256 (prefix) | status |
|---|---|---|
| `PHASE1-REPORT-R5.md` | `3e51f229…` | operative base assessment (extent, ownership annex, OP plan v1) |
| `CORRECTIONS-008.md` | `827a7678…` | comune/alias/M-elab corrections (premise-transport now further corrected by CORRECTIONS-009) |
| `CORRECTIONS-009.md` | `a906172f…` | absence withdrawal, alias rule, M-elab** closure method |
| `CORRECTIONS-010.md` | `897b7afd…` | cost-class split, witness scope, 974 partition (counts corrected below) |
| `OP10-RESULT.md` | `fd07fc90…` | granted OP-10 record + 239↔1213 reconciliation |
| `FINAL-RECEIPT.md` | `dc616c6a…` | counters + model-scope reconciliation |
| `OP10-identity-classes.txt` | `2abb21bb…` | **identity-to-class artifact: all 1213 compiled names, one class each** |
| this `INDEX.md` | — | packet map + NOTE-011 finishes (operative) |

## History (superseded where corrected above; preserved, never rewritten)

`PHASE1-REPORT.md` (`dbc2cb68…`), `PHASE1-REPORT-R2.md` (`32c06530…`),
`PHASE1-REPORT-R3.md` (`9b5bf6c2…`), `PHASE1-REPORT-R4.md` (`5216adc3…`).

## Evidence retained (hashes)

`OP10-stdout.txt` (`e2770204…`), `OP10-stderr.txt` (`28cff59b…`),
`OP10-identities.txt` (`8fa4cc7c…`), `P1A-qualified-inventory.txt`
(`efdeb3…`), `P1A-qualified-classified.txt` (`ef93b9…`), P1-C build logs
(`6dedd2…`, `43fae2…`), variant diff (`0bdf4e…`), check summaries (`a17fcf…`,
`4d4bfc…`).

## NOTE-011 finishes (operative corrections)

1. **Validate arithmetic corrected by convention, not by envelope.** Step-class
   counts 2 rebuilds + 1 check per three-node closure; Validate-class now the
   same: Validate → Integration → Invariants = **2 rebuilds + 1 check = 3/op**
   (the fourth command did not exist — no such command is named because none
   was ever specified). B-admit envelope 12 → 9. Corrected future envelope:
   42+21+6+12+20+9+6+8+5 = 129 targeted + OP-71 (1 build + 2 elaborations),
   unfunded, batching unsubtracted. t57 worst-case-3 stands ONLY as
   module-unknown worst case, reducible to 2 at authoring.
2. **Identity-to-class artifact delivered** (`OP10-identity-classes.txt`, 1213
   rows, exclusive classes): SOURCE 163, SOURCE-PRIVATE 76, GEN-PROOF 357
   (incl. all `inst*-decide` proof obligations found — the GEN-INST rule
   matched nothing further, stated not hidden), GEN-SIZEOF 133, GEN-PRIVATE
   133, GEN-EQ 130 (rule: all `.eq_N` + match equations — verified disjoint
   from `_proof_` in this output: zero names match both), GEN-INJEQ 95,
   GEN-INJ 95, GEN-PROJECTION 12 (each a verified structure field with no
   declaration site), GEN-EQDEF 9, GEN-CTORIDX 9, GEN-RECURSOR 1
   (`Reach.brecOn`). No name left in a bucket: every row carries exactly one
   class. Limits: generated classes by name-shape + site-absence; source
   classes by exact/mapped match; mechanism of `thmInfo` status for
   projections/recursor not further established. Nothing stronger invented on
   top of the 239 exact matches.
3. **Terminal event below** (STATUS tail) supersedes all earlier "Next" lines,
   which remain above as history.

## Receipt-by-receipt admissibility / ownership / plan (pointers)

- Admissibility: R4 §1 as corrected by R5 §1 (hunk-level demotions) — 0
  REUSABLE-BOUNDED; STALE-DEMONSTRATED only with named hunks; else
  UNESTABLISHED-REUSE/UNRECOVERABLE with retrieval records.
- Ownership: R3 annex + R5 §2 + CORRECTIONS-008/009 (premise-transport,
  no-quota, proof-dependency maps). Solvent/alias DEFERRED labels removed
  where mapping finished; execution-side kills pending under future grant.
- Per-op plan + cost: R5 §3/§5 through CORRECTIONS-009/010 and finish 1 above.

## Honest missing evidence (open, owned)

- Past-provenance permanents (t62 KILLED, t54-auditor R-rows, t59/Haskell/
  simulator rows): no execution closes them.
- UNESTABLISHED-REUSE rows (GUARDS, FENCE, t57 ×10, t54-auditor, evidence-NONE
  rows): reuse unestablished; fresh runs need new grants + (t57) new
  instruments.
- Cold-cost log: permanently lost, never re-run. Environment extent: closed by
  OP-10. Helper satisfiability: exhibited statically (§4/R3 as corrected).

*End of INDEX. A zero-execution auditor inherits this packet and nothing else;
every row above is challengeable as NOTE-011 states.*
