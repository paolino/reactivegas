# CORRECTIONS-019 — reach-based generation; per-row grounds; eight dispositions (no execution)

Static only: proof-body reads at base `3590c001`, writing. No builds, queries,
probes, mutations, Phase-2, hidden workers, or audit. Prior artifacts preserved
as history (CORRECTIONS-018 and OPMAP-v6 stay cited where read); this file +
`OPMAP-v7-requirement-verdict-grounds.txt` govern the generation method and map
where they conflict with earlier operative text.

## 1. Method corrected: reach, not unfold

CORRECTIONS-018:44-46 (unfolding/matching = KILL) is WITHDRAWN as the generation
rule. The operative rule is CORRECTIONS-018:10-14, now enforced per row: a KILL
needs the mutated component REACHED by something the proof consumes — (a) the
goal projects the changed field, (b) a consumed hypothesis mentions it, or (c)
the statement is false at a witness satisfying its antecedent — or (P) proof
sensitivity with the actual failing obligation bound (induction-unification
mismatch, exact-term mismatch, split-shape mismatch, decide-flip). Unfolding is
evidence a definition is reached, never the ground.

Self-test from NOTE-019: `governance_enacts_windUpAdmin` does `simp only
[windUpAdmin]` yet is KILL-grounded (a) on its collections/rest projection
(collections-part mutant), with the refundAll mutant correctly excluded by the
projection-erasure test. The rule no longer re-derives the conceded row. The
four guard inversions carry no KILL (pure-Bool bodies, nothing reached).

## 2. Retained map with grounds

`handoffs/OPMAP-v7-requirement-verdict-grounds.txt` (`89337291…`, 207 lines
`OP|requirement|verdict|mutated-atom-or-tag|required-input|GROUND:…`):
75 KILL (every one carrying (a)/(c)/(P) + carrier), 29 OBSERVED (upstream row
named), 31 OPEN-KILL (bounded search stated), 60 ELAB-STATIC, 9 RECOVERED, 2
STATIC, 1 ACCEPT. Machine-audited: 158/158 authored present, 0 helpers, 0
family tokens in mutant column, one mutant per kill op (suffix splits where two
were needed; OP-41 threshold-value kill and OP-29/31 redundant arm-cases
withdrawn with reasons).

## 3. Disposition of the eight original findings (owners + acceptance status)

| # | finding | disposition in this packet | owner | accepted? |
|---|---|---|---|---|
| F-01 | receipt transcription | corrected transcriptions retained (E-TOJSON KILLED; docs M1–M5/M1–M4; recut1 10 rows; t74 five G74-* OPEN; 42/43 split) | seat | desk decision pending |
| F-02 | provenance recovery | report-sha vs candidate distinguished; `000ff76a` in DB; t57 six instruments read as fixtures; nothing auto-upgraded | seat | desk decision pending |
| F-03 | ownership relation | literal per-row atoms + reach-based taxonomy above (role/hook atoms kept; R-canAdd linkage OPEN) | seat | desk decision pending |
| F-04 | `no_expiry` scope | corrected to accepted arbitrary-event statement | seat | desk decision pending |
| F-05 | non-vacuous witnesses | per-hypothesis instantiations; vacuous cases replaced | seat | desk decision pending |
| F-06 | executable complete plan | finite verdict-tagged map above (this file's envelope is arithmetic, not authority) | seat | desk decision pending |
| F-07 | attribution vs isolation | three-way split kept (attribution / separation-OPEN / historical-fence) | seat | desk decision pending |
| F-08 | append-only journal | kept; all writes since NOTE-013 by append-to-EOF with tail readback | seat | desk verifies by readback |

D6 clarity items → #71 via desk. S5 (comune connection question,
ONWARD-68-INV-01, retention) → S5. V-5 lifecycle → #81. ONWARD-68-INV-01 text
unchanged by this packet (no model edit).

## 4. Envelope restated, separate from authority (unfunded; batching unsubtracted)

Kill invocations by mechanical closure class: Step-closure 3 ×20 ops
(OP-11..24, OP-25, OP-59, OP-63, OP-67G, OP-69, OP-69L) = 60; Fold-closure 2
×14 (OP-39,40,42K,46a/b,50,51,52,52B,60,61,62,63B,64) = 28; Validate-closure 3
×4 (OP-49,54,55,56 — OP-56 dup-mutant kills validateDirectAdmission_ok only;
non_admin/membership go GREEN under it: notAnAdmin branch first) = 12;
Integration-closure 2 ×6 (OP-57,57B,58,58B,58C,58D) = 12; K-State-closure 5
×2 (OP-67,67b) = 10; Vote-State-closure 4 ×1 (OP-53) = 4; State-R-closure 4
×1 (OP-25B) = 4. Kill sum: 60+28+12+12+10+4+4 = 130. Re-runs OP-42..45: 4.
ELAB file runs (OP-66E/68/69/70/71/72/74): 7. Acceptance OP-73: 1 build + 2
elaborations. **Total: 130+4+7+2 = 143 targeted + 1 build — unfunded
arithmetic, not a grant or budget.** All prior envelope totals WITHDRAWN as
fitted artifacts. OPEN set retained non-empty (31 witness/param rows + 5 solvent conjunct-level notes in RELATION; alias roles now live as OBSERVED lines in the map itself).

*End of CORRECTIONS-019. The target is a correct assessment, not a zero-OPEN
table.*
