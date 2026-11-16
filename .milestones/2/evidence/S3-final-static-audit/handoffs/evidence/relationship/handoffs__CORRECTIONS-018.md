# CORRECTIONS-018 — causal taxonomy: kill needs the obligation reached (no execution)

Static only: proof-body reads at base `3590c001` (unfold-vs-call audit of every
kill-op row), writing. No builds, queries, probes, mutations, Phase-2, hidden
workers, or audit. Prior artifacts preserved; this file +
`OPMAP-v6-requirement-verdict-taxonomy.txt` govern the taxonomy where they
conflict with earlier operative text.

## The corrected rule (inheriting NOTE-017's wording correction)

Proof sensitivity IS a legitimate kill basis — named as such, with the actual
failing obligation bound. A KILL needs the mutated component REACHED by
something the proof consumes: the goal projects the changed field, a consumed
hypothesis mentions it, or the statement is false at a witness satisfying its
antecedent. Upstream-lemma calls are OBSERVED (real protection, never the
row's RED). Neither holds for tautological/projection/checker-local content —
ELAB-STATIC. Unidentified falsifiers stay OPEN-KILL with bounded searches.

## The five rows, verified and fixed

1. `close_permission_to_close` → OBSERVED (upstream `step_close_inv`): hypothesis
   IS `stepEvent`, proof calls `step_close_inv` + `close_guard_inv`; S4-B P07
   corroborates upstream-only (single error at the inversion). Not ELAB, never KILL.
2. OP-67a keeps KILL with CORRECTED mutant (strengthening
   `(adminCount+1)/2` → `adminCount+1`) and CORRECTED witness **n=2**
   (`6≤2` false, 2 positive and even); n=1 failed the even antecedent. The
   weakening mutant genuinely cannot touch `not_strict` (verified arithmetic).
3. OP-67G (governance) and OP-61-members (commitBaseChange_members): projection
   erasure conceded — conclusions project fields the stated mutants never
   touch. Replaced: governance by collections-part mutant (stripCollections
   application breaks); members by pre/post-swap mutant (members projection
   breaks). `commitBaseChange_ok` keeps its post-appFold mutant (second
   conjunct projects the changed field — verified mechanism, not transferred).
4. RefundAll obligation → real consumer: `conservation_preserved` deny/fail
   branches via OP-24/OP-17 refund-drop mutants (same mutants that kill those
   arms' inversions) + OP-25B refundAll-body mutant. Never dropped.
5. COLL universal claim WITHDRAWN as stated; replaced by per-arm effect
   mutants across OP-11..24 runs (deposit branch uses bal equations, never a
   guard witness — stated). One admitted mutant per op enforced throughout
   (OP-50a/b … OP-67a/b splits where two were needed).

## Generation pattern (applied to all 158 rows, audited)

Every kill row's proof was read for unfold-vs-call: unfolding/matching the
mutated definition (incl. definitional elaboration as in `placeBallot_clean`,
`approvePending_wellFormed`, fold inductions) = KILL; sibling-lemma calls only
= OBSERVED with the named upstream row; neither = ELAB-STATIC; unidentified
falsifier = OPEN-KILL with bounded search. Retained:
`handoffs/OPMAP-v6-requirement-verdict-taxonomy.txt` (see receipt for final
hash): 207 lines, machine-audited 158/158 authored present, 0 helpers, 0
family tokens in mutant column, one mutant per kill op (suffix splits present
in the file: OP-42K, OP-46a/b, OP-52B, OP-54a, OP-57B, OP-58B/C/D, OP-63B,
OP-64a/c, OP-66E, OP-67a/b, OP-67G, OP-69L; withdrawn with reasons: OP-41
threshold-value kill, OP-29/31 redundant arm-cases). Verdicts: 75 KILL / 29 / 29 OBSERVED / 31
OPEN-KILL / 60 ELAB-STATIC / 9 RECOVERED / 2 STATIC / 1 ACCEPT (sums to 207).

## Eight-finding disposition (current packet state)

F-01 corrected transcriptions stand (verified against archive re-reads in
AUTHORITATIVE). F-02 recovered evidence stands (report-sha vs candidate
distinction; t57 instruments read as fixtures). F-03 relation now literal
per-row atoms + the taxonomy above (role/hook atoms kept; R-canAdd linkage
OPEN). F-04 `no_expiry` corrected scope stands. F-05 non-vacuous witnesses
stand. F-06 finite map now verdict-tagged per row above. F-07 three-way split
kept (attribution / separation-OPEN / historical-fence). F-08 append-only kept.

*End of CORRECTIONS-018. A retained non-empty OPEN set (31 witness/param rows)
beats any fitted count; the envelope below is unfunded arithmetic.*

## Envelope recomputed from the final map (unfunded; batching unsubtracted)

Kill invocations by mechanical closure class: Step-closure 3 ×20 ops
(OP-11..24, OP-25, OP-59, OP-63, OP-67G, OP-69, OP-69L) = 60; Fold-closure 2
×14 (OP-39,40,42K,46a/b,50,51,52,52B,60,61,62,63B,64) = 28; Validate-closure 3
×4 (OP-49,54,55,56 — OP-56 dup-mutant kills validateDirectAdmission_ok only;
non_admin/membership go GREEN under it: notAnAdmin branch first) = 12;
Integration-closure 2 ×6 (OP-57,57B,58,58B,58C,58D) = 12; K-State-closure 5
×2 (OP-67,67b) = 10; Vote-State-closure 4 ×1 (OP-53) = 4; State-R-closure 4
×1 (OP-25B) = 4. Kill sum: 60+28+12+12+10+4+4 = 130.
Re-runs OP-42..45: 4. ELAB file runs (OP-66E/68/69/70/71/72/74): 7. Acceptance
OP-73: 1 build + 2 elaborations. **Total: 130+4+7+2 = 143 targeted + 1
build — unfunded arithmetic, not a grant or budget.** All prior envelope
totals WITHDRAWN as fitted artifacts.
