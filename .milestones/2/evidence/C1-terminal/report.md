# Commit Audit — partial administrative handback

- Campaign/submission: **S62-SIM-C1R, 1/2**.
- Verdict: **FINDINGS — PARTIAL AUDIT**, three demonstrated blocking findings. No acceptance recommendation; the full commissioned assessment is unfinished.
- Original scope: FULL **6879970fdb1a797263843387e14704eaa1e3a2e7..9717405e52664c9a520fcd0c65edb4e90612110a**, including inherited implementation and prefix requirements.
- Candidate: **9717405e52664c9a520fcd0c65edb4e90612110a**; checkout /code/reactivegas-sim-fable-audit-c1r-s1. Initial and pre-v16 checks established detached, clean identity; origin/master resolved to accepted integration base 3590c0015b84fd58004bf6fb44dd18b107304c48.
- Auditor: fresh Codex %562, gpt-6-astra / high; ticket owner %313, subject owner muse %540; separate processes in window @33. The ledger's dispatch %560 is the predecessor, not this seat.
- Authority: complete brief including amendment 1; original invariant mandate c013e595e7af8175a01b431b2891aec024ffa2006a59b412bf586b948cab8f58; successor proposal 533e5070182944867b952c45eeb1a1b45a706a23bc539cfe457b8bf9a27c665e; prefix requirements from handoffs/EXPANDED-REVIEW-PACKET-s2.md. Historical acceptance was not inherited.
- Ledger at binding: campaign-ledger-S62-SIM-C1R.md, c2e5628318bdbac950ef7d1401564f16d1ecf0fbdb77287b0ba32267dbd681b8. Only §B receives this handback.
- Actual spend: **5/5 substantive; 7/30 targeted**. Campaign audit allocation 10 substantive / 60 targeted; this handback spends no further verification budget.
- Ceiling history: five owner increases and one auditor increase; the explicit task-specific exception governs. No new grant inferred.
- Stopping state: **administrative handback under NOTE-A1**, pointer POINTER-1788646390-2519415. No new execution after that instruction. This is neither set-point nor tail-stop nor exhaustion of the targeted allowance.
- Campaign remains **OPEN**: 4 failed requirement rows / BLOCKED, 13 OPEN / UNJUDGED, 0 KILLED, 0 RESIDUAL. No requirement is promoted to PASS from aggregate GREEN.

## Demonstrated blocking findings

### F-01 — imported economic arguments override the recorded signer

**INV-3; property class: the authenticated caller identity must remain bound through application-event decoding, transition validation and replay/adoption. Payload fields cannot substitute another actor.**

At economics-simulator-core.mjs:1610, applyIntegrated calls attempt with an object assembled as { tag, author: signer, ...ae[tag] }. The later spread can replace author. The corresponding page adoption path at economics-simulator.html:2576 repeats the same precedence.

T7 started from the actual founded page, admitted non-admin bruno, and tried opening purchase 99 with signer bruno. The ordinary event was refused for AUTH. Adding author: "anna" inside the application's event arguments caused the same recorded signer to be accepted, creating the purchase with referente anna. The resulting session passed the production normalization, replay and governance verification functions, then adoptSession installed purchase 99. The recorded result still reports brunoAdmin: false.

The Lean AppEvent.openPurchase has only the collection argument; Reactivegas.appFold supplies its separate signer to step (lean/Reactivegas/Step.lean:191). The observed JavaScript behavior substitutes an actor at the import boundary that the model does not supply.

Evidence: evidence/T7-authority.log, sha256 **4de61e7480942ed572e5ffbf90e42c9851df0e6d24796ae9ab81354cae4eef4d**; complete retained session evidence/authority-forged-session.json, sha256 **a00067f9acea2473f4690356fdb9c840b2c4bf0a82e91f7d8fc6d4408c00c9fd**.

Limit: this is a demonstrated simulator import/replay defect, not a claim about a blockchain transaction, cryptographic signature or deployed service. The browser probe called the production verification/adoption functions; it did not use the file-picker interface. Only openPurchase was executed; the property class concerns actor binding across application-event arguments.

### F-02 — reachable purchase counts exceed the non-overlapping ring

**R-GEO.5 and INV-11; property class: layout separation must hold over reachable collection cardinalities, including the packed fallback. A fixed small fixture count cannot stand for that extent.**

T4 opened purchases 1 through 10 using successful production RG.runAttempt transitions from the founded group, without assigning a fabricated state. Ten purchase nodes rendered, and the exported session passed production verification. Member coordinates and repeated geometry were unchanged for identical complete inputs.

At eight purchases the minimum centre distance was **101.02842614438366**. At nine it fell to **90.29331783797649**, below the layout's 92-unit separation. At ten it was **81.58048651498609**, below even twice the declared 42-unit purchase radius. The automatic fallback remains on radius 132 (economics-simulator.html:3456 onward). The existing page selftest's largest non-overlap fixture has eight purchases (:4750 onward).

Evidence: evidence/T4-geometry.log, sha256 **79a7bf0556840cd25d9198f3d5544613744fb5b2de0b1e1ada3bac29cfd88d20**. The same instrument measured both the separated eight-purchase control and the overlapping ten-purchase case, with successful transitions and render/export prerequisites retained.

Limit: numeric layout output and ten rendered nodes were measured; no screenshot was retained. Drag, closest-referente optimality and stability across membership changes were not settled.

### F-03 — handler discovery misses a live bracket-notation key control

**C-KEY; property class: discovery and witness reconciliation must cover actual key-writing handlers independently of their property-access spelling.**

deriveExtent scans only dot-notation dataset accesses and builds the required witness set from KNOWN_READS (economics-simulator-ui-gate.mjs:212–235). T5 inserted one visible button with a working click handler reading vip.dataset['vip'] and coercing "01" before person navigation. The existing --derive-only returned GREEN with its unchanged 14 controls / 22 dataset-read names. The browser admitted "01" successfully; clicking the new button left navigation at the group, rather than at that exact person key.

T6 was the matching calibration: the inserted handler used vip.dataset.vip instead. The same derivation command returned RED specifically for the unclassified vip read; the button and key-coercion behavior remained executable. The missing discovery is therefore not a failed insertion, unreachable handler, syntax failure or broken probe transport.

Evidence: evidence/T5-derive-bracket.log, sha256 **8cdb391765de370565d83c1ed64a6ae27205c986b5e1a6db1042596f4491b507**; evidence/T6-derive-dot.log, sha256 **378cb93b93cd1221085604574abd8220423576be41b6411cb00819551ef345fa**. Retained mutant HTML hashes are **5faf6999a926d83533b7e1aefdfde8c8a106c4be20f734a4cc1e21e44b8d1835** and **b29c9ab156b10179cd85bdc2054c745046b747b045e7831c3df7ec89a2313112**, respectively.

Limit: these are auditor-created extra controls, not an assertion that the candidate already contains this added handler. The finding concerns the commissioned discovery/coverage guarantee. Only the focused derivation sub-gate and live click were run on these mutants; no full mutated ui-gate or v16 run was performed.

## Per-requirement disposition

All severities below are the owner-declared BLOCKING severity. UNJUDGED means the row assessment was not completed; it is not a pass or an assertion that no relevant execution occurred.

| Row | Verdict | State | Evidence and remaining limit |
|---|---|---|---|
| INV-1 | UNJUDGED | OPEN | S2 includes payload-users mutation evidence; complete absence of secondary stores was not settled. |
| INV-2 | UNJUDGED | OPEN | S2 drove named string-key witnesses; the complete identity surface was not assessed. C-KEY has a separate coverage finding. |
| INV-3 | FAIL | BLOCKED | F-01: non-admin recorded signer overridden in app arguments and accepted on replay/adoption. |
| INV-4 | UNJUDGED | OPEN | S2 records 14 routes and retired-surface controls; independent class assessment unfinished. |
| INV-5 | UNJUDGED | OPEN | S2 includes hook and replay checks; the whole sealed-transition and failure-mode boundary was not settled. |
| INV-6 | UNJUDGED | OPEN | S2 records admission-row and cleanup controls; full row judgement unfinished. |
| INV-7 | UNJUDGED | OPEN | Named UI election journey ran in S2; full deletion/absence judgement unfinished. |
| INV-8 | UNJUDGED | OPEN | Hashes and S2 pin controls recorded; SELF-2 was not executed independently. |
| INV-9 | UNJUDGED | OPEN | S2 re-emitted and compared economic and vote corpora; producer/provenance completeness was not settled. |
| INV-10 | UNJUDGED | OPEN | S2 records threshold values and a threshold mutant; full independent row judgement was not finalized. |
| INV-11 | FAIL | BLOCKED | F-02 violates an expressly inherited geometry guarantee despite S1/S2 GREEN; other regression axes remain unjudged. |
| R-GEO | FAIL | BLOCKED | F-02; same-input determinism and stable members observed in T4, other geometry/drag requirements unfinished. |
| R-CIT | UNJUDGED | OPEN | S2 claim/permalink checks ran; full semantic citation and control assessment unfinished. |
| R-ITA | UNJUDGED | OPEN | Named S2 visible-copy checks ran; full ordinary surface and transient states not assessed. |
| R-LAY | UNJUDGED | OPEN | No independent focused layout assessment completed. |
| C-KEY | FAIL | BLOCKED | F-03. Existing named controls passed S2, but discovery failed on the bracket-read mutant. |
| C-CHROME | UNJUDGED | OPEN | Source leads concerning transient snapshots and vocabulary were not executed; no finding claimed from them. |

## Verification receipts and accounting

Paths below are relative to this runtime root. Individual nested-suite durations were not recorded by v16 and are not invented.

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| S1: nix develop --quiet -c just ci | 0 | 167208 ms | evidence/S1-cold-ci.log; 32f82fb0bb4844adbaf738bd53c170c3536b7bb644a469abaf9cd81c3426086e |
| S2: bash /tmp/reactivegas/ms2/t-simulator-fable/handoffs/gate-v16-one-membership.sh /code/reactivegas-sim-fable-audit-c1r-s1, with own RG_GATE_EVIDENCE | 0 | 264176 ms | evidence/S2-v16.log; ebb228d65c37c73012b7f4e846142c8aa9a7a3973ec3ccd669df7d431ad9d79b |
| T4: node probe.mjs geometry | 0 | 1890 ms | evidence/T4-geometry.log; observed product defect asserted by probe |
| T5: node probe.mjs derive-bracket | 0 | 1875 ms | evidence/T5-derive-bracket.log; nested derivation exit 0, live failing navigation |
| T6: node probe.mjs derive-dot | 0 | 1861 ms | evidence/T6-derive-dot.log; nested derivation exit 1 for expected unclassified read |
| T7: node probe.mjs authority | 0 | 1877 ms | evidence/T7-authority.log; observed signer substitution asserted by probe |

S1 spent 1 substantive; S2 spent 4 (v14 body plus three full driven suites). The three nested focused modes were initially reserved and subsequently all executed: derive-only exit 0, vocab-only exit 0, vocab-expect-red-clean exit 1. They spend targeted T1–T3. T4–T7 each completed, spending four more. **Reservations and completed spend agree: substantive 5, targeted 7.** No standalone ui-gate repeats, additional full gate, chrome probe or SELF-2 experiment executed.

S1 output includes actual Lean build work, inversion negative control, axiom inventory and the corpus live-binding step. Project .lake and lean/.lake were absent before S1. Nix/store dependencies need not have been cold. Free bytes recorded before/after S1 were 217570541568 / 217273700352; this is host free-space observation, not attributable build size.

S2's completed receipt was retrieved during handback from the already-finished command; the missing journal completion was not repaired by rerunning it. S2 ends with clean-tree assertions and GREEN. No candidate-source mutations were performed: T5/T6 wrote HTML only inside this runtime root. No new candidate check was run after NOTE-A1.

## Retained suite evidence and limits

The independent ordinary, omit-K2 and omit-K2-noop logs contain invocation headers and exits 0 / 1 / 0. Omission is named as copertura incompleta: K-2; neutralized discard retains the K-2 witness. The final-candidate logs distinguish invocations in their headers. Their hashes and the three focused-mode hashes are in evidence-inventory.md.

The original historical S13/S15 pair was not independently compared by this seat. Its testimony limitation is not closed here. SELF-2 remains unexecuted: S2's moved-pin control prints a reachability diagnostic first, which alone does not establish or falsify sensitivity of the pin-identity predicate. The fixture-shape exemption remains the expressly accepted bounded limitation; no new cleanup judgement is made. The withdrawn claim about cited-line checking was not resurrected.

## Failure modes and unfinished work

- Import/replay: F-01 demonstrates acceptance and adoption of a payload-supplied actor despite rejection of the ordinary non-admin event.
- UI identity navigation: T5/T6 preserve a valid member but lose navigation after key coercion; the derivation detector distinguishes access syntax, not the whole handler class.
- Layout: T4 establishes successful state transitions beyond the tested packing capacity while rendered geometry loses separation.
- Full base-to-candidate provenance, every changed failure path, replay quarantine/degradation behavior, browser resource failures, all producer seeds, every original invariant's mutation floor, geometry dragging, semantic citations and rendered chrome were not completed.
- Source review identified further leads, including normalization outside a recovery catch and transient-copy observation, but they remain **UNJUDGED leads**, not additional findings.

No residual was accepted or downgraded. No outside-scope finding was opened. No claim of exhaustive coverage or zero survivors is made.

## Instrument and custody

probe.mjs is frozen at handback with sha256 **ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c**. It reuses the candidate's Chromium/CDP transport and contains modes geometry, derive-bracket, derive-dot, authority and **an unexecuted chrome mode**. Presence of that last mode is not evidence.

The final instrument source is retained; per-invocation instrument hashes were not frozen before T4–T7. Mutant HTML content hashes were printed during their executions, and command outputs were recorded directly by run-receipt. This provenance limit is stated rather than filled with reconstructed historical hashes.

evidence-inventory.md inventories retained files and hashes, excluding its own self-referential hash. Original logs, mutant HTML, forged session, brief, note, launch record, instrument and this report are retained. Build outputs in the detached checkout are retained under NOTE-A1's administrative-only fence; no retirement/deletion was performed. The ticket owner owns subsequent worktree disposition.

The commissioning owner's note describes a provider interruption. This report does not independently diagnose it. The controlling stop is NOTE-A1's explicit no-new-execution handback. All post-note work consists of reading retained artifacts, recording completion receipts, hashing and writing this handback.
