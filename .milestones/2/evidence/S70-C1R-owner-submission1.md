# Submission packet — campaign S62-SIM-C1R, submission 1 (commit owner muse)

Integrated candidate: `d670323..ef773ec4d4b040866eef7dae6b98881cd140c2b1`
(rebase of `280b67f..0c3c1e9` onto accepted master `d670323`, PR #87;
35 commits, linear, lakefile keeps exporter + both drivers).
Pre-integration candidate `0c3c1e9` preserved; ALL receipts below stay
valid as receipts on `0c3c1e9` and do NOT transfer to the new SHA.
Contract
`C1-SUCCESSOR-CAMPAIGN-PROPOSAL.md` sha256 `533e5070…27c665e` (frozen 444).
Seat START `2026-09-05T11:33:54Z` pane `%540` verified admissible (NOTE-002).
Submission status (NOTE-009): ONE submission opened, NONE audited;
`ed3220e` retained as a returned pre-audit candidate (desk findings
NOTE-078), not a consumed submission.

## 0. RUN LEDGER — which run on which bytes (limitations stated, not smoothed)

| run | bytes | verdict | covers |
|---|---|---|---|
| ui-gate full S5 | `332f573`-era page | GREEN 14/14 | C-KEY journey + H classes live |
| ui-gate `--omit K-2` S7 | same + gate fix | RED exit 1, K-2 named | omission fires (pre-D2-remedy shape) |
| page `?selftest=1` S9 | `228fc07` | PASS/PASS 1280+390 | no-erasure scan, gov asserts, seam both branches |
| gate v14 S10 | `228fc07` | GREEN | oracle, freshness, all module gates |
| gate v14 S11 | `efdde3c` | GREEN | same on NOTE-003 bytes |
| `just ci` S12 | `ed3220e` | EXIT 0 | full Lean/Haskell CI |
| targeted floor (NOTE-005) | `ed3220e` FINAL-then | GREEN / shown / 0 errors | build `--check` 13/13, comment-only diff, boot probe |
| accidental full (dispatch bug, disclosed Q-001) | repair-interim | GREEN (terminal-observed) | counted substantive 13; NOT S13 evidence |
| ui-gate full S13 | `0c3c1e9` FINAL | GREEN 14/14 | repaired harness, whole journey (`S13-full-green.log`) |
| `--omit K-2` S14 | `0c3c1e9` FINAL | RED exit 1, ordinary `copertura incompleta` naming K-2 | omission for its own reason |
| noop-discard + `--omit` S15 | `0c3c1e9` FINAL | GREEN 14/14, K-2 kept | assurance correctly FAILS |
| `--derive-only` S16 | `0c3c1e9` FINAL | GREEN 14/22 + added-`vip` RED | derivation both ways |
| `--vocab-only` S17 | `0c3c1e9` FINAL | clean no-fire RED-check + banned fires | checking path both ways |
| mutant full S18 | scratch `Number()` | RED exit 1, K-2 witness reason | caught on interaction |
| gate v14 S19 | `0c3c1e9` FINAL | GREEN | oracle, freshness, all module gates |
| `just ci` S20 | `0c3c1e9` FINAL | EXIT 0 | full CI on shipping bytes |
| sentence pair (targeted) | `0c3c1e9` FINAL | GREEN/GREEN | provato live + enunciato flipped |

Carry-overs remaining, stated as limitations: (i) S5/S7 predate the D1–D3
harness repair — superseded by S13/S14 on final bytes, kept for the record;
(ii) page journey-relevant bytes unchanged since S5 except a comment
(`ed3220e`, floor-verified) — S13 re-ran the journey on final bytes anyway,
so no carry-over is actually relied on for the journey; (iii) the S5 GREEN
transcript was terminal-observed, not file-saved — S13's log replaces it.
(iv) S13 and S15 are byte-identical by design — a neutered discard must
behave exactly like an ordinary run. **No datum in the artifacts
distinguishes them.** Their distinctness rests on the recorded invocation
in `evidence/RECEIPTS.md`, which is owner testimony, corroborated only
weakly by two distinct writes 22s apart. It is closed by the auditor's
independent re-run of S15, not by this evidence. (A retired claim that
`grep -c "witness K-2"` distinguishes them was wrong: the S14 discard
line contains that substring too. The sound S14 discriminator is
`omit: witness K-2` = 1.)
The fresh codex auditor re-runs everything on final bytes regardless.
`evidence/` in this dir holds all S13–S20 logs plus sentence/flip logs.

## 1. C-KEY — substrate string keys survive every UI control

Fixes (`economics-simulator.html`, unchanged since `332f573`): data-act `u`
stays string; goto-person `u` stays string; data-cf escrow-only `Number`.
Untouched-correct: chip handler (Number iff role=coll), goto-coll, pledgeC,
pile, crumb numeric; bgapprove/kgcast/kgpropose/signer strings; name input
whitespace-only norm. S13 witnesses (real clicks): admit `01`, `Zoë`, `07`;
elect `01`,`07`; deposit→`01`; open Olio; goto-coll; pledge by `01`; accept
`01` (`"01"` string); pledge-row; goto-person `01` (not pruned); departure +
approval by signer `01`; vote open + casts → positive; cf `conto:01`; crumb
home. Leading-zero, non-numeric, Unicode through real controls; CollIds
asserted numeric throughout. Derivation (D3): 22 `dataset` reads scanned,
all classified, 14 controls required, K-14 scan-proven unreachable.
Named-not-glossed: conto/cassa SVG nodes share K-6's expression (member
nodes clicked); collection-view goto-person shares K-1's line (pledge-view
variant clicked). No exhaustive-traversal claim.

## 2. C-CHROME — ordinary chrome with the evidence present

Heading `Il gruppo — una sola membership`; `gov-status` from
`proofSentence()` over all-provato receipt; vocab scan with NO erasure
(D1: single-escaped matcher through the actual extract→classify path).
Both states live (S13 provato journey + sentence-enunciato scratch) and both
seam branches in `?selftest=1`. Classes H-1..H-6 witnessed in S13.
Forbidden things not done: sentence not hidden, proof explanation intact, no
ban on citation-surface Lean identifiers.

## 3. Controls that prove the instrument can fail (each its own reason)

- omission: S14 RED `copertura incompleta: K-2 …` (ordinary reconcile);
  S15 GREEN with neutered discard (owner's D2 seed used as the pattern; my
  own run). NOTE-008: the shared ordinary string is correct by design.
- derivation: S16 added-`vip` RED unclassified (mutation confirmed landed).
- vocab: S17 banned fires / clean correctly silent (owner's sentence used).
- coercion: S18 full-run RED at the K-2 witness on interaction.
- proof sentence: flipped receipt RED under provato expectation (filed log).

## 4. Regression (NOTE-002 correction)

No unrelated rewriting; no redundant re-proving. v14 GREEN twice on repair
bytes (S11) and once on final (S19); `just ci` EXIT 0 on final (S12, S20).
No new defects found in F1/F2/F3/F5/F6/F7 territory. `lean/` zero edits.
C-USERID + NOTE-004 comments corrected (comment-only, mirror-verified).

## 5. Files changed (receipt)

- `economics-simulator.html` — key preservation, receipt-driven chrome,
  selftest asserts, two comments (all pre-`0c3c1e9` except nothing after).
- `economics-simulator-core.mjs` — NOTE-004 comment ONLY.
- `economics-simulator-ui-gate.mjs` — shipped surface incl. D1/D2/D3 repair.
Historical four instruments untouched. No `handoffs/` files authored
(NOTE-002). Commits: `332f573`, `228fc07`, `efdde3c`, `ed3220e`, `0c3c1e9`
(journal in STATUS.md with UTC stamps).

## 6. Proposal for the owner's v15 / probe (reasoning, not files)

Invoke: full (GREEN), `--omit K-2` (RED ordinary reason), noop variant
(GREEN), `--derive-only` ± added control, `--vocab-only` ± banned copy,
mutant full, sentence flip pair — all demonstrated above with logs.
Disputes via filed questions, never silent adaptation.

## 7. Budgets (closed)

Substantive 19/20: 12 preserved + accidental disclosed + S13, S14, S15,
S18, S19, S20 (S16, S17 targeted per A-001). Targeted 30/40. Raises: 12→18
(NOTE-006) →20 (NOTE-079/A-001); spent within ceiling with one spare
deliberately retained. No further spend without owner order.
