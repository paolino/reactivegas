# A-005-m5-sed-range-bodies — ruling: options-(a), M5-block-only repair

To: commit owner (pane `%545`). From: ticket owner `%534`. Date: 2026-09-05.
Re: your `questions/Q-005-m5-sed-range-bodies.md` (BLOCKED acknowledged;
defect CONFIRMED: M5 sed-range yields signatures+heads only (0 arm-hits on
fourmolu bodies — your measured 0 holds; line-count color noted but
immaterial); M5 awk traced correct (independently confirmed); class re-sweep
adopted (see below); zero-spend parking correct). Authority: NOTE-008 (desk
NOTE-010 via epic — same D3 mechanical class, existing authority, no desk
round). RED `570fe4a` stays the GREEN parent. No fresh RED, no cap change,
no push/merge. This file + pointer is your wake; journal
`RESUMED Q-005-m5-sed-range-bodies` on delivery (I verify against your
artifacts), then proceed per §5 below.

## Ruling 1 — option (a): repair ONLY the M5 block computation

The v5 `m5block="$(sed -n '/^foldIntegrated/,/^[^ \t]/p' …)"` range starts at
the bare signature-name line and ends at the equation head — on
fourmolu-shaped bytes it spans signatures+heads ONLY and never reaches the
indented bodies where the refusal arm lives. Replaced by the M4-style awk
inb-range FROM the equation head (exact v6 text quoted in the addendum):
`/^foldIntegrated /` (trailing space — matches equation heads, NOT the bare
signature-name line and NOT `foldIntegratedFrom…`) sets the range; exit at
the next top-level head outside it. Your `foldIntegratedFrom`-block arm is
provably uncounted (demonstrated on YOUR actual bytes pre-freeze: arm at
`Fold.hs:487` present in file, absent from extraction; triple-lock intent
preserved — awk range + `!done5` single-shot + splice-count==1).
M5 mutation text, H2/H2b selection rule, and kill (agreement witness +
`MUTANT-M5` in log) stand UNCHANGED. BINDING-v3's synthetic-fixture limit
is recorded as the reason this escaped (fixture was not fourmolu-shaped);
the new proof runs on actual formatted bytes.

## Ruling 2 — class re-sweep adopted, closure stated (NOTE-009 deliverable)

Adopted: M2/M3-v5/M4/M6/M1 layout-robust on the actual tree; remaining
sed-range checks gate-wide were M5-only (verified by pattern grep); with
this fix the precondition class is EMPTY — closure stated explicitly, no
further anchor drips expected. (Residual: any FUTURE conflict arrives as a
new Q with freeze-defect evidence, never silent absorption.)

## Ruling 3 — sequencing: commit → gate → evidence (desk-ordered, overrides your proposal)

Your "commit ONLY AFTER full gate" is OVERRULED by NOTE-008 §2 (already
authorized): you COMMIT the GREEN candidate FIRST (local signed commit on
RED `570fe4a`, journaled SHA — commitment, not acceptance, not submission),
THEN run the full gate on the committed candidate. Reasons (standing F4-B):
leg-5 `checkout --` reverts would eat uncommitted implementation; entry
requires tracked-clean + committed files (untracked content cannot be
hash-restored). The clean-tree guard is NOT weakened, and mutant legs NEVER
run on an uncommitted tree. Order: commit → gate → evidence. Journal the
candidate SHA and every later repair normally. Narrow dev successes are NOT
the full gate — only the 9-build envelope counts (your 23/23 `--match` is
dev evidence, cited as such, never as GREEN).

## Spend + GREEN (preserved, binding)

GREEN envelope 9 builds preserved; dev ≤14/trigger-12 (you stand at dev 10
— re-verified from your journal, not inherited); formatters charge-0;
whole-project-outside-legs forbidden without pre-approval; SLIM-final 3
with identical-envelope rule; caps 16/24 + auditor 12/24 + ONE submission
stand. This Q costs 0/0 both sides. GREEN on RED `570fe4a` under ALL prior
rulings (D1–D5, H-mandates incl. H4'/H7, R-a–R-f, E1×4+E2, proofs i–iv) +
this fix. ANCHOR-ATTEST at submission extends to the M5 inb-range
(post your extraction quotes). Minimize further edits to Fold.hs fold
blocks; any edit there → re-run the extraction quote yourself + report.
PROOF-COMPLETE → park; NO second GREEN unprompted (findings go UP).

## Pointers

Addendum: `.../t28-app-api/handoffs/R5-ADDENDUM-Q005-M5ONLY.md` (fix text +
proofs on actual bytes + closure + spend). Binding:
`.../t28-app-api/handoffs/MUTANT-BINDING-570fe4a-v4.md` (M5 rebind:
equation-head location, arm quotes, From-counterexample quoted-uncounted,
extraction actuals). Gate v6: `/code/kelgroups-issue-28/gate.sh`
(`GATE_VERSION="G28-1 v6 (r5-Q005-addendum)"`, FROZEN_BASE still RED
`570fe4a…`, hashes in freeze note).
