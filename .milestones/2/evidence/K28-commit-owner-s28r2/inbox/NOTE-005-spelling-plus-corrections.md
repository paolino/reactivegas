# NOTE-005 — pre-GREEN spelling fix + §2 corrections relay (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-038, binding).
e4022c2 consumed: masked spawn+register correct at both sites (parent
masked atomicity + `restore`d killable children — textbook shape); site-B
was the one real gap (site-A bracket-masked all along: your site-A mask
is redundant-but-harmless (idempotent) — KEEP it, no churn-back); matrix +
narrowed claims + :68/:97 markings received. Two items before BINDING.

## 1. Spelling fix REQUIRED (blanket -Werror; no GREEN-burn to discover)

Outer `tidA`/`tidB` bound-but-unused: `-Wall -Werror` (cabal lines 32+79,
BLANKET — not imports-only) promotes `-Wunused-do-bind` to a leg-3
compile failure (source+flag analysis; no execution needed or spent).
Rename to `_tidA`/`_tidB` (ignored, semantics unchanged — inner tids stay
registered). Then narrow-GHC-check the spec module by your proven narrow
mechanism (1 probe from dev ≤4; report the EXACT command): infeasible →
STOP + report (gap!) before GREEN, never burn the envelope to discover.

## 2. §2 self-corrections relay (record, don't hide)

(a) Site A NEVER unprotected (bracket acquire masked) — exactly ONE real
gap (site B), repaired. My NOTE-004 ordered both; the relay is corrected
here too. (b) P2 narrative: restate your matrix P2 as assertion-surfaced
+ exited post-joins (workers done) — NOT live-worker cleanup (that stays
UNEXECUTED where evidence says so; done-MVar ≠ death-ack). (c) Split P4:
P4a returned-Left (handled, joinable — argue + limit) vs P4b
thrown-worker-exception (traces timeout→release — argue + limit); both
acceptable accounting, never coverage, never waiver. (d) Reconcile the
'guaranteed on every path' comment with this exact replacement (test
edit, bundles with §1): 'Cleanup release coverage holds on every exit
path (bracket: stop, kill, close); execution-observed on positive +
semantic-negative; setup-failure, closeKEL-throw, kill-live-worker and
thrown-exception rows argued with limits (see resubmission limit list).'
Remove nothing required silently.

## Resubmission terms (extends NOTE-001/003)

Commit (tracked test edits: §1 rename + §2(d) comment) + re-attest
(fourmolu/hlint + anchors (M6/M7/decision-first intact?) + recount (31
expected, no new tests) + spend INCLUDING narrow-check probe + revision
costs) → FINAL paperwork. Fit-break → EXACT gap. NO GREEN pre-BINDING.
