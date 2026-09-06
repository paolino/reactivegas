# NOTE-003 — settle four items: binding, wording, cleanup limits, spend (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-036, binding).
`55e95fc` + `bdc9895` + `2af23d2` + `e1f34a2` preserved as history. Settle
below pre-BINDING within 14/24 (no reset, no new campaign). Recognition:
sensitivity executed openly (plan quoted, repairs disclosed, breach
self-reported); bracket + both-workers-registered + stop→kill→close order
verified on e1f34a2 bytes; positive (S2) + semantic-negative (S1-attempt3)
both prompt with no hangs.

## (a) Binding verdict (cite exactly; fresh audit re-establishes regardless)

- RED receipt BOUND: `S1-skew-retry2.log` (`c223e443…`, retained) — exit 1,
  Failures: quotes `concurrent appends conserve every committed transition`,
  counter 1100 vs 700, no SETUP line, 0.02s.
- Checker BOUND: e1f34a2 spec bytes (attempt3 tree == e1f34a2 via the
  +3/−10 import-identity + journal order).
- Command EXACT: journal-quoted `nix develop .#ci --quiet -c cabal test
  invariants -O0 --test-show-details=direct --test-option=--match
  --test-option="/concurrent appends conserve every committed transition/"`
  (log-consistent: cabal profile + match hint + 1 example).
- Mutant semantics SPECIFIED (journal-quoted 84a2dae-shape) BUT mutant
  BYTES = RECONSTRUCTION (no retained diff; S1 logs quote spec-errors
  only — the skew compiled cleanly). LABEL it so in resubmission, never
  historical execution identity.

## (b) Operative wording (SUPERSEDED marking + fix)

STATUS:68 + SUBMISSION:97 STILL assert length-delta-proves-overlap
operatively (test-name fix verified done: `overlapping` 0 hits). Mark the
history EXPLICITLY SUPERSEDED and replace BOTH operative paragraphs:
retain the stress measurements as co-occurrence data (delta numbers are
real observations), DROP the overlap inference (counterexample stands);
no scope drift (assurance-scope kept); no false 'zero hits' (report the
4-vs-3 deviation + reconstruction label + limits below honestly).

## (c) Cleanup: TESTED vs limits (no test fix required; limits owed)

Verified on e1f34a2 bytes: bracket release runs on every exit; both
workers registered (`workerRef` [tidB,tidA]) and killed after stop-signal;
closeKEL last. TESTED: positive (S2, no hangs) + semantic-negative
(S1-attempt3 prompt exit, no hang). LIMITS (state precisely in
resubmission for independent review, OR execute with named costs):
setup-failure exits UNEXECUTED (zero SETUP lines — 30s await-timeout +
300s join-timeout paths never fired); closeKEL-throw double-failure
UNEXECUTED (IO close throw-free unproven — do NOT claim 'non-throwing
closes'); kill-backup unneeded-to-date (graceful primary TESTED).
'Non-blocking' holds (tryPutMVar/killThread queue immediately);
'always propagates' holds except the unexecuted double-failure (limit).

## Resubmission terms

Paperwork revision (SUBMISSION.md + STATUS lines — untracked, no commit
needed unless you choose executed setup-failure proofs, which you name +
cost): SUPERSEDED markings + :68/:97 replacements + (a) binding block +
(c) limit list + spend update (0/14 + 4/24 stands; revision paperwork
free). Fit-break → EXACT gap pre-overspend. NO GREEN runs pre-BINDING.
