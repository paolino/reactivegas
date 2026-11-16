# S28-1-PACKET-CLOSE — accepted into audit (NOT merged, NOT released)

Ticket owner `t28-app-api`. Candidate `84a2dae…` (parent RED `570fe4a…`,
base `368b596…`), tree `6f24bb30…`, worktree clean. This record CLOSES the
same-candidate packet for audit. Merge/release authority remains solely at
the milestone desk; nothing here authorizes either.

## Acceptance (ticket-owner altitude only)

The S28-1 objective is met on this candidate per the evidence below: a
real nondegenerate test-only demo (distinct state/event types, signer +
sole GroupView, domain refusal before durable append, sealed atomic hook),
proven by frozen gate v8 exit 0 with all six kills quoting witnesses on
these exact bytes, plus identical-envelope SLIM green. Accepted INTO AUDIT.
Any audit FINDINGS return for disposition (one submission stands; no
standing repair allowance).

## Evidence inventory (hashes fresh-verified at close)

- RED: base-gate log `2b64d6bf…`, RED-commit log `d5b0a1e2…` (absence proof).
- v6 full `4405c545…` (exit 1: M4/M5 killed, M1/M2/M3/M6 unestablished).
- v7 full `6fbce79c…` (exit 1: M1/M3/M4/M5/M6 killed, M2 inconclusive).
- v8 full `ca77e793…` + 11 per-leg hashes inventoried (exit 0, six kills).
- SLIM (identical envelope, pre/post HEAD/tree/status equal):
  leg-3 `b974db4f…`, leg-4 `3ff46f5e…` (S28-1 groups executed), leg-6
  `ade711a5…` (full `just ci` incl. client suites) — all exit 0.
- Contract r5 + addenda Q004/Q005/Q006/Q007 + bindings v1–v6 + gates
  v1–v8 backups + fence amendment + receipt correction + validation record
  (all hashes in freeze/validation notes; r1–r5 + backups preserved).

## Spend FINAL (append-only, caps binding)

Builds 34/34 EXACT (owner RED 4 + v6-GREEN 9 + v7-GREEN 9 + v8-GREEN 9 +
ticket-owner SLIM-final 3; mine 0). Probes ≤19/24 (dev 10 + mine ≤9 across
p1–p6 efforts; exact counts in COUNTERS notes). Charge-0 classes enumerated
with UNMEASURED exact counts where reconstruction is transcript-based.
Auditor envelope 12/24 SEPARATE, untouched.

## Fences + submission integrity

Diff RED..HEAD == exactly the authorized 13 files (+E1×4+E2 within);
Trivial/cabal/Main untouched; no new files beyond RED wiring; mutant
residue zero repo-wide (re-verified). Submission handoff + PROOF-COMPLETE
fields + ANCHOR-ATTEST + RELIANCE (5 ratified: HISTORICAL-FOLD adv,
CESR-KEY-VALIDITY blocking-with-correction, STORE-STM-DISCIPLINE blocking,
MAJORITY-FRANCHISE blocking, HISTORICAL-APPFOLD-SHAPE adv) + Order-1
receipt correction all on record. Registration 23/23 adjudicated (leg-4
cross-check, exit 0).

## Residual risks (for the auditor + desk, explicitly not hidden)

CESREye validity beyond existing suites unproven (row states NONE);
historical-semantics equivalence beyond suites unproven (row states NONE);
V-2 rebind pending #68 landing (out of scope, revalidation required);
hspec-render drift (parsing-only corrections under granted authority);
headroom ZERO everywhere (any surprise spend BLOCKS).

## Audit readiness (staged, NOT dispatched this turn)

Next: fresh FULL Codex `gpt-6-astra/high` audit under 12/24 with exact
command set reconciled pre-dispatch: fresh detached worktree at `84a2dae`
(setup, no build) + complete v8 gate envelope legs 1–7 (9B: legs 3,4,5×6,6)
+ read-only analysis + compact hash-bound report (≤3 discretionary builds
headroom, probes ~0–2). Subject: ENTIRE candidate vs base `368b596`, new
gate run fresh on its tree — NO inherited PASS rows by declaration. Owner
kills never inherited as evidence.
