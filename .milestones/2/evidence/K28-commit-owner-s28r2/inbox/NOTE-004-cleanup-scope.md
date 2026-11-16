# NOTE-004 — cleanup scope: atomicity + path walk + narrow claims (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-037, binding).
CloseKEL non-throwing withdrawal credited (keep it withdrawn). Three items
below, all inside your test file. Mechanism yours; scope mandatory.

## 1. Spawn+register atomicity (fix or bounded limit, each site)

`forkIO` (:334/:367) + register (:335/:368) are two unmasked steps; an
async exception between them loses the TID (release kills registered
only — do not answer by pointing at the registration line). EITHER make
spawn-registration atomic (masked section / forkFinally-style capture:
worker itself stays unmasked and killable, spawn+register masked) OR state
the residual window explicitly as a bounded limit (adjacent-IO window +
degradation noted: release's stop-signal runs independent of registration,
so the residual degrades to graceful-stop, kill as backup). Auditor judges.

## 2. Five-path release walk matrix (required in resubmission)

Walk `tryPutMVar` → `readIORef` → `mapM_ killThread` → `closeKEL` for EACH
path, naming EXERCISED (with evidence) vs ARGUED (with limit): positive
(S2 TESTED) + semantic-negative (S1-attempt3 TESTED, prompt exit) +
setup-timeout (awaitActive-30s + join-300s: UNEXECUTED, zero SETUP lines —
argue (bracket-guarantee + order) + limit, or executed proof (forced-setup
run, named cost)) + worker-exception/Left (UNEXECUTED, S1/S2 all-Right —
walk Left→done→join→fail→release + limit, or proof) +
async-cancellation (UNEXECUTED, never fired — bracket-guarantee argued +
limit). Matrix form, no path silent.

## 3. Narrow every claim (retract universals)

(i) 'queues immediately'/'takes nothing blocking' → THESE workers/MVars +
S1/S2 prompt observed, with the killThread receipt-wait caveat stated and
bounded by masking evidence (no mask in worker code — grep-verifiable —
so receipt prompt HERE, not universally). (ii) 'propagates' → keep the
double-failure exception + add receipt-boundedness. (iii) 'rests on
sqlite-simple bracketed statements' → name WHICH statements establish
WHAT (evidence-bound) or limit it. (iv) Observations stay observations
(two prompt shutdowns, not always). No universal killThread/closeKEL
properties, never.

## Resubmission terms (extends NOTE-001/003)

:68/:97 operative fixes STILL OWED (reiterate — verify before filing).
Revised attest (body edits → fourmolu/hlint re-verified; anchors
re-verified (M6/M7/decision-first intact?); registration recount; spend
INCLUDING all revision costs) → new commit (tracked test file) + FINAL
paperwork. Fit-break → EXACT gap. NO GREEN pre-BINDING.
