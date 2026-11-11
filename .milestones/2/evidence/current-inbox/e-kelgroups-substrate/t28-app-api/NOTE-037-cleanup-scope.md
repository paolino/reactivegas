# NOTE-037 — cleanup claims: precise scope, no API-name proofs (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk cleanup-scope note (read in full).
Epic-verified at source just now (`S28AppApiSpec.hs` acquire/release/use
bodies). The e1f34a2 note DID mean this current bracket code (not bdc9895's
discarded-IDs defect, which stays credited as improved). Three precise
corrections, all inside the owned test file:

## 1. Registration ≠ atomic spawn-and-registration

`forkIO loopA` (:334) + `writeIORef workerRef` (:335) are two separate steps;
an async exception between them loses the TID. Do not answer the atomicity
question by pointing at the registration line. Either make spawn-registration
atomic (masked section / `forkFinally`-style capture) or state the residual
window explicitly as a bounded limit for review.

## 2. forkB registration sits in the unmasked use body — review it

`forkIO loopB` (:367) + `modifyIORef workerRef` (:368) run unmasked: an async
exception between them leaks B (release only kills registered TIDs). Same
remedy class as (1) or an explicit stated limit. The release path itself
(`tryPutMVar` → `readIORef` → `mapM_ killThread` → `closeKEL`) must be walked
for EVERY failure path (positive, semantic-negative, setup-timeout,
worker-exception, async-cancellation): name which are EXERCISED versus
argued, per path.

## 3. No universal properties from API names or one observed shutdown

'Non-blocking holds' is not established by `killThread`/`closeKEL` names:
killThread-via-throwTo CAN wait for the target to receive the exception (GHC
base docs — a contract reference, not an execution proof on this 9.8.4
candidate), and closeKEL is IO that can itself throw (which would REPLACE,
not preserve, an in-flight original failure). Any bounded/nonblocking claim
must be a NARROWER argument about THIS test's reachable workers with
evidence — never a universal killThread/closeKEL property, never one
observed clean shutdown promoted to always. A successful shutdown observation
stays exactly that.

## Standing scope (unchanged)

Protect the REQUIRED test failure paths and report assurance scope honestly —
not termination under every arbitrary process/OS failure. Preserve observed
RED/GREEN. Any proposed residual is EXPLICIT for review (mandate + auditor
decide coverage), never silently promoted to covered. No added production
functionality, no infinite campaign, no new budget. Continue the authorized
full validation sequence once your coherent packet is bound — no new
checkpoint for unchanged authority.

Wake: this file + pointer. Ack with `NOTE NOTE-037 read` + claim states.
