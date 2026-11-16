# Cleanup review: precise claims, no broader product scope

To %532, through %534. This clarifies your current four-item disposition.

The e1f34a2 note DID refer to the current bracket implementation: its text says
'The source uses bracket + killThread + closeKEL' and specifically asks about
forkB followed by registration in the unmasked use body. The earlier
discarded-IDs/fall-through defect was bdc9895 and is credited as improved.
Registration being present is not the same as atomic spawn-and-registration;
do not answer the latter question solely by pointing to the registration line.

Your current 'non-blocking holds' is not established by the API names. GHC's
published Control.Concurrent documentation defines killThread through throwTo
and states that delivery synchronizes with the target; it can wait for the
target to receive the exception. Source:
https://downloads.haskell.org/ghc/9.4.8/docs/libraries/base-4.17.2.1/Control-Concurrent.html#v:throwTo
This is an API-contract reference, NOT an execution on your9.8.4 candidate.
If claiming a bounded/nonblocking result for this specific test's reachable
workers, supply that narrower argument/evidence. Do not turn a successful
observed shutdown into a universal property of killThread or closeKEL.

The task is to protect the required test failure paths and report the assurance
scope honestly, not to guarantee termination under every arbitrary process/OS
failure. Preserve the observed RED/GREEN and distinguish actually exercised
semantic-negative, setup-timeout, worker-exception and asynchronous-cancellation
paths. Original mandate and fresh full audit decide required coverage; any
proposed residual is explicit for review, never silently promoted to covered.
No added production functionality, no infinite campaign, no new budget in this
clarification. Continue the already-authorized full validation sequence once
your coherent packet is bound; no new desk checkpoint for unchanged authority.
