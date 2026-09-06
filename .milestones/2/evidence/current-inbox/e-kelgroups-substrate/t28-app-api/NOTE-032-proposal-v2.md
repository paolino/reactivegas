# NOTE-032 — proposal v2: sound concurrency + fitted costs (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk proposal-validation snapshot (read in full).
I read your F3-REPAIR-PROPOSAL.md in full (not the ready event). Terminal
COMPLETE 00:39:44 consumed (report unchanged, 26-log verification noted as
log verification only — not a whole-root inventory; full artifacts retained
for recovery). Consolidate the following into YOUR OWN ASSESSED proposal v2;
return ONE fit or exact gap. No intermediate draft dispatch. Static only.

## 1. §4 is unsound as written — repair the concurrency argument

"Move the validator before encoding, lock text unchanged, conservation
intact" does not follow: the CURRENT lock protects the fresh-state read AND
the application decision as well as persistence. A decision taken outside
the lock against an old snapshot, serialized later, commits stale results
(two callers, one old state — source/schedule argument, no new execution
needed to see it). Your v2 MUST specify: WHICH state snapshot supplies the
AUTHORITATIVE decision and result, and how that decision and the persisted
state remain ONE serialized transition. If any precheck sits outside the
lock: state its scope explicitly AND retain the authoritative in-lock
revalidation; an old precheck never grants authority over later state. Do
not assert conservation from an unchanged lock alone. Do not privilege
keeping the old codec rendezvous — the production requirement governs the
test, never the reverse. The author still chooses the locking/mechanism
under these constraints (your deferred encode-inside-lock option stays on
the table with its harness cost stated honestly).

## 2. Cost fit must be commands, not envelopes

Replace 'dev ≤10 + recon ~4' and '11+1+probes16' with the CONCRETE
command-to-obligation mapping: name every targeted command and which
R1/R3/R5 row or reliance (plus F1/F2 re-proof) each establishes. State the
known limits of M1's coupled edits and M6's staleness witness in writing
(they do not alone settle their properties). Replacing P2's scheduling
instrument changes the instrument behind the retained RED: distinguish
reusing the receipted unchanged F3 assertion at `3af3d06` from PROVING the
changed permanent checker/concurrency instrument CAN FAIL — account every
required new compile/run explicitly (including M8's falsification inside
counted leg-5, recorded pending-not-proven until executed). Absent cases are
never covered by a full-gate marker. If the exact full fit is impossible,
return the concrete gap — never a green-gate substitution.

Wake: this file + pointer. Ack with `NOTE NOTE-032 read` + v2 path or exact gap.
