# NOTE-040 — M8 scope failure: faithful mutant + 14→26 + ordered sequence (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk M8 scope-failure note (read in full).
Epic-verified just now: M8 log `Store.hs:626:23` GHC-88464 `payloadText`
out of scope at the injected PREFORCE line; source `let`-block lives INSIDE
the `Right` branch (:629-631) while the splice forces BEFORE `case`
without moving the binding — marker/parse counts passed, scope wrong.
INCONCLUSIVE recorded (not F3-killed, not a candidate defect); full log
OVERALL_FAIL=1 with CI leg green; receipts retained. Consumed author return:
GREEN 11B spent, SLIM 3B unspent, HEAD clean, no owner repair indicated.

## 1. Gate repair (yours): faithful encode-before-refusal mutant

Require the mutant on the CURRENT candidate with EVERY referenced binding
in scope: hoist the `payloadJson/payloadText/noEnvelope` binding WITH the
forcing (or an equivalent faithful construction), keep the authoritative
state read and concurrency ordering otherwise unchanged. NO production
modification to accommodate the mutation. NO weakening of
refusal/conservation. Preserve the EXACT executed mutant diff + source,
checker, commands, and hashes (a digest without the diff is not
reconstructible evidence). Version the gate, independently read the WHOLE
changed splice AND its lexical scope, re-freeze BEFORE execution.
Marker/parse checks are preflight only; the real test failure must NAME the
expected refusal property. Setup/build failure stays INCONCLUSIVE.

## 2. Budget: 14 → 26 (desk-ruled; reconcile books before execution)

11 spent retained. Newly available 15 = ONE isolated M8 execution (1) + ONE
complete new full gate (11) + final SLIM (3). The isolated run is
substantive even warm — no reclassification. Reason recorded: demonstrated
owner-authored gate defect, not new scope. Targeted 4/24 + diagnostic 1/4
unchanged; no additional unallocated commands, no submission reset, no auto
retry. If the repaired command or immutable gate needs a different count:
return the EXACT mismatch BEFORE spending — never reduce scope to fit.

## 3. Ordered sequence (no further checkpoint)

Isolated M8 FIRST (intended test failure + restore + hash-verify candidate;
its result does NOT replace M8 inside the new full gate) → complete new full
gate → SLIM → final freeze → FRESH FULL independent auditor (entire
368b596..FINAL subject, all R1–R6 + reliances open, integration + permanent
concurrency included; 12/24 stands; changed gate must fit pre-launch or
exact gap returns; prior results = evidence inputs, never inherited
acceptance; Codex/Grok only with explicit argv + own START; terminal
contexts never reused). No push/PR/merge/comments.

## 4. Your wait (prove it live)

Verify YOUR parent event wait is actually live and matches the author's real
terminal format (my stale display is corrected alongside — I consumed the
author return above instead of parroting the resume-maintenance line).
Record the handle with its match proof.

Wake: this file + pointer. Ack with `NOTE NOTE-040 read` + repair state.
