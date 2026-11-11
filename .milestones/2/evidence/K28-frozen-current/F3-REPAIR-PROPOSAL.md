# F3-REPAIR-PROPOSAL (static proposal ONLY — no execution authorized)

Ticket owner `t28-app-api`. Authority: NOTE-031 §1+§2 (desk F3-handback via
epic). Status: PROPOSED (no ticket/branch/budget consumed, no work
dispatched, no source edits/investigation/tests/builds performed or
granted). Execution awaits ruling.

## 0. Provenance (report-grounded, site-verified)

Terminal `24252ef1…` (FINDINGS incomplete, F3 BLOCKING R2, F1/F2 credited
for tested classes, zero OPEN rows, candidate unaccepted). F3 site
`Store.hs:618-627` verified on candidate bytes: `evaluate payloadText`
(line 623) before `withMVar` (624) + fresh reads + `applyIntegratedEvent`
case (626+). P2 `7e9bdb49…` verified: 8/8 conservation, CODEC-CONTROL
(member-exception-observed=True, pure refusal Left NotAMember), CODEC-REFUSAL
actual=AUDIT-SEED-SERIALIZATION, tuple (0,0,0,0,0).

## 1. F3 substance

The F1 repair moved encoding + strict-Text evaluation ahead of the member
decision. Previously a refused event returned without demanding its JSON
encoding; now the durable wrapper THROWS from a faulting application codec
before reaching the refusal the pure boundary returns independently
(`Left (IEValidation (NotAMember …))`). This contradicts preserved refusal
behavior and the wrapper's own documented validate-first ordering.
Firewall (explicit): tuple (0,0,0,0,0) = CHANGED REFUSAL behavior, NOT
unauthorized append, NOT lost state, NOT corruption — the repair scope is
refusal-ordering, never data recovery. Property class: a validation refusal
independent of the payload must stay the same caller-visible refusal when
payload processing would fail. Limits (auditor's, kept): controlled fault
injection into the generic serializer; not demo-codec failure, not
every-app, no external exploit demonstrated.

## 2. Joint requirement (binding constraint on any repair)

Preserve validate/refuse behavior AND concurrent conservation TOGETHER — a
fix breaking one to satisfy the other is REJECTED in advance.

## 3. Rendezvous dependency (head-on assessment)

The production comment documents the motive: early encoding accommodates
the auditor's serialization rendezvous (barrier inside test-only `ToJSON`
rendering). That coupling — a harness scheduling trick dictating production
evaluation order — is the defect class F3 exposes. Interface requirement
for the next audit's P2'': the harness MUST observe the intended production
property WITHOUT forcing production evaluation order merely to make its
scheduling trick work (rendezvous on lock/timing-agnostic signals, or
test-only seams that do not reorder validation; the next auditor designs it
within freedom budget — not this proposal's production scope).

## 4. ONE concrete repair direction (no locking implementation prescribed)

Validate-first reorder WITHOUT touching the lock: move the validator
(`applyIntegratedEvent` decision) BEFORE `evaluate payloadText`; encoding
stays pre-lock (conservation intact, rendezvous intact, no harness change).
Accepted + faulting codec still throws post-acceptance (an un-encodable
event cannot persist — preserved behavior, now reachable only after the
decision); nonmember + faulting codec returns the clean refusal (F3 fixed).
Considered and DEFERRED: encode-inside-lock-post-validation (cleanest
production order; kills the coupling entirely; deferred for its harness-
redesign cost and rendezvous risk — the commissioned author may still
choose it; the acceptance below binds, the mechanism is the author's).
No locking implementation is prescribed from anywhere (the proposed
direction does not change locking at all).

## 5. Required acceptance (verbatim)

Existing full original rows + faulting-codec accepted/refused controls
with state/count checks + concurrent conservation + lock release +
appropriate can-fail controls.

## 6. Appropriate can-fail controls

Faulting-codec probes (accepted-throws-observable + refused-returns-Left +
state/count tuple checks) + conservation rerun (8-pair class) + lock-release
(post-failure successful append) + M8 gate kill (append-only evolution:
mutant reverting to encode-first, i.e. F3 reintroduced, must RED quoting
the faulting-codec refused control; exact program authored at gate re-cut
against repaired bytes, fail-closed preconditions, hash-verified restore).

## 7. Prior PASS is evidence only

Re-gate (v10 family) + re-audit at the new bytes; nothing inherited as
verdict at any new SHA.

## 8. Cost proposal (explicit; RULED, not granted)

- OWNER: RED 0 (inherited F3/P2 evidence as failing-first — no fresh RED
  runs) + GREEN 11B (legs 3,4 + M1-M8 (8 mutants) + 6) + SLIM 3B = 14B;
  probes dev ≤10 + recon ~4 = ≤14/24.
- FRESH FULL AUDITOR: 12/24 (envelope 11B + ≤1 discretionary named-only +
  probes ≤16: P2'' faulting-codec + conservation + freedom).
- REQUEST: ONE new submission + owner 14 builds / 24 targeted + auditor 12
  builds / 24 targeted. NO automatic submission, cap reset, smaller audit,
  push/PR/merge (all require explicit ruling).
- LEDGERS (separate, never refunded): S28-1 owner 34/34 + audit 9/12+7/24,
  one spent submission; S28-R1 owner 13/16 + audit 10/12+16/24, one spent
  submission; invalid admission 0/0. Unused margin never implies submission.
