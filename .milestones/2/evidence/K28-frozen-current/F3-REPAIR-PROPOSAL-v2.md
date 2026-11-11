# F3-REPAIR-PROPOSAL v2 (static proposal ONLY — no execution authorized)

Ticket owner `t28-app-api`. Authority: NOTE-031 (F3 handback) + NOTE-032
(sound concurrency + fitted costs, binding). Status: PROPOSED (nothing
consumed, dispatched, edited, built, or tested). Execution awaits ruling.
v1 (`688f202b…`, preserved at `handoffs/F3-REPAIR-PROPOSAL.md`) is
superseded as proposal, retained as history — its §4 was UNSOUND (admitted
below) and its costs were envelopes, not commands.

## 0. What v2 fixes (owned)

v1 §4 ("move the validator before encoding, lock unchanged, conservation
intact") does NOT follow and is EXPLICITLY REJECTED here: a decision taken
outside the lock against an old snapshot, serialized later, commits stale
results (two callers, one old state — source/schedule argument, no execution
needed). That shape reintroduces F1 through the back door. v2 specifies the
authoritative snapshot and the single serialized transition (§4), and fits
every cost as named commands (§8).

## 1. Provenance (VERIFIED = recomputed here; REPORTED = auditor-hash-bound,
to be verified at packet consumption)

- VERIFIED: terminal `24252ef1cdc49f2c…` (FINDINGS incomplete, F3 BLOCKING
  R2, F1/F2 credited, zero OPEN, unaccepted); P2.log `7e9bdb49092e3d91…`
  (8/8 conservation, CODEC-CONTROL member-throw + pure-Left, CODEC-REFUSAL
  AUDIT-SEED-SERIALIZATION, tuple (0,0,0,0,0)); F3 site `Store.hs:618-627`
  (forced payload line 623 before shared validator 626+); COMPLETE 00:39:44Z
  + AUDIT-RESULT + campaign-closed (6 rows killed=5 blocked=1 open=0).
- REPORTED (cited, hash-bound in report): StoreProbe.hs `6775a8cc…`,
  ledger/inventory/receipts hashes, F1/F2 credit evidence. Full verification
  at packet consumption, never before.

## 2. F3 substance

The F1 repair moved encoding + strict-Text evaluation ahead of the member
decision. A refused event previously returned without demanding its JSON
encoding; now the wrapper THROWS from a faulting application codec before
the refusal the pure boundary returns independently
(`Left (IEValidation (NotAMember …))`) — contradicting preserved refusal
behavior and validate-first ordering. Firewall (explicit): tuple
(0,0,0,0,0) = CHANGED REFUSAL, not unauthorized append, lost state, or
corruption. Property class: a payload-independent validation refusal must
stay the same caller-visible refusal when payload processing would fail.
Limits (kept): controlled generic-serializer injection; not demo-codec
failure, not every-app, no external exploit.

## 3. Joint requirement (binding constraint)

Preserve validate/refuse behavior AND concurrent conservation TOGETHER — a
fix breaking one to satisfy the other is REJECTED in advance.

## 4. Sound concurrency (replaces v1 §4 entire)

AUTHORITATIVE snapshot: the in-lock fresh `readState` at decision time.
ONE serialized transition: fresh-read → authoritative decision → encode
forcing → SQL INSERT → TVar commit, ALL under the SAME lock hold. The
forcing (`evaluate`) moves to AFTER the authoritative in-lock decision
(post-acceptance only): refused returns Left without ever demanding the
codec (laziness: unforced lets are harmless thunks — NO throw, F3 fixed);
accepted + faulting still throws (an un-encodable event cannot persist —
preserved, reachable only post-decision). Pre-lock scope: NONE (no
precheck sits outside the lock). Rules (explicit): an old precheck NEVER
grants authority over later state (any pre-lock check would need stated
scope PLUS authoritative in-lock revalidation — none proposed);
conservation is NEVER asserted from an unchanged lock alone (it follows
from the single serialized transition, proven by the conservation probes);
the old codec rendezvous is NEVER privileged (production requirement
governs the test, never the reverse — the harness redesigns P2''s
rendezvous without forcing production order, within its freedom budget).
Reference shape (author chooses locking/mechanism UNDER these constraints):
decision→force serialized post-validation in-lock (no lock change needed).
Considered, not proposed: throw-capture-and-defer (complexity serves the
harness, not production). v1's outside-lock-decision shape: REJECTED
(F1-recurrent, argued above).

## 5. Rendezvous dependency (interface requirement for P2'')

Early encoding accommodated the auditor's serialization rendezvous; that
coupling is the defect class. The next harness MUST observe the production
property WITHOUT forcing production evaluation order (rendezvous on
lock/timing-agnostic signals or non-reordering test seams; next auditor
designs it in freedom budget — not production scope).

## 6. Required acceptance (verbatim)

Existing full original rows + faulting-codec accepted/refused controls
with state/count checks + concurrent conservation + lock release +
appropriate can-fail controls.

## 7. Appropriate can-fail controls + M1/M6 limits (in writing)

Faulting-codec probes (accepted-throws-observable + refused-returns-Left +
tuple checks) + conservation rerun (8-pair) + lock-release (post-failure
append) + M8 gate kill, PENDING-not-proven until executed inside counted
leg-5 (append-only evolution: mutant reverting to encode-first, F3
reintroduced, must RED quoting the refused control; program authored at
gate re-cut, fail-closed, hash-verified restore). M1's coupled edits do
NOT alone settle event-parameter coverage (TypeNegative isolates; TYP'
re-proves at new bytes). M6's staleness witness does NOT alone settle
every authority property (it proves log-explains-state dependence; M6
re-kill re-proves at new bytes). Absent cases are never covered by a
full-gate marker.

## 8. Cost fit as commands (exact; NO gaps → no BLOCKER)

REUSED (no re-run): P2.log `7e9bdb49…` = F3 RED baseline at `3af3d06`
(failing-first record). PROVEN at new bytes: M8 kill (pending-not-proven!)
+ auditor P2'' faulting runs (counted below) — receipt-reuse and
can-fail-proof distinguished, every new compile/run accounted.
- OWNER (ask 14 builds / 24 targeted; probes ≤4): GREEN 11B — leg-3
  `$NIX just build` (1) + leg-4 `$NIX cabal test all -O0
  --test-show-details=direct` (1) + M1-M8 legs, each splice + test + kill
  + revert (8) + leg-6 `$NIX just ci` (1); SLIM 3B (slim-build/test/ci
  commands echoed + EXIT-trailed); dev diagnostic narrow-compiles ≤4
  (trigger 3, categories: Store.hs module check post-edit ≤2, spec
  probe-compile checks ≤2; journal-each; whole-project outside legs
  FORBIDDEN; exceeding categories = BLOCKER); recon (reads/hashes/greps/
  diffs) charge-0, enumerated.
- FRESH FULL AUDITOR (ask 12 builds / 24 targeted; probes ≤19 exact):
  envelope 11B (legs 3,4 + M1-M8 + 6) + ≤1 discretionary (infra-flake
  disambiguation ONLY); probes — P2''-compile (1; R2/F3+F1) +
  P2''-conservation (1; F1 8-pair) + P2''-faulting (1; F3 direct) +
  P2''-lockrelease (1) + R1-C1 (2; R1 values) + R3-C1 (2; R3 hook) +
  R5-C1 (2; R5 agreement) + MAJ-C (2; MAJORITY) + P3' (1) + P4' (1) +
  P5'/P6'/P7' (3; F2) + TYP' (2; R1 type isolation) = 19 ≤ 24 (headroom 5,
  unallocated-needs-ruling).
- REQUEST (ruled, not granted): ONE new submission + owner 14/24 +
  auditor 12/24. No auto submission/cap-reset/smaller-audit/push/PR/merge.
- LEDGERS (separate, never refunded): S28-1 owner 34/34 + audit 9/12+7/24
  (one spent submission); S28-R1 owner 13/16 + audit 10/12+16/24 (one
  spent submission); invalid admission 0/0; cumulative audit 19/23.
  Unused margin never implies submission.
