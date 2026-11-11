# S28-1-VALIDATION — ticket-owner validation of the v8 packet (RESUME Order-2a)

Ticket owner `t28-app-api`, candidate `84a2dae…` (parent RED `570fe4a…`,
base `368b596…`). Every figure below re-read at artifacts this turn unless
marked inherited-with-receipt (in which case the receipt hash is quoted).

## V-contract (frozen versions + hashes, re-read)

r5 `91818167…` + addenda Q004 `85a41df7…` / Q005 `67f681a5…` / Q006 `f6c8fcab…` /
Q007 `dbcb06eb…` + bindings v1 `b15ce7a1…` / v2 `2761a02e…` / v3 `366bbabc…` /
v4 `34ee2e6e…` / v5 `cb92d536…` / v6 `cdcc01b2…` + gate v8 frozen
(`f5796d1e…` / `7a7a99e3…`, FROZEN_BASE RED `570fe4a…`, ancestry covers
`84a2dae`) + fence amendment (`34221833…`, RED hashes inside) + receipt
correction (this turn). All match journaled records.

## V-evidence (hashes fresh unless noted)

RED logs `2b64d6bf…` / `d5b0a1e2…` (unchanged); v6 full `4405c545…`
(inherited-with-receipt: owner journal + epic verification); v7 full
`6fbce79c…` (fresh); v8 full `ca77e793…` + 11 per-leg hashes inventoried
(fresh) with all six LEG-PASS + kills quoted fresh (M1 unification, M2
rejecting-step, M3 atomic-hook, M4 exhaustiveness, M5 agreement, M6
authority); v8 leg-4 `23 examples, all inside groups, executed, exit 0`
(fresh — REGSELF dispute resolved mechanically).

## V-spend (re-read, append-only)

Owner: RED 4B (base-gate log + RED-commit log) + v6 9B (full log) + v7 9B
(full log) + v8 9B (full log) = 31 builds; dev 10 probes (DEV journal) +
formatters charge-0. Mine: 0 builds (measured — zero whole-project
invocations, ever); 6 probe-efforts / 9 invocations itemized (p1 toolchain
4-version query; p2 gh CI-status 2 queries; p3 GHC/spago/nix versions;
p4 exact first-line pins; p5 ghc-no-code ×2, environment-limited-negative;
p6 F1 dry-run ×2); charge-0 classes enumerated with UNMEASURED exact counts
(reads, greps, git status/rev-parse/diff/log, sha256sum, bash -n, synthetic
awk/perl evals in /tmp, fourmolu parse scans, file writes, tmux
liveness reads, pane captures ≤15 lines). Campaign: 31/34 builds, ≤19/24
probes. SLIM-final 3 reserved (mine, next). Auditor 12/24 DISTINCT untouched.

## V-fences (fresh)

`git diff RED..HEAD --name-only` == exactly the authorized 13 (7 lib incl.
Server/JSON + Generators + 3 E1 specs + demo/spec; E1-additional
ValidateSpec inside; NO Bootstrap/Server.hs changes needed; Trivial/cabal/
Main absent). E1×4 + E2 within authorized bounds (amendment record;
completion substantiated by the diff itself). No other files touched.

## V-submission (integrity)

Handoff `S28-1-SUBMISSION.md` exists (7487 bytes) + PROOF-COMPLETE fields
(base/red/candidate/receipt/handoff) + ANCHOR-ATTEST + spend ledger +
RELIANCE (5 rows — verdicts below) + Order-1 receipt correction (above).
RELIANCE verdicts (mine): HISTORICAL-FOLD (ADV) RATIFIED (suites green all
runs; shapes present+marked; beyond-suite equivalence explicitly not
proven — disclosed residual); CESR-KEY-VALIDITY (BLOCKING) RATIFIED-WITH-
CORRECTION (mechanism as written overstates reuse: the new route performs
NO key-format check, exactly like Lean `validateDirectAdmission` — verified
identical guard order/shape, no crypto imports in new paths; existing key
checks untouched on historical paths; JwkSpec green; no new crypto surface);
STORE-STM-DISCIPLINE (BLOCKING) RATIFIED (H5 shape on committed bytes + M6
executed kill + StoreSpec green); MAJORITY-FRANCHISE (BLOCKING) RATIFIED
(existing majority properties green + end-to-end enactment exercise);
HISTORICAL-APPFOLD-SHAPE (ADV) RATIFIED (present + compile-checked).
No RELIANCE row discarded; no CONTRACT-CHALLENGE outstanding.

## V-registration (mechanical)

23 examples registered == 23 in file (v8 leg-4 cross-check exit 0);
executed + no-pending + 0 failures (same evidence). Kill-closure: each row
killed in-envelope (M1–M6 quotes above).

## V-gate (frozen + re-verified)

v8 bytes == frozen hashes; FROZEN_BASE RED with ancestry over `84a2dae`
(enforced in every leg-2 incl. v8 run); pins unchanged; evidence
teed+hashed per leg.

## V-auditor-readiness (pending SLIM + packet + dispatch)

Subject defined (Order-2d staging): ENTIRE candidate vs base `368b596`,
auditor runs the complete v8 gate FRESH on its own detached worktree (no
inherited PASS rows), 12/24 reconciled pre-dispatch with the exact command
set enumerated. No auditor dispatched yet.
