# Campaign ledger — ms2/t-simulator round2, submission 1

Auditor `auditor-claude-round2-s1` (claude, pane `%79`). Candidate
`9c521e56b742475051d9b15a24832bf83c5ccc4e`, base `ab88d5ec96f18c79c21ff28b777addf2144083b1`.
Gate `verify-round2.mjs` sha256 `1e669bdaeefb746a4f5358fb31df358a4df37e39679ea60fba2f3455afca4dcb`.

All nine rows declared `BLOCKING` by the ticket owner. No row may terminate as
`RESIDUAL`.

Baseline for every mutant: candidate exit 0, `197 passed, 0 failed`. A mutant
counts only if the harness confirmed its own edit applied (`applied=YES`,
non-zero byte delta) **before** the gate ran.

| Row | State | Killing mutant (verified applied) | Gate response | Evidence |
|---|---|---|---|---|
| R2-CORE | KILLED | core `s+=escrowOf(c);` → `s+=0;` (inside MACHINE-CORE) | exit 1, 15 new fails incl. `C: machine core byte-identical to accepted base` + flow invariant rows | `receipts/mutants.log` |
| R2-SUBJECTS | KILLED | suppress `data-node` on `conto:/cassa:/escrow:` keys | exit 1, 61 new fails incl. `U: stage has clickable conto/cassa quantity` | `receipts/mutants.log` |
| R2-CLICK-PARAMS | KILLED | inject `<select>` + `<input data-pkey="u" name="u">` | exit 1, 3 new fails: no-`<select>`, no-`data-pkey`, no entity-named inputs | `receipts/mutants.log` |
| R2-GUARDS | KILLED | six independent guard-name renames + reason-derivation neutralisation (G1–G7, G2) | each exit 1; `AUTH`, `REFERENTE`, `COVERED`, `L8`, `PERMITTED (L2)`, `NO-PENDING (L4)` each has its own killing mutant; `G2` (reason → `null`) trips 5 | `receipts/mutants2.log` |
| R2-VALUES | KILLED | drop the commit-time clamp `v = b.max;` | exit 1, 8 new fails incl. `B: over-max commit clamps to 100`, `B: input snaps to the applied bound` | `receipts/mutants.log` |
| R2-FLOW | KILLED | `F2`: refund count forced to `0` (assertion-shaped) — plus `toy.refundEvents.push` → `toy.donePurchases.push` (crash-shaped) | `F2` exit 1, fails `U: refunds shown separately from purchases` and the in-page selftest | `receipts/mutants2.log` |
| R2-NAMES | KILLED | `state = Object.assign(bootState(0), {ownerName:'Anna'})` | exit 1, fails `U: boot state is Lean boot`, `U: toy label attaches toy-side only` | `receipts/mutants.log` |
| R2-SURFACE | KILLED | reintroduce `<div class="legend">` | exit 1, fails `S: no legend blocks` | `receipts/mutants.log` |
| R2-PUBLISH | KILLED | inject `<script src="https://cdn.example.com/x.js">` | exit 1, fails `S: no <script src=…>`, `S: no http(s) URL` | `receipts/mutants.log` |

## Campaign termination

`state=closed stopped=set-point rows=9 killed=9 residual=0 blocked=0 open=0`.
Every row is terminal as `KILLED`; none reached the tail-stop or budget path.
Builds `0/3` — the artifact is a single interpreted HTML file, so no compile
was needed and no build tree exists to retire.

## Instruments (frozen)

| Path | sha256 |
|---|---|
| `mutants.mjs` | `802e038b180ba3b6730b2d5a6af7076e8a98a2d92cd2e145280ab3aabce28be1` |
| `mutants2.mjs` | `3b82dca6c60d8ea427935510985bcb9606adefcc8f1ae4c0a217f0fcda09ea7a` |
| `receipts/mutants.log` | `3c11a60bb21058da0b5a42e326d369d2c29c59641f32ef52c562737db3f9f24e` |
| `receipts/mutants2.log` | `a163886c36ebcafc47927e537db1eed28ce822e285aa2d5da51db0cb116d78fc` |
| `receipts/gate-candidate.log` | `865036bb81a608fca234ef17cbcbb9028b9159a00608d5a1ad9830abcfb9ec5c` |
| `receipts/gate-base-RED.log` | `320ad77cffded4007fbdd1b2c70348780e684a6bbc85588917890112947e6f79` |

Both harnesses were pre-flighted against the candidate before judging: the
baseline run is asserted at `0` fails, and every mutant is refused unless its
own string substitution changed the bytes. `git status --porcelain` on the
tracked candidate tree was empty before and after every run.
