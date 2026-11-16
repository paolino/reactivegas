# T48-PLEDGE-RELAX audit campaign ledger

Append-only ledger. Submission 1, candidate `747be540a963497fe2c450ca93b6ebc12b33f301`, accepted base `5ad653d3d019c925a2a3dd87bd40e367ebe7474b`. All rows are BLOCKING.

## Declared audit rows

| Row | Severity | Initial state | Contract |
|---|---|---|---|
| A1 | BLOCKING | OPEN | Identity, detached cleanliness, ancestry, trees, subjects/trailers, diff hash, three-path fence, live origin-tip absence |
| A2 | BLOCKING | OPEN | Independent source/proof review of `step`, `authorizedStep`, `pledge_guard_inv`, `step_pledge_inv`, `step_authorized` |
| A3 | BLOCKING | OPEN | Relaxed behavior, exact public surface, 59 unrelated types, 62-theorem inventory, zero new debt |
| A4 | BLOCKING | OPEN | Trace/emitter contract 18/18/0, 34/34, five v1 envelopes |

## Declared mutation rows

| Row | Severity | Initial state | Mutant class |
|---|---|---|---|
| M1 | BLOCKING | OPEN | Remove self disjunct; static exact-source control first |
| M2 | BLOCKING | OPEN | Widen author condition to any member/admit third party |
| M3 | BLOCKING | OPEN | Weaken monetary and stall guards |
| M4 | BLOCKING | OPEN | Weaken both changed inversion public types |
| M5 | BLOCKING | OPEN | Change unrelated authorization branch and unrelated theorem type |

## Events

- `A1 PASS` — provenance receipt `evidence/provenance.log`, sha256 `d964315c08bc7ffb343872784195d849233616d042925529631d13ce7eef60f0`: detached clean candidate/tree, exact base→RED→GREEN ancestry, mandated subjects/trailers, diff sha256 `443522e142adbb6e2900a0c5a5ba54d425933089dd2a705a6f0c708ac224f496`, three regular `100644` paths, and live `git ls-remote --heads origin` absence.
- `A2 PASS` — independent line review: `Step.lean:120-129`, `Predicates.lean:73-83`, `Invariants.lean:148-167,225-243,502-590`. The author disjunction alone moved in `step`; `authorizedStep` splits pledge only; both inversion statements quote the relaxed Boolean prefix; `pledge_guard_inv` derives the Prop disjunction plus the five retained facts; `step_authorized` consumes the relaxed guard and all other constructor arms remain base-identical.
- `A3 PASS` — exact-candidate full receipt `evidence/candidate-final-full.log`, sha256 `1544edff7563394211a02dca529f2c8d0360398ce3fd5808d226d1699f5c6a26`: self and responsabile success; third-party/nonmember/zero/overdraw/stalled refusal; exact relaxed AUTH/type probe; 59 unrelated types; inventory 62; zero new debt.
- `A4 PASS` — same exact-candidate receipt: TraceTests exit 0, inventory `18/18/0`, summary `34/34`, five `TRACE-JSON` v1 envelopes, and full CI exit 0.
- `M1 KILLED` — exact alteration `Step.lean:122`, `(isResponsabile s a || a == u)` → `isResponsabile s a`. No-build gate rejected the missing changed path and exact `Step.lean` target: `evidence/mutant-static-self.log`, sha256 `4d3af3bbbc029e5df75eb88eb80a61d71db86c192e50b15cdf569e2b5db02a8d`. Restored hashes: Step `781b35966992e8037c000ccba24a0e68fdeb3086`, tree `9361dcc6ed5b60ba164c435f755c83a4efe0f8dc`.
- `M2 KILLED` — exact alterations: `Step.lean:122` self equality → `s.users.contains a`; `Predicates.lean:83` self equality → author membership. Fresh behavior instrument `evidence/mutant-behavior.lean`, sha256 `798479cc6460ae1d19d85744913bf201a8fcb848c5f56741cc587c7d687cf100`, retained self/responsabile positives and rejected the mutant because third-party `pledge 3 2` became successful and its negated AUTH became false: log sha256 `5c89f08645b607c852825c04d3c78c0be7344c3af78e3078c5370fe08e4bc4fb`.
- `M3 KILLED` — exact alterations: `0 < v` → `0 ≤ v`, `bal … ≥ v` → `bal … ≥ 0`, and removal of `!(decide (stalled s))` in `Step.lean:125-126`. The same instrument reported candidate-negative zero, overdrawn, and stalled pledges all false on the mutant; the bundled full log also showed the weakened funds/positivity facts break downstream `omega` proofs. Behavior log sha256 `5c89f08645b607c852825c04d3c78c0be7344c3af78e3078c5370fe08e4bc4fb`.
- `M5 KILLED` — exact alteration: removed `.donate` from the responsabile-only `authorizedStep` arm and added `.donate _ _ => True`; the static exact `Predicates.lean` target rejected it before compilation. Bundled full receipt `evidence/mutant-bundled-full.log`, sha256 `10484cac2a203734473002a4cf6dff995be9d9a104942e6f5822865269b10936`.
- `ATTEMPT NOTE` — the first bundled M2/M3/M4/M5 full run did not compile because the simultaneous monetary and initial public-type weakening invalidated downstream proofs. Per budget revision 001 it is not M4 evidence. Its source/debt/behavior results count only for M2/M3/M5. Full/build ceiling raised 2→3, revisions sha256 `0babfa6414aa4039c357124662f1405381bd3a5d19e56834eabb5aaacbb6b57c` and `682098a9dde32d63127ee6dccbbc430a4df2c35d1db304625fe5ccb6ddc96782`.
- `M4 KILLED` — isolated compiling mutant weakened `pledge_guard_inv` from `G = true → AUTH ∧ five facts` to `(G = true ∨ True) → (AUTH ∨ True) ∧ five facts`, and `step_pledge_inv` from `G = true` to `G = true ∨ True`; only the induced bodies used disposable `sorry`. Receipt `evidence/mutant-isolated-m4-full.log`, sha256 `db2f849696444f91f0c18e7ed4e17cf2af4b36f5effd00718aae126dc2efb120`: `lake build` and CI exit 0, 59 unrelated types and inventory 62 intact, immutable v2 probe rejected both exact types, and the debt leg rejected the placeholders.
- `RESTORE PASS` — after M4, Step/Predicates/Invariants Git blobs are `781b35966992e8037c000ccba24a0e68fdeb3086`, `d5deb2ba7e00ece22421664f6fc310c018a20649`, `5378595a05152b27deb477aa759d5574e541e4d8`. HEAD is detached at candidate, tree `9361dcc6ed5b60ba164c435f755c83a4efe0f8dc`, status is clean. Final no-build gate `evidence/candidate-final-static.log`, sha256 `16c27fe6f706ac976f6434632bc0160ce885a1357646b19607eb857f648e0c23`, is `GATE-PASS pledge-2`, failures 0.

## Terminal state

Audit rows: `PASS=4 FAIL=0 OPEN=0`. Mutation rows: `KILLED=5 RESIDUAL=0 BLOCKED=0 OPEN=0`. Campaign `CLOSED`, stopped `SET-POINT`. Full/build-equivalent invocations `3/3` under revisions 001/002; one additional behavior typecheck and the final static gate were non-building controls.
