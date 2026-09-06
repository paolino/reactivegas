# Commit audit — FINDINGS, incomplete coverage

- Candidate: `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2`.
- Base: `368b596fef0b6d393c2ac7afc631d236c55d86d1`; contextual RED: `570fe4a68f510fad3c9912ea59c1e492f3e11740`.
- Mandate: brief VERSION 2, SHA256 `e86eb45c842ed031b0f935cd3d361d4b678fb302970cc5e7e33436bf0ee10df4`, with r5 and its listed amendments. A-002 SHA256 `ba1856f07ebe555ef10e91792392f859a2f287b15509af726462fa8540d7c4e6` governs the commissioning rulings.
- Scope: FULL base→candidate, including the RED changes. Independent Codex `gpt-6-astra/high`, pane %557; ticket owner %534; Muse author %545; no children or author contact.
- Verdict: **AUDIT-FINDINGS — two BLOCKING findings; audit incomplete. No acceptance conferred.**
- Submission: 1 under the one-submission mandate; another submission is not authorized. Three prior funding decisions 8→16→25→34 remain recorded under the express A-002 exception. Owner spend remains 34/34.
- Auditor spend: **9/12 substantive invocations, 7/24 targeted executions**. One cold-first full gate envelope; later work used the warm build. No new execution during NOTE-012 finalisation.
- Campaign: **OPEN**, with 3 OPEN and 3 BLOCKED requirement rows. Assigned frozen mutants killed 6/6; these are not six complete requirement verdicts. See [REQUIREMENT-LEDGER.md](REQUIREMENT-LEDGER.md).

The gate completed successfully, but the independent probes establish defects in state conservation and proof coverage. The evidence floor was not fully assessed before the administrative stop. NOTE-011 required parking; NOTE-012 permits only organizing retained artifacts. NOTE-012 attributes the post-P7 stop to a provider block; this is parent-reported. The auditor has **no direct observation** of a provider refusal. The residual allocation is unspent by freeze, not an elective saving.

## F1 — accepted concurrent appends lose state and length updates

**BLOCKING**, R2/R6 and INV-28-STORE-STM-DISCIPLINE. Frozen source: `lib/KelGroups/Store.hs:604`, `:611`, `:626–629`; represented by `evidence/base-candidate.diff`.

The new route reads state and length before the final STM transaction and later writes values computed from those snapshots. The historical route reads the current state and increments the current length inside STM. Grouping the new writes atomically does not protect their earlier reads.

The compiled `StoreProbe.hs` uses the actual built candidate package and an explicitly controlled serialization rendezvous. Its test-only ToJSON wrapper emits unchanged DemoEvent JSON; it coordinates two calls after their state reads and before their inserts. Both calls return success. For additions (1,2), SQL contains two decoded rows and replay reaches 3, but hot state is 2 and `kelLength` is 1. Three other positive value pairs reproduce the same conservation failure. This is an observed lost update, not a timeout or a failed setup.

Negative control detects a seeded inconsistent tuple; sequential candidate control yields `(hot=3, rows=2, length=2, replay=3, decoded=2)`. A real SQLite trigger rejection is delivered to the caller and leaves those values unchanged. See `evidence/P2-store-probe.log:1–8`.

**Property class:** overlapping accepted appends must conserve all committed transitions and event counts, and their durable replay must agree with the observable hot state. Both calls reporting success while one update disappears violates this class. The controlled four-pair experiment is not an exhaustive scheduling, crash-recovery or optimized-build proof.

Evidence SHA256: `P2-store-probe.log` = `5b93f9edeefd7028b91ac0207f0d6c02e7bf2ff327e55df894fcc5401a2f0cbe`; instrument = `ae6fff29a1aa781e9e6ecbced19235f1545db78737d9d2515de3332d934f30a7`. P1 compile succeeded; P2 exited 1 for the conservation assertions. An enclosing login-shell logout subsequently returned 127 for an unbound logout variable; that is separately journaled and is not the probe's exit or finding cause.

## F2 — the voted non-insertion property does not observe the effect

**BLOCKING**, R4's mandated executable correlate. Frozen source: `test/S28AppApiSpec.hs:359–364`; effect at `lib/KelGroups/Fold.hs:299–309`.

The property named “voted mutations never insert members” maps a proposal to a constructor and returns True for both constructors. It never applies a mutation or compares membership before and after. The structural M4 compiler kill therefore leaves the required effect property unprotected.

A single-expression shadow mutation replaces `Map.adjust` in the voted role-change effect with `Map.insert`, retaining the original candidate files untouched. P4's compilation output explicitly names the shadow Fold module. The independent effect witness first detects insertion into an empty relation by the known defective mutant (P5, exit 1), then passes against the original candidate (P6, exit 0). The same compiled mutant subsequently passes all six shipped direct-only examples, including 100 cases of the named property (P7, exit 0).

**Property class:** a non-insertion assertion must constrain the resulting member-key relation over meaningful present/absent targets, not merely enumerate mutation constructors. This finding concerns the shipped proof's adequacy. It does not assert that the original candidate's role-change effect inserts absent keys, nor that the mutant passed the entire repository gate; P7 is explicitly the six-example group.

Evidence SHA256: P5 = `296644b1288336d92c923b5ddc7c017e80b5662610a211e8764f20c00d70cdbd`; P6 = `4011917b4c980039eb1ee92cd6e3048aa1ca771d1fbb5181a1f7fd60ceacdf3d`; P7 = `787e766448de4f0fff44b34b6a0dce2399e746acac2de163a7e2ee101a7e9449`. Exact-one-edit diff = `93aa23971b6fd72797abbb98c3c40a02d588083284ce734312a866375baf1b82`; shadow source = `71c2dbebdbb5cda2cc08dcc4d31ebf9a1026e3a60a615fa7cc43adc26f405b28`; Row4Probe source = `97e017e2e0c43bd108e826ad0fa7ccd54c246da92858acb3c77008ce3089571d`.

## Failure modes and limits assessed before freeze

- Resource acquisition: new SQLite/founding/key-generation work stays synchronous; explicit founding-corruption/mismatch branches throw IO failures. Independent failure-cleanup and resource-leak assurance was not completed. No claim that all such failures were tested.
- Threading: the reviewed production additions introduce no async/fork operation. The concurrency finding concerns overlapping callers. Existing “thread blocked indefinitely in an STM transaction” output was also found in the retained owner base log; it was not attributed to this candidate.
- Synchronization: TVars remain, but state/length reads moved outside their write transaction; F1 proves a lost-update failure with successful returns.
- SQLite ordering: the new success path inserts before updating TVars. P2 proves SQL rejection is caller-visible and leaves the tested hot state unchanged. Interruption between a committed insert and the TVar writes was not tested.
- Replay/degradation: malformed row decoding is filtered and refused replay steps keep state. These paths were reviewed, not exhaustively fault-tested. Full corruption/mixed-format recovery remains unjudged.
- JSON: executed witnesses cover nonempty pendingBase roundtrip, missing-field backward compatibility, malformed numeric pendingBase refusal, and pending-content reopen. Complete backward/forward field-domain coverage remains unjudged.
- Value coverage: the demo has distinguishable positive counters and nonempty logs; founding agreement includes actual accepts. The empty-start property alone exercises refusal-only traces. F2 establishes one specific persistent coverage failure; exact hook-view values and broader event/franchise coverage remain open.

The historical API, unsigned integrated-row placeholders, corrected CESR reliance, and forthcoming V-2 rebind are bounded by the supplied mandate. This report does not extend assurance to signatures, remote CI, release readiness, or historical semantics beyond the reviewed diff and executed suites. No unrelated discovery is opened and no new product invariant is ratified.

## Verification receipts (existing executions only)

`G` below means `evidence/20260905T213306Z-84a2dae-`. All gate commands use `nix develop .#ci --quiet -c` in the isolated execution tree, with G28_EVIDENCE_DIR and TMPDIR overrides recorded in pre-build-envelope.txt. Frozen gate full SHA256: `7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a`; normalized: `f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650`. Full log SHA256: `12c36103dbc6b8a282ca5b6c9569270690f44e315fb0c1b4e28c9357c38df023`.

| Invocation | Exit | Seconds | Retained log / result |
|---|---:|---:|---|
| leg3: just build | 0 | 16.948 | Gleg3-build.log; cold |
| leg4: cabal test all -O0 --test-show-details=direct | 0 | 29.085 | Gleg4-test.log; 23/23 registered; 127+91 examples, 0 failures |
| M1: cabal build all --enable-tests -O0 | 1 | 8.611 | Gleg5-M1-build.log; boundary unification error |
| M2: cabal test all -O0 --test-show-details=direct | 1 | 39.864 | Gleg5-M2-test.log; nonmember rejection witness fails |
| M3: same test command | 1 | 26.451 | Gleg5-M3-test.log; hook refusal witnesses fail |
| M4: cabal build all -O0 | 1 | 2.264 | Gleg5-M4-build.log; non-exhaustive BaseMutation match |
| M5: same test command | 1 | 27.438 | Gleg5-M5-test.log; named agreement witness throws MUTANT-M5 |
| M6: same test command | 1 | 28.961 | Gleg5-M6-test.log; live/replay witness fails |
| leg6: just ci | 0 | 65.479 | Gleg6-ci.log; full local CI |
| P1: single StoreProbe compilation against candidate package | 0 | 4.064 | evidence/P1-compile.log |
| P2: compiled store-probe +RTS -N2 -RTS | 1 | 0.395 | evidence/P2-store-probe.log; F1 |
| P3: Row4Probe and existing S28 spec compilation | 0 | 4.683 | evidence/P3-row4-compile.log |
| P4: Row4Probe with shadow effect and original dependent modules | 0 | 5.911 | evidence/P4-row4-mutant-compile.log:8 proves shadow loaded |
| P5: row4-mutant witness | 1 | 0.388 | evidence/P5-mutant-negative.log; intended effect failure |
| P6: row4-candidate witness | 0 | 0.387 | evidence/P6-candidate-positive.log |
| P7: row4-mutant --match 'S28-1 direct-only admission' | 0 | 0.396 | evidence/P7-mutant-shipped-property.log; survivor |

Gate durations are observer-measured phase intervals, not exact child CPU times. Per-command full hashes, byte sizes and before/after free space are retained in command-receipts.jsonl; P1–P7 receipt identities are in STATUS-before-terminal.txt and probe-receipts.txt. Targeted compilation disk deltas were not individually sampled. Passing raw S28 output and every mutant's actual failure section were read during the audit; summary markers were not the sole basis.

## Evidence disposition

[REQUIREMENT-LEDGER.md](REQUIREMENT-LEDGER.md) supplies all row judgments and open limits. [EVIDENCE-INVENTORY.md](EVIDENCE-INVENTORY.md) is the readable identity index; EVIDENCE-INVENTORY.json includes retained temporary artifacts without following symlinks. Its SHA256 is `bf984360cad4cdfed372deed5e448e4e2d9fb441b95646086f9fe7ef79a8ec9c`.

The recorded post-envelope check has clean author, reference and execution trees, matching candidate SHA, unchanged gate, and all three reserved /tmp staging files consumed as frozen. It is not a new finalisation-time verification. All artifacts and build trees remain preserved per NOTE-011/012; no cleanup, build-tree retirement or fresh resource measurement occurred. Report identity hashes and the single terminal COMPLETE event are appended in STATUS. Acceptance and any subsequent disposition belong to the ticket owner.
