# Commit Audit

- Submission: **2/2** (last). Auditor `%535`, grok-4.6/xhigh. Owner family `muse`. Alternate `true`. START `family_set=claude,codex,grok` is **not** authority for a Claude substitution; operator eligibility is `codex|grok`.
- Base: `6879970fdb1a797263843387e14704eaa1e3a2e7`
- Candidate: `280b67f14fa74d352b36bca98f87f03a3819308b` (clean detached; tracked porcelain empty before and after every run)
- Mandate: `../commit-owner-one-membership-glm/brief.md` sha256 `c013e595e7af8175a01b431b2891aec024ffa2006a59b412bf586b948cab8f58`
- Packet: `../handoffs/EXPANDED-REVIEW-PACKET-s2.md` sha256 `8544e26631456d400b2c59548048c024c80e8c6511dff0c698d11c3e059f5795`
- Ledger: `../campaign-ledger-S62-SIM.md` sha256 `cb48443e1fbdf1c3692a83dbaa3fc8be1426a57c320e846c63d3e2bb1c72f3c0` (submission-2 section; NOTE-002)
- Prior report: `../commit-auditor-s62sim-codex-s1b/report.md` sha256 `19d5a79e38327339ab36cfd5270a9411fb8f841467360f35eed38c64d9c74ffe` — F1–F8 immutable open set
- Scope: FULL `6879970f..280b67f` (nine unaccepted prefix commits + C1/repairs) plus integration against `origin/master` `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (already an ancestor)
- Verdict: **FINDINGS**
- Audit loop: submission `2/2`; next submission `FORBIDDEN`
- Ceiling raises: `0/2`
- Campaign: **OPEN** — ended by none; two BLOCKING rows remain `BLOCKED`. No third submission.
- Builds: `5/30` this ticket; this audit `1`, `cache=cold` from **absent** `lean/.lake`. Owner-warmed tree (23M, 24 oleans, birth 2026-09-05T11:59:44+0100, before START) was recorded and discarded unused (NOTE-001).

Frozen instruments (all `chmod 444`, hashes match the packet before any green was trusted):

| instrument | sha256 |
|---|---|
| `../handoffs/gate-v14-one-membership.sh` | `1164ae9d8a6221d8be237abdb5cad0947d2a4c8749617cbaf67fc57c1904359d` |
| `../handoffs/retired-surface-probe.mjs` v3 | `9fba77d3f5070de48941bc1ac1410f0dd213e9968a7ba216bc907403ac591eb3` |
| `../handoffs/oracle-one-membership-v2.mjs` | `b90327db82099171423a748a94d51d7363d60bc742a7180f8b833d341f4ffdbc` |
| `../handoffs/derive-cited-sources.mjs` | `5efd430530f310040c2dd31a2f7537947b34abc69e8e0a810324f39d5288c752` |

## Invariant matrix

Every row is mandate `BLOCKING`. PASS is bounded by the cited checks.

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| INV-1 one membership | BLOCKING | PASS | KILLED | Oracle mutant `payload-regains-users` red for `State.empty` extra `users`. Gate v14 + oracle v2. `evidence/oracle-v2-selftest.log` |
| INV-2 substrate Key | BLOCKING | FAIL | BLOCKED | F4 remainder: chip path and `/\s+/g` name path are fixed; `Number` on `data-goto-person` and digit-shaped `data-act` `u` still coerce `"01"`→`1`, then `pruneNav` drops the view. `evidence/f4-goto-coerce.json` |
| INV-3 canonical authority | BLOCKING | PASS | KILLED | Oracle `authority-stops-reading-the-view` red. Gate v14. |
| INV-4 fourteen only | BLOCKING | PASS | KILLED | Live F6 `case 'removeMember'` red at derivation. Parser-blind apply red at execution. Inventory 14=11+3+0. `evidence/probe-holes.log`. Text-layer hole is not a live branch (Advisories). |
| INV-5 sealed consequences | BLOCKING | PASS | KILLED | F1 stalled adopt restores; F2 hook-closure export verifies; F3 `baseChange` import refused. `evidence/browser-probes-s2.json`. Oracle stall/absorb mutants red. |
| INV-6 zero read / departure row | BLOCKING | PASS | KILLED | Oracle `admission-creates-a-conto-row` red (`T1`). |
| INV-7 one election | BLOCKING | PASS | KILLED | No admission-proposal constructor; elect is one `changeRoles` propose. Teaching journey elects after direct admit; gate v14 teaching GREEN. |
| INV-8 freshness | BLOCKING | PASS | KILLED | 22 cited sources derived and fresh vs `origin/master`. Claim-gate stale-pin / composition-pin controls red. Gate v14 line 27. |
| INV-9 emitted fixtures / hook seeds | BLOCKING | PASS | KILLED | `TraceDriverV1` calls `Reactivegas.apply` (not bare `economicCleanup`). Trace gate: sha `1bec2e6a…`, 35+35 replay. Fixture mutants red. F7 closed. |
| INV-10 unruled threshold | BLOCKING | PASS | KILLED | Oracle `threshold-anticipated-to-strict-majority` red; n=2 remains 1. |
| INV-11 existing guarantees | BLOCKING | FAIL | BLOCKED | F8: ordinary group-card heading still leaks `KelGroups` / `GroupState.members`; visible status still says «enunciate, non dimostrate» against 0 enunciate / 56 proved. Prefix R-GEO/R-CIT/R-ITA.1–4/R-LAY hold. |

## S1 named findings at this candidate

| ID | At `280b67f` | Evidence |
|---|---|---|
| F1 stalled adopt→null | **closed** | export v3 verifies; adopt restores `{bruno:40, comune:-40}`; tamper rejected; `stateNull=false` |
| F2 hook-vote discontinuity | **closed** | sweep `closed:1`; export accepted before and after the next open question |
| F3 unbound cleanup import | **closed** | `baseChange` step refused: «un effetto del gancio non è un evento». v2 envelope refused by name |
| F4 key/name coercion | **open** (remainder) | chip `selectedType=string` `selected="01"` `isAdmin=true`; name `Alessio`→key `alessio`. `Number("01")===1` on goto-person / pledge-u; `includes(1)` false; view pruned |
| F5 sorry-detector vacuity | **closed** | production confirms on=enunciato/off=provato; `const sorried = false` does **not** confirm. Claim-gate selftest control fires `sensibilità del rilevatore confermata`. `evidence/f5-sorry-mutant.log` sha of mutant `4ab1189a…` |
| F6 live retired branch | **closed** (live class) | exact s1 mutation red: «ramo eseguibile per 'removeMember'». Parser-blind apply also red |
| F7 producers bypass hook | **closed** | driver uses `Reactivegas.apply`; 35 integrated steps; byte-identical regen |
| F8 Italian chrome / stale proof status | **open** | heading and status still visible; see Blocking findings |

## Prefix survival (R-GEO, R-CIT, R-ITA, R-LAY)

Checked at the candidate, not inherited from pre-rebase receipts.

| Row | At candidate | Can fail |
|---|---|---|
| R-GEO.1–6 layout | `purchaseRingLayout` clusters on referente angle; members on outer r=220; purchases on inner ring; minDist=92; two runs identical | yes — a non-deterministic layout would fail `stableMembers`/`detPlacements` |
| R-GEO.7 drag | same members under a pinned pose; purchase angle changes; members do not | yes |
| R-CIT | 67 permalinks, pins are 40-hex (`e6c59242…` and `934de7a8…`), 0 branch-like | yes — claim-gate «collegamento citazione senza pin» |
| R-ITA.1 euro | `conto 0 €`, `cassa 0 €` | yes |
| R-ITA.2 canale base in feed | not in ordinary feed; CLAIMS proof-panel text still names «Canale base» (R-ITA.5 permits citation identifiers) | feed empty at boot; teaching copy checks other banned words |
| R-ITA.3 decision not id | govcard uses `descrProposal`; selftest `!/depart:\|roles:/` | yes |
| R-ITA.4 usernames | scene `.nm` = `Anna`, not a letter badge; C1 re-key to `KelGroups.Key` still displays names | yes |
| R-ITA.5 ordinary chrome | **fails** — F8 | shipped vocab selftest **strips `.mono`**, so the heading cannot turn that check red |
| R-LAY | conto/cassa same x, |Δy|≈25 | yes |

No acceptance is inferred from patch-id or subject. The nine map by content; survival is the live question above.

## Failure modes altered

| Boundary | Before / now / observability |
|---|---|
| Session envelope | Three streams (`trace`/`kel`/`base`+`seq`) → one `reactivegas-integrated.trace` v3. `normalizeWrap` **throws** on v2 by name (`formato precedente a tre flussi non più accettato`). Restore catches and quarantines. Not silent `undefined`. |
| Adoption | Duplicate `economicCleanup` on paired `e` is gone. `adoptSession` replays `applyIntegrated` once per step and throws on refuse/divergence. F1 no longer installs null. |
| Hook provenance | No standalone cleanup event. `verifyIntegratedV1` rejects `event.baseChange`. Forged/omitted hook effects fail before adopt. |
| Key selection | Chip `Number` restricted to `coll` (named F4 site). **Remaining:** `data-goto-person` and digit-shaped `data-act` `u` still `Number`. `pruneNav` uses `members().includes(d.u)` (strict), so `"01"`→`1` is dropped with no error — a click that does nothing. |
| Name acquisition | `replace(/\s+/g,' ')` — `Alessio` stays `Alessio`. |
| Proof-status prose | Receipt derives 56 proved / 0 enunciate. Visible `#gov-status` still asserts «enunciate, non dimostrate». Glyphs can be `⊢` while the sentence is false. |
| Detector sensitivity | `RG_GATE_SORRY_DETECTOR=off` plus `on=enunciato/off=provato`. Unreachable fixture is not success. F5 class closed. |
| Gate teardown `rmQuiet` | Desk C1 limitation, not re-executed as five forced-cleanup pairs this audit. Semantic selftests still exit 1 on domain defects (gate v14). |

## Residuals

None terminated `RESIDUAL`. Blocking rows may not be residual. Desk silent-cleanup limitation remains the named non-blocking C1 limitation, not a campaign residual.

## Candidate invariants

None ratified here.

Proposed (unratified, non-blocking): **CI-PROBE-FIXTURE-SHAPE** — probe v3 deroga (ii) matches any `const NAME = {"` line, not a Lean-emitted fixture whose bytes a trace gate verifies. A parked `const HIDDEN = {"removeMember":1}` greens the probe. Live dispatch is still killed. Proposed severity ADVISORY (text layer; executable layers hold F6). Evidence: `evidence/probe-holes.log` (`fake-fixture-token` exit 0).

## Onward discoveries — outside this ticket

- Stale comment in `economics-simulator.html:386–392` still describes `users:[Nat]` / numeric UserId. Lead only. `RECORDED, NOT-OPENED`. Owner: ticket/census. No follow-up ID filed from this seat.
- Otherwise none.

## Blocking findings

1. **F8 / INV-11 / R-ITA.5** `economics-simulator.html:3931` and `:3934–3936` — ordinary group card still shows `KelGroups — una sola membership (GroupState.members)` and «le prove del modello sono **enunciate, non dimostrate**» while `CHECK_RECEIPT.axioms` is 56 `provato` / 0 `enunciato`. Live DOM `innerText` includes both. The page selftest deletes `.mono` before its vocabulary scan, so the heading cannot fail that check; the status sentence is not in `.mono` and is still unasserted. **Property class:** every ordinary reachable chrome string (cards, not only teaching strips) must match the approved Italian vocabulary, and visible proof-status prose must be derived from the same receipt as the glyphs — a `.mono` exemption or a badge presence check must not hide a false sentence. Evidence: `evidence/browser-probes-s2.json` `f8-vocab`; gate v14 teaching GREEN.

2. **F4 remainder / INV-2** `economics-simulator.html:4541`, `:4528`, `pruneNav:3123` — named chip/`/\s+/g` sites are fixed (`selectedType=string` for elect of `"01"`). `data-goto-person="01"` still does `Number` → `1`; `members().includes(1)` is false; the person view is pruned; the click is a no-op. Digit-shaped `data-act` `u` becomes number `1` (`pledgeU=1`). **Property class:** the complete UI-to-event path must preserve arbitrary substrate string keys, including leading-zero keys, through every control that writes `nav().u` or an event `user`/`target` — not only the chip picker. `includes` after `Number` is an identity bug, not a missing member. Evidence: `evidence/f4-goto-coerce.json`; `evidence/browser-probes-s2.json` `f4-keys`.

## A-003 and A-001 (did the ruling weaken the gate?)

- **A-003** (permit true `comp-base-threshold` text naming live `removeMember` vocabulary): executable dispatch/refusal layers still kill a live `case 'removeMember'`. The product was not bent. The later count-of-two text rule is gone; that was F6, not A-003 itself.
- **A-001** (fixture-shape exemption after Q-001): necessary for the F7 Lean corpus. It **did** weaken the text layer: any ALL_CAPS JSON `const` hides `removeMember` (`fake-fixture-token` GREEN). Owner claim that executable layers are the real protection holds for live dispatch (F6 killed). The twelve shipped controls never include a dressed-as-fixture parked token. Not opened as a candidate blocking row.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `nix develop --quiet -c just ci` at `280b67f`, `lean/.lake` absent | 0 | 134113 ms | `evidence/just-ci-280b67f.log` sha256 `5a22a2fe9d1acd67112dbd4f0b4bd0749526b3d5318dd442ff754054e3f2d49b` cache=cold |
| `bash …/gate-v14-one-membership.sh <WT>` | 0 | 232233 ms | `evidence/gate-v14-independent.log` sha256 `890d522b77700467590dc05f3c1580fffe87d3bee315e796194723a1e866cd48` cache=warm |
| oracle v2 `--selftest` | 0 | — | `evidence/oracle-v2-selftest.log` 8/8 production mutants red on reason |
| probe v3 `--selftest` | 0 | — | `evidence/probe-v3-selftest.log` 12/12 red on stated reason |
| `instruments/f5-sorry-mutant.mjs` | 0 | — | production confirms; disabled detector does not |
| `instruments/probe-holes.mjs` | 3 | — | F6 live red; fake-fixture GREEN (instrument hole) |
| `instruments/browser-probes-s2.mjs` | 0 | — | F1–F3 closed; F8 open; geometry deterministic |
| `instruments/f4-goto-coerce.mjs` | 0 | — | `"01"`→`1`; view pruned |

Gate v14 GREEN does not close F4 remainder or F8: those paths are outside its detectors.

## Advisories

- Probe v3 deroga (ii) is a regex over fixture *shape*, not over Lean-emitted constants the trace gate verifies. Instrument `instruments/probe-holes.mjs` sha256 `0b44cfbdf49a1e44aa1d5abbf6e915bf16b76ffcb5d4e482ef0d5dee9b179f1d`. Property shape: quantify allowed `removeMember` sites from (claim field value) ∪ (constants byte-compared by the trace gate), and require a control that plants a fake `const NAME = {"removeMember"…}` and demands RED.
- `instruments/f5-sorry-mutant.mjs` sha256 `7808dd380b38ba57f43d92d443d4878332be3ad0515f069306b5e0bdc20d039b` — F5 seed.
- `instruments/browser-probes-s2.mjs` sha256 `b36d863ab680969fb578a98cb599831fa034d33176274c51f9c27d9476443112`
- `instruments/f4-goto-coerce.mjs` sha256 `7c2e0d062523dcbdc296ccf07ba7d913bb3add7be63e55f22b059d3f7a014b85`
