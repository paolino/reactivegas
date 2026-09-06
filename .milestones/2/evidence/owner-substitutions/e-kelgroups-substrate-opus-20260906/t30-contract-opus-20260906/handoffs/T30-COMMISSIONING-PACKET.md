# T30-COMMISSIONING-PACKET — consolidated `#30` commissioning packet (static assembly)

Author: ticket preparation owner `t30-contract-opus-20260906`, pane `%572`.
Authority: NOTE-005. **Static assembly only — nothing executed in this slice,
nothing granted by it.** No new planning registry: the mandate is referenced,
not rebuilt. Seats are the epic owner's and are recorded, not proposed.

## 0. Two corrections carried

**C-1. Not "the only way".** What the executed result plus source inspection
establish is a limitation **of this harness as built**, not an impossibility.
Both halves are properties of r9's own code: in live mode the hash channels
read through the frozen oid (`git show "$FROZEN_HS_BASE:$path"`), so a
working-tree edit cannot move them — A17 and A26 both leave `1-hash`/
`1-hash-hs` green while other checks fire; in overlay mode `USE_GIT=0` and D-3
prints `3-skipped in overlay`. Two cheaper instruments would reach M22b, named
here and **not requested**:

- **(a) zero product builds.** Let overlay mode run D-3's *pin comparison*
  against the dump the runner already stages, instead of skipping D-3 whole.
  One overlay case could then edit the HS overlay source (source-hash fires)
  while the staged dump is unchanged (pin green) — both channels live in one
  run. **Limit:** it would establish that the two *code paths* are independent.
  It would not establish that a real compiler leaves interface bytes unchanged
  for an unexported-only edit, which is the substantive premise.
- **(b) narrowest real-compiler instrument.** Two compilations of **one
  existing** module with an unexported-only edit between them, comparing
  `ghc --show-iface` bytes. That answers the premise directly, needs no `#30`
  candidate, and is strictly narrower than B22a+B22b as scoped (whole-project
  export plus two builds).

**C-2. One build cannot establish stability across rebuilds.** This lane has
**zero** prior product builds, so no valid comparison baseline exists at all.

*What a single B3 does establish, narrowly:* that the project builds at the
frozen base under the pinned toolchain (exit 0 plus receipt); that
`ghc --show-iface` is invocable on the produced `.hi` and yields non-empty
bytes for the named module; that the selector
`find dist-newstyle -name '<Leaf>.hi' -path '*<rel>*'` resolved **exactly one**
candidate **at that moment in that build tree**; and that the
marker → build → emit ordering left the `.hi` newer than the marker for that
build.

*What a stability claim would require:* at least **two** builds of
byte-identical source under the same toolchain pin, with the intervening state
named — warm rebuild and clean rebuild are different claims — comparing
`--show-iface` bytes across them. Note the direction of risk: `dist-newstyle`
accumulates, so a second build can *add* candidates and make the
exactly-one selector ambiguous. That is precisely a failure a two-build test
surfaces and a one-build test cannot.

## 1. Current full mandate — by reference

| artifact | path | sha256 |
|---|---|---|
| contract | `e-kelgroups-substrate/t30-contract/handoffs/T30-CONTRACT-r8.md` | `ea6c2019892da2148ef237128156e0aed9ee2e8c123ca38eb8d13baff201e71e` |
| command map | `…/handoffs/T30-COMMAND-MAP-r8.md` | `ca033b1edcd7def8466a90909ceee941d521a948ea4f27c84ea361dc4500b15d` |

Scope rows R30-1…R30-14, spec S1–S6, the 26 REQ IDs, the 7-file Lean extent
and the identity ground stand exactly as filed. Not restated here.

## 2. Frozen executable gate inputs, with demonstrated status per row

| artifact | sha256 | status |
|---|---|---|
| `T30-DRIFT-LEG-r8.sh` | `f0afa32b4fbb13ac6084b6c3c5abd503f7e21f051fef458265b97fd56a4de3e3` | superseded; **defect witness**, never a gate input |
| `pf8/run.sh` | `62025c179e85b6ab982e848a863daa7d48594eccbc8b25c46c616298fe84c39c` | invocation 1 harness, spent |
| `pf8r/run.sh` | `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611` | invocation 2 harness, spent |
| **`T30-DRIFT-LEG-r9.sh`** | **`69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`** | **the leg a `#30` gate would use** |
| `pf8r2/run.sh` | `86533877935f6ea21f9e822f106c421a00699233b654848c6e8fb97c46a587cf` | invocation 5 harness; SUITE PASS |

The same leg bytes are present in `pf8r/` and `pf8r2/` — `cmp` byte-identical,
which is why the green is attributable to the fixtures and not to an edited
check.

Per-leg status. **"Demonstrated"** below always means *synthetic layer, stubbed
`ghc`, fixture repos* — never the real compiler and never the `#30` extent.

| leg row | synthetic layer | real-compiler layer | `#30` extent |
|---|---|---|---|
| D-1 position (both repos), full-oid exactness | demonstrated (A1/A18/A19) | n/a — pure git | **undemonstrated** |
| D-1 file-sets (both scopes) | demonstrated (A1) | n/a | **undemonstrated** |
| D-1 clean samples (both repos) | demonstrated both ways (A1/A17/A26) | n/a | **undemonstrated** |
| D-1 byte-hash tripwires | fire only in overlay (A20/A22); silent by construction in live | n/a | **undemonstrated** |
| D-2 mapping self-check | demonstrated (A1) | n/a | **undemonstrated** |
| D-3 `.hi` select / freshness / emit / pin | demonstrated against a **stub** (A1/A7/A8/A9/A10/A11) | **UNDEMONSTRATED** | **undemonstrated** |
| D-4 join: count, uniqueness, exact-line, exact-success REQ | demonstrated (A1/A2/A5/A6/A15/A16/A24) | n/a | **undemonstrated** |
| D-4 dump provenance | demonstrated (A27) | **UNDEMONSTRATED** | **undemonstrated** |
| overlay base binding | demonstrated both ways (A21/A28) | n/a | **undemonstrated** |
| source-hash ⟂ `.hi` independence (M22b) | **not reachable in this harness** (C-1) | **UNDEMONSTRATED** | **undemonstrated** |
| exit taxonomy | demonstrated under v2 | n/a | see §3 |

## 3. Taxonomy reconciliation, before freeze

TAXONOMY-v2 is bound to the synthetic experiment only; contract §8 and the
command-map block still say v1. Reconciling them requires naming any row whose
**promised outcome** moves. Determined mechanically over the invocation-5
streams: with a green baseline, exactly **two** of 31 cases reach a refusal
after a verdict has already been rendered — the only situation in which v1 and
v2 differ:

| row | r8 map promise | v1 delivers | v2 delivers |
|---|---|---|---|
| **A10** stale `.hi` | **RED(1)** | **3** — `3-fresh` RED, emission skipped, `4-missing` refusal exits 3 | **1** |
| A27 inherited dump | *(new r9 control, not an r8-map row)* | 3 | 1 |

**Exactly one r8-map row changes: A10 — and v2 moves it *into* agreement with
the map's own promise.** Every other row is either pure-RED or pure-refusal, so
v1 and v2 agree on all of them; no other promised outcome moves in either
direction.

Two ways to reconcile, both silent-change-free:

- **Adopt v2 in contract §8 and the command-map block.** No promised outcome
  changes; the map's `RED(1)` label for A10 becomes true instead of false.
- **Keep v1.** Then A10 must be **re-labelled REFUSAL(3)** in the map, and the
  contract must say that a rendered verdict can be reported as a refusal.

Honesty marker: the v1 column for A10 under a green baseline is **derived from
the leg source** (`refuse()` → `exit 3`), not observed — `TAXONOMY_V2=0` was
never executed. The v2 column is observed.

Either way this is a **separate versioned act** before r9 judges anything for
`#30`. It is not performed here.

## 4. Original `#30` acceptance rows still open

**The fixture layer passing discharges none of these.** Every row below is open
at `933e385d`.

Contract §10, all open: threshold-parameterized verdicts; retained and
unduplicated records with PRODUCED causes; refusal pre-effect (aggregate and
log unchanged, accepted-KEL replay identical); validate/fold agreement;
negative delivery at the boundary; zero producing sites; §7-REQ complete (26
IDs registered and executed with exact-success records, B19 RED observed);
drift GREEN plus all six directional REDs on the real extent; `.hi` inventory
hash matches frozen (B22a GREEN + B22b diff-fire); source-hash channel
demonstrated independent (SRCADD: source RED + `.hi` clean); M10a COMPILER-kill
plus M10b GREEN-ENUM; bounded surface closed with review-only remainder
labelled; M4a/M4b criteria with extras classified; client additions under
client CI; `Trivial` intact; full `just ci` green; tracked-clean both ends;
founding guard held; L-1–L-7 recorded; per-identity rows all resolved; fresh
audit PASS complete.

Contract §12 freeze-validation (i)–(xi): (i)–(x) all open. **(xi) alone is
partially discharged** — the shell-portability pre-check (tool availability,
printf sanity, smoke assertions, no leading-dash formats, POSIX ERE) is what
the synthetic campaign exercised, and it now carries the taxonomy caveat in §3.
Nothing else in (i)–(xi) is touched by a synthetic run.

§11 dependencies, unchanged and not reopened: `#68` → R30-9 rebind and
revalidation only; `#81` (§1–§3, L-7 gated `#76`) → R30-10 content; `#76` →
Reactivegas side, kelgroups exposes interface and closure evidence; `#75`/R3.1
→ threshold is a test input, never a shipped default; upstream Lean gaps
enumerated, never invented. `#33`/`#34` stay downstream and untouched. **No
inferred `#73` closure.**

## 5. Feasibility test — verified at source, per build

**Verification (mine, read-only, at `933e385d`).** `git ls-files lib` returns
exactly eleven modules: `Bootstrap`, `Event`, `Fold`, `Jwk`, `Server`,
`Server/JSON`, `State`, `Store`, `Trivial`, `Types`, `Validate`. `git ls-files
| grep -i vote` returns **nothing** — no tracked path anywhere in the
repository matches "vote". `kelgroups.cabal` `exposed-modules` lists the same
eleven and no Vote module. **The drift leg's frozen HS extent
`lib/KelGroups/Vote/{State,Types}.hs` is exactly the missing `#30` candidate
declaration.**

A consequence worth stating before the table: **the drift leg as frozen for
`#30` cannot produce a meaningful live run against the current base at all.**
`1-fileset-hs` compares the live `lib` set against a frozen set containing Vote
paths, so it REDs in D-1, before D-3 is ever reached. Its first meaningful live
run is against the first candidate.

Two subjects exist today that stand in for the missing extent:

- **`KelGroups.Event`** — already a named frozen-module row in the command map
  (`…emission per frozen-module row incl. KelGroups.Event`, map line 80).
- **`KelGroups.Server.JSON`** — the repository's **only** nested module, so its
  `.hi` path shape (`KelGroups/Server/JSON.hi`) is the structural analogue of
  `KelGroups/Vote/Types.hi`. It is the right subject for the selector question.

| build | on `933e385d` as it stands | on the `#30` extent | baseline | mutation target / input | restore | what the observation establishes, narrowly |
|---|---|---|---|---|---|---|
| **B3** cold build + marker + receipt + per-module emission + hash-pin | **OPERABLE NOW** with `KelGroups.Event` and `KelGroups.Server.JSON` as the frozen-module rows | **IMPOSSIBLE** — `find` returns zero candidates for `KelGroups.Vote.Types`; the leg refuses `3-select: ZERO .hi candidates` | tree `933e385d`, clean | **none** | none needed | the four C-2 points, for those modules, at that moment: build exit 0 + receipt; `--show-iface` invocable, non-empty; selector resolved exactly one candidate; `.hi` newer than the marker |
| **B22a** scratch export + baseline GREEN build | **OPERABLE NOW** | same, needs no candidate | export of `933e385d` | **none** | export discarded | that the accepted base builds green **in an exported scratch tree** — the export procedure is sound. This is the §12(viii) pre-check, which the contract already requires before any overlay edit and which is *blockable*, never skippable |
| **B22b** overlay edit + build + emission + diff-fire + discard | **OPERABLE NOW** on the existing module set | **IMPOSSIBLE** for the Vote extent | export of `933e385d` | one **exported-interface** edit to an exported copy of an existing module (never the worktree) | export discarded — no tracked file is ever mutated, which is why this is safer than an in-worktree mutant | that a real interface change moves `--show-iface` bytes and the pin fires. For **M22b** it additionally needs the *unexported-only* variant — see C-1(b) |

**What transfers to the `#30` extent, and what does not.** A real-compiler
observation on a different module set is a real-compiler observation; it is not
the `#30` extent's evidence.

- *Transfers:* toolchain-level facts — `ghc --show-iface` is invocable in this
  project's build environment; its output shape; that the selector resolves for
  a **nested** module in this project's real `dist-newstyle` layout; that the
  marker/receipt ordering survives a real build; that an interface edit moves
  the bytes.
- *Does not transfer:* anything about Vote identities, the mapping rows, the
  join, the 26 REQ executions, or that the Vote extent's own `.hi` will be
  unique — a newly declared module can appear in more than one component's
  build tree, and only its own build answers that.

**Impossible before an implementation candidate exists**, stated plainly: every
observation about the `#30` extent. `1-fileset-hs` and `1-hash-hs` over
`lib/KelGroups/Vote/*`; D-3 emission, freshness and pin for
`KelGroups.Vote.Types`; every D-4 join row, since the rows name Vote
identities; all 26 REQ-ID executions; M10a and M10b; and every §10 acceptance
row. No number of synthetic or existing-module observations substitutes for
one of these.

## 6. Implementation decomposition — compiler boundary first, `#30` scope whole

The ordering principle: reach the real compiler **before** the behavioural work,
not after it, so a toolchain fault surfaces at slice 0 rather than at
demonstration time. Nothing is dropped — this changes order, not scope.

| slice | content | why here | real-compiler contact |
|---|---|---|---|
| **S30-0** toolchain preflight | B3-class build + emission + pin on the **existing** module set (`KelGroups.Server.JSON` for the selector, `KelGroups.Event` for the frozen-module row) plus the B22a export/GREEN pre-check | payable **before any candidate exists**; if the selector is ambiguous in this project's `dist-newstyle`, or `--show-iface` is not invocable as assumed, it is known before a line of Vote code is written | **first contact, slice 0** |
| **S30-1** extent declaration | declare `lib/KelGroups/Vote/Types.hs` + `State.hs` with the Lean-mirrored identities, add to `exposed-modules`; data declarations only, no behaviour | the minimal candidate that makes the `#30` extent exist, so D-1/D-3 run against the real extent and the selector question is answered **for the actual subject**; also the first `-Wall -Werror` exposure of the mirrored identities | second contact |
| **S30-2…n** behavioural rows | R30-1 open; R30-2 placement/switch/recast; R30-3 sweep/closure/retention **and** non-duplication; R30-4 verdictOf; R30-5 refusals produced; R30-6 franchise; R30-7/14 negative delivery at the boundary; R30-8 route separation; R30-10 mechanism surface; R30-12 client adapt-only | the r8 map's own order, each bisect-safe, each carrying its REQ rows and mutants | each slice |
| **S30-final** closure | replay/closure evidence, `Trivial` presence, full `just ci`, tracked-clean both ends, founding guard | — | full CI |

Retention, explicit: client (R30-12), integration, replay and closure are
**slices in this sequence**, not deferred past the compiler boundary. R30-9
(rebind, `#68`-gated), R30-10U/R30-11 (unscheduled / evidence-only), R30-13
(Lean-owned), R30-X fences and L-1–L-7 records all remain in the mandate
unchanged. `#30` is not shrunk to what is convenient to build first.

## 7. Budget arithmetic — every historical count, rejected attempts included

Counted, never netted out. Sources are filed epic artifacts, cited.

| campaign | owner | auditor | submissions | source |
|---|---|---|---|---|
| S28-1 (original) | **34** builds | **9/7** | plus one **zero-build invalid admission**, retained separately | `artifacts/KELGROUPS-S28R1-F3-HANDBACK-AND-SUPERVISION-20260906.md` |
| S28-R1 | **13** | **10/16** | one submission, spent by its report | same |
| S28-R2 | granted **14/24**; **actual completed 26 substantive + 4 targeted + 2 diagnostic** | granted **12/24**; **actual 11 substantive + 22 targeted** | one submission | grant: `artifacts/KELGROUPS-S28R2-CONDITIONAL-F3-REPAIR-GRANT-20260906.md`; actuals: `artifacts/KELGROUPS-LANDING-RECOVERY-READBACK-20260906.md` |

**Flagged, not resolved by me:** the S28-R2 grant line (14/24) and the actual
line (26 substantive) are recorded in different artifacts and do not obviously
reconcile. Which is authoritative is the epic owner's ledger call; I report both
with their sources rather than pick one or silently net them.

T30 synthetic campaign, this lane's own counter:

| run | invocations | outcome |
|---|---|---|
| pf1 | 2 | both failed at runner setup; retained, never refunded |
| pf7 | 0 | written, never executed |
| pf8 | 1 | SUITE FAIL, 17 mispredicts |
| pf8r | 1 | SUITE FAIL, baseline BROKEN, 11 mispredicts |
| pf8r2 | 1 | **SUITE PASS**, baseline GREEN, 0 mispredicts |
| **total** | **5 of 5** | ceiling reached; no retry reserve used or requested |

**Product builds: 0**, across every campaign in this lane, unchanged.

## 8. Ceilings

Unit definition: **1 build = one whole-project `cabal build`/`cabal test`
invocation**. Grouping, from the map: each mutant B5–B19 is
apply + build-or-test + revert + restore-verify = **one** unit; M6/A-K6 and
M12/A-K12 are named **shared** reruns, not separate builds; M10b rides B4;
drift overlays ride probes. Stop conditions: INCONCLUSIVE abort on unattributed
extras; BLOCKED on a fit gap; no automatic raise.

**Owner builds: 26 — held, and here is why I am not reducing it.** The synthetic
PASS touches none of the 26 units: B1/B2 (2) + B3 + B4 + B5–B19 (15) + B20 +
B21 + B22a/B22b (2) + SLIM S1–S3 (3). What the demonstration removed is the
**risk of spending owner builds diagnosing harness defects** — three campaigns
did exactly that — which is a risk reduction, not a unit reduction. Reducing
the build envelope on the strength of a different layer's green would be the
same error as inflating it because it was proposed before.

**Owner probes: 24 → 22.** Removing the 2 **dispute** probes. A dispute is now
resolvable against a demonstrated instrument's per-direction diagnostics plus
the frozen streams, and the contract already routes an unresolved dispute to
BLOCKED — so the reserve was never the resolution path. Retained: 7 REQUIRED
drift + kill-confirm ≤13 + transient ≤2. The transient reserve stays: an
environmental re-run is an operational need, and dropping it would trade a real
campaign-death risk for a cosmetic reduction.

**Auditor 25/24 — no change proposed.** The auditor's obligations (A-K×15
unconditional, A-COLD/A-TEST/A-CI, A-OMIT, A-REBIND conditional, A-RESERVE,
A-HIDEMOa/b, 8 drift probes) are untouched by a synthetic result. Trimming them
to look responsive to the green would be unjustified.

**Both remain PROPOSALS** pending fit-proof and authorization. `#30`
implementation and audit remain **UNGRANTED**; nothing in this packet grants
anything.

## 9. Seats — recorded from NOTE-005, not selected by me

Ticket owner `claude-opus-5[1m]` / high; commit owner **Muse** (`muse
--approve`); independent auditor **Codex** (`gpt-6-astra`, effort high, explicit
in live argv), never Muse/GLM/Claude, with Grok only inside the one-family-seat
cap. Recorded for the packet's dependency rows only.

## 10. What this packet is not

It is not a grant, not an execution, and not evidence about the real compiler or
the whole substrate. The synthetic PASS is a demonstration of the harness's own
plumbing under a stub. Whole-substrate and real-compiler behaviour remain
**unaccepted**, and the first observation that changes that is S30-0.
