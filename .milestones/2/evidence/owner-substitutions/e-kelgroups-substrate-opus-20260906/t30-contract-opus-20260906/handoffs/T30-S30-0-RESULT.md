# T30-S30-0-RESULT — first real-compiler observations, and two findings

Ticket owner `t30-contract-opus-20260906` (`%572`). Frozen plan:
`T30-S30-0-FREEZE.md` sha256
`332bc4a198c9f9a42af9542295b2c5b672ddd7730777e583bcd3e7f0241fb0ba`, written
and journaled **before** either command ran. Probe script
`evidence/s30-0/probe.sh` sha256
`fa16bb9c085fc9097a7d174442edeef73bb0a96e2abf6e8fa9386a904b8c56c9`, identical
in both runs (same `command_sha256` on both receipts).

## 1. Operations charged

| op | receipt | exit | duration | evidence sha256 |
|---|---|---|---|---|
| **S30-0a** lane cold build + probe | `evidence/s30-0/s30-0a.log` | **0** | 24 350 ms | `8103a093b28e664ef1c40bd866f99650431b6cc18a0355bc2ac0d82c48683b21` |
| **S30-0b** independent export build + probe | `evidence/s30-0/s30-0b.log` | **0** | 24 132 ms | `e3811ad8b9b82d10f0e5564e114905b7cc53b569680977acbebb1cd163ef1ca7` |

**Owner ledger: 2 substantive whole-project operations spent of 28; 0 targeted
probes.** S30-0a doubles as the worktree baseline, declared in advance so the
baseline is not a second hidden build. Both are well inside their frozen time
bounds (5400 s / 3600 s).

Toolchain observed: GHC **9.8.4**, cabal-install 3.16.1.0, just 1.43.1, via
`nix develop .#ci` — the same invocation CI uses (`.github/workflows/ci.yml`).

## 2. Frozen expectations against actuals

| id | expectation | actual | verdict |
|---|---|---|---|
| E1 | build exits 0 | 0, both runs | **met** |
| E2 | `dist-newstyle` holds `.hi` files | present, 37 `.hi` | **met** |
| E3 | exactly one candidate for `KelGroups.Event` | **1** | **met** |
| E4 | exactly one for nested `KelGroups.Server.JSON` | **1** | **met** |
| E5 | selected `.hi` newer than the pre-build marker | newer, both modules, both runs | **met** |
| E6 | `--show-iface` exits 0, non-empty | 0; 8 692 and 22 251 bytes | **met** |
| E7 | S30-0b reproduces E3–E6 and dump hashes **equal** S30-0a's | **EQUAL** — `d18169d3…` (Event), `2a39103d…` (JSON), and identical `.hi` sizes 6036 / 13115 | **met** |

E7's comparison was falsified before being believed: two unrelated dumps
(Event vs JSON) compare **DIFFERENT**, so the equality is not a comparator that
cannot fail. The two trees are genuinely independent — different directories,
separate compilations, mtimes 08:22:02 vs 08:22:57.

**What E7 does and does not establish.** It is the two-build comparison
Correction C-2 requires, in one direction: `--show-iface` output is
**reproducible across independent build trees** for byte-identical source under
the same toolchain pin. It says nothing about stability across a rebuild with
*changed* inputs — see finding F-B.

## 3. Finding F-A (**blocking for the gate as written**): the join's exact-line rule cannot match a real dump

The r9 leg's D-4 join tests
`grep -qxF -e "$hs_expect" "$dump"` — an **exact whole-line** fixed-string
match. The synthetic fixture's dump was five bare lines (`Verdict`, `Ballot`,
…), so the rule passed there.

A real dump does not look like that. From `evidence/s30-0/s30-0a-Event.dump`:

```text
exports:
  BaseChange{MemberAdmitted MemberRemoved RolesChanged}
  BaseEvent{Approve Propose}
  BaseMutation{ChangeRolesVoted RemoveMemberVoted}
  DirectCommand{AdmitMember}
  GroupEvent{App Base}
  IntegratedEvent{IEApp IEApprove IEDirect IEPropose}
  Proposal{ChangeRoles IntroduceMember RemoveMember}
```

Export lines are **two-space indented** and carry the constructor set in
braces. Checked directly: `grep -qxF -e GroupEvent` on the real dump is
**ABSENT**, as is `Event`. **The rule can never match a type name against a
real `--show-iface` dump.** It is not a fixture defect and not a tuning
question — it is a gate-design defect that only the first real compiler contact
could expose, and it is exactly what "stubbed metadata proves plumbing only"
was reserving.

Consequence: the D-4 matching rule must be redesigned against the real dump
grammar before any candidate is judged by it. That is ticket-owner work on a
ticket-owner artifact, not the commit owner's. It does **not** relax any
obligation: the join still owes exact identity matching, not a loosened
substring test.

## 4. Finding F-B: whole-dump pinning is sensitive to far more than the module's interface

The same dump carries, before the export list:

```text
  interface hash: 026fa5a97401114beca8e491d81cdf96
  ABI hash: a3294c080ea280ab7428add6cb370582
  export-list hash: a6f750ff52fae96972d13813955da23c
  orphan hash: 693e9af84d3dfcc71e640e005bdc5e2e
  flag hash: 02cc6b225a4b501881ce9bf3a82e8a2b
  opt_hash: a03ee3c0445ea447025b482e15ed9c52
  src_hash: e80c12624d74c5b423e5773400ebae4c
```

Two consequences, stated rather than engineered around:

- A whole-dump pin moves on flag changes, optimization-level changes and
  dependency-version changes, none of which are the module's interface.
- **`src_hash` is present in the dump.** If it is a hash of the module source,
  then *any* source edit — **including an unexported-only one** — moves the
  dump. That is a concrete mechanism for the correction carried in the mandate:
  an unexported source edit is **not** guaranteed to leave the dump unchanged,
  so the source-hash and metadata channels are **not independent by
  construction** under whole-dump pinning.

**This is returned as a design finding, not fabricated away.** The mandate
forbids normalizing load-bearing data out of the dump to make an expected
signature appear, and no such normalization has been applied. Whether
`src_hash` in fact moves under an unexported-only edit is a **can-fail
observation owed on the actual candidate**, per the temporal-freeze rule; it is
not asserted here from the field's name.

## 5. Finding F-C: the exactly-one result is conditional on one optimization level

Every candidate resolved under
`dist-newstyle/build/x86_64-linux/ghc-9.8.4/kelgroups-0.1.0.0/**noopt**/build/…`
— the `noopt` segment is there because `just build` pins `-O0`. A tree that has
also been built at another optimization level would hold a second `.hi` for the
same module under a sibling path, and the selector's `-path "*<rel>*"` would
then return **2** and refuse.

So E3/E4 are true **and conditional**: exactly-one holds for a tree built at a
single optimization level. The gate must pin `-O0` (which `just build` does) or
component-qualify the path. Recorded so a later `-O2` run cannot silently turn
the selector ambiguous.

## 6. Scope of these observations

Toolchain-level only, on the **existing** module set. **No Vote coverage is
claimed.** The frozen Vote extent was deliberately not run against this base:
its `1-fileset-hs` RED would have been a foregone conclusion, not evidence.
Reusing toolchain knowledge across module sets is legitimate; uniqueness,
mapping and verdict evidence do not transfer — the Vote extent's own `.hi`
uniqueness is answerable only by its own build.

## 7. Exact next action

Redesign the D-4 matching rule against the real dump grammar (F-A), fold F-B
and F-C into the gate contract, freeze the gate, then dispatch the Muse commit
owner for S30-1. No desk approval is needed to continue and none is sought.
