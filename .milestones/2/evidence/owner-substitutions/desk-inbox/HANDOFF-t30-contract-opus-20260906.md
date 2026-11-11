# POINTER — t30-contract-opus-20260906 — #30 COMMISSIONED: first compiler result in

From: ticket owner `t30-contract-opus-20260906`, pane `%572`.
To: epic owner `%532`. Date: 2026-09-06. Local files only.

Root: `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/`

The three things the mandate asks for in this handback:

**1. Frozen mandate/gate hashes**

| artifact | sha256 |
|---|---|
| `handoffs/T30-CONTRACT-r9.md` (TAXONOMY-v2 versioning) | `7c8ec26c767300a8a69a5662e8b7e0070ca64938d801bec3e7d7028108707624` |
| `handoffs/T30-COMMAND-MAP-r9.md` | `d8bbf63709d990bc130ec681e54f9ba2439ce246f1aa15a7bdb3569e94654ed8` |
| `handoffs/T30-S30-0-FREEZE.md` (frozen **before** execution) | `332bc4a198c9f9a42af9542295b2c5b672ddd7730777e583bcd3e7f0241fb0ba` |
| `evidence/s30-0/probe.sh` | `fa16bb9c085fc9097a7d174442edeef73bb0a96e2abf6e8fa9386a904b8c56c9` |
| `handoffs/T30-S30-0-RESULT.md` | `5917702ff6cb300111368337c11566a3b8975ac146aaddf4bdcc5c08dc4fcd28` |

The versioning is a **hash-bound delta**, not a rewrite: re-typing 34 KB of
contract prose to change one block is the likeliest way to widen semantics by
accident. A10's corrected promised exit and A27 as the newer control are named;
v1 is labelled **source-derived, not executed**. Error-identity preservation is
**verified at source**, not asserted: `OVERALL_FAIL` is never assigned 0 after
init, so a refusal cannot lower it, and `FINAL: PASS` is unreachable from any
refusal path.

**2. Commit-owner identity and `START` — not yet.** No child has been
dispatched, because of finding F-A below. Nothing is running.

**3. The actual first compiler result** — `S30-0a` and `S30-0b`, both exit 0,
GHC 9.8.4 via `nix develop .#ci`. **All seven frozen expectations met**,
including E4 (the nested `KelGroups.Server.JSON` selector returns **exactly
one**) and E7 (two independent build trees produce **identical** dump hashes —
the two-build comparison C-2 asked for, with the comparator falsified first).
**Owner ledger: 2 of 28 substantive, 0 of 22 targeted.**

## Three findings, one of which blocks the gate

**F-A, blocking.** The r9 leg's D-4 join uses `grep -qxF` — exact whole line —
against the emitted dump. Real export lines are two-space indented with the
constructor set in braces (`GroupEvent{App Base}`), so `grep -qxF -e
GroupEvent` on the real dump is verifiably **ABSENT**. The rule **can never
match** a real `--show-iface` dump. Not a fixture defect and not tuning: a
gate-design defect that only first real compiler contact could expose — which
is what the compiler-first ordering was for. I redesign the matching rule
before any candidate is judged by it, and it relaxes no obligation.

**F-B.** The dump carries interface/ABI/export-list/orphan/flag/opt/`src_hash`
before the export list, so a whole-dump pin moves on things that are not the
module's interface — and `src_hash`'s presence is a **concrete mechanism** for
your correction that an unexported edit is not guaranteed to leave the dump
unchanged. Returned as a design finding with **no normalization applied**;
whether `src_hash` actually moves under an unexported-only edit is a can-fail
observation **owed on the actual candidate**, not asserted from a field name.

**F-C.** Every candidate resolved under a `noopt` path segment because
`just build` pins `-O0`. Exactly-one is true **and conditional** on one
optimization level; an `-O2` run would make the selector ambiguous. The gate
pins `-O0` or component-qualifies the path.

## State

Lane `/code/kelgroups-issue-30`, branch `feat/30-vote-substrate`, created fresh
from `933e385d` — `/code/kelgroups-issue-28` and every terminal root untouched.
One orchestrator-owned setup commit `7c67c81` adding only the `/gate.sh` ignore
rule. Porcelain clean, no children, nothing running.

Next, without seeking approval: redesign the D-4 matching rule against the real
dump grammar, fold F-B/F-C into the gate, freeze gate and instruments, then
dispatch the one Muse commit owner for S30-1.
