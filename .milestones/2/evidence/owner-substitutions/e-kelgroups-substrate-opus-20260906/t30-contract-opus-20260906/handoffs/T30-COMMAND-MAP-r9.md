# T30-COMMAND-MAP-r9 — versioned command map adopting TAXONOMY-v2

Companion to `T30-CONTRACT-r9.md`. Same delta form: binds the r8 map by hash
(`ca033b1edcd7def8466a90909ceee941d521a948ea4f27c84ea361dc4500b15d`) and
replaces only the rows named below. Every other r8-map row stands unchanged.

## 1. TAXONOMY block — replaced

The r8 map's TAXONOMY-v1 paragraph is replaced by **TAXONOMY-v2** as written in
`T30-CONTRACT-r9.md` §3, including the precedence rule. Predictions in this map
cite v2 exit codes.

## 2. Row corrections

| row | r8 map | r9 map | basis |
|---|---|---|---|
| **A10** stale `.hi` | listed under RED(1); v1 delivered 3 | **RED(1)**, and now delivered as 1 | observed `scratch/pf8r2/cases/A10/exit` = 1 |
| **A26** | absent | **RED(1)** — `1-clean-hs` negative control, retained after the fixture `.gitignore` fix | new r9 control |
| **A27** | absent | **RED(1)** — inherited-dump provenance control (`4-provenance`) | new r9 control |
| **A28** | absent | **REFUSAL(3)** — `0-overlay-base` negative control | new r9 control |

No other row's promised outcome moves. Mechanically determined over the
invocation-5 streams: with a green baseline exactly **two** of 31 cases reach a
refusal after a rendered verdict — **A10** and **A27** — and those are the only
places v1 and v2 can differ at all.

**v1 for A10 is source-derived, not executed.** pf8r2 ran v2 only;
`TAXONOMY_V2=0` was never run, so no v1 execution exists to cite.

## 3. Gate inputs — replaced

| input | sha256 | role |
|---|---|---|
| `T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` | **the drift leg** |
| `T30-DRIFT-LEG-r8.sh` | `f0afa32b4fbb13ac6084b6c3c5abd503f7e21f051fef458265b97fd56a4de3e3` | defect witness only, never an input |
| `pf8r2/run.sh` | `86533877935f6ea21f9e822f106c421a00699233b654848c6e8fb97c46a587cf` | synthetic demonstration harness, campaign closed |

Demonstrated status per leg row is unchanged from
`T30-COMMISSIONING-PACKET.md` §2 and is **not re-asserted here**: every
"demonstrated" there means *synthetic layer, stubbed `ghc`, fixture repos* —
never the real compiler and never the `#30` extent.

## 4. Exact commands — unchanged from r8

`nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci` (including the kelgroups-own `just lean`
sub-step); probes
`nix develop .#ci --quiet -c cabal test invariants --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/`.
Reactivegas-side reads only (`rev-parse`, `git show $FROZEN_OID:<path> |
sha256sum`, `status --porcelain`, `ls-files`, `git archive`), each riding its
leg's or probe's counter. **No `lake build` on the Reactivegas side.**

Verified at source for this version: `just build` = `cabal build all -O0`;
`just ci` = format, cabal-fmt, lint, build, test, lean, build-client,
test-client; `test-suite invariants` exists with the `--match` surface the
probe commands assume.

## 5. Budgets — replaced by the granted ceilings

The r8 map's proposed owner 26/24 and auditor 25/24 are **superseded** by the
mandate's grant, cumulative from product spend 0:

- **owner 28 substantive / 22 targeted** — the 26-unit candidate envelope
  **plus** S30-0a/0b, which **add to** and never replace candidate B3/B22a;
- **author submissions max 2 total**, sharing that same ceiling, no separate
  repair pool;
- **auditor 25 substantive / 24 targeted**, cumulative across max 2 fresh
  audits.

Counting rules as in `T30-CONTRACT-r9.md` §5. Running ledger is kept in this
ticket's `STATUS.md`, one event per substantive phase, and every operation is
itemized as it is spent — nothing invisible.

## 6. Unchanged

REQ groups and the 26 IDs; the 7-file Lean extent and per-identity rule; the
drift machinery L1/L3/L4/L5 descriptions; the 7 REQUIRED probes; the mutant set
B5–B19 and its classification procedure; acceptance rows; obligation→owner→
auditor rows. All as filed in the r8 map, bound by its hash above.
