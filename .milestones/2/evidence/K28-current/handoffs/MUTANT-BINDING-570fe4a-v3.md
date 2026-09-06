# MUTANT-BINDING-570fe4a-v3 — Q-004 class reconciliation (addendum record)

Ticket owner `t28-app-api`. Lineage: v1 (RED bindings: hashes, anchors,
extraction 19/19, dry-runs, M1-applicability, TBB production) + v2 (header
correction refs) preserved unmodified; this v3 reconciles EVERY remaining
frozen precondition against fourmolu output + required declarations
(NOTE-009 deliverable, one pass). Charge-0 source/instrument prep only
(synthetic fixtures in /tmp, greps, hashes); no builds; worktree untouched
(clean asserted pre/post every run).

## Reconciliation table (anchor × frozen v5 check × layout class × verdict)

- H1 `if isMemberInView signer view` (M2): substring check, ~46ch fits 70 →
  STABLE single-line; count==1 fail-closed; tail-preserving perl covers
  both guard layouts. No change.
- H2 `Left _ -> gs` (M5): short arms; `foldIntegratedFrom`-prefixed second
  block textually in awk range but contained (triple lock). PROOF (synthetic
  two-block fixture): line-4 arm → `Left _ -> error "MUTANT-M5"`, line-8 arm
  intact, count==1. No change.
- H4' `ChangeRolesVoted` arm (M4): data-decl one-per-line; collision scan
  clean; freshness runtime-checked. No change.
- H5 success-write literal (M6): ~60ch mandated single-line; `$`-shape
  distinct from historical. No change.
- M1 flattened + prefix + boundary mentions + freshness: PROVEN on RED bytes
  (D3 record: actual 144–145 split matched). No change.
- `^`-prefix anchors (M2/M4/M5/M6 fn names, M4 `^data`, demo `^data`,
  `^demoIntegration`): line-start, layout-free by construction. No change.
- closeKEL/writeTVar anchors: short lines, RED-verified. No change.
- Two-layout it/prop rule: 19/19 proven on RED bytes, zero warnings. No change.
- M3 signature conjunct: THE conflict (full-line single-line check vs
  70-col splits). FIXED in v5 (prefix + flattened `commitBaseChange +::
  +Integration` + equation check). PROOF (synthetic split-signature
  fixture mirroring the RED demo split shape): preconditions `m3_pre=1`;
  awk passes name + `::` lines byte-identical, stubs equation, marker
  count==1, next top-level intact.
- Exhaustion scan (gate-wide, quoted): `grep` for remaining
  `grep -q '^<name> ::'` single-line signature checks → NONE (M1, M3 both
  flattened). Class empty.

## TBB restated

Production anchors (H1–H5 spellings in GREEN code) bind at GREEN submission
via ANCHOR-ATTEST + leg-5 preconditions (fail closed). No production exists
at RED; nothing here asserts unverified spellings as fact.

## Freeze refs

Gate v5 normalized + full hashes + `gate-v5.sh.backup`: STATUS NOTE
GATE-FROZEN-v5. FROZEN_BASE RED `570fe4a…` unchanged.
