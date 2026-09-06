# S2R Submission 2 — comment-only advisory correction (ONE additional submission)

**Candidate (frozen):** `ab617d88af9d080de71218f3cc553d60ef0b6de0`
(`chore/66-s2r-ownership`, parent `714cb2a`, committed-clean, never pushed.)
**Scope, exclusively:** one comment correction in `scripts/check-lean-axioms`
for the audit's documentation advisory. No driver, shell-behaviour, policy,
test, fixture, Lean-code, output, dependency, or `docs/en/design/` change.
`getD "."` NOT "fixed" — the behaviour stands, the comment now describes it.

## 1. Exact new comment-only diff against `714cb2a`

One file changed, `+14/−5`, every changed line a `#` comment (mechanically
proven: `git diff` changed-lines filtered to non-`#` is empty; `bash -n` clean):

- Old claim: ``Fail-closed: missing/unresolvable `REACTIVEGAS_ROOT`''.
- Actual behaviour, now stated: a MISSING variable does NOT fail closed — the
  driver defaults it to `"."` (line 286, intact); an UNRESOLVABLE one fails via
  `canonRoot` `.error` (established by driver inspection, explicitly NOT by an
  executed control — the comment says so); the mandatory wrapper always exports
  `REACTIVEGAS_ROOT` (line 302, intact), so the mandatory path never exercises
  the default. Nothing here outruns its evidence — that restraint IS the fix.

## 2. Every other tracked byte unchanged from `714cb2a` — blob proof

`git diff 714cb2a ab617d8 --name-only` = exactly `scripts/check-lean-axioms`
(single file above). All other tracked blobs identical by construction of the
single-file commit (worktree committed-clean, verified). Line-number note: the
audit cited 277/293; verified actuals in the frozen file are 286 (`getD`) and
302 (export) — same lines, counting offset only.

## 3. Prior full audit bound as an input (not rewritten)

- Audit report: sha256
  `d634df52c51d4351699d36927b5b0c662357a4ac08a7d689a6708db2d34def90`
  (AUDIT-FINDINGS: 24 rows closed, 1 partly, no blocking functional defect).
- Its `FINAL.sha256`:
  `64dbd5ad2300dec49c4ecc71b703ce86031b673782e363fc9f2f115df536892e`.
- Old final manifest and report retained intact in this runtime root
  (`handoffs/SUBMISSION.md` sha256 `32299d25…`, full `handoffs/evidence/`).
  A fresh independent Codex auditor reviews this corrected candidate with every
  inherited row and the comment-only equivalence proof in scope; no prior row
  is immune and nothing carried forward is an inherited PASS.

## 4. Final local `just ci` — exit 0

`nix develop --quiet -c just ci` on `ab617d8`, committed-clean tree:
**EXIT 0** (`handoffs/evidence/S18-final-ci.log`,
sha256 `5efd36bb2291fd0a57d65638136adc29b9e479ed1b98b197af914d62b81c28c4`).
Markers: toolchain agree, dep-direction OK (17), inversion-audit ok ×2,
axiom-gate ok, agreement 14/14/0, corpus exe + both JSONs OK + checks
live-bound, builds 27 + 42 jobs.

## 5. Spend (this submission only; submission-1 retained, never reset)

- Substantive 18/18 (17/17 retained + S18 final `just ci`).
- Targeted 35/37 (retained; zero probes spent — the one `just ci` sufficed).
- No push, PR, comment, merge, or outward action from this seat. PR #88 is the
  parent's under desk grant.

Next state: SUBMITTED (submission 2) → fresh independent Codex audit of
`ab617d8` → exact final remote CI → explicit desk merge grant.
