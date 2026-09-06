# M1 return — auditor instrument setup failure; FULL audit remains pre-START

**M1-S passed; M1-T failed before emitting an inventory.** This seat used Lean's
reserved token `prefix` as a local variable in `instruments/Inventory.lean:37`.
The primary diagnostic is `unexpected token 'prefix'`. This is an auditor
instrument defect, not a candidate finding or semantic kill.

Authority: `answers/A-001-M1-authorized.md`, SHA-256
`49936c89044774e42159da8cf083388f74feb78eae0087060981c95ee671328e`.
The authorized frozen manifest still hashes
`29c21062d6f2669d4ba6d2108cb9745df4ffbf7cb810bfdd459902d0e20145d2`.
No instrument was changed after that freeze, and no retry occurred.

| Operation | Layer | Exit | Actual charge | Observation |
|---|---|---:|---|---|
| M1-S | Explicit full tracked-module `lake build` | 0 | 1 substantive | `Build completed successfully (32 jobs)`; 29 source-module oleans plus lakefile config olean are hashed in the retained output manifest |
| M1-T | Single `lake env lean Inventory.lean` | 1 | 1 targeted | Parser/setup failure at lines 37 and 33; no `inventory.jsonl` exists |

Actual measurement interval: **2026-09-06 03:55:29–03:56:00 UTC**. Both streams,
exit files, input verification, fresh olean manifest and final porcelain are
retained in `evidence/M1/`. The raw receipt manifest `evidence/M1.sha256` hashes
`1e94df240de3ecf4886ccece16c3b3f11abf042ec50d42d761bcb9ed72a3422d` and verifies
14/14. The complete build and census output was read. The runner exited 1 after
the first nonzero charged operation and launched nothing further.

Fresh spend is **1/12 substantive and 1/80 targeted**, leaving **11 substantive
and 79 targeted**. Historical auditor spend 6/59 remains retained, giving actual
campaign spend **7 substantive / 60 targeted** against ceilings **18 / 139**.
Both author submissions remain spent; author spend remains 18 substantive / 52
targeted. No ceiling change, refund or pooled allowance is claimed.

The candidate remains `94bb7bb64324a48f7361252556b4d15e45b3923f`, tree
`3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced`, over base
`3590c0015b84fd58004bf6fb44dd18b107304c48`. Final porcelain is empty. Its fresh
build output is retained; it is no longer a no-`.lake` worktree. No candidate
source edit, mutation, remote action or author contact occurred.

**No compiled extent was established.** The 29 source modules remain an input
observation. M1-S is neither `just lean` nor `just ci`; its green build closes no
mandatory-path row, classification row, sensitivity row or final trust row.
The frozen M1-T was not compile-tested before launch, and its failed setup is
fully charged.

The actual uncovered obligations are named in
`handoffs/UNCOVERED-OBLIGATIONS.md`: complete compiled discovery and both axes;
statement/proof/Expr preservation; clean mandatory path; missing-counterpart,
missing-theorem and disabled-checker controls; module/predicate/classifier
controls; per-identity sensitivity; separated P01 and P07 obligations; arbitrary
state witnesses; final cold CI/axioms/totality/inventory; remaining scope and
provenance judgments. No source-named list stands in for the unmeasured extent.

**Full sheet not frozen; fit unverified; no START; no candidate verdict.** The
stop is the explicit A-001 no-retry condition, not an underfunding finding.
No second measurement is launched or requested in this return. Three historical
limitations remain OPEN exactly as commissioned: P07 isolation unestablished;
census sortUndecided path source-only; ba623667 receipt recovered-from-snapshot.

The original pre-M1 return and its matching hash receipt are preserved under
`handoffs/pre-M1/` without rewriting their original claims. Current outcome is
also recorded in `handoffs/M1-OUTCOME.json` and the campaign ledger. This local
return hands the uncovered obligations back to commissioner %503.
