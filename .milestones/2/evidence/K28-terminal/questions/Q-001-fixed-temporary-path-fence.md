# Q-001 — frozen gate writes outside the execution-only fence

To: commissioning ticket owner t28-app-api. Seat commit-auditor-s28b, pane %557.
State: pre-execution BLOCKED; no terminal audit verdict, no candidate findings.

The replacement brief was read in full and acknowledged for POINTER-1788642400-relaunch. Its entire-history precedence exception is ACCEPTED: three owner funding decisions, owner 34/34 retained, auditor 12 substantive / 24 targeted untouched. This question does not reopen CB-001 or request another raise.

## Concrete conflict

The brief's permitted-writes section says: "Inside the execution worktree ONLY: gate-generated temporary M1–M6 mutant file states + their `checkout --` restorations" and "A write outside this list is a SCOPE-FAIL: stop, journal it, BLOCK."

The exact frozen gate (full SHA256 7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a; normalized f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650) contains:

- gate.sh:225 — M3 redirects into `/tmp/g28m3.hs`, then moves that file into lib/KelGroups/Fold.hs.
- gate.sh:235 — M4 redirects into `/tmp/g28m4.hs`, then moves that file into lib/KelGroups/Event.hs.
- gate.sh:249,251 — either M5 branch redirects into `/tmp/g28m5.hs`, then moves it into lib/KelGroups/Fold.hs.
- gate.sh:76–77 — two `mktemp` stderr artifacts, whose location can follow TMPDIR; unlike these, the three absolute paths cannot be redirected with TMPDIR.

G28_EVIDENCE_DIR redirects logs only. Launching from the authorized execution root does not contain these absolute temporary source paths. I have not invoked the gate, created the named temporary files, altered its bytes, or tried a wrapper/namespace substitution.

## Decision needed

Please bind a lawful execution envelope for these exact temporary writes while retaining the frozen mutation semantics and full nine-invocation gate. Options: explicitly authorize the three named temporary source paths plus bounded temporary stderr files, with collision/ownership conditions; or supply a parent-approved containment mechanism or newly frozen gate that keeps those writes under this audit root. A new gate would require explicit version/hash authority and any necessary reconciliation; I will not silently repair it.

Recommendation: resolve the location fence once at the commissioning layer, covering all three fixed source paths and the two mktemp sites together. No increase to this audit's 12/24 allowance is requested.

## Verified preflight and limits

Live argv verifies cli=codex model=gpt-6-astra effort=high; auditor %557, ticket owner %534 and author %545 are distinct in reactivegas:8. Author family is Muse. The selection helper confirms Codex with the explicitly commissioned seat choice applied; Grok remains allowed for other lane seats, not a newly prohibited family.

Execution checkout is detached at candidate 84a2dae and clean; author HEAD is the same candidate and clean under the brief-required read-only Git checks. All reads so far concern commissioning artifacts, skills, gate mechanics and identity; no substantive candidate diff review or semantic verification has run.

Evidence: handoffs/evidence/preflight-fence.txt (hash in STATUS). Spend: 0/12 substantive, 0/24 targeted. All six semantic rows remain unexamined. Wake condition: matching parent-authored answers/A-001 or inbox ruling with a durable pointer. Parked without terminalizing this audit.
