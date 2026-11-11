# Factory skill update — application to existing lanes

User authority: fix the skills to improve factory economics; prefer concrete improvements while lanes continue producing. Staffing discussion is a recommendation, not a fleet migration ruling.

Accepted skill change: /code/llm-settings commit c5ce3014c16a07ea271932a746a31b775a466465. Local HEAD and origin/main independently read back equal. The repository post-commit hook performed the push. Existing codex/config.toml edits and the untracked browser-seat skill were untouched.

Changes:
- Cheap authorized checks precede handoff polishing; distinguish harness setup failure from semantic rejection, retain separate attempt evidence and charge the existing budget.
- Consolidate the parent's known blockers before a repair dispatch. Repeat checks for changed inputs, new evidence or a named discrepancy; do not add a second semantic audit of an unchanged accepted candidate.
- Attribute failures to implementation, contract/gate, environment/transport or supervision before choosing an escalation. Carry cumulative spend into a genuinely revised successor; changing models or roots does not reset a cap.
- Prefer Claude/Codex for new ticket ownership, with the approved exact model/effort. Existing explicit Muse assignments remain authorized. Muse remains eligible for bounded implementation. No Opus-versus-Sol cost superiority is claimed.
- The authoritative-family selector now enforces the existing Muse/GLM shared-harness alternation prohibition in both directions.
- Measure total author, owner and auditor cost including rejected attempts against accepted outcomes. Unknown telemetry remains unknown; zero accepted outcomes does not imply zero cost per acceptance.

Validation: five affected skill folders passed quick_validate; git diff --check passed. The new selector regression failed against the original implementation (Muse selected GLM; suite exit 1), then the complete tmux-orchestrator suite passed after the repair on its isolated test socket. A separate read-only reviewer identified the cap and selector contradictions, then inspected both repairs with no remaining new conflict reported. This is validation of contracts and selector behavior, not a model-performance benchmark.

Installation: Codex, Claude and Pi shared-skill paths resolve to the edited source. New loads see the change. Factory-map impact was checked: YAML node descriptions, dispatch/load topology and the existing system-design map's represented relationships did not change; no map refresh or broader currency claim is made.

Application through immediate owners only:
- Quality owner: retain S3's zero-execution fence and its frozen full static audit, S4's full scope and remaining counters, and all explicit operator grants. Do not inject this policy into a running auditor as new acceptance rows. Apply the consolidated assessment and failure-attribution rules to your next disposition. Continue existing authorized supervision.
- Kelgroups owner: your existing Muse assignment remains valid. T30's two-call synthetic campaign remains exactly as granted, with zero product/compiler execution. Complete the already-authorized mechanical preflight and experiment through your child; this note creates no new desk checkpoint. Apply the revised role-selection rule on a future dispatch, not by replacing the current child.

Record receipt in your own STATUS at actual delivery time. No build, submission, fourth repair, auditor replacement, wider implementation, merge or model switch is authorized here. Existing operator-specific mandates prevail over generic skill defaults. No new report is required beyond the ordinary next result and this acknowledgement.
