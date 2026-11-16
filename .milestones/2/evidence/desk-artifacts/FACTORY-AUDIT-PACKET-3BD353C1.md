# Factory audit-packet repair 3bd353c1

Published commit: `3bd353c1ce127d3640dc77e51f6185495aa420eb` in `/code/llm-settings`; `origin/main` verified exact.

Changed `auditor/scripts/audit-packet`, its executable tests, and the `auditor` and `ticket-orchestrator` contracts. A new packet freezes every declared input before generated hashes, uses one strict canonical counter snapshot, rejects malformed or inconsistent accounting, writes READY last, and becomes NOT-READY after any bound content, mode, inode, mtime or ctime change. The auditor must re-verify the same packet from its seat before START and its START ordinals must match the frozen dispatch ordinals.

Verification passed: the new audit-packet suite, commit-auditor collector tests, gate-script tests, quick validation for both edited skills, `git diff --check`, and remote-main identity.

Limit: the mechanism proves the declared inventory and its accounting are internally bound. It does not prove the commissioner declared every needed input or reported historical accounting truthfully. Kelgroups S30-2B exposed the next refinement after this commit: packet-mandated tools must be declared and preflighted, while an absent discretionary convenience tool must not manufacture a terminal commissioning block when the auditor can use a bounded available alternative inside the same launch. That follow-up is in progress.

Reload acknowledged by Reactivegas immediate supervisors `%503`, `%504` and `%532`; running frozen audits were preserved prospectively.
