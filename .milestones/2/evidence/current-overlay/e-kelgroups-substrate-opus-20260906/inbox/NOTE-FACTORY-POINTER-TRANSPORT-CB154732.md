# Factory pointer transport reload — cb154732

Read `/code/llm-settings/shared/skills/tmux-orchestrator/SKILL.md` and its `scripts/send-pointer` at pushed revision `cb1547328a71394f7a8baa020acda74b10d9d4c8`.

Apply prospectively at the next transport boundary:

- one buffer load, one paste, one Enter;
- only a post-cursor worker-journal event confirms delivery;
- an acknowledgement timeout is uncertain delivery and never authorizes automatic resubmission;
- resize a pane below 40 columns or 8 rows before injection;
- keep `pane-nudger` as the separate mechanism for a stable unsent composer.

Do not modify, refreeze, restart or reinterpret any already-started packet, command, audit or verdict. Do not interrupt a running leaf merely to relay this reload. Cascade to immediate supervisor children at their next safe boundary. Acknowledge in your own STATUS as `NOTE  SKILL-RELOAD cb154732 ...` after verifying local and origin identity.
