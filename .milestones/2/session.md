# Reactivegas session — successor snapshot 2026-09-05T09:09:06.188Z

Inventory before recreating anything. Resume existing owners from their own
fragments; each owner reconstructs only its descendants. Desk: resume/ms.md.

| scope | pane / window | worktree and runtime | owner fragment |
|---|---|---|---|
| desk | %510 / @26 ms2-reactivegas-kelgroups | /code/reactivegas-issue-47; /tmp/reactivegas/ms2 | resume/ms.md |
| #66 | %503 / reactivegas-e-lean-compliance | /code/reactivegas-lean-compliance; e-lean-compliance | resume/e66.md |
| #67 + #74 | %504 / reactivegas-e67-t74-corpus-exporter | /code/reactivegas-haskell-impl; e-haskell-impl | resume/e67.md |
| #70 | %313 / reactivegas-ms2-t-simulator-fable | /code/reactivegas-sim-fable; t-simulator-fable | resume/t70.md |
| #68 | %512 / reactivegas-ms2-t68-proposer-assent | /code/reactivegas-issue-68; t68-proposer-assent | resume/t68.md + evidence/A-001-t68.md |
| #71 | %516 / reactivegas-ms2-t71-design-record | /code/reactivegas-issue-71; t71-design-record | resume/t71.md |

Runtime names above are relative to /tmp/reactivegas/ms2.

Verified launch for the three Opus owners:
`claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high`.
The first e67 fragment omitted effort: the verified argv here includes it.
Desk: `codex --dangerously-bypass-approvals-and-sandbox -m gpt-6-astra -c model_reasoning_effort=high`.
New Muse ticket owners: `muse --approve`; fixed wrapper executes
`pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve`.

#68 build hold is released; A-001 settles its interpretation, A-002 the release.
#71 is the sole commissioned design-record writer; other lanes hand off content.
No #69 or upstream #73 lane yet. Do not restart epic #43. Bare orch %12 is not
a session owner. Former desk %37 and temporary window @154 were removed at the
operator's request. No other paused session is released by this record.

Verify stage/HEAD/argv against the actual host and current journal before
resuming. A fragment can lag an active turn; reconcile it, do not infer authority
from the presence of a pane. Candidate code and some raw evidence remain local;
this snapshot does not claim complete host-loss recovery.
