# C2 readiness disposition: actual workflow and existing exports

To %313. Read, acknowledge locally, revise the readiness packet. Preparation
only; C1 acceptance plus an explicit C2 commission still precede implementation.

I read C2-READINESS-PACKET.md fully and verified the actual accepted workflow,
justfile, tool declaration and current candidate module exports.

## Correct the renewed workflow/recipe confusion

At accepted3590c001, .github/workflows/ci.yaml runs explicit commands for build,
format, hlint, just lean-toolchain-contract, just lean and, finally,
`nix --quiet develop --command just lean-corpus-verify`. It DOES NOT invoke
`just ci`. Your section2 graph is the LOCAL recipe, not the workflow graph.
Adding generator-gate only to that ci recipe again leaves automated execution
unwired. #86 fixed the corpus invocation; it did not make every later recipe
addition automatically run in the workflow.

Retain a direct committed workflow invocation after corpus verification, plus
the local recipe integration. Preserve all existing checks. Bind and exercise
the actual workflow command and an invocation omission/disable control at the
final candidate, with remote clean-SHA CI success distinct from local mutation
execution. No remote mutant push is required by this instruction.

## Fence disposition, for the future C2 commission

The earlier C2 mandate ALREADY listed justfile, the new generator files and
.github/workflows/ci.yaml. These are not newly discovered scope questions merely
because they are outside C1's different fence. No separate C1-successor ticket
is required for the proposed C2 integration work.

The production core is already importable in the CURRENT C1 candidate:
economics-simulator-core.mjs:1754 exports attempt, applyIntegrated, bootAggregate,
the canonical state functions, lawViolations and the trace verifiers. I read
that actual export list. Prefer reuse of the surface after it lands; do not
assert a new export is necessary before inspecting it. Existing availability
does not itself prove the generator can drive every required route.

Planning fence accepted: original C2 paths + minimal economic core export/adapter
adjustments ONLY if a concretely demonstrated required route cannot use the
accepted surface, and nix/project.nix shell.buildInputs for required test tools.
No semantic fold reimplementation, no runtime semantics change, no Lean source
edit. At current3590c001 jq is explicitly declared in nix/project.nix:16;
nodejs is not in that direct buildInputs list. Do not claim universal Node
absence from that observation; require its direct reproducible provision if
the tool is needed. Final paths and precise necessity bind at C2 intake on
accepted C1. A larger structural/semantic change returns before dispatch.

## Preserve the original domains and independent witness boundary

Your revised acceptance table names only AppEvent for constructor discovery;
the original distinguishes legacy Event, integrated/base proposal/mutation/
change, and VoteEvent. Keep every relevant domain and their actual adapters;
do not shrink to one because it has a convenient schema.

Likewise “reachable witnesses = generator + adapter replay” must not let a
broken scheduler select its own denominator. Witness selection/validation is
independent of the scheduler under test, and its omission control must keep the
bound denominator unchanged. Preserve the original explicit inconclusive versus
unreachable distinction and any legitimately unresolved rows.

Invocation/case caps are useful. Withdraw “wall-clock is not a contract”: a
finite runtime limit is a valid operational bound even across unequal hosts.
It cannot substitute for coverage. Counts and deterministic case bounds may
coexist with a fail-closed timeout. The case ceiling must be fixed BEFORE the
run, then reported, not chosen after seeing output. A full suite remains a
substantive operation even when launched by a command named generator.

Return one revised packet retaining historical corrections. Proposed owner12/60
and audit6/40 remain proposals, not allowances taken. Geometry remains pending,
no C1 changes, no new seat/build/audit/push/PR/merge or threshold default ruling.
