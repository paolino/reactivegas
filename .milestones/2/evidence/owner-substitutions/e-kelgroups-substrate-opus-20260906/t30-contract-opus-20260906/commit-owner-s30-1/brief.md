# Brief — commit owner `commit-owner-s30-1` (kelgroups #30, slice S30-1)

Role: **commit owner**. You are the accountable implementer for this slice.
You write the RED proof, the production code, the repair, and the local commit.
Load: `commit-owner`, `worker-protocol`, `tdd`, `verification`, `invariants`,
`haskell`, `gate-script`. You are not alone in the codebase; do not revert
edits made by others.

| field | value |
|---|---|
| worker ID | `commit-owner-s30-1` |
| runtime root | `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/commit-owner-s30-1/` |
| ticket owner | Opus `claude-opus-5[1m]`/high, pane `%572`, window `reactivegas:8` |
| your seat | Muse — `muse --approve` (Pi / opencode-go / `muse-spark-1.3-contributor` / xhigh) |
| worktree | `/code/kelgroups-issue-30` (branch `feat/30-vote-substrate`) |
| pre-slice base | `7c67c81` (setup commit) on top of accepted `933e385df2f2a251bb54a08bb7663f0d41fafb64` |
| `draft` | **NONE** — you dispatch no children |

## Observable outcome

`lib/KelGroups/Vote/Types.hs` and `lib/KelGroups/Vote/State.hs` exist, are
listed in `kelgroups.cabal` `exposed-modules`, compile under the project's
`-Wall -Werror`, and export exactly the mandated identities in §3.

**Declarations only.** No transitions, no verdict logic, no fold wiring, no
tests of behaviour. Those are later slices and are **out of scope here**.

## The gate

`/code/kelgroups-issue-30/gate.sh`, sha256
`5316e9c846a9fcdd3fabc4d54eaa552a197b580ffb16713316c66fd8a3d2976a`.
Frozen before this brief. **You may not edit it, or the oracle it reads
(`instruments/s30-1-identities.expected`, sha256
`50c4c05fd22e8264e2e17333ff072b87d38aec4ddd0df2b49d0ac8dddd3c3935`), or any
instrument under the ticket root.** If you believe the gate is wrong, file
`GATE-CHALLENGE` in your STATUS and stop; RED remains authoritative until the
ticket owner versions it.

The gate has already been falsified per class on real bytes: its identity rows
fail when a mandated identity is missing, and fail when an excluded one is
exported.

## RED first — the mechanism, not the intent

`tdd` binds. Your RED must **execute the subject and fail because the behaviour
is absent**. The mechanism for this slice:

1. Create both modules as **empty-export stubs** (`module KelGroups.Vote.Types () where`)
   and add them to `exposed-modules`.
2. Run the gate **once**. Required RED: `G-5 ... missing mandated identity
   [...]` naming the absent identities, and `GATE: FAIL`.
   That is a real RED — the check reads the **real `--show-iface` dump of your
   real new modules** and fails because the identities are not there.
3. Commit the RED bundle locally.
4. Implement the declarations. Run the gate **once**. Required GREEN.
5. Commit the candidate locally.

A gate run that is green *before* the implementation is not a RED, whatever it
is called.

## 3. Mandated identities — type heads and constructors, exactly

Ground: `T30-IDENTITY-MAP-r5.md` rows 42–49 over the frozen Lean extent at
reactivegas `3590c001`. **Record-selector spelling is yours** — it is not
mandated and the gate does not check it.

`KelGroups.Vote.Types` **MUST** export:
`QuestionId`, `Threshold`, `Verdict`{`Positive` `Negative` `Open`},
`Ballot`{`Assent` `Dissent`}, `QuestionKind`{`Collective` `Permission`},
`ClosureCause`{`Tally` `FranchiseChange` `ProposerDeparted` `Renounced`}.

`KelGroups.Vote.Types` **MUST NOT** export `legacyThreshold` or
`zeroThreshold`. They are **exhibits, never defaults** — this is a fenced
product rule (R30-X), not a style preference, and the gate fails if either
appears.

`KelGroups.Vote.State` **MUST** export `Question`, `ClosureRecord`,
`VoteState`.

Truths that must hold, for you to satisfy how you see fit:

- `Verdict` has **exactly three** constructors and `Open` is a distinct
  constructor — never `negative` plus a flag, never an `Option`/`Maybe` of a
  two-valued type.
- `QuestionKind`'s permission arm **carries its designee**, so a permission
  question without a designee is **not representable**.
- `Threshold` is a **parameter type**. Nothing anywhere hard-codes a policy,
  and no default is shipped.
- `ClosureCause` carries all four causes **as data**, never as producers.
- The designee type is the shared substrate's key notion; reuse
  `KelGroups.Types`, do not introduce a parallel one.

## 4. Budget — you share the ticket's cumulative ceiling

Owner ledger before you: **2 of 28 substantive whole-project operations, 0 of
22 targeted probes.** Every whole-project build/test/CI invocation counts, and
**one `./gate.sh` run is one substantive operation** because it invokes
`just ci`.

**Your cap for S30-1 is 3 substantive operations** (RED gate, GREEN gate, one
spare for a repair). Do not iterate the gate to converge — compile locally with
narrow commands if you must, and note that a "narrow" command that compiles
beyond its declared scope **is** a whole-project operation and is counted as
one. No parallel heavy builds.

**If the slice cannot fit in 3, return the exact additional operation and its
scope BEFORE spending it.** Do not overrun and reconcile afterwards.

## 5. Forbidden

No edits outside `lib/KelGroups/Vote/*.hs` and `kelgroups.cabal`. No edits to
`gate.sh`, the ticket instruments, `lean/**` (either repo), `Trivial.hs`, the
client, or any S28 production path. No push, no PR, no merge, no issue
comments, no release. No children. No Lean edits. No threshold default, no
expiry, no dormant refusal producers, no `#81`/`#76` content by anticipation.

## 6. Reporting

Journal every event with
`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event <your-root>/STATUS.md <TAG> "<msg>"`.
Never hand-write a timestamp.

```text
START mode=COMMIT-OWNER pane=<%id> cli=muse harness=pi provider=opencode-go model=muse-spark-1.3-contributor effort=xhigh parent_cli=claude alternate=true
RED-COMMIT <sha> gate=FAIL evidence=<path>
COMMIT <sha> <subject>
PROOF-COMPLETE submission=1 base=<sha> red=<sha> candidate=<sha> receipt=<path> ops_spent=<n>
```

Every stop is terminal: `PROOF-COMPLETE`, `BLOCKED  Q-NNN-<slug>` with a
question file, `GATE-CHALLENGE`, or `COMPLETE` for a capacity stop with a
handoff path. **A park whose wake only you can satisfy is not a valid stop.**
Blocked → `questions/Q-NNN-<slug>.md`, then park for my answer in `answers/`.
Check `inbox/` before each phase and before any terminal event.

You have **no push authority** and you do not create the final squash until I
send `IMPLEMENTATION-ACCEPTED` after an independent audit.
