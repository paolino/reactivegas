# T30-S30-0-FREEZE — prerequisite observations frozen BEFORE execution

Ticket owner `t30-contract-opus-20260906` (`%572`). Authority:
`T30-COMMISSION-MANDATE-v4.md` §4. **Written before any command is run.**
Its sha256 is journaled before execution so the ordering is checkable.

## 1. Subject and fences

Base: kelgroups `933e385df2f2a251bb54a08bb7663f0d41fafb64`, accepted.
Lane: `/code/kelgroups-issue-30`, branch `feat/30-vote-substrate`, created
fresh from that exact SHA. `/code/kelgroups-issue-28` and every terminal `#28`
or audit root is **not reused**.

**No Vote coverage is claimed by anything below.** These are toolchain
observations on the **existing** module set. The frozen Vote extent is
deliberately **not** run against this base: `1-fileset-hs` would RED in D-1
because the extent is absent, which is a foregone conclusion and not evidence.

## 2. Subjects, chosen for a reason

| module | file | why this one |
|---|---|---|
| `KelGroups.Event` | `lib/KelGroups/Event.hs` | already a named frozen-module row in the r8 command map (line 80) |
| `KelGroups.Server.JSON` | `lib/KelGroups/Server/JSON.hs` | the repository's **only** nested module, so its `.hi` path shape is the structural analogue of `KelGroups/Vote/Types.hi` — the right subject for the selector question |

Verified at the base: `git ls-files lib` returns exactly eleven modules and no
path anywhere matches "vote".

## 3. Toolchain, bound

`flake.nix` exposes `devShells = project.devShells`; `nix/project.nix` defines
`devShells.default = project.shell` and **`devShells.ci = ciShell`**, so
`nix develop .#ci` is the shell the command map names. `cabal.project` pins
`index-state: 2026-02-01T00:00:00Z`, packages `.`. `justfile` defines
`build: cabal build all -O0`. `dist-newstyle/` is gitignored at this base.

Input hashes bound before execution:

| input | sha256 |
|---|---|
| `flake.nix` | (recorded in the STATUS event that freezes this file) |
| `flake.lock` | (same) |
| `cabal.project` | (same) |
| `kelgroups.cabal` | (same) |
| `justfile` | (same) |
| `lib/KelGroups/Event.hs` | (same) |
| `lib/KelGroups/Server/JSON.hs` | (same) |

## 4. S30-0a — cold build + selection/emission/freshness, in the lane

**Charged: 1 substantive whole-project operation.** It is also the worktree
baseline; folding them is deliberate so the baseline is not a second hidden
build. Everything after the build in this operation reads artifacts and
compiles nothing.

Commands, in one `nix develop .#ci` invocation, in that order:

```text
1. touch  <evidence>/marker            (pre-build marker)
2. just build                          (= cabal build all -O0)
3. record exit + a build id            (receipt)
4. for M in KelGroups.Event KelGroups.Server.JSON:
     find dist-newstyle -name '<Leaf>.hi' -path '*<rel>*'   -> ALL candidates + count
     [ candidate -nt marker ]                                -> freshness
     ghc --show-iface <candidate> > <dump>                   -> emission
     sha256sum <dump>                                        -> pin
```

Time bounds, binding: the whole operation **5400 s** (first `nix develop .#ci`
may fetch or build the toolchain); each post-build read step **120 s**.
Exceeding a bound is **charged and returned**, not retried.

## 5. S30-0b — independent cold export build, outside every worktree

**Charged: 1 substantive whole-project operation.**

`git archive 933e385d` into `/tmp/.../s30-0b-export` (outside every worktree,
no `.git`), then the same build and the same four read steps.

Time bound **3600 s**; read steps **120 s** each.

This is simultaneously the contract §12(viii) export pre-check *and* the
**two-build comparison** that Correction C-2 says a stability claim requires:
byte-identical source, same toolchain pin, an independent build tree.

## 6. Expected observations — predictions, frozen, with their alternatives

Each row states what would count as the alternative outcome, because the point
is to observe, not to confirm.

| id | expectation | plausible alternative, and what it would mean |
|---|---|---|
| **E1** | `just build` exits 0 | non-zero → charged and returned; no Vote claim; not retried |
| **E2** | `dist-newstyle` exists and holds `.hi` files | none → the emission design is wrong about where artifacts land |
| **E3** | the leg's selector returns **exactly one** candidate for `KelGroups.Event` | **>1 is genuinely plausible** — `cabal build all` builds library, executable and test-suite, and haskell.nix trees can carry `noopt/` variants. **>1 is an ambiguous selector: charged and returned**, and it would mean the leg's `exactly-one-or-REFUSE` rule needs a component-qualified path before it can be used for `#30` |
| **E4** | same, for the nested `KelGroups.Server.JSON` | as E3; this is the row that matters most, being the structural analogue of `Vote/Types` |
| **E5** | each selected `.hi` is **newer** than the pre-build marker | older → the marker/build ordering assumption fails against a real build |
| **E6** | `ghc --show-iface` exits 0 and emits non-empty bytes | non-zero/empty → the emission step is not viable as designed |
| **E7** | S30-0b reproduces E3–E6, and its dump hashes **equal** S30-0a's | **unequal is a real possible result** and is the finding, not a failure to hide: it would mean `--show-iface` output is not stable across independent build trees and cannot be hash-pinned as the contract assumes |

**E7 is the observation the single-build claim could not make.** One build
cannot establish stability; this pair can, in one direction — two independent
trees of identical source. It still says nothing about stability across a
*rebuild with changed inputs*, and that limit is stated rather than elided.

## 7. Receipt capture

Every command runs under `gate-script`'s `run-receipt`, which writes complete
output to durable evidence under `evidence/s30-0/` and returns only exit,
duration, command hash, evidence hash, bytes and path. Raw logs are opened only
on a failing or disputed receipt.

## 8. What these observations may and may not support

May: toolchain-level facts — that `ghc --show-iface` is invocable in this
project's shell, its output shape, whether the selector resolves for a nested
module in this project's real `dist-newstyle`, whether marker/build ordering
holds, and whether dumps reproduce across independent trees.

May **not**: anything about Vote identities, the mapping rows, the join, the 26
REQ executions, or uniqueness of the Vote extent's own `.hi`. A newly declared
module can land in more than one component's build tree and only its own build
answers that. **Reusing toolchain knowledge across module sets is legitimate;
transferring uniqueness, mapping or verdict evidence is not.**
