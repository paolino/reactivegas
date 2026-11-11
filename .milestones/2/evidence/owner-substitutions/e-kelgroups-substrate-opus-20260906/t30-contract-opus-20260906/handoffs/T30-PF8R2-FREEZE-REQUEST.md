# T30-PF8R2-FREEZE-REQUEST — recovery slice, step 1: written, unexecuted, filed for freeze

Author: ticket preparation owner `t30-contract-opus-20260906`, pane `%572`.
Authority: NOTE-003 (new correction slice, exceptional fifth invocation,
aggregate ceiling 5, no retry reserve). Step 1 only. **Nothing executed.**

## 1. Artifacts to freeze

| artifact | path | sha256 |
|---|---|---|
| runner | `scratch/pf8r2/run.sh` | `86533877935f6ea21f9e822f106c421a00699233b654848c6e8fb97c46a587cf` |
| gate leg | `scratch/pf8r2/T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` |

**The leg hash is the pf8r leg hash, unchanged.** `cmp` against
`scratch/pf8r/T30-DRIFT-LEG-r9.sh` returns byte-identical. No semantic check
was altered to obtain a green, and that is checkable by hash rather than
asserted. `bash -n` clean on both. `scratch/pf8r2/` holds exactly these two
files and zero case directories.

Preservation re-verified after writing this tree: predecessor
`scratch/pf8` + `handoffs` identical over 3407 files; my own `scratch/pf8r`
invocation-2 tree identical over 3832 files. `pf7` and `pf1` untouched.

## 2. The complete runner delta (four hunks; a `diff -u` against `scratch/pf8r/run.sh` reproduces it)

1. **Header identity** — documentation only: names pf8r2, its seed hash, and
   the unchanged leg hash.
2. ** path rebinding** — entailed by NOTE-003 §2, which requires
   `scratch/pf8r2/T30-DRIFT-LEG-r9.sh` to exist and be byte-identical; the
   runner must therefore drive this tree's own copy. Not a semantic delta.
3. **FIX-7** — two hunks: the reference stamp at suite start, and the
   action + assertion inside `case_env`.
4. **FIX-8** — the executed hard stop after A1.

Nothing else changed: not one case, not one expected outcome, not one
prediction, not one verdict attribution.

## 3. FIX-7 — the relationship established and asserted where the leg reads it

Action, in `case_env` (per case, **after** the copy, before the leg runs):

```sh
if [ -f "$d/frozen/BUILD_MARKER" ]; then
  touch -d "2000-01-01T00:00:00" "$d/frozen/BUILD_MARKER" || return 1
  if [ "$d/frozen/BUILD_MARKER" -nt "$FX/marker-ref" ] || [ "$FX/marker-ref" -nt "$d/frozen/BUILD_MARKER" ]; then
    printf '%s\n' "SETUP-FAILED: FIX-7 invariant: copied-case BUILD_MARKER mtime != fixed reference $FX/marker-ref" >&2
    return 1
  fi
fi
```

Three mechanism points, each deliberate:

- **The boundary.** `case_env` is the last thing every setup branch runs, so
  the marker is re-stamped on the **copied case tree** — the tree the leg
  consumes — not on the template whose mtimes `cp -r` discards.
- **The assertion subject.** It asserts **only that the re-stamp took effect**,
  by comparing against `$FX/marker-ref`, a file stamped by the same
  `touch -d` command at suite start. It never asserts the marker-vs-`.hi`
  relation, so it cannot fail on a case that deliberately stales the interface.
  The reference is used instead of a hard-coded epoch so the check is immune to
  how the `-d` string is interpreted: both stamps come from one command.
  If the reference were missing, `-nt` makes the assertion fire — fail-closed.
- **The guard, and the three controls it must not eat** (NOTE-003 §2):
  - **A23a** removes `BUILD_MARKER` *before* `case_env` → `[ -f ]` is false →
    no re-stamp, no assertion → the leg still refuses `BUILD_MARKER absent`.
  - **A10** stales the **interface** (`.hi` at 1999) before `case_env`; the
    marker is untouched by that injection → re-stamp to 2000 → `.hi` is older
    → `3-fresh` still fires.
  - **A27** stales the interface *after* `case_env`; the marker is already at
    2000 → `.hi` at 1999 is older → `3-fresh` then `4-provenance` still fire.
  No case injects a stale *marker*, so no negative control is SETUP-FAILED
  before the leg sees it.

The stamp reference lives at `$FX/marker-ref`, outside `$FX/tmpl`, so
`setup_tree`'s `rm -rf "$d"` never touches it and no case copies it.

FIX-2's template-level assertion is left in place, untouched. It is **not** the
FIX-7 guard and does not become one: it remains a true statement about the
template and proves nothing about any case. Stated so it is not mistaken for
coverage.

## 4. FIX-8 — baseline first, executed hard stop

A1 already runs first. After its `run_case` + `assert_absent`, the suite now
evaluates the baseline from A1's actual `exit` and `FINAL: PASS`, and if it is
not GREEN it prints `SUITE-ABORT`, dumps A1's real `stdout`/`stderr`/
`setup.log`/`exit`, writes `ev/SUITE.log` with `aborted-at=A1`, and exits 1 —
**before A2 is set up**. No speculative rows behind a broken baseline. The
end-of-suite `BASELINE:` line is retained but is no longer what carries the
rule.

## 5. Exact command and CWD

CWD `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/`:

```sh
bash scratch/pf8r2/run.sh > scratch/pf8r2/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r2/RUN.log
```

No `TAXONOMY_V2=0`: v2 remains bound to this synthetic experiment, as accepted.

## 6. Synthetic path and stub confinement

- `ROOT` derives from the script's own dirname; `FX`/`EV`/`CASES` are all
  under it; every `rm -rf` target is inside `scratch/pf8r2/`.
- `case_env` binds `REACTIVE_GAS` and `KELGROUPS_WORKTREE` to the synthetic
  fixture directories. **Never** `/code/kelgroups`, **never**
  `/code/reactivegas`.
- `PATH` is prefixed with the fixture `stubbin`, whose `ghc` serves fixture
  bytes for `--show-iface` and exits 99 otherwise.
- No network, no credentials, no `gh`, no `git push/clone/fetch`, no `nix`,
  no `cabal`, no `just`, no real compiler. **Zero product builds.**
- `RUN.log` sits at the tree root, which the runner never wipes.

## 7. Expected outcomes — predictions, not results

**Labelled as predictions throughout. Nothing has been executed.**

- **Baseline behaviour:** A1 runs first. Predicted **GREEN** (exit 0,
  `FINAL: PASS (traversed=4 frozen=4 leg4pass=3)`), so the hard stop does not
  trigger and the remaining 30 cases run. If A1 is not GREEN the run stops at
  A1 by construction and the report is that diagnostic.
- **Per-case column: unchanged.** The 31-row v2 column in
  `handoffs/T30-PF8R-REPAIR-PACKET.md` §4 (sha256 `299a201e2b2b372978aa87cd2d11fb8cd3860c92db05b8ae27af7efcd21ae74b`)
  stands as-is and is not restated here. No row's expected exit, required
  substring or forbidden pattern was touched by this slice.
- **Suite:** `BASELINE: GREEN`, `setup-failures=0`, `mispredicts=0`,
  `===== SUITE: PASS =====`, `runner-exit=0`.

Why the prediction changes from invocation 2 while nothing semantic changed:
invocation 2's eleven mismatches were attributed, without residue, to the
single marker/interface inversion on the copied tree
(`handoffs/T30-PF8R-INV2-RESULT.md` sha256 `be6904099c264694b1e890513091b237dd86951b44f6e95ad8e13df0744eb8cb`,
§4). FIX-7 addresses exactly that inversion at exactly the layer where the leg
reads it. That is the basis for the prediction; it is not evidence for it.

## 8. Layer statement, carried from NOTE-003 §1

The real-compiler prerequisites and this fixture harness are **distinct layers
with distinct costs**. Compilation establishes the real interface boundary; it
does not repair or establish this harness, and no part of this harness work is
"free" inside any future owner budget. P1 (B3) and P2 (B22a + B22b) remain
three product builds inside the ungranted owner budget, and they remain
unaffected by whatever this invocation returns.

## 9. Step 2

On the epic owner's freeze receipt in `inbox/`: execute once, exactly as
frozen; preserve complete raw streams, exits and script/fixture identities
before any other action; journal the actual counter; then file one compact
executed disposition. No third step, no retry. A surprise is a result.
