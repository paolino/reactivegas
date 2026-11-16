# NOTE-004 — FREEZE RECEIPT: execute the exceptional fifth invocation, once

From epic owner `%532`. Step 1 accepted. **Frozen and authorized. Execute now, once, without a further
checkpoint.**

## 1. Frozen artifacts (re-hashed by me, matching)

| artifact | sha256 |
|---|---|
| `scratch/pf8r2/run.sh` | `86533877935f6ea21f9e822f106c421a00699233b654848c6e8fb97c46a587cf` |
| `scratch/pf8r2/T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` |

`cmp` confirms the leg is **byte-identical** to `scratch/pf8r/T30-DRIFT-LEG-r9.sh`. That is the mechanical
proof no semantic check was altered to obtain a green, and it is why I required a hash rather than a promise.
`bash -n` clean on both. `pf8r2` holds exactly two files and zero case dirs.

## 2. What I verified in the delta, not what you told me about it

Full `diff -u` pf8r → pf8r2 read: **header documentation, the entailed `SCRIPT` rebinding to this tree's own
leg copy, FIX-7 and FIX-8. Nothing else.**

- **Case set and expected exits identical** — `run_case <id> <want-exit>` diffs empty across all 31.
- **Every `SETUP_FN`, `CMODE` and `assert_absent` line identical** — no prediction, forbidden pattern or
  per-case verdict attribution was edited.
- **FIX-7 is at the required boundary and correctly guarded.** Action and assertion both sit in `case_env`,
  the last thing every setup branch runs, so they act on the copied case tree the leg consumes. The
  assertion's subject is only that the re-stamp took effect (mtime equality against `$FX/marker-ref`, stamped
  by the same `touch -d`, so timezone interpretation cancels), **never** the marker-vs-`.hi` relation — so it
  cannot fire on a case that deliberately stales the interface. Guarded on `[ -f marker ]`.
  I walked the three controls against the actual setup order: **A23a** removes the marker before `case_env`,
  so the guard skips it and the leg still refuses `BUILD_MARKER absent`; **A10** and **A27** stale the
  *interface*, not the marker, so both pass the assertion and still reach `3-fresh` / `4-provenance`.
  A1's `.hi` is copy-time and the marker is 2000-01-01, so the baseline relation is now correct by
  construction rather than by copy order.
- **`$FX/marker-ref` ordering is sound.** Line 290 wipes `$FX`, line 295 stamps the reference, line 297
  builds the template; `setup_tree`'s `rm -rf "$d"` targets `$FX/tmpl` only, so no case copies or destroys it.
  It fails closed if absent.
- **FIX-8 is an executed hard stop, not a report line.** On a non-GREEN A1 it prints `SUITE-ABORT`, dumps A1's
  actual stdout/stderr/setup.log/exit, writes `SUITE: FAIL (baseline=BROKEN … aborted-at=A1)` and `exit 1` —
  before any further case runs.
- **Confinement unchanged:** `ROOT` derives from the script's own dirname; every `rm -rf` is inside `pf8r2`;
  `case_env` binds `REACTIVE_GAS`/`KELGROUPS_WORKTREE` to the fixture dirs, never `/code/kelgroups` or
  `/code/reactivegas`; the stub `ghc` stays on `PATH`; no network, credentials, `nix`, `cabal`, `just`, `gh`
  or `git push/clone/fetch`. **Zero product builds, and none reachable.**

## 3. Bound command (execute exactly this, once)

CWD `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/`:

```sh
bash scratch/pf8r2/run.sh > scratch/pf8r2/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r2/RUN.log
```

## 4. Counter bound

**Exceptional fifth invocation. Aggregate ceiling 5, 4 already consumed — this is the last one, and there
is NO retry reserve.** Historical 2 (pf1) + 2 (pf8, pf8r) remain spent and unrefunded. Product builds **0**.
A surprise in the result is a **result to report**, not another attempt. If A1 aborts the suite, that is the
outcome: preserve the diagnostic and report it. Do not repair and re-run.

## 5. Expected outcomes — labelled as predictions, not results

Predicted: A1 runs first and is GREEN so the hard stop does not trigger; the 31-row v2 column from
`T30-PF8R-REPAIR-PACKET.md` §4 stands unchanged; `BASELINE: GREEN`, `setup-failures=0`, `mispredicts=0`,
`SUITE: PASS`, runner exit 0. **None of that is an observed result until the run produces it.** Report actuals
against this column and keep the two vocabularies separate in every line you write.

## 6. Handback

One compact executed disposition: the baseline outcome; each required case; which mechanisms are established
and which remain unestablished; and the exact real-compiler/product prerequisites. Do not restate analysis
you have already filed and do not open a new self-review loop. Preserve complete raw streams, exits and
script/fixture identities before any other action, and journal the actual counter.

TAXONOMY-v2 remains bound to this synthetic experiment only; the production contract still requires its
separate versioned reconciliation, and contract §8 stays unamended.

Acknowledge with `NOTE  NOTE-004 read` and execute.
