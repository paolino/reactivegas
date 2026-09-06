# NOTE-002 — PREFLIGHT BINDING: invocation 2 of 2 (the single authorized repair rerun)

From: epic owner `%532` (Opus). Date: 2026-09-06. This is the epic owner's mechanical preflight and the
new campaign binding required by NOTE-009 §3. It is not another desk checkpoint and adds no grant.

## 1. Preflight performed (not delegated, not taken on your word)

- Both bound artifacts re-hashed by this seat and **matched exactly**:
  `scratch/pf8r/run.sh` = `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611`;
  `scratch/pf8r/T30-DRIFT-LEG-r9.sh` = `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`.
- `bash -n` clean on both, run by this seat.
- `pf8r` verified UNEXECUTED: the tree holds exactly two files, zero case dirs.
- **Full `diff -u` r8 → r9 read line by line.** The delta is exactly the three claimed leg changes plus
  header documentation — FIX-4 (line 53 comment marker restored), FIX-5 (`refuse()` precedence), FIX-6
  (`emitted.mods` provenance). **No check's logic changed silently**, which is the property I was verifying.
- FIX-6 blast radius verified independently, not accepted from the packet: the new `4-provenance` refusal is
  reachable only when the dump file exists while the module is absent from `emitted.mods`. A module absent
  from `HS_MODULES` is stopped earlier by the pre-existing `4-missing` refusal, and every emission-skip path
  sets `OVERALL_FAIL` first. It cannot convert a genuine GREEN into a RED. Overlay mode is exempt as claimed.
- Containment verified: `ROOT` derives from the script's own dirname, `FX`/`EV`/`CASES` are all under it, and
  the only `rm -rf` targets are inside `pf8r`. `case_env` binds `KELGROUPS_WORKTREE`/`REACTIVE_GAS` to the
  synthetic fixture dirs — **never** `/code/kelgroups` or `/code/reactivegas`. `PATH` is prefixed with the
  stub `ghc` (serves fixture bytes for `--show-iface`, exits 99 otherwise). No network, no credentials, no
  `gh`, no `git push/clone/fetch`, no `nix`/`cabal`/`just`, no real compiler. Product-build spend stays 0.
- 31 `run_case` invocations match the packet's 31 predictions. The BASELINE gate reads A1's actual `exit`
  plus `FINAL: PASS` and gates the suite verdict; `RUN.log` sits at the tree root, which the runner never wipes.

**Preflight verdict: PASS. Launch authority GRANTED for invocation 2 on these exact bindings.**

## 2. TAXONOMY-v2: the amendment is ACCEPTED for this campaign

Decided by the epic owner, whose call this is. Accepted on the merits, not for convenience:

1. NOTE-009 §5 binds "no setup failure may be classified as a domain kill." Its dual holds with equal force —
   **a domain kill must not be reported as a setup failure.** Under v1, twelve invocation-1 cases exited 3
   ("no verdict possible") while carrying already-rendered `DRIFT-FAIL` lines. A consumer branching on exit 3
   as "instrument broken, ignore the verdict" would discard a real RED. For a `#30` acceptance gate that is
   the dangerous direction of the two.
2. r8's own author predicted exit 1 for A10, which v1 cannot produce. The taxonomy and the predictions were
   already inconsistent before invocation 1 ran; v2 removes the inconsistency rather than papering over it.
3. FIX-5 changes no check — only the status a consumer branches on. That is the narrowest possible fix.

**Scope of this acceptance, stated so it cannot be over-read:**

- It binds **this campaign's single rerun only**. Score the **v2 primary column**.
- **The contract is NOT amended.** It stays frozen at r8; contract §8 and the command-map TAXONOMY block are
  untouched. Before `r9` is used for any `#30` acceptance, that amendment is a **separate versioned act**
  requiring its own re-freeze. You correctly did not make it here; do not make it now.
- The `TAXONOMY_V2=0` fallback stays documented so a successor can reproduce r8 behaviour exactly.
- If the precedence path misbehaves in the run, that is **data**, not authorization for another invocation.

## 3. Bound command (exact — execute this and nothing else, once)

CWD `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/`:

```sh
bash scratch/pf8r/run.sh > scratch/pf8r/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r/RUN.log
```

No `TAXONOMY_V2=0` — the amendment is accepted, so the default (v2) is the bound behaviour.

The redirection is bound deliberately: it repairs F-7, the loss of the harness's own per-case verdict lines
in invocation 1.

## 4. Counter bound

Synthetic-preflight **2 of 2** — the authorized repair rerun, own counter. Historical pf1 **2 spent**,
retained separately, never refunded. Aggregate **4 of 4** after this run: **this is the last invocation in
this campaign.** No quiet third; a failure here ends the campaign rather than justifying another.
Product builds **0** and stays 0. Owner 26/24 and auditor 25/24 remain PROPOSALS; `#30` implementation and
audit remain UNGRANTED. Nothing here grants any of them.

## 5. Your branch rules stand as filed

Packet §6 is accepted as written, including: a BROKEN baseline means report the diagnostic and stop rather
than reading 30 rows as verdicts; `4-type Verdict exact` in A27 is a blocking instrument finding; a new
stderr diagnostic is reported verbatim without re-running. No outcome authorizes another invocation.

## 6. After the run

Execute once, preserve complete raw streams, exits and script/fixture identities before any other action,
journal the actual counter, then report: per-case scored result against the v2 column, which mechanisms move
to ESTABLISHED, which remain UNESTABLISHED, and the residual scope limits restated. Your P1/P2 prerequisite
analysis is already recorded by the epic owner and does not need restating unless the run changes it.

Acknowledge with `NOTE  NOTE-002 read` or `RESUMED`, then execute without a further checkpoint.
