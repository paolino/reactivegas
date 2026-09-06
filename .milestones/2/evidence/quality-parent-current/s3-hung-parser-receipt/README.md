# Retained receipt — S3 repair owner's hung static parser (2026-09-06)

Promoted from the commissioner's session-private scratchpad into this lane so the
artifact itself is durable, not only its journal description. The desk could not
find it because my original path was session-local; that was my placement error.

- `argv.txt` — the full 2506-byte `/proc/3226574/cmdline`, captured **before** any
  signal was sent.
- `ps-record.txt` — `ps -o pid,ppid,pgid,stat,lstart,etime,%cpu,args` for the same
  PID, also captured before the signal.

## Facts at capture

PID **3226574**, PPID **3215777** (the Muse repair seat, not the S4 seat),
PGID **3226574** (its own process group), state `Rsl`, **99.1% CPU**, elapsed
**10:19** as recorded in `ps-record.txt`, **no descendants**, cwd `/code/reactivegas-66-s3-repair/lean`, and no
output files open (anon inodes only), so nothing partial existed to preserve
beyond the argv.

`SIGINT` was sent to that PID only, at its own command boundary. Verified after:
the Muse session alive at its prompt, runtime root complete, worktree still
`3590c0015b84fd58004bf6fb44dd18b107304c48` with porcelain empty, S4 seat untouched.

## Two defects in one condition

```js
while(j<L.length && !DECL_RE.test(L[j]) || /^\s/.test(L[j]) && false){}
```

1. **Empty body, no progress** — neither `j` nor `L` is modified, so once the
   condition holds it never terminates.
2. **Dead disjunct** — by JavaScript precedence this is
   `(j<L.length && !test(L[j])) || (test(L[j]) && false)`; the right disjunct is
   always false, so the intended continuation clause is dead and the guard
   silently reduces to the first conjunct.

Fixing only the body would leave a guard that does not mean what it reads.

## Correction (append-only) — elapsed time

An earlier version of this README, and my journal entries describing it, gave the
elapsed time as **10m33s**. **The raw `ps` line in `ps-record.txt` records
`10:19`**, with `STARTED Sun Sep 6 05:44:23 2026`. The 10:33 figure came from a
later `ps` I ran immediately before sending the signal, and it is **not** the
value in the retained raw line.

The distinction is preserved rather than reconciled away: **10:19** is what the
retained artifact says; **10:33** was a subsequent recheck about fourteen seconds
later. No corruption is implied and nothing about the interruption or the
recovery depends on which figure is used. Quoting a number that is not in the
artifact it cites is the defect, and it was mine.
