# Commit-auditor brief — S62-SIM submission 2, FULL audit

**Role:** commit auditor, submission **2 of 2** — the last one.
**Parent:** ticket owner `%313` (claude, `claude-opus-5[1m]`, high). Desk above: `%510`.
**Seat:** `grok`, model **`grok-4.6`**, effort **`xhigh`**, both pinned explicitly.
**Candidate:** `280b67f14fa74d352b36bca98f87f03a3819308b`.
**Your worktree:** `/code/reactivegas-sim-fable-audit-s62sim-s2` — clean detached checkout at exactly that SHA, `.lake` warmed.

You **read**. No edits, no commits, no push, no contact with either commit owner.
Report to me, then stop.

**Never write into pane `%510` or any human chat bridge.** Upward reporting is
local files only: your own `STATUS.md`, `report.md`, `evidence/`. Propagate that
rule to anything you spawn, before it does any work.

---

## THE PACKET IS THE CONTRACT

Read it in full first:

```
../handoffs/EXPANDED-REVIEW-PACKET-s2.md   sha256 8544e26631456d400b2c59548048c024c80e8c6511dff0c698d11c3e059f5795
```

It carries the extent, the requirements derived from the actual operator
requests and the nine diffs, the campaign ledger and budget, the complete
four-instrument identity, and the author-side receipts. This brief does not
restate it.

## Extent — wider than the slice

```
6879970fdb1a797263843387e14704eaa1e3a2e7 .. 280b67f14fa74d352b36bca98f87f03a3819308b
```

**10 commits: nine previously unaccepted, plus C1 and its repairs.** Integration
against current accepted master `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` is
independently in scope.

**No acceptance may be inferred from a patch-id or a subject line.** The nine
map by content to earlier identities; that justifies the range boundary and
transfers nothing.

## What you are auditing, in one line each

- the C1 re-bind against INV-1..INV-11 (owner mandate, hash in the packet);
- the repair of **eight blocking findings** from submission 1
  (`../commit-auditor-s62sim-codex-s1b/report.md`, `19d5a79e…`) — F1/F2/F3/F7 as
  one integration boundary, F4 the UI key path, F5 detector sensitivity, F6
  (mine, closed in the instrument), F8 the visible surface;
- the **nine unaccepted prefix commits** against R-GEO, R-CIT, R-ITA, R-LAY in
  the packet — and for each, the **survival question**: does it still hold *at
  the candidate*, and can that be seen failing?

## Things I want you to attack, not confirm

1. **My instruments.** All four are frozen `444`; verify their hashes before
   trusting any green. The retired-surface probe is at **v3** because two
   previous versions were defeated — once by the submission-1 auditor, once by
   the desk, once by my own commit owner. Assume a fourth hole. Its twelve
   controls are in `evidence/retired-surface-probe-v3-controls.log`; **one of
   them first fired on a syntax error rather than its stated rule** and I
   rewrote it. Look for the others I did not catch.
2. **My rulings.** I ruled twice that my own gate was wrong rather than bend the
   product (A-003, A-001). Judge whether either ruling weakened the gate.
3. **Can every check still fail?** Submission 1's answer was **no** — a detector
   passed its own negative control while disabled. I verified F5's closure by
   mutation; verify it independently, and ask the same question of every other
   control in the candidate and in my instruments.
4. **Failure modes altered.** The session layer moved from three streams to one
   integrated stream, and `normalizeWrap` now refuses the old format by name.
   What used to be a refusal may now be an exception, a silent `undefined`, or a
   guard reading a key that is absent rather than one that is not admin.

## Receipts you are given — reproduce, do not inherit

Listed with hashes in the packet. They are **author-side**: my runs, or the
owner's. They are inputs to your scepticism.

## Verdict

```
START mode=COMMIT-AUDITOR pane=<%id> cli=grok model=grok-4.6 effort=xhigh owner_cli=muse alternate=true submission=2 scope=full ceiling_raises=0 base=6879970f candidate=280b67f
AUDIT-RESULT verdict=<PASS|FINDINGS|CONTRACT-BLOCKED> report=<hash> findings=<n>
COMPLETE
```

Each finding: what is broken, how to reproduce, what you observed, the evidence
pointer, and **the property class it generalizes to**. Freeze any instrument you
build and give me its path and hash.

**This is the last submission.** There is no third. If the packet is
under-specified, say so as `CONTRACT-BLOCKED` rather than guessing — the
previous auditor did exactly that and it was right.
