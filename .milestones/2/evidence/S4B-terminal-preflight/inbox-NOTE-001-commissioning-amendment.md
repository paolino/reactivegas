# NOTE-001 — commissioning amendment. Two defects in my brief, both mine.

**Timing, stated truthfully: this is POST-ACK and PRE-START.** Your ACK is at
`2026-09-05T21:50:22Z`, phase preflight, no builds or probes run, and no START
event exists in your journal. Nothing you have done is invalidated and no
allowance is consumed. Do not restart.

Your brief `f62132c6…`-style binding stands except where corrected here. **Where
this note and the brief conflict, this note governs.**

## Defect 1 — my brief named no operative instrument, and the admitted owner brief points at a superseded one

`OWNER-BRIEF.md` cites `../handoffs/S4B-ACCEPTANCE-INSTRUMENT-v1.md` and base
`4a6cd87`. **Both are stale**, and that relative path does not resolve from
`admitted/`. My brief then named **no operative instrument hash at all** — which
left the owner's final submission free to define its own acceptance test. That
is backwards and it is my error.

**Do not let the submission define the test it is judged by.**

The operative authority is now frozen in `admitted/` with `MANIFEST.sha256`,
absolute paths, and precedence:

| document | sha256 | standing |
|---|---|---|
| `INSTRUMENT-v2-OPERATIVE.md` | `2214ff8a0d25f47afded7b7215e9873b5a237d97caea55eb72b1d8f884c5ca4f` | **OPERATIVE acceptance instrument** |
| `AMENDMENT-NOTE-002.md` | `2cd32f053d7c7b7f0d4ea2d1cb05b28277d6f09e5f7954b7ece241bd91a44ae9` | **normative amendments to v2** |
| `AMENDMENT-NOTE-001-reconciliation-v2.md` | `72a47113a7214e5a956c3808638621dc1d66881d62b59f68cea838efcbfd2f67` | normative |
| `AMENDMENT-NOTE-004-landed-base.md` | `7b57b4e818ee40dda98b3fdff59b91bff9b8b026b40b4f1b69a8b12ab1e2989b` | binds base `3590c001` |
| `S4-CONTRACT-ORIGINAL-REQUIREMENTS.md` | `f872255f8fffe24f5b7ab360dbac50dda692b3887ab846703637fe2c696e4d87` | **the complete original S4 requirements** |
| `COMMAND-RECONCILIATION.md` | `ee665122e955cda9a98e1211038abf248cd7f3066c2a51b138ef68a599fe3e71` | allocation |
| `INSTRUMENT-v1-SUPERSEDED.md` | `44c48239d9b62ef7ec896d7fe4964159c4b23f117865c8f9dd4bd3ebeae29501` | **history only — not authority** |
| `OWNER-BRIEF.md` | `b4a792017caa8847855d9e79924ed5f4434f489ec1c84da88cd62b58efbcd9da` | what the owner was told; its v1/`4a6cd87` citations are **stale** |
| `SUBMISSION-ADMITTED.md` | `363999bddcface9dbc856b9d060340ec689eb5840f734569facdcce618747b61` | the claim under audit, **not** the test |

**Precedence:** `INSTRUMENT-v2-OPERATIVE` + its amendments + the original S4
contract govern. The submission is evidence to be judged against them.

**Verification commands, exact:**

```
cd /tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex/admitted
sha256sum -c MANIFEST.sha256          # every entry must report OK
sha256sum INSTRUMENT-v2-OPERATIVE.md  # expect 2214ff8a0d25f47a…
```

`2214ff8a…` also appears in the owner's own submission header, so it
cross-checks independently of me.

**Scope correction:** audit **the complete original S4 requirements**, not only
the five areas I told you to press hardest and not only the owner's selected 26
rows. Those five were emphasis, never a denominator.

## Defect 2 — my Boundaries sentence forbade the mutation the audit requires

My brief said "no candidate edit" without qualification, while a mutation audit
needs per-row production-definition mutants. **A mutation audit cannot carry an
unqualified no-edit sentence and rely on you to infer the exception.** Here is
the typed fence, replacing that sentence:

**WRITABLE**
- Your own detached audit worktree `/code/reactivegas-66-s4b-audit` — required
  mutations, builds, temporary drivers, and restoration.
- Your own runtime `candidate-auditor-s4b-codex/` — evidence, instruments, logs.
- A **separately retained deliberate checker-control copy**, if and only if the
  operative instrument requires one; keep it distinct and named.

**NOT WRITABLE**
- The **owner/reference candidate** `189e1ed…` and
  `/code/reactivegas-66-s4b` — untouched.
- The **frozen acceptance instruments** in `admitted/` — unchanged.
- **Any other lane's worktree or runtime**, and **anything remote** — no push, no
  PR action, no comment, no publication, no merge. The branch is unpushed and
  stays so.

**Required of every mutation:** preserve the **raw mutant** and evidence of
**final restoration**. A mutation you cannot show you reverted is a finding
against your own run.

## What I need back before you spend anything

1. Your acknowledgement that the mandate is now **coherent** — or a concrete
   contract gap if it still is not. Reporting a genuine gap is a correct outcome.
2. Your **command-budget reconciliation** under the existing **TOTAL 8
   substantive / 60 targeted across BOTH submissions**, enumerated before START,
   with an exact gap returned **before** any overrun.

No new execution budget is granted here and no candidate changes. Your findings,
severities and verdict remain entirely yours — nothing in this note instructs
them. Continue the full audit once these bindings are coherent; there is no
further checkpoint after this.
