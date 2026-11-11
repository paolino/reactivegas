# NOTE-001 — dispatch conditions met. Execute Phase 1 on the landed base.

Your brief `brief.md` (sha256 `77c9d6bf8425afdd`) says: *dispatch on the accepted,
landed S2 base — not before, and no further desk checkpoint is needed once S2
lands.* **That condition is now met.** This note supplies only the bindings the
brief left to dispatch time; it does not amend the brief.

## Bindings

| | |
|---|---|
| **Accepted base — derive everything at this** | **`3590c0015b84fd58004bf6fb44dd18b107304c48`** |
| its single parent | `d67032313acf3699cc50358a057391b88d002192` |
| tree | `44a1f0bce4796c63203070e23b96172a7774956e` |
| landed via | PR #88, squash, `closingIssuesReferences []`, audit AUDIT-PASS 32/32 |
| **Your worktree** | `/code/reactivegas-66-s3-phase1`, detached at that base, verified clean, **zero oleans** |
| Runtime root | `/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s3-phase1/` |
| **#66** | still **OPEN** — Phase 1 does not close it and must not imply it does |

## The contract, and one thing to know about it

Full contract is `../handoffs/S3-MANDATE.md`, **operative revision 3** — the one
carrying the "Changes from revision 2 (NOTE-021)" table.

**Known artifact defect, stated so you are not misled:** that file's opening line
still reads *"S3 contract — revision 2"* and its preservation note mentions only
v1, although `S3-MANDATE-v2-superseded.md` also exists. **The header lags the
content.** Read it as revision 3, exactly as your brief already instructs. It is
deliberately not rewritten — a frozen artifact is not edited to tidy an
inconsistency; the inconsistency is disclosed instead.

## What changed at this base that matters to you

The **hardcoded declaration quota is gone**. The base gate no longer compares a
count against `163`; the extent is discovered and identities are printed.
Re-derive that at the base rather than trusting this sentence — and if what you
measure disagrees with anything here, **that disagreement is a finding about this
note**, not something to reconcile away.

## Caps, unchanged

- **3 substantive builds, maximum**, for the cost measurement only.
- **No coverage claim.** Phase 1 may not assert any theorem row is covered.
- **No production, model or theorem-statement change** to the candidate or its
  sources. Scratch variants are allowed and are how P1-C gets measured.
- **Phases 2..n are unauthorized.** They are costed by your P1-D proposal and
  need explicit numeric authorization that does not yet exist. There is no
  implicit ceiling waiting for them.

## Deliver

`handoffs/PHASE1-REPORT.md` — P1-A with identities, P1-B with its bindings, P1-C
with each measurement's kind and cost, P1-D as a costed proposal, the build spend
against the 3-build ceiling, and any limit you could not close, honestly stated.

Missing required evidence you can already see is recorded as an **owned finding
with its owner** — never as a deliverable, and never closed by narrowing the
denominator. Do not invent a state-machine antecedent for a helper fact so a
reachability column can be filled.

If your reconciled command set does not fit 3 builds, **return the exact gap
before overrunning**, not afterwards.

## Boundaries

Local files only. Nothing typed into any other pane or human composer. No push,
PR action, comment, gist, publication, deployment or merge. No `docs/en/design/`
writes. A fresh independent auditor **of another family** reads your packet
later — never muse auditing muse — so write it to be checked by someone who
inherits nothing.

Record an actual START with your live PID/PGID, argv, cwd, HEAD and olean count.
Then return the next real artifact, not an acknowledgement. Do not park between
routine steps.
