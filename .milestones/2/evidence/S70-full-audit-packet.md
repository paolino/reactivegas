# Expanded review packet — submission 2, extent `6879970f..280b67f`

Per NOTE-058. **Reported to the desk before dispatch, as required.** No seat is
launched on this packet yet.

## First: my recommendation was wrong, and here is the evidence

I recommended landing the nine with an honest PR limitation, calling them
"visual and copy changes whose behaviour the frozen oracle and teaching gate
fully exercise." The desk rejected that. It was right, and the diffs say so:

| commit | files touched | scale |
|---|---|---|
| `4a90e36` | **claim-gate + core + html** | +290 / −27 |
| `41e5317` | **core + teaching-gate + html** | +163 / −53 |
| `de89511` | html | **+187 / −19** |
| `d446280` | html | +72 / −31 |
| `c798bd6` | html | +68 / −32 |
| `8bc6bbb` | html | +65 / −51 |
| `bffcad6` | html | +32 / −32 |
| `efd7b8a` | html | +7 / −4 |
| `dbd1ed8` | html | +4 / −3 |

Two of the nine touch the **core and the gates**, not only the page. `de89511`
is 187 lines of new drag behaviour. Calling that set "visual and copy" was a
mischaracterization, and "fully exercises" was an assertion of completeness I
had not measured — which is exactly what NOTE-058 says a statement like that is
not evidence of.

## Extent — versioned after the rebase (NOTE-071)

My earlier range `125409b53..candidate` was **stale**. The branch was rebased
onto the accepted master, so `125409b53` is **no longer an ancestor** of the
candidate. I verified every figure below myself rather than adopting them:

| check | result |
|---|---|
| `125409b53` ancestor of `280b67f`? | **NO** |
| `git rev-list --count 125409b53..280b67f` | **31** — not the 10 I stated |
| `6879970f` ancestor of `280b67f`? | **yes** |
| `git rev-list --count 6879970f..280b67f` | **10** |
| `git diff --name-only 125409b53 6879970f` | `justfile`, `lean/Reactivegas/Trace.lean`, `scripts/check-trace-coverage-agreement` — the accepted S1 base integration, **no simulator-content delta** |

| field | value |
|---|---|
| **review extent** | **`6879970fdb1a797263843387e14704eaa1e3a2e7..280b67f14fa74d352b36bca98f87f03a3819308b`** |
| commits in extent | **10** — the nine unaccepted, plus C1 |
| historical accepted receipt identity | `125409b53` — *fidelity-recut-v2*, audit pass + gate pass, 2026-08-30T12:18:46Z. Retained as the **receipt**, not as an ancestor |
| mapped prefix tip | `6879970f`, same *"fix: require complete claim selftests"* commit after rebase |
| candidate | `280b67f14fa74d352b36bca98f87f03a3819308b` |
| integration base | `origin/master = 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` — integration against current accepted master is **independently in scope** |

**The acceptance does not travel with the content.** `125409b53` and
`6879970f` are content-preserving counterparts at the prefix tip, and their only
differences are the three base files above. That justifies using `6879970f` as
the **range boundary**; it does **not** transfer an acceptance to changed
content, and no acceptance may be inferred from a patch-id or a subject line
anywhere in this packet.

### The nine, mapped by content

Verified by `git patch-id --stable`, not by subject. All nine match:

| current ancestor | old identity | subject |
|---|---|---|
| `da1c69f` | `4a90e36` | clarify purchase geometry and proof links |
| `29ebc18` | `c798bd6` | keep clustered purchases from overlapping |
| `690232e` | `efd7b8a` | park member chips outside the purchase ring |
| `eebe61e` | `de89511` | drag purchases around the ring |
| `3be1c65` | `8bc6bbb` | show amounts in euro |
| `e6f0f4a` | `bffcad6` | drop canale base from the feed |
| `b958615` | `d446280` | name the decision, not the proposal id |
| `4db52ea` | `41e5317` | usernames instead of letter badges |
| `dca1de0` | `dbd1ed8` | stack conto and cassa vertically |

Every requirement in this packet keyed to an old identity applies to its
current-ancestor counterpart. **None of them is accepted.**

## Requirements derived from the actual operator requests and diffs

Not derived from the gate. Sources cited per row.

### R-GEO — purchase geometry (`4a90e36`, `c798bd6`, `efd7b8a`, `de89511`)

Source: NOTE-043, operator verbatim — *"it has to improve the circles geometry,
the acquisti circle has to be a circle where acquisti are as possible next to
their referent."*

1. each acquisto sits **angularly as close as possible to the responsabile who
   opened it**; the association readable without clicking;
2. **members keep their circle** — a member's position is their identity on
   screen and must be stable across steps;
3. acquisti on their own ring;
4. **deterministic layout** — same state renders the same picture every time; no
   randomness, no animation-order dependence, no jitter. *A presentation
   replaying a trace must produce identical frames.* This one is testable and
   is the strongest row here;
5. clustered purchases do not overlap (`c798bd6`);
6. member chips park outside the purchase ring (`efd7b8a`);
7. purchases are **draggable around the ring** (`de89511`). Corrected per
   NOTE-059 — my first wording read as forbidding the very interaction the
   operator asked for, which would be a contradictory gate:

   - the **automatic layout** is the nearest-referente placement of R-GEO.1;
   - **explicit user presentation state** is where the reader has dragged a
     purchase. That is an *input*, not a violation. The later operator-directed
     drag functionality is to be **preserved**, not fenced away;
   - **determinism is tested over the SAME COMPLETE INPUTS**, drag state
     included: the same state *plus the same drag state* renders the same
     picture. R-GEO.4's "a presentation replaying a trace produces identical
     frames" holds because a replay supplies the same complete inputs — not
     because interaction is forbidden;
   - what drag must still not do: move a **member** (R-GEO.2 — a member's
     position is their identity on screen), or introduce randomness or
     animation-order dependence for fixed complete inputs.

   **Limit of this reading:** derived from the retained request in NOTE-043 plus
   the `de89511` diff, not from any operator statement about drag *persistence*
   specifically. If the desk or operator intends drag state to be ephemeral
   rather than an input, R-GEO.7 changes — and the auditor must be told before
   it builds a determinism check around it.

### R-CIT — citation permalinks (`4a90e36`)

Source: NOTE-044 §1. Every citation is a permalink **at its own pinned commit**,
`https://github.com/paolino/reactivegas/blob/<pin>/<file>#L<line>`. Before the
change there were zero `github.com` links in the page. Auditor should confirm
the pin used is the citation's own, not a branch and not a shared tip.

### R-ITA — Italian surface (`8bc6bbb`, `bffcad6`, `d446280`, `41e5317`)

Source: NOTE-044 §2 and NOTE-034's standing constraint.

1. amounts display in euro (`8bc6bbb`);
2. `canale base` removed from the feed (`bffcad6`) — internal routing vocabulary
   must not surface;
3. the feed names **the decision**, not the proposal id (`d446280`);
4. usernames replace letter badges (`41e5317`);
5. no implementation vocabulary in **ordinary explanatory prose or UI chrome**:
   route names, constructor names, `retired-by` markers, pin jargon.

   **Corrected per NOTE-059.** NOTE-044 explicitly *permits* Lean names, paths
   and identifiers **inside evidence citations** — that is what a proof panel is
   for, and R-CIT requires those very identifiers to be linkable. This row bans
   implementation vocabulary where it leaks into ordinary reading; it must
   **not** be read as banning legitimate proof-panel identifiers. An auditor
   applying it to the citation surface would be enforcing the opposite of
   NOTE-044.

### R-LAY — layout (`dbd1ed8`)

Conto and cassa stack vertically.

### R-C1 — the re-bind slice

The eleven invariants INV-1..INV-11 in
`commit-owner-one-membership-glm/brief.md`,
sha256 `c013e595e7af8175a01b431b2891aec024ffa2006a59b412bf586b948cab8f58`.

## Survival question the auditor must answer per row

C1 rewrote the page onto the one-membership surface **after** all nine landed.
So for every row above the live question is not "was it implemented" but:

> **does it still hold at the candidate, and can that be seen failing?**

`41e5317` in particular replaced letter badges with usernames keyed by `Nat`;
C1 re-keyed identity to `KelGroups.Key`. R-ITA.4 must be re-established on the
new keying, not inherited.

The independent auditor selects the checks. Historical receipts may be reused as
evidence **with their scope limits stated** — they were taken on pre-rebase
SHAs against the pre-#62 model.

## Complete instrument identity — bind ALL of these, not just the gate

NOTE-069: gate v14's own bytes did not change, but its **executable dependency
changed at the same path**. So the gate hash alone no longer identifies the
acceptance input, and quoting it as if it did would understate what the auditor
must verify. Every instrument the gate invokes at runtime:

```
handoffs/gate-v14-one-membership.sh    1164ae9d8a6221d8be237abdb5cad0947d2a4c8749617cbaf67fc57c1904359d
handoffs/retired-surface-probe.mjs     9fba77d3f5070de48941bc1ac1410f0dd213e9968a7ba216bc907403ac591eb3   ← v3, Q-001
handoffs/oracle-one-membership-v2.mjs  b90327db82099171423a748a94d51d7363d60bc742a7180f8b833d341f4ffdbc
handoffs/derive-cited-sources.mjs      5efd430530f310040c2dd31a2f7537947b34abc69e8e0a810324f39d5288c752
```

All `chmod 444`. **Verify every one before trusting a green run**, and run the
complete gate with exactly this set — a green produced against an earlier probe
is not a green against this acceptance input.

The probe's own controls travel with it:
`evidence/retired-surface-probe-v3-controls.log`. Twelve controls, each red for
its own stated reason. **Judge them.** One of the twelve first fired on a syntax
error rather than the rule it claims to test; I caught and rewrote it, and you
should assume there are others I did not catch.

## The final-SHA CI receipt

The candidate is `280b67f14fa74d352b36bca98f87f03a3819308b`. It differs from
`182997c` by exactly the dead `setAggregate` removal — `economics-simulator.html`,
1 insertion, 6 deletions.

Neither the owner's `just ci` (run before the squash) nor my independent one
(run at `182997c`) is a receipt **on the final bytes**. The change is
low-impact, but unequal trees are not identical and an earlier run is not a run
on later bytes. A completed `nix develop --quiet -c just ci` at the clean frozen
final SHA, with before/after identity and command/exit/log hash, is part of this
packet and is supplied with it.

## Receipts supplied with this packet

| receipt | sha256 |
|---|---|
| `evidence/final-just-ci-280b67f.log` — `nix develop --quiet -c just ci`, EXIT=0, before/after HEAD `280b67f` clean=0 | `4d2e57d1251d36214d6e22857b7e5c385fc7d522dbf3eca1be436063c3798cc2` |
| `evidence/gate-v14-post-ruling-280b67f.log` — full gate v14 GREEN on the corrected probe set, 15 controls red for the expected reason | `ef47b0ebccc31cebb7e656fc30dbf7482d852d873bc5e936858ba39f3fd856e0` |
| `evidence/retired-surface-probe-v3-controls.log` — the probe's 12 layer-attributed controls | in evidence root |
| `../commit-auditor-s62sim-codex-s1b/report.md` — submission 1, 8 blocking findings | `19d5a79e38327339ab36cfd5270a9411fb8f841467360f35eed38c64d9c74ffe` |

These are **author-side receipts**. They are inputs to your scepticism, not
acceptance, and you should reproduce rather than inherit them.

## Campaign accounting

| field | value |
|---|---|
| campaign | `S62-SIM`, ledger `../campaign-ledger-S62-SIM.md` — to be versioned for this extent before dispatch |
| submission | **2 of 2** — the last one. Not a new campaign; the cap is not reset |
| submission 1 | `%517`, **terminal**: `FINDINGS`, 8 blocking, report `19d5a79e…`. Retired, root retained. Its eight findings are the repair's actual inputs, all addressed in `280b67f` |
| rejected-before-review | `%515`, `AUDIT-CONTRACT-BLOCKED` CB-001, report `849e1e76…`. Recorded as rejected before substantive review; does not consume a submission |
| `ceiling_raises` | **0** — no automatic raise, no third submission |
| builds | carried forward from the live campaign; `%517` reported `builds=3/30` at 07:50:49Z |
| immutable records kept | `.archived/commit-auditor-one-membership-codex-s1-CONTRACT-BLOCKED`, and `%517`'s root on completion |

**Resolved, not conditional.** C1 returned findings; the one repair bounce was
used and is complete. The combined candidate **is** the repaired SHA
`280b67f`. Submission 1's receipt is retained as **scoped** evidence covering
`dbd1ed8..af9c1e5` only — an earlier tree — and this wider audit closes the
prefix gap.

## Seat

| field | value |
|---|---|
| family | `grok` — eligible per NOTE-058, and alternate from both the `glm` owner and the `codex` submission-1 auditor |
| model | **`grok-4.6`**, pinned explicitly; no other Grok model is approved |
| effort | **`xhigh`**, passed explicitly — NOTE-059: model alone in the launch line is insufficient |
| launch | `grok --always-approve -m grok-4.6 --reasoning-effort xhigh` — flag verified against `grok --help`: `--reasoning-effort <EFFORT>`, alias `--effort` |
| verification | live argv checked against the durable identity, and a pane-bound `START` matching the id tmux returned, before any verdict is admissible |

Per NOTE-059 the current handover's eligibility **supersedes** the older scarcity
preference, and **no inference is to be drawn from the 2026-09-01 reading** of
0% weekly limit — current usage may be checked at dispatch instead. If the
launch actually fails I report the real failure rather than silently
substituting; `codex` on a fresh root is the fallback I would propose, since
`claude` is my own family and `glm`/`muse` are barred from auditing.

Argv and live config are verified against the durable identity, and a
post-cursor `START` matching the pane id tmux returned is required, before any
verdict is admissible.

## Not in this packet

- no amendment of the frozen candidate;
- no replay of superseded intermediate UI states;
- no nine separate retrospective audits;
- no merge, no publication.
