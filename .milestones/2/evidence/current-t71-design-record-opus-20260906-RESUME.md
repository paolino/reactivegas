# Resume fragment — #71 design-record TICKET OWNER (Opus successor, PARKED)

Resurrection-grade. Replaces the predecessor fragment at
`/tmp/reactivegas/ms2/t71-design-record/RESUME.md` (kept, pointer prepended).

## Identity and launch (replay exactly, quotes included)

```sh
claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
```

- family=claude, pane `%516`, window `reactivegas:11` (`design-wait-model`),
  cwd `/code/reactivegas-issue-71`, pid 3358067. Argv and active session model
  both verified as `claude-opus-5[1m]`, effort high.
- Live runtime root: `/tmp/reactivegas/ms2/t71-design-record-opus-20260906`
  (brief sha256 `d89a92d4…`). Preserved scope root:
  `/tmp/reactivegas/ms2/t71-design-record` (brief sha256 `8a72a5a6…`;
  its initial base/zero-spend values are historical).
- Parent: milestone desk `%510`, `/tmp/reactivegas/ms2`.
- Authority: `artifacts/ROLE-SUBSTITUTION-OPUS-20260906.md`,
  POINTER-1788674093-3359899. Role substitution only.
- Ownership record: `<preserved scope root>/OWNER-CURRENT.md`.

## Stage

PARKED, admitted 2026-09-06. Campaign S71-B, Round-B repair terminal VERIFIED
and UNACCEPTED. Nothing in flight; no builds, auditors, monitors or schedules.

## Ticket state (verified at admission, read-only)

- `/code/reactivegas-issue-71`, branch `docs/71-design-record`,
  HEAD `77f8be62b6bbe6d2f3e2117464b0c72d0e736e58` == origin, clean.
- **No accepted candidate.** `77f8be6` = UNACCEPTED Round-B terminal
  (full v8 GREEN receipt `2af22b6e`, frozen diff `460411b2` + manifest, pushed).
  Rejected: `36666dc` (`b5d3199f`), `67877b1` (`a6a0d9f5` F-01/02/03).
- Mandate: `specs/71-design-record` at planning commit `90dae99`
  (R71-01..12 + claim-syntax definition + T71-06/07/08).
- Gate: frozen v8, `./gate.sh` sha256
  `7aa3f2b5c3f4b23447a9e32e5ddecf2510a10ec978af843d0b885a4512fa7939`
  == `evidence/gate-v8.sh`. v1–v7 + batteries + falsification logs preserved.
  If a hash here disagrees with the files, the FILES win — re-hash before use.
- Base: merge-base `d670323` (PR87); 5 ahead / 1 behind `origin/master`
  `3590c00` (PR88, #66 S2R). **No rebase performed.** Old pins are not current:
  S2R moves Invariants source/line identities and the justfile path.
- PR #77 OPEN, **draft held**, head `77f8be6`.

## Children

- `%542` commit owner, pid 1296754,
  `pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve`,
  zero descendants, PARKED write-idle since `2026-09-05T15:03:20Z`.
  Adopted without restart or wake; root `<preserved>/commit-owner-s2/`.
  Sole authorized writer of `docs/en/design/`. This owner writes no
  product/docs implementation.
- No auditor alive. Retired panes: `%518`, `%521`, `%533`, `%537`, `%546`.

## Counters and ceilings (carried verbatim; unlike denominators NOT reconciled)

- Owner S71-B 2/4 full + targeted-per-journal (recount from
  `commit-owner-s2/STATUS.md` before spending). Owner S71-A 4/6, closed.
- Auditors 2/3 builds + 20/40 targeted spent; **reserve 2 builds + 20 targeted
  untouched for the final FULL audit** (the reserve is 2+20 under both readings).
  Those two denominators come from two different ledgers and are **left
  unreconciled on purpose**: `2/3` is the ticket-wide audit-build ledger
  (predecessor journal 11:03:39Z, plus audit-b1's 1 build); `20/40` is the S71-B
  campaign grant (11:30:39Z `auditors-4-builds-plus-40-targeted-total-max-2-submissions`,
  13:11:53Z `spent-1-build-plus-20-targeted reserve-final-2-plus-20`). Do not
  merge or guess them; ask the desk if a spend decision ever turns on it.
- T.O. static work ledgered, 0 builds, no seat cap consumed.
- Submissions 1 of max 2 — one repair→re-audit cycle remains. No reset.
- Ceiling raises 0/2. `draft=NONE`.

## Open Q/A and inbox

None open in either root. Predecessor consumed NOTE-001..008 (ticket),
NOTE-009..019 (artifacts), UPWARD-REPORTING, SEQ, RELEASE, substitution order.
Monitor both `<live root>/inbox/` and `<preserved scope root>/inbox/`.

## Wake condition and next authorized action

WAKE ONLY on desk announcement of the **accepted final model/quality base**,
followed by its concrete authorized rebind sequence. Then, in order:

1. Verify announced base vs `origin/master`; rebase the lane branch iff it
   differs (abort + escalate on conflict).
2. Re-read required anchor lines + discovery count at the new PIN; re-verify
   the gate end-to-end; version/freeze the gate delta if the base moved it.
3. Final full validation + **fresh FULL independent audit** on the rebased
   candidate (reserved 2+20; row-level AUTH/pending/claim truth; local-only).
   **Scope is desk-ruled FULL-candidate — one fresh Codex inspector over the
   full actual final candidate and its integration, all original required rows,
   prior findings challengeable, no parallel campaign, no repeated adjudication.
   Fit that complete scope to the remaining allowance BEFORE dispatch; if it
   does not fit, state the exact gap and stop.** See the RULING section.
   Auditor seat: **codex**. The preserved brief's `grok-4.6` pin is STALE —
   operator-ordered NOTE-008 (predecessor journal 2026-09-05T11:01:55Z and
   11:03:39Z) narrowed the set to codex-or-grok and recorded the grok cap as
   exhausted ticket-wide; audit-s2r (`%537`) and audit-b1 (`%546`) both ran
   codex-gpt-6-astra-high. Never muse/GLM; never reuse a terminal auditor; fresh
   pane, root, detached worktree and context per submission. Do not reintroduce
   the stale brief pin, and do not widen the recorded set on this seat's
   authority.
4. Accept/handback packet for merge authorization, or one bounded repair +
   re-audit within the remaining submission cap, or re-cut.

Stop and escalate on: scope contradiction, missing honest citation, cap
pressure (state the exact gap; never silently overrun or narrow), base
ambiguity.

## Standing prohibitions

No Lean/model/theorem edits to make prose true. No merge, publication, issue
or PR comments, semantic edit, submission/cap reset, new implementation seat,
or terminal-auditor reuse. No tests, citation rerun, audit dispatch or build
was authorized during substitution. Upward delivery is **local files + own
STATUS only** — never type into `%510` or any human composer. Do not wake the
parked commit owner for acknowledgements it does not owe; do not restart
terminal historical owners; do not anticipate #68/#69. Deferred with rationale:
#75/#76 rows; #68 rebind only after landing (handoff + precision rule held).
No stale snapshot may be called final.

## POINTER TRANSPORT — one submission, journal-only proof (llm-settings `cb154732`)

Loaded 2026-09-06 from `NOTE-FACTORY-POINTER-TRANSPORT-CB154732`; identity
verified local `HEAD` == `origin/main` == remote `refs/heads/main` == `cb154732`.
`tmux-orchestrator/SKILL.md` `5e81db2c`, `scripts/send-pointer` `aecde3e5`, plus
the `references/recovery.md` and `pane-nudger` deltas. **Prospective** — applies
at the next transport boundary, which for this lane is delivering the inspector
brief pointer at wake. Nothing already started was touched, no leaf interrupted.

`send-pointer` no longer retries. It now: checks pane geometry, loads one
buffer, pastes once, settles, presses Enter **exactly once**, then waits for a
post-cursor journal event.

- **Only a post-cursor worker-journal event proves delivery.** Scrollback
  containing the pointer, a visible paste, or a busy spinner proves nothing.
  The old code used a scrollback marker as a submission proxy and pressed Enter
  up to three times — that is what could double-submit.
- **A timeout is *uncertain* delivery, not failure and not permission to
  resend.** It returns non-zero having pressed nothing further. Classify the
  pane and read the journal before authorizing any next action; never a reflex
  second Enter.
- **Resize before injection:** minimum 40 columns × 8 rows (`MIN_PANE_WIDTH` /
  `MIN_PANE_HEIGHT`); below that `send-pointer` refuses rather than delivering
  into a pane that cannot receive. Checked read-only at load: `%516` 218×27 and
  `%542` 218×26 — both well clear, no resize needed today, and re-check
  whatever pane the inspector gets since it does not exist yet.
- **`pane-nudger` stays the separate mechanism** for a *stable* unsent composer.
  It is not a resubmission path for a timed-out pointer, and its candidate test
  tightened (background-command markers now count as busy; bordered `❯`
  composers for claude/grok recognized).

## AUDIT PACKET V2 — binding at this lane's next packet boundary (the final audit)

Loaded 2026-09-06 from `NOTE-FACTORY-REQUIRED-PACKET-ROLES-6AA0AD7C-20260906`
at llm-settings `6aa0ad7ce39caa4e47a5c428947a3c32383a4173` — verified
independently equal at local `HEAD`, `origin/main` and remote `refs/heads/main`.
Loaded: `auditor` `d33fb612` (base role, first load), `ticket-orchestrator`
`a6ca6653`, plus the `commit-auditor` and `orchestrator-contract` deltas and
`auditor/scripts/audit-packet`.

**Prospective only.** No already-STARTed packet or verdict is touched; nothing
here reinterprets a frozen verdict. This lane has **no live packet**, so the
next packet boundary *is* the parked final audit — these obligations land in
full on it, and the commissioning work is mine.

What the commission must now produce **before** freezing, in this order:

1. Complete the environment first, then prove it in a **hash-bound
   dispatch-preflight receipt**: clean detached worktree at the exact candidate;
   every input readable and hash-matching; commands and gates present at the
   paths the seat itself will execute (copy the untracked `gate.sh` into the
   audit worktree, bind its hash and mode, run *that* copy — never a sibling
   lane's); independent identity evidence for commissioner and subject author;
   row ledger belonging to this campaign and containing every assigned row;
   authoritative campaign state captured once in the canonical counter snapshot;
   evidence/report paths, finite denominator, stopping rule, execution
   allocation and launch-attempt authority.
2. **Three required role bindings**, exactly one each, via `ROLE` in the packet
   spec against declared input labels (no fixed filenames):
   `dispatch-preflight-receipt`, `current-campaign-ledger`, `current-row-ledger`.
   Specializations add roles with `REQUIRE`. Freeze/verify refuse missing,
   duplicate, undeclared, dangling, legacy role-free, or changed-after-READY
   bindings.
3. One `TOOL` record per executable any command, gate or instrument needs.
4. `audit-packet freeze <spec> <manifest>` — **the manifest is the sole
   authority for hashes and counter figures. Never copy a digest or a spend /
   reservation / allocation / attempt number into brief prose.**
5. `audit-packet verify <manifest>` immediately before launch; then
   `audit-packet preflight <manifest>` **in the target pane, from the exact
   worktree, environment and cwd the seat will inherit**. Keep READY and
   PREFLIGHT-READY plus the manifest hash in a **separate launch receipt** —
   appending them to the sealed dispatch-preflight receipt invalidates it
   circularly. Cite the launch receipt in `START`.
6. After launch, require the `START` attempt and cumulative launch ordinals to
   equal the manifest's reserved `DISPATCH_ATTEMPT` / `DISPATCH_LAUNCH`.

**Launch topology must be frozen before the first CLI invocation.** Default for
a submission-2 seat: the initial inspector **plus one aggregate corrected
redispatch**; a higher exact cap needs parent authority recorded *before* that
submission's first launch. One CLI invocation = one attempt; creating a pane
does not consume one, restarting a CLI in the same pane does. An attempt stays
charged even if the seat never reaches `START`, is invalidly commissioned, or
returns at zero executions. Unused execution allocation returns to the counter;
**a seat attempt never returns.** A recovery launch needs preflight evidence
that the reported commissioning defect actually changed — otherwise return the
unresolved dispatch defect rather than chaining replacements. Maintain
cumulative launches and each submission's frozen allowance in the campaign
ledger.

**Two frictions specific to this lane, to settle at commissioning, not now:**

- This campaign's spend lives in **journal events across two denominations**
  (2/3 builds, 20/40 targeted, reserve 2+20, ceiling raises 0/2). Packet V2
  wants **one canonical counter snapshot in a ledger file**. `commit-auditor`
  is explicit that existing campaigns keep their explicit counting rules until
  their owner records a *prospective* amendment, and that historical spend is
  never silently reset or relabelled. So: build the snapshot **from** the
  recorded history, carry both denominations honestly, and if the strict
  `RESOURCE_*` schema cannot express them without distortion, say so and return
  the exact gap rather than flattening the record to fit the form.
- A failed preflight consumes **no** seat attempt (no auditor process exists
  yet), which makes getting the packet right cheap — and makes launching into a
  known-incomplete packet inexcusable.

**Unchanged by this note:** the desk's full-candidate scope ruling, one fresh
Codex inspector, all original required rows, the 2+20 reserve, the wake
condition, and the fit-scope-to-allowance-before-dispatch precondition.

**Limit the note states itself:** role binding proves the required artifacts are
present, uniquely bound and immutable. It does **not** prove their contents are
complete, current or true — the inspector still judges that.

## BROWSER REVIEW SURFACE — this lane HAS one, and it is currently unserved

Determined 2026-09-06 under `SKILL-RELOAD-PREVIEW-FRESHNESS-8C974D1`
(POINTER-1788693623-134058), reloading `documentation`, `resolve-ticket` and the
role contract at llm-settings `8c974d1`. **Answer: YES.** Verified at bytes, not
inferred from the ticket's description:

- `mkdocs.yml` `nav` lists both files this ticket rewrites, under **Design**:
  `design/state-machine.md` and `design/kelgroups-vote-machine.md`.
- `site_url: https://paolino.github.io/reactivegas/`, published by
  `.github/workflows/deploy-docs.yaml`. The site is live: `/` → 200 and
  `/design/state-machine/` → 200.

**The invariant is not satisfied, and cannot be satisfied from inside this
ticket's fence.** Evidence:

- `deploy-docs.yaml` triggers **only** on `push: branches: [master]` (plus
  `workflow_dispatch`). `ci.yaml` does not build docs at all. `grep -rniE
  'static-preview|pr-preview'` over `.github/workflows/` returns **nothing** —
  the repo has **no PR preview whatsoever**.
- So the only live browser surface is the default-branch page built from
  `master`, and it is **stale relative to PR #77's head**:
  `git diff --stat origin/master..HEAD -- docs/en/design/` = **493 insertions,
  178 deletions** across the two files. The live page still serves "15 events"
  (3 occurrences); PR head has 0.
- Under `documentation` at `8c974d1` that is precisely the named delivery
  failure: an older default-branch page offered as the current review surface,
  with no preview bound to the PR head.

**Not mine to fix; recorded for the desk.** Wiring the shared
`paolino/dev-assets/static-preview` action is a `.github/workflows/` change —
repository CI infrastructure. A ticket owner's authored set is `specs/`, the
ignored `gate.sh`, task stamps, PR metadata and its own orchestration state;
and this ticket's brief forbids changes beyond the design record. The desk owns
whether to commission it here, as a separate ticket, or not at all. **No preview
work was manufactured and nothing was built** — the note forbids exactly that
for lanes that would have to invent it.

**Tension to resolve before any such commission:** the shared preview action
*"posts or updates a PR comment"*, while this lane carries a standing **no
issue/PR comments** prohibition. An action-posted upsert comment is not authored
prose, but the two rules touch and this seat is not resolving that alone.

**Already satisfied:** the draft PR was opened at the first coherent candidate
(#77, at planning) and holds draft — honest unaccepted status. Post-merge
publication through the default-branch route is automatic via `deploy-docs.yaml`.

**Owed at wake, not now:** when the commit owner is next briefed or rebriefed,
its brief must carry this invariant, since it is the seat that writes
`docs/en/design/`. It is a commit owner, not a supervisor, so the note's
"propagate through immediate child supervisors" clause selects **no** recipient
today, and a parked implementer is not woken for a policy-only reload.

## RULING — Voci is OUT of M2, IN M3. SETTLED and FILED. Content change owed.

Operator ruling **2026-09-06** ("keep it out, that is milestone 3, file it",
clarified as the legacy Voci feature): the `Voci/` catalogue and order-bound
pledges are a documented **M2 non-goal** and **planned M3 feature**. It removes
or narrows no other M2 requirement. Delivered via
`NOTE-VOCI-M3-OPERATOR-RULING-AND-FILING` (POINTER-1788684895-3922917).

Filed by this owner, 2026-09-06 — no prior Voci/catalogue issue existed in any
state (searched `voci`, `catalogue`, `catalogo`, `listino`, plus a full title
scan of all 60 issues), so a new one was created rather than a duplicate:

- **https://github.com/paolino/reactivegas/issues/91** — "Voci catalogue and
  order-bound pledges: milestone 3 follow-up", OPEN, assignee `paolino`,
  on project 2 (General Planning) item `PVTI_lAHN3B7OAT-p6s4OaQxn`, no labels
  and no single-select fields set — matching the convention of the sibling
  reactivegas issues (#82/#83/#84/#90), which sit on the board with fields unset.
- **Milestone 3** created because none existed: repo milestone `number=3`,
  title `Milestone 3`, open, **no due date**, https://github.com/paolino/reactivegas/milestone/3
  — a plain planning milestone; no wider roadmap was invented.
- No issue comments posted (verified: 0 on #91).

**Body corrected 2026-09-06** on desk readback
(`NOTE-VOCI-91-DEFERRED-MODEL-ORDER`, POINTER-1788685082-3930198): the Eventual
integration section had carried the 2026-09-05 record's clause "that change
belongs in the Lean *before* the Haskell app fold is written", which under the
new M3 timing would have made deferred M3 work a prerequisite for the
**currently commissioned M2 Haskell fold** — an unintended M2 blocker. Replaced
with the desk's exact text: *"When the M3 feature is commissioned, any required
catalogue/order payload semantics must be specified in the Lean before
implementing those M3 changes in the shared Haskell core. This does not delay or
change M2's amount-only pledge contract or its Haskell implementation."* The
dated 2026-09-05 consequence is preserved as historical evidence, now explicitly
dated. **M2's pledge contract is amount-only and unaffected — do not read #91 as
blocking any M2 work.**

**Content change owed by the design-record rewrite, carried into the eventual
final update under existing ownership — not dispatched now, no implementer
woken, no candidate edit, build or audit from this:** the record's Voci non-goal
section must now state the **dated operator ruling and its M3 disposition with
the #91 reference**. Its "open operator question" framing is **closed**. The
underlying facts are unchanged and still required: 21 of 95 legacy modules,
`ImpegnoVincolato`/`CorrezioneImpegnoVincolato` at `Eventi/Impegno.hs:68`, Lean
`pledge` carrying a bare `(user, c, v)`, and the outcome test naming no
catalogue. Prior statements that the question was open remain valid **as dated
history**; they are not deleted, they are superseded with their date.

## RULING — final-audit scope for S71-B (#71). SETTLED, carry verbatim.

Desk disposition 2026-09-06, `NOTE-S71-B-FULL-AUDIT-SCOPE-RECONCILED`
(POINTER-1788682635-3831135), answering the conflict this lane raised under
`NOTE-AUDIT-PROCESS-RELOAD-AF60AC2`. **The conflict is closed. Do not reopen it
and do not re-derive it from the generic skill text.**

The explicit existing **full-candidate** final-audit requirement remains binding
for S71-B. The `af60ac2` generic submission-2 delta default does **not** replace
it. This is recorded as an **explicit desk scope exception to `af60ac2`** for the
pending final #71 audit:

- **one fresh authorized Codex inspector** — not a parallel blind-inspector
  campaign;
- it reviews the **full actual final candidate and its integration**, not an
  `8e4cbb8..77f8be6` delta;
- **all original required rows remain**;
- **prior findings remain challengeable**;
- **no second parallel inspection campaign, and no repeated higher-level
  adjudication** at epic or desk altitude.

What the ruling explicitly does **not** do: it does not release the parked
audit, does not change the accepted-final-model/quality-base wake condition,
grants **no** new execution budget, resets **no** spend, and does **not** accept
`77f8be6`.

**Binding precondition on dispatch:** the later concrete base-rebind commission
must **fit the complete scope to the remaining allowance before dispatch** —
i.e. confirm full-candidate scope with every original row fits inside the
reserved 2 builds + 20 targeted *first*. If it does not fit, state the exact gap
and stop; never silently narrow the scope and never silently overrun.

Also settled earlier and still standing: the new process binds **new eligible
campaigns**; S71-B is frozen, so its historical spend is preserved unreset and
unrefunded (2/3 builds, 20/40 targeted, reserve 2+20, ceiling raises 0/2 stay as
recorded historical denominations even though `af60ac2` abolishes ceiling raises
and second denominations going forward — one declared execution counter per
campaign, kept in a ledger file, never as journal events).

## Skill reload 2026-09-06 (llm-settings `af60ac2`) — what changed for this seat

Reloaded current bytes from `/home/paolino/.codex/skills` (symlinks into
`/code/llm-settings/shared/skills`): workflow `71b522a8`, orchestrator-contract
`81e7c852`, ticket-orchestrator `ca98eac8`, resolve-ticket `6b27d602`,
context-compiler `f136788c`, worker-protocol `92920204`, tmux-orchestrator
`86b9c9d0`, verification `7872ddfc`, invariants `83db18c5`,
tmux-orchestrator/references/recovery.md `9d729230`. The first pass bound rev
`a55a25a` and was superseded by `af60ac2` six minutes later; both are journaled.

Completed at `af60ac2` under NOTE-AUDIT-PROCESS-RELOAD-AF60AC2, role-scoped:
commit-auditor `70297801`, commit-owner `f38cd45a`, gate-script `88534bec`,
plus `commit-auditor/scripts/collect-findings`.

- **Supervision is event-driven and bounded.** One long blocking wait per turn
  (harness Monitor, or `wait-status` at the max tool timeout) on the child
  journal — never a loop of short waits. At most 20 API calls/hour outside a
  transition. At each transition verify the child's claim against the artifact
  it *names* with a check returning one line (hash match, gate exit, receipt
  digest); never read a candidate or log into this context. Capture-pane only
  after an event or a stale verdict, 10–15 lines.
- **Audit topology changed.** Submission 1 gets 2–3 parallel *blind* inspectors
  with distinct fault scenarios (provenance+gate / semantics-vs-mandate /
  negative-controls+mutation-adequacy), each writing a prose report **and** a
  structured `findings.jsonl`; `commit-auditor/scripts/collect-findings` unions
  and dedupes by `(fault_class, location)`; the T.O. adjudicates that union
  **once** into a single `REPAIR-BATCH.md`, and the commit owner receives the
  batch — never the raw reports. Submission 2 gets **one delta inspector**.
  No repeated adjudication at epic or desk altitude. **This lane is an explicit
  desk-ruled exception to that topology for its pending final audit — see the
  RULING section above; it is settled, not open.**
- **Audit packet is three fields.** An inspector refuses only for a missing
  candidate SHA (plus rejected SHA on a delta), mandate hash with invariant IDs
  and severities, or report path. Everything else defaults and is recorded as a
  warning, never a dead launch.
- **One budget, no raises**, for new campaigns: a single execution counter
  (commands that build or run) in a campaign ledger *file*, never journal
  events; setup/transport failures logged, not charged; at a limit the parent
  decides the re-cut in the same turn.
- **Builds before synthetic runs.** A fixture or synthetic run is admissible
  only after the campaign's first product build, and at most one per campaign.
- **Severity is fixed at spec time**, by the ticket owner, per row — never
  argued at audit time. `BLOCKING` = the value reaches chain state, money, or a
  signature; undeclared is `BLOCKING`; a `BLOCKING` row may terminate only as
  `KILLED` or `BLOCKED`, never `RESIDUAL`.
- **Failure-mode coverage is not waivable by my brief** — resource acquisition,
  work moved into threads, swapped synchronisation primitives, degradation
  paths. A brief listing only steady-state questions does not suppress it, and
  "none altered" is valid only with what was checked named.
- **Retirement ≠ acceptance.** New `RETIRED` event (worker/root, report-or-gap,
  transport identity, process-cleanup result), a parent-owned `.retired-workers`
  inventory, and archive-*in-place* when frozen manifests depend on absolute
  paths — which is the case here (`evidence/` and the gate backups are
  path-bound). Reconcile terminal children **before** commissioning the next
  inspector. `%542` is explicitly **not** terminal: parked, authorized remaining
  work, named wake condition → preserve.
- **Note acknowledgement is one line.** No receipt files, no restatement, no
  re-verification. Write a file only when a brief explicitly orders one.
- **No invented tags.** The vocabulary is the core set plus the role-skill tags
  (`PROOF-COMPLETE`, `REPAIR-COMMIT`, `AUDIT-PASS`, `AUDIT-FINDINGS`,
  `AUDIT-CONTRACT-BLOCKED`, `CONTRACT-CHALLENGE`, …). A parent can only wait on
  a tag it knows.
- **Two revisions, then build.** A contract or packet gets at most two revisions
  before a build exercises it; the third revision is the build.
- **Never park a campaign on a counter** — at a submission or budget limit,
  decide in the same turn. This lane is parked on a *desk announcement*, which
  is not that.
- **Usage-limit stalls** (recovery.md): a provider-limit banner in old scrollback
  is not a restart reason; one durable `send-pointer` continuation retaining
  model/scope/budget; reconcile the acknowledgement even after a helper timeout;
  on a persistent block record the observed error and reset time and stop
  retrying. Never switch model or provider as part of a nudge.
- **Context pin** — the rule is now that supervising seats run the standard pin,
  not `[1m]`. This seat holds `[1m]` by explicit operator order predating the
  rule; the correction is a desk-owned relaunch (AMENDMENT-CANDIDATE-B). Do not
  self-relaunch.
- **Unchanged by the reload:** frozen mandate `90dae99`, gate v8 `7aa3f2b5`,
  candidate identities, the 1-of-2 submission cap, all budgets and the 2+20
  reserve, the codex auditor fence, the `docs/en/design/` sole-writer boundary,
  and the wake condition. Upward delivery stays local-file + own STATUS: the
  workflow gist rule does not reach worker control records, and this lane's
  local-only restriction is explicit.

## Operational warnings carried forward

- `wait-status` patterns must match the two-space tag column; preflight every
  wait with a non-zero grep count; keep blocking calls <= 60s.
- Bare `grep -o` counts in `$()` under pipefail need `|| true`;
  `out=$(failing)` bare under `set -e` exits silently — capture with `|| rc=$?`.
- Recorded mistakes not to repeat: leg-12 authorized after NOTE-018 withheld it
  (spent within cap, owned); grok-seat contract breach on audit-s2
  (terminalized, archived); one stale gate hash in an admission line
  (corrected, old line preserved).
