# Prepare to own Reactivegas #69 — pledge agency (NOT DISPATCHED)

Parent: milestone desk %510, session reactivegas @26. This is a prepared intake
packet, not a live START. Implementation begins on the accepted #68 base,
including accepted quality gate changes then on master. Inspect/reuse any real
lane before creating one. Initial desk cwd /code/reactivegas-issue-47 is retained
history and is not an implementation worktree.

Authority: issue #69 and /tmp/reactivegas/ms2/questions/A-V2-AND-PLEDGE-AGENCY.md,
pledge-agency section. Later explicit rule: pledge signer==u; member freely
corrects/retracts while pending; accepted pledge corrections retain the existing
referente authorization. This supersedes older keyless-member on-behalf behavior
where it conflicts; do not restore old #48 departure events. No threshold,
assenso-composition, vote lifecycle or base assent change belongs in this slice.

Current reference (re-derive at dispatch): Step.lean on 4a6cd87 has pledge
requiring a responsabile, correctPledge reading only accepted, positive-amount
creation, nonnegative corrected amount and affordability guards. closePurchase
requires pending.isEmpty. A zero correction in the ACCEPTED arm currently keeps
a zero-valued accepted entry; preserve that arm's behavior unless the mandate
requires otherwise, and do not conflate it with withdrawing a PENDING pledge.

Acceptance, each supported by reachable signed production-root witnesses and
can-fail mutants against actual guards/effects:
1. An admitted non-admin creates their own positive affordable pending pledge;
   signer==u, membership, uniqueness, amount, solvency and existing stall guard
   enforced. Another member/admin cannot impersonate u by choosing the payload.
2. That member decreases/increases a pending pledge, settling exactly v-v' to
   their conto. An unaffordable increase and a negative v' refuse atomically.
3. Pending v'=0 withdraws: exact refund, pending entry removed, no accepted
   entry created. Removing the last pending pledge eliminates THAT blocker to
   closePurchase; all its other guards remain necessary. A zero-valued pending
   row that still blocks closure is not a completed withdrawal.
4. Acceptance transfers the same pledge to accepted exactly once. Thereafter
   the member cannot unilaterally correct/retract; existing referente-only
   correction including affordability and authorization is unchanged. A different
   responsabile is still insufficient. Pending and accepted branches must both
   be executed; no duplicate/invalid handcrafted state used as reachability.
5. Reject missing collection/user/pledge, impersonation, duplicate pledge and
   insufficient funds with unchanged state. Distinguish each observable guard
   the accepted model actually exposes; do not invent new public errors solely
   to make a test have one.
6. Preserve accepted conservation/solvency/uniqueness statements and prove them
   for the revised transition. Audit the discovered affected production extent,
   including historical stepEvent, signed .app root, authorization predicates,
   inversions, traces/corpus tests and proof dependencies. Do not weaken a
   theorem's hypothesis to conceal a reachable failure.
7. Re-derive compiled trust / source-to-compiled coverage using the actual
   accepted gates. No new numeric theorem quota. Full local just ci and fresh
   independent FULL candidate audit; a focused module build is not that evidence.
8. Emit a precise simulator/design handoff: current/ruled states, signer and
   enabled-action boundary, executable journeys and source pins. #70 owns UI
   changes and must show pending/accepted distinction before action; #71 alone
   writes docs/en/design. Issue closure must account for this UI requirement,
   not lose it because Lean is green. No anticipatory simulator change.

At commissioning: standalone Muse ticket owner plus Muse commit owner under
operator exception, fresh Codex/Grok independent auditor only, exact model/effort
and post-cursor START, visible isolated worktrees/roots, full brief/gate/provenance.
Owner prepares concrete file fence, immutable executable contract and numeric
build budget before dispatch; maximum two submissions, no implicit cap reset.
This intake does not itself spend or assign a build budget.

Local signed commits, factual issue body/draft PR preparation under normal
workflow; no comments, publication or merge without exact desk authorization.
No sibling edits or direct grandchild contact. Keep journals/RESUME current,
continue through RED/GREEN/audit/repair and exact handback without checkpoint
permission barriers. Start condition remains accepted #68; no seat commissioned
by this file.
