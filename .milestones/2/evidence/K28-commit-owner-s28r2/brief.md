# Brief — commit-owner-s28r2 (S28-R2 F3 repair implementation)

Worker: commit-owner-s28r2. Seat: pane %545 (continuing process, S28-R2
campaign — continuity decided: adjacent repair on own F1 mechanism, active
15 min ago, never exposed to rejected v1 shapes; fully self-contained
brief; respawn fallback if wedged). Family: Muse. draft=NONE. Authority:
ticket owner t28-app-api per NOTE-033 (desk S28-R2 grant via epic).
Worktree: `/code/kelgroups-issue-28`, branch `fix/28-r2-refusal-order`
(@ `3af3d06…`, clean). No push/PR/merge/comments/remote writes. Signed
commits. ONE submission (findings go UP, never second GREEN unprompted).

## Read first (all of it)

1. Mandate: `handoffs/S28-R2-COMMAND-PLAN.md` (frozen; it IS your mandate).
2. Gate v10: `/code/kelgroups-issue-28/gate.sh` (M8 fail-closed TBB inside).
3. Finding: S28-R1 terminal report F3 section + P2.log tail (faulting-codec
   refusal preemption at Store.hs:618-627; tuple (0,0,0,0,0) = changed
   refusal, NOT corruption — repair scope is refusal-ordering).
4. Your S28-R1 record (archived sibling): `.archived/` + `commit-owner-s28r1/`
   (own F1 mechanism + rendezvous analysis — context, not orders).

## RED-equivalence (no fresh RED runs)

RED-first satisfied by inherited P2 exit-1 (`7e9bdb49…`) at `3af3d06`
(defect evidence only — not replay, not RED of new checkers). Proceed
directly to GREEN implementation per mandate.

## Repair (sound constraints — binding; mechanism yours within them)

- AUTHORITATIVE snapshot: in-lock fresh `readState` at decision time. ONE
  serialized transition (fresh-read → decision → encode-forcing → INSERT →
  commit, same lock hold). Pre-lock scope: NONE. Old precheck never
  authoritative. Reference shape: decision→force serialized post-validation
  in-lock (no lock change needed). v1 outside-lock-decision shape REJECTED
  (F1-recurrent: two callers, one old state).
- Preserve refusal short-circuiting: faulting-codec member THROWS
  observably (post-acceptance only), EXACT nonmember refusal (Left, never
  throw), unchanged state/counts; real SQL failure + lock release;
  concurrent agreement + reopening. No capability claim from finite
  schedules. No rendezvous-only prod workaround (harness redesigns P2''
  rendezvous; production governs test).
- Fence: committed changes to Store.hs + S28AppApiSpec.hs + NOTHING else
  (no Fold/Validate semantics, no client/model API, no other tickets).
  E-class helpers only if mechanically forced (disclosed).

## Permanent checks (you ship them; M8 kills them)

Faulting-codec accepted (throws observably) + refused (exact Left) +
state/count tuple checks, ALL inside the `S28-1 rejecting step before
append` describe (row-2 M8 kill mapping), same-line layout, descriptive
names. Keep the F1-regression it + all row counts green. M8 splice binds
at BINDING-GREEN (keep the repaired validate/encode region GREPPABLE +
note its new shape; ticket owner authors the concrete F3-reorder splice).
M6 success-write region = repair-adjacent (M6 rebinds at BINDING-GREEN;
keep it greppable). M7 target (Fold.hs) + M1-M5 anchors: PRESERVED
(byte-identical or legs refuse).

## Discipline + submission

- Probes ≤4 diagnostic narrow-compiles (trigger 3, journal each with
  command + obligation): Store.hs module check post-edit (≤2) + spec
  probe-compile checks (≤2). Whole-project invocations ONLY via gate legs
  (GREEN 11B + SLIM 3B) and ONLY on instruction. Recon (reads/hashes/greps/
  diffs) + formatters charge-0. Exceeding categories = STOP + report (gap,
  not silent spend).
- Journal EVERYTHING in `commit-owner-s28r2/STATUS.md` (child-authored).
  Questions → `commit-owner-s28r2/questions/`. Inbox `commit-owner-s28r2/inbox/`.
- Implement → ANCHOR-ATTEST (anchors + registration self-count + spend +
  RED-equivalence citation + new-shape notes incl. validate/encode region)
  → await BINDING-GREEN. STOP-and-ask on anything beyond mandate (anticipation
  is not authorization — S28-R1 SLIM lesson recorded).

Acknowledge: journal `START commit-owner-s28r2` (worker ID + ledgers
0/14+0/24 opened) after reading mandate + gate + finding above.
