# NOTE-030 — mandate amendment: founding/agreement witness wording (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk founding/agreement mandate (read in full).
Epic-verified at source just now (all four bindings): `Fold.hs:472-488`
(`foldIntegratedFrom` = same shared step over a founding aggregate; Store
uses it for persisted replay); `Store.hs:510-516` (`openIntegratedKEL`
takes `GroupState s` founding — no bootstrap arm); `Validate.hs:80-84`
(`validateEvent` HISTORICAL-NON-PRODUCTION; production = the three
integrated validators); replacement brief lines 42/48-49 (the mismatched
witness wordings). Auditor %567 live-verified (codex gpt-6-astra/high,
preflight, NO START, NO question yet) — amendment lands pre-START.

## Parent-owned mandate amendment (versioned, referencing H7 + declarations)

- **R1-C1:** there is NO founding-add event and none may be required. Setup
  = guarded initial aggregate → `openIntegratedKEL` → DIRECT admission via
  `validateDirectAdmission` → role change via
  `validateBaseMutation`/`validateBaseApproval`. Assert EXACT pre/post
  `GroupView`s per step with auditor hand-computed expectations
  (independence kept); nonmember attempt leaves views exactly unchanged.
- **R5-C1:** stepwise agreement uses the INTEGRATED validators ONLY —
  historical `validateEvent` never arbitrates integrated events — on the
  SAME initial aggregate/event/signer/semantics. Replay equality = 
  `foldIntegratedFrom` over the founding aggregate PLUS real persisted
  reopen/replay. Plain `foldIntegrated` (empty start) is NEVER the comparison
  target; calling the same wrapper twice is NOT independent assurance
  (auditor hand-computes each side).
- Record the amendment timing accurately: post-launch (pane %567 live),
  pre-START. The auditor independently judges whether the resulting full
  obligation fits (it may still BLOCK with a concrete gap — that is its
  right, not defiance).
- If the amendment exposes a REAL missing production capability (not a
  wording defect), preserve it as a FINDING candidate — never a setup excuse.

## Bounds (unchanged)

No verdict coaching. No waiver of R1/R3/R5 or any open row. No semantic
repair under the auditor. No new ceiling, build, or merge grant. Full audit
+ candidate remain the subject; no terminalize/relaunch for this answerable
preflight item. Read + answer any actual auditor Q through you.

Wake: this file + pointer. Ack with `NOTE NOTE-030 read` + amendment version.
