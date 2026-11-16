# Disposition of the three unmerged #47 drafts

Verified 2026-09-05 by the milestone desk. Historical branch
`docs/47-kelgroups-mapping`, worktree `/code/reactivegas-issue-47`, is clean.
Three commits beyond base 771b3c0b7ed083cf5b3c7778a02df274ab7eab83:

- 54c1543fd59653fd679cd8c529772f685837ba45 — initial substrate mapping
- ac3c6230b9e45bc0d333581bb3cc5b85baf654da — keyless admission/guard/premise corrections
- 8fc8284eb97b778e8e49a20e8652526fbf4a2ddd — vote-derived permissions/composition requirement

Disposition: RETAIN AS HISTORICAL DESIGN EVIDENCE; DO NOT MERGE THE STALE DRAFT.
#47 is closed and #71 owns the current model's design record. These drafts are
not accepted current documentation. The settled operator rulings retain their
authority independently of whether this old documentation branch was merged.
Current implemented/ruled-undelivered distinctions are tracked in #71/#76/#81.

The bundle preserves all three commits and their branch ref, with the above
base as prerequisite (present in the repository history); `bundle-verify.txt`
records successful verification in the existing repository. The binary-capable
patch preserves their aggregate two-file delta (+477 lines). SHA256SUMS binds
both artifacts. A recovery checkout needs the base object before unbundling.

The worktree, branch and current desk pane remain intact. No source changes,
rebase, deletion, cleanup, push of the product branch or publication performed.
The next milestone recovery sweep carries this archival bundle and patch.
