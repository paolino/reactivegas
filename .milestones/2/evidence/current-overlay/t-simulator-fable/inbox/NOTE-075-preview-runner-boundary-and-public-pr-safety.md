# NOTE-075 — preview runner boundary and public-PR safety

The PR-preview invariant remains binding. I independently verified the present delivery state:

- PR #94 head is `f2a534864c968432f62c8ca939cc25016b1dbb20`.
- workflow run 34030005881 is queued with zero steps started;
- `GET /repos/paolino/reactivegas/actions/runners` returns `total_count=0`;
- the host's ten active `epyc-N` runners are registered at `https://github.com/lambdasistemi`, so they are ineligible for the personal repository `paolino/reactivegas`.

This is a runner-registration boundary, not queue contention and not a simulator acceptance decision. The desk is adding a separate repository-scoped runner without changing the existing ten-runner fleet.

Before that runner is admitted, make one delivery-only repair on the existing candidate and push it through the owning lane:

1. On the `preview` job, retain the existing non-closed action condition and additionally require `${{ github.event.pull_request.head.repo.full_name == github.repository }}` at job level. The repository is public; no fork PR may acquire a self-hosted job merely by selecting the `nixos` label.
2. Apply the corresponding same-repository condition to any self-hosted cleanup job only if GitHub would otherwise assign it before deciding the condition. Cleanup must remain functional for same-repository PRs.
3. Preserve exact PR-head checkout, byte binding, live-URL smoke, draft/unaccepted status, and every simulator byte unchanged.
4. Push the delivery-only commit to PR #94 and journal the new exact head and run ID. Do not merge, mark accepted, or change simulator semantics.

The currently queued run may be cancelled by concurrency after the new push. The new repository runner will be admitted only after the server-side job condition is present on the pushed head.
