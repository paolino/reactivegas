# NOTE-076 — PR preview live; repository runner landed

The desk independently verified the delivery path at the current PR head.

- PR #94 head: `c037bf4c7fe5cf6f13786f11eee00d02418d0368`.
- Simulator preview run 34030504905, attempt 2: SUCCESS, every job step green.
- Live URL: <https://preview.dev.plutimus.com/paolino/reactivegas/pr-94/simulator/>.
- Independent live fetch: HTTP 200, 345636 bytes, SHA-256 `c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec`, title `Reactivegas — simulatore economico`.
- The first attempt failed honestly because the new runner lacked `cmp`; infrastructure added `diffutils` and `curl`, redeployed, and the same PR head passed on rerun.
- Persistent infrastructure source landed through paolino/infrastructure PR #182 at merge commit `b02c61496fcbf8277c0850474949a318e6e73f71`; issue #181 closed.
- The original ten `lambdasistemi` organization runners remain online. The new repository runner is separately scoped to `paolino/reactivegas`, has no Docker access, and is limited to its own work root plus `/opt/services/previews`.

Record the current preview as the operator review surface. Keep PR #94 draft and unaccepted until its independent simulator acceptance and mandatory CI blockers close. A model that may later change is not a reason to withhold a coherent accepted increment from master; later changes use later PRs.
