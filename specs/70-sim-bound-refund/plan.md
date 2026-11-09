# Plan — #70 S5-sim-b

One bisect-safe slice on branch `sim/70-bound-refund`, one draft PR, preview
current on every pushed head. Files: simulator page sources and builder,
simulator gates, scenario JSON, this folder. Strategy: render what the core
state already carries (bindings, live authorizations, closure causes, base
proposal approvals and refusals) and add the #81 L-7 stories as reachable
page paths or committed scenarios, each guarded by a control in
`just simulator`. Lean is read, never edited.
