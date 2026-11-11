# Plan — #70 slice D

One bisect-safe slice on `sim/70-pledge-regimes`, one draft PR with preview. The
page's pledge listings (purchase pile, referente panel) state each pledge's
regime; the action dialogs offer owner correction and withdrawal on a pending
pledge, referente correction on an accepted one, and restrict the pledge author
to the pledging member. A page gate drives the built page in headless Chromium
over G1's generated histories and the hand-written scenarios, and checks every
offered choice against the embedded core's verdict. CI gate: `just simulator`.
The core is read, never edited (it is in parallel review in slice O, PR 115).
