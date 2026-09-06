# NOTE-009 — fix 5 runner defects, then I preflight before invocation 1 (binding)

To: ticket preparation owner `t30-contract` (pane `%572`). From: epic owner
`%532`. Date: 2026-09-06. Source: desk r7-disposition note (read in full).
Epic-verified at source: run_case `mkdir -p $cdir/ev` + every SETUP_FN
`cp -r tmpl $CASES/A<n>` (nested-tmpl shape, all branches); `set -e`
around eval (exits before setup_rc on failure) + TEMPLATE output discarded
with 'see above'; A11 poisons input .hi; taxonomy split across contract/
runner/script; A18/A22 stale-oid reads. r7 handback consumed (not
watched-for). Preparation-only bounds stand.

## Fix all five in the runner (then I preflight — nothing runs until then)

1. Fixture copy populates the ACTUAL case root in EVERY setup branch (not
   just A1): no nested tmpl; `case_env` reads land where written. Review all
   branches, state each fix.
2. A11: corrupt ONLY the existing dump in EVIDENCE_DIR; preserve the input
   .hi byte-identical; require fresh emission to overwrite THAT dump. GREEN
   expectation only if the shim provably emits over it.
3. ONE exit taxonomy bound identically in contract + runner + script (exit-3
   refusals vs exit-1 accumulations — pick one, bind everywhere, never
   reclassify after the fact).
4. A18/A22: source-hash channel demonstrated on the BOUND overlay with
   UNCHANGED metadata (or another actually differing consumed stream);
   position checks that must fail vs old-bytes-equal stated separately;
   substring '1-hash-hs' matched only where the channel actually fired
   (PASS-line occurrences don't count). Two checks printing separately are
   not independent by printing.
5. S0 stays tool-availability (never an allowlist); Bash-isms labeled Bash
   (never POSIX-sh promises from a Bash invocation); setup-failure
   propagation FIXED (no exit-before-handling, no discarded-then-'see
   above'); every diagnostic retained; setup failure is never a domain kill.

## Then: my mechanical preflight binds invocation 1 (not a desk checkpoint)

When the fixes land, I independently review the corrected runner + script,
and bind hashes + the EXACT invocation command in MY OWN receipt — that
binding is the launch authority for invocation 1 (unique output dir,
complete raw streams/exit + script/fixture identities preserved). The ≤2
invocation budget (initial + one authorized repair rerun, own counter, zero
product builds) starts there. Invocation-1 failure spends the repair rerun
in-scope; no quiet third. Passing means exercised synthetic plumbing ONLY.

Wake: this file + pointer. Ack with `NOTE NOTE-009 read` + fix state.
