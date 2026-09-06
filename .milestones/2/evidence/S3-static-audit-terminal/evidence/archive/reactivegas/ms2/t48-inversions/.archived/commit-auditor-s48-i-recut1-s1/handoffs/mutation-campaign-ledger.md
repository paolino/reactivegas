# Mutation campaign ledger

Terminal state: `closed`; stop reason: `set-point`; invariant rows `10`; killed `10`; residual `0`; blocked `0`; open `0`; audit builds `2/3`.

Credited cases: 30. Each case was preflight-valid for its intended claim, differed from baseline, produced its expected status, and left the candidate clean. Mutation sources and raw outputs are frozen by `instrument-manifest.sha256`, supplemental manifests, and `evidence-manifest.sha256`.

| Row | Credited probes | Outcome |
|---|---|---|
| INV-48-I-SURFACE | removed inversion; hidden/imported/unimported/indented theorem; comment/string/private/non-theorem decoys; duplicate theorem | killed; four controls passed |
| NAME-BINDING | swapped association; deposit/withdraw swap; invalid name; phantom constructor | killed |
| GUARDS | duplicate hypothesis; dropped guard; weakened guard to true | killed |
| CANFAIL | wrong last hypothesis; weakened successor; removed `step`; removed `stepEvent` | killed |
| AXIOMS | proof escape; valid custom axiom | killed |
| REGRESSION | no-op checker with restored baseline; unwired checker | killed |
| FENCE | forbidden `Step.lean` path | killed |
| STEP-ITE | wrong successor and live-branch guard/successor probes | killed |
| STEPEVENT-DELEGATE | unfold-dependency probes | killed |
| BOUNDARY | normal/negative space roots; invalid option/root/excess args/outside Nix; child build/generated-Lean failures; final normal | expected statuses; cleanup passed |

The primary and supplemental logs record full source/evidence SHA-256 values. Campaign summaries: `supplemental-campaign.log` SHA-256 `e5485fdd3df0c63d16df097f7cd136fece4f2328870ef5401c8d1347d4d13779`; `boundary-v2-campaign.log` SHA-256 `8eb1d1aabc55bdd6a1e51b5b5042c06da274bcfcf6a9edb9c999ca5a5144b213`.

Non-credited retained attempts: duplicate-inherited-grant with downstream breakage; post-`#eval` unimported parse failure; proof-invalid dropped guard; proof-invalid wrong successor; invalid `Inhabited False` opaque mutant; stale-olean no-op attempt; top-level indented unimported parse failure. All were replaced by valid probes. Boundary-v1 was likewise replaced by v2 after isolating checker `TMPDIR` from the Nix wrapper.
