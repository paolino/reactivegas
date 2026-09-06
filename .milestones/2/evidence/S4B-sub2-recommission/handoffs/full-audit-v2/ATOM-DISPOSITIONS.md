# Executed body controls

All 44 rows are distinct charged Lean invocations. Each definition marker succeeded and each original proof failed within its frozen proof interval; no earlier error is credited. Positive production proofs compiled in S01 and cold S10. Exact substitutions, input hashes and diagnostic locations are retained in ATOM-DISPOSITIONS.json; complete streams remain under evidence/full-v2.

| Row | Obligation | Original theorem | Proof errors | Disposition |
|---|---|---|---|---|
| A01 | P02 cassa contribution | `conservation_corr` | 124:unsolved goals | CLOSED |
| A02 | P02 conto contribution | `conservation_corr` | 124:unsolved goals | CLOSED |
| A03 | P02 escrow contribution | `conservation_corr` | 124:unsolved goals | CLOSED |
| A04 | P03 member balance nonnegative | `solvent_corr` | 149:unsolved goals | CLOSED |
| A05 | P03 accepted pledge carrier | `solvent_corr` | 149:unsolved goals | CLOSED |
| A06 | P03 pending pledge carrier | `solvent_corr` | 149:unsolved goals | CLOSED |
| A07 | P03 pledge amount nonnegative | `solvent_corr` | 149:unsolved goals | CLOSED |
| A08 | P03 canonical member carrier | `solvent_corr` | 149:unsolved goals | CLOSED |
| A09 | P04 negative balance condition | `insolvent_corr` | 159:unsolved goals | CLOSED |
| A10 | P04 existential member carrier | `insolvent_corr` | 159:unsolved goals | CLOSED |
| A11 | P05 equal-user pledge equality | `uniquePledges_corr` | 176:unsolved goals | CLOSED |
| A12 | P05 equal-user antecedent | `uniquePledges_corr` | 176:unsolved goals | CLOSED |
| A13 | P05 accepted outer carrier | `uniquePledges_corr` | 176:unsolved goals | CLOSED |
| A14 | P05 pending outer carrier | `uniquePledges_corr` | 176:unsolved goals | CLOSED |
| A15 | P06 every collection | `allUniquePledges_corr` | 185:unsolved goals | CLOSED |
| A16 | P08 selected held amount | `escrowHeld_corr` | 213:Type mismatch; 215:Application type mismatch: The argument | CLOSED |
| A17 | P08 absent held amount | `escrowHeld_corr` | 206:unsolved goals | CLOSED |
| A18 | P09 removed referent exclusion | `governanceEnacts_corr` | 223:unsolved goals | CLOSED |
| A19 | P10 conto effect | `doubleEntry_corr` | 233:unsolved goals | CLOSED |
| A20 | P10 cassa effect | `doubleEntry_corr` | 233:unsolved goals | CLOSED |
| A21 | P12 member balance zero | `canCloseGroup_corr` | 259:unsolved goals | CLOSED |
| A22 | P12 no open collections | `canCloseGroup_corr` | 259:unsolved goals | CLOSED |
| A23 | P12 all cassa balances zero | `canCloseGroup_corr` | 259:unsolved goals | CLOSED |
| A24 | K1 approval uniqueness | `KelGroups.pendingWellFormed_corr` | 62:unsolved goals | CLOSED |
| A25 | K1 proposer assent | `KelGroups.pendingWellFormed_corr` | 62:unsolved goals | CLOSED |
| A26 | K2 keyed canonical member | `KelGroups.membersCoherent_corr` | 76:Type mismatch; 78:Type mismatch | CLOSED |
| A27 | K3 pending coherence | `KelGroups.pendingCoherent_corr` | 91:Type mismatch; 94:Application type mismatch: The argument | CLOSED |
| A28 | K4 member key uniqueness | `KelGroups.wellFormed_corr` | 110:Application type mismatch: The argument; 113:Application type mismatch: The argument | CLOSED |
| A29 | K4 proposal key uniqueness | `KelGroups.wellFormed_corr` | 110:Application type mismatch: The argument; 113:Application type mismatch: The argument | CLOSED |
| A30 | K4 member coherence | `KelGroups.wellFormed_corr` | 110:Application type mismatch: The argument; 113:Application type mismatch: The argument | CLOSED |
| A31 | K4 proposal coherence | `KelGroups.wellFormed_corr` | 110:Application type mismatch: The argument; 113:Application type mismatch: The argument | CLOSED |
| A32 | K5 actual enactment presence | `KelGroups.enacts_corr` | 138:`simp` made no progress | CLOSED |
| A33 | K5 exact resulting state | `KelGroups.enacts_corr` | 133:Tactic `rewrite` failed: Did not find an occurrence of the pattern; 136:Application type mismatch: The argument | CLOSED |
| A34 | V1 assent uniqueness | `KelGroups.Vote.questionClean_corr` | 150:unsolved goals | CLOSED |
| A35 | V1 dissent uniqueness | `KelGroups.Vote.questionClean_corr` | 150:unsolved goals | CLOSED |
| A36 | V1 tally disjointness | `KelGroups.Vote.questionClean_corr` | 150:unsolved goals | CLOSED |
| A37 | V2 open question key uniqueness | `KelGroups.Vote.sweepReady_corr` | 201:Application type mismatch: The argument; 208:Application type mismatch: The argument | CLOSED |
| A38 | V2 closed question key uniqueness | `KelGroups.Vote.sweepReady_corr` | 201:Application type mismatch: The argument; 208:Application type mismatch: The argument | CLOSED |
| A39 | V2 open closed disjointness | `KelGroups.Vote.sweepReady_corr` | 201:Application type mismatch: The argument; 208:Application type mismatch: The argument | CLOSED |
| A40 | V2 lookup-selected open question cleanliness | `KelGroups.Vote.sweepReady_corr` | 201:Application type mismatch: The argument; 208:Application type mismatch: The argument | CLOSED |
| A41 | V2 closed question cleanliness | `KelGroups.Vote.sweepReady_corr` | 202:Tactic `introN` failed: There are no additional binders or `let` bindings in the goal to introduce; 210:Function expected at | CLOSED |
| A42 | V2 no open verdict in closed | `KelGroups.Vote.sweepReady_corr` | 205:Type mismatch; 212:Type mismatch | CLOSED |
| A43 | V3 sweep shape | `KelGroups.Vote.voteWellFormed_corr` | 254:Application type mismatch: The argument; 257:Application type mismatch: The argument | CLOSED |
| A44 | V3 callable threshold open verdict | `KelGroups.Vote.voteWellFormed_corr` | 255:Application type mismatch: The argument; 257:Application type mismatch: The argument | CLOSED |
