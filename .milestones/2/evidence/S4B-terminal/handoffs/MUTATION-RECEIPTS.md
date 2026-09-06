# Per-atom mutation receipt

44 retained single-definition executable mutants, each type-valid through its definition, with original theorem statement/proof suffix byte-identical. All errors fall within the target theorem; no earlier definition error, parser/import failure, or unrelated theorem failure is credited. These are kernel elaboration sensitivity observations over this finite operator ledger, not a claim about all possible mutants. Target diagnostics include explicit altered equations, guards, and composed properties.

| ID | Definition / atom | Original theorem | Verdict | Mutant SHA-256 | Diagnostic SHA-256 |
|---|---|---|---|---|---|
| M01 | conservationB / cassa | conservation_corr | KILLED | ec05699c32941c1675dc4b2e36842454b1f44a0153678f9ee9d4cbd3cf2a2907 | 8f02881e4896243ce4838f5c06a9e4e3756178ea082aa171ee60ea0d7123604d |
| M02 | conservationB / conti | conservation_corr | KILLED | 5a96a1f798404c83af2f03bd7fdbf599d1f448793b345bf23a31e8f54704fe4b | 2587f98e98746d7e0c1c7fcf224ece962ef19a06d0e7dc35f1086779a3e535b2 |
| M03 | conservationB / escrow | conservation_corr | KILLED | febed820ae5ea271551469b7601a3d5d1325fc8406c23844f87afb0f0bc9f76d | 5f239ad3ecc62db5f9818b8d141905282a0f9c8357fe5e27a92ecea6cab72110 |
| M04 | solventB / member-domain | solvent_corr | KILLED | 8f8e1c26de6c32b1fbe895033f3f1f170c4321d72d5c7c4503ba8ff1a4a46eb6 | 4013a9c8f2f60012f4c627e2f3cd1a43231e50a7aa0dc3b4ef2b5f91f897378a |
| M05 | solventB / balance | solvent_corr | KILLED | 9c9121732a27c2dcc6b8ac406d8aa1e4909a2785a2927cdb4d591d3f9c306fde | 30c9467c785d0759663b74fe057c7bfcb046399639038105ced2a1afca5333ea |
| M06 | solventB / pledge-domain | solvent_corr | KILLED | a04d2232a65a03cfb54cda3350b997a76c8c9521d06593834a053c81aa77e83f | 64b1bfedf90f66caf281d7035d90f64f2c87559a1cb05c1798061451347d0bf2 |
| M07 | solventB / pledge-amount | solvent_corr | KILLED | 99b8b33c8d42628809cad3e46bcbb1fcbee27bdb918affff5c3dc423d69ad3ca | 0357b17b294ca4ae35bbdb6890f3dc736172a1f7ea7a7332b0698be185218106 |
| M08 | insolventB / existential | insolvent_corr | KILLED | 44aed45653241a3d357eb6d9ab534defdddb11bcc18b1eb635593aa0c08994de | 9c406045d34987ad6444456ba86b57295c262b93e8d9db77acfce62855879449 |
| M09 | uniquePledgesB / user-binding | uniquePledges_corr | KILLED | 54add25fde11c26e7bf6504201fc60095e2419586742a064315748df7a704002 | 8a3264f5834089e98b7c549ef595fb6e380e1ceedaf5de915a4c29cde82d90cf |
| M10 | uniquePledgesB / pledge-equality | uniquePledges_corr | KILLED | 0bebfd84b3b33e4711a7cfb17c40e84924535c3cb97baf88147b3a06439373fe | eccc3cdf644156490fdb2e31a902bd8a322000f1a53dd29e6705a787e900622d |
| M11 | allUniquePledgesB / composition | allUniquePledges_corr | KILLED | ed564643bc2cf061085dca11d59fadf0b260c45cf9d0aefaf2dc6e127a057bdd | 52147508bbf0b67d06017ea12da5476e70a1d27129501d9ea8af1e1d6ecf456b |
| M12 | escrowHeldB / actor | escrowHeld_corr | KILLED | 00bc7267ed8bea7dba066169d9be14de4aff027ee59154991b026a62f8c798c1 | ea08e391ae182863114b4aad2ead2a9bd810ed7b3a03bf8ecb9c9edb2d39d026 |
| M13 | escrowHeldB / amount | escrowHeld_corr | KILLED | 96001b79d50bfbb3107ed72e4a87a3bf818a2131e998c5247afb945f5287235b | 0be688973bdf239ff42bdde855250e27e79d3fcdd98f0b7c358b9ad24f8c1515 |
| M14 | escrowHeldB / absence | escrowHeld_corr | KILLED | a2abbdb100ee4af5aa17d18445878b6c343efa29b00a0a2bf1d3a4277edb877a | 5258da572d00103baca07e2c6a1c6315177e8bb950276d9a7f04413f560df54e |
| M15 | governanceEnactsB / actor | governanceEnacts_corr | KILLED | 97c64f2e53d618addc3824f78866e27a26d9dd25db8e0942c5e94f65343f2f14 | 71f92eaf11e39a98329bf6e30ea7b4880fa42dfa123f352e4cf6b6396c4b5ba6 |
| M16 | governanceEnactsB / all | governanceEnacts_corr | KILLED | 0303cd6ec9d721d719e8a148e27f9cdb4cc2aaf740f4a1476788cbbadd5835d7 | 6ffc5a1821b8cbf6700b4fb8ac866293133353f796959c728755159a52848ad9 |
| M17 | doubleEntryB / conto | doubleEntry_corr | KILLED | 06d061f9d71694fef98324bf4d4871f30a70685789dd257bb9a71d7b8aab91c2 | b4908d5903cc48c7456b82fb743f26588ac19722f3fc14ef68e4de34d45fc3f1 |
| M18 | doubleEntryB / cassa | doubleEntry_corr | KILLED | 467b02b7ce48b4c092a19690368942f85074fdabd74acb6932c426e8ad0b4416 | 4c0beaf945557535effc51b145e62c99855d67bfd97fa18f07ad279d409d0efa |
| M19 | canCloseGroupB / member-balance | canCloseGroup_corr | KILLED | d11bac8cd1c4a84d19372044b7d65f78272ec884e5590f19cf486a7be92d5009 | 6e18ddad7e49c8c46b4ee719f5d6f4ba5834458436487212a887596946ec26fe |
| M20 | canCloseGroupB / collections | canCloseGroup_corr | KILLED | 60c3038687303be584f65ea86fa22dabd64567a4cf3f4638af6ba0778767ce5b | 2bed86068b64b2b34fcc272cbf09bddba741932da9cc769f8ea910ef0d8f3396 |
| M21 | canCloseGroupB / cassa-balance | canCloseGroup_corr | KILLED | 2708d928b25918755cf3715323d09277de60931298f3718307a302f279b7b095 | daefffe36f9a108a84d959b65da7c993e75a72e31be1a18fa30c0358b0852d35 |
| M22 | pendingWellFormedB / nodup | pendingWellFormed_corr | KILLED | 9a2312ad2b9296d3a364c1d22396c2531fe4f958db0941db531420876148ee02 | cfca3a85dc40a22493105a380807f9158d743f2a13a085fb75a72070214dd6bf |
| M23 | pendingWellFormedB / proposer | pendingWellFormed_corr | KILLED | a933ac93c7c3ea340a8950aa1a4fb283cf61976592bd173353933d01057e9d67 | d25a187f41da3c157e14fc2a0a3ca4d055be329c26237e1a1fb0882d54f7fe9f |
| M24 | membersCoherentB / key | membersCoherent_corr | KILLED | 55139230a51444f894060bf4dd0dec4cb0cc11375b3519cf1ea28ed311c10a54 | 2ec7115b30f915fbf08c57655b5b1140d3b1bc1db87db49e7f6f78a6e227111d |
| M25 | pendingCoherentB / composition | pendingCoherent_corr | KILLED | 856e44ff0a6bda0043d88a97ae7b7009713281e106c6eac7c13baeb2e5815fdc | f87f30191bba39e26b631b49cb6eac671ab6feea14908ef4dccfb42bd28f7625 |
| M26 | wellFormedB / members-nodup | wellFormed_corr | KILLED | 253f4c4ccc6c09554f0c2c83ccd1f0d928a17b605c338cb349c2b204057b2c62 | 44f85764ae0bf20d46037fefb3f36df491227d001aa8cb71993f47eb72c7fd59 |
| M27 | wellFormedB / pending-nodup | wellFormed_corr | KILLED | e4ff5b5880f26733287b9a30dc005b04fe729a6cd8d4aa6af6299366a94d5e32 | f6dd06d3a56fd28bd624e44907fdb5c92484097031ecbe9a81c5623d2e333ed5 |
| M28 | wellFormedB / member-coherence | wellFormed_corr | KILLED | 775b156d35dd0963f7bed2a87aed86e14f5d16e10e4b52ae1bfc291e52c5ea3b | d6d5b52616b869bd32113f40bcd445ba7bd4a57ae0d21e3675c94e90e1ec8fb5 |
| M29 | wellFormedB / pending-coherence | wellFormed_corr | KILLED | 6988e3f547779b189b9767445593472f6f008797f6b84066723beb24a6be1d76 | d0e9c04d44518a35856867c5a772110ff4894796eeb6227d0974f63c18cef922 |
| M30 | enactsB / enactment | enacts_corr | KILLED | 1277ee243b337f9d94ee35bd29694df5d07c707a61c6dccd25308a73d843025e | 93dcafbcdbf3f5373c8d18f0740b6f3249050d666b30264aeaa0128a2a0ae0f3 |
| M31 | enactsB / state | enacts_corr | KILLED | a3f066402b73e90bd8e84efd0f44982f1e4c6e4934f9e483223aceddc88b0af7 | d87e3211c361dc9c3910bebb0784b89972674d5f495de773d02f5e4dce336066 |
| M32 | questionCleanB / assents | questionClean_corr | KILLED | bc9e08fa647ea1bb777d7d379b80096974c55a44c6721f68145bcfac37f39c91 | 4d024211d7bda0ba883341789071b1dde468eddeab14e509a331e167106cb7ab |
| M33 | questionCleanB / dissents | questionClean_corr | KILLED | d1230a6b639d47a4fa0be1e472120f598d4106d7baad36dbf905369b9ca3bc32 | f607ab799de9a5b0bf1ffbfcbafb6682b42471676d6e673f5af45a036aee9969 |
| M34 | questionCleanB / disjoint | questionClean_corr | KILLED | 9cbce839aeff32f76c59ab6bd20f755ff7eba7940cd4c50ef77fa6edbdeaf837 | 08e2cdb297fe2d74f92b924837566278cd0fbaa2b6de7b2c753f73568c612efd |
| M35 | sweepReadyB / open-nodup | sweepReady_corr | KILLED | 6f697ee8d85346c4439861630941e804d61762844492630995d25f6a2be2e94c | d156370e4c2093a4973f1ab92a88ef984beb115c305771b7699f6db10ee56865 |
| M36 | sweepReadyB / closed-nodup | sweepReady_corr | KILLED | 5276d2f0ed305b6d56c3036e14de8c2b84f9a62883502061a296e7540d11a53b | 1435af7e3f83a20d6c447e9f83ad63e491b1a52923c5f5226a77375c4bd10980 |
| M37 | sweepReadyB / disjoint | sweepReady_corr | KILLED | e37441a7f284a6059857ea8fd68ef8705d3b17d93145ea9092ba19606a4e9234 | 2e6a6490b0e1423106be069a910a951cf0878f2e38970fdf281043cb6069f468 |
| M38 | sweepReadyB / open-clean | sweepReady_corr | KILLED | 47e16ba5d528fe5311113efbb3963973d7cf86b0ba74a6c277da42bb597a07da | 5e040bafb1e405369e33ad02ad8cfe430fa5a9724bd1f23ea7b831298a92b0da |
| M39 | sweepReadyB / closed-clean | sweepReady_corr | KILLED | 2445526b8b01591acbae12e665d4780362287fc6be61312dfd8bf2fcfbb9bee3 | 0e76354f243fd1c5ebcb2076f941110471cbfc8f94c709964c69ae193ae40fd5 |
| M40 | sweepReadyB / closed-verdict | sweepReady_corr | KILLED | 8e6e73563b638dc765d5f2d0c56cfac8a0c89f6a3e6463ba28d91074a90aa692 | 5f006bfd06ef909261c483ca76ccc7b6088f0cffe8d3969436b4d2464ac27f25 |
| M41 | sweepReadyB / lookup | sweepReady_corr | KILLED | 1dcdad655a0657d81993138f823364a1912ff65aa7b550fda6acedb4ef4095e5 | a32621f9630fca2d0368a68989d592e7441b06865245169c237ab0ce28da42e5 |
| M42 | voteWellFormedB / sweep | voteWellFormed_corr | KILLED | 153936973803403a714ea3b0975608a85c9a9ae819d41de956f3761ca548a3a1 | 3e4067966808c8a09bd0dbc9410ce04cbc344c82dfa95eca091887c13e1056d6 |
| M43 | voteWellFormedB / open-verdict | voteWellFormed_corr | KILLED | 04f2b33091c0de388e359e30e92268a224961f6686262676e067616dcd965a5a | 9a8e6f7af7ff97a4db0b00f627e0e606d0ee8481ec7eb404112e8286078fc524 |
| M44 | voteWellFormedB / threshold | voteWellFormed_corr | KILLED | a2e4ce1fc57f81836597b537f661fb83aedd7e006eceefb49525f02ba8f4efcc | c145bde4abe0cab1b0dc59ee1734d1d63978c834e6857be0107957bdcc82cf4b |

P01 and P07 are separate: their relatum mutants are red, but the executable-body probes E-P01-R/E-P07-R survive. Thus 17/19 correspondence identities have the required executable-definition kill; the two remaining identities have valid proofs and truthful narrower relatum evidence, but do not receive an executable-body KILLED designation. See F02. WIT, E-P01-PROBE and E-P07-PROBE and ACCIDENTAL-SETUP are four counted setup failures (three Lean, one conservatively charged non-Lean), excluded from valid mutant totals. I-NEG is the intended empty-denominator control, not a setup failure.
