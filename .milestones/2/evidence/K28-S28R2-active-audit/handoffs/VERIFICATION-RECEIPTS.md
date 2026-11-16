# Verification receipts — S28-R2 FINAL

Candidate ab25cd11b554bcd5ba64ca56a050c2eb21432d3c. All executions fresh.
11/12 build-class, 22/24 targeted. The exact argv, start time, real exit,
cache state, before/after free-space observations and full hashes are in
evidence/verification-receipts.json. Build duration is the observed gate
phase interval; targeted duration brackets the invoked process. Expected
compiler/test RED exits below are successful controls, not candidate failure.

| ID | Class | Exit | Seconds | Log SHA256 |
|---|---|---:|---:|---|
| [leg3](evidence/20260906T025721Z-ab25cd1-leg3-build.log) | build | 0 | 16.827 | e9263c2f9b8b0a2628fa4deb010cb35c89191ff4f6011c7674ba7f8261e269e8 |
| [leg4](evidence/20260906T025721Z-ab25cd1-leg4-test.log) | build | 0 | 30.102 | 3e098193eaf5c1b166662b478ccbd6ee8cee96c7019f912031466102cdba5db8 |
| [M1](evidence/20260906T025721Z-ab25cd1-leg5-M1-build.log) | build | 1 | 8.786 | 1e03d1efee98688460be613b6482072251718c1c324005d9d795f7f824348882 |
| [M2](evidence/20260906T025721Z-ab25cd1-leg5-M2-test.log) | build | 1 | 33.098 | f978cdf403a5c108670ded87e698c49081e8c79f678fce5599ea0b18c7e69ed8 |
| [M3](evidence/20260906T025721Z-ab25cd1-leg5-M3-test.log) | build | 1 | 26.157 | bd9963ea2230e8c255872ac7a94afe35821f52e3cc13974ff6a1352c0265ed34 |
| [M4](evidence/20260906T025721Z-ab25cd1-leg5-M4-build.log) | build | 1 | 2.295 | ee90a141ea5c8aaefdc9fbe7412c80ad7ee41ea8fd8308ff460cc746e8200704 |
| [M5](evidence/20260906T025721Z-ab25cd1-leg5-M5-test.log) | build | 1 | 27.040 | 627a714529a86baafa627a9e44cec92731dd2a3efd146808130d030998bab371 |
| [M6](evidence/20260906T025721Z-ab25cd1-leg5-M6-test.log) | build | 1 | 29.107 | fce8613f2b1f8534574af7778f5e95192f5eb14e4e9bece277dedd60753c0879 |
| [M7](evidence/20260906T025721Z-ab25cd1-leg5-M7-test.log) | build | 1 | 29.472 | aabf3b4cfeaefa9bd696f80a28010db608c1553441a803ab79e79727d5627820 |
| [M8](evidence/20260906T025721Z-ab25cd1-leg5-M8-test.log) | build | 1 | 27.193 | 841b15e35ac6f26f27a99e27ab1fba5a583578e4acc4a69bb18f45c431b9d3b6 |
| [leg6](evidence/20260906T025721Z-ab25cd1-leg6-ci.log) | build | 0 | 65.128 | 5a9c83ecdba5bbfe065af8f2f547d4ac34a1a6d8023685f871171804bc58587a |
| [P2-compile](evidence/P2-compile.log) | targeted | 0 | 2.564 | 2a9e0aee57598c4337fe7e27d7da554d4a9a657d8a44d8979cdbb802e5527734 |
| [P2-conservation](evidence/P2-conservation.log) | targeted | 0 | 1.585 | ab5c5b9ecb20711814647207ca87be7f7eef39a5614d447fb384eb20bdb83e69 |
| [P2-codec](evidence/P2-codec.log) | targeted | 0 | 0.018 | 9a932299fa33228b17e945ecd0336b5728f5808679fdb4ddcdc59027244a324d |
| [P2-lock](evidence/P2-lock.log) | targeted | 0 | 0.020 | 251b3aee74fec48309d29922d58cd32e007c8d0005f41d29456491b4986f2809 |
| [R1-compile](evidence/R1-compile.log) | targeted | 0 | 2.515 | 1df6128e86d36924785901f19a2fb2cdafde68d390fdb2637b379c1cb15fdc33 |
| [R1-run](evidence/R1-run.log) | targeted | 0 | 0.021 | b73adbf3e17ee97cef7b58ea97ae786d00c8743ee603e644fc89d6b842fd1813 |
| [R3-compile](evidence/R3-compile.log) | targeted | 0 | 2.546 | c9ecbe3a182f71d83a807b5ecbc9e933f4214b878bc644ba742aa4dd53796379 |
| [R3-run](evidence/R3-run.log) | targeted | 0 | 0.032 | 189d618e9d57d5666010d6a975b75f432d39e858e63e3fa420046d3627087e31 |
| [R5-compile](evidence/R5-compile.log) | targeted | 0 | 2.516 | c4cd60a9e6963922bcc109417459f929f94d1eb10c63173d30c0c7255a180d79 |
| [R5-run](evidence/R5-run.log) | targeted | 0 | 0.028 | 23f08058fd47d9069732a623b532bb9ff38f4848617fb934e7da2fb508ce37e6 |
| [MAJ-compile](evidence/MAJ-compile.log) | targeted | 0 | 2.579 | 60aaff4d423655f49550383546b0377ea8291e7eb53a5262da71d1f7f14ab798 |
| [MAJ-run](evidence/MAJ-run.log) | targeted | 0 | 0.021 | e47f3b4c579ed75e687e15412425b0bfc6b6e938df5e445005ff506daebf030b |
| [P3](evidence/P3.log) | targeted | 0 | 3.248 | ffcebc6611f9f2d197163e1fec4a4617972bd11f9e2124f042d397fad8aba203 |
| [P4](evidence/P4.log) | targeted | 0 | 4.390 | 780e8c7756515d483aa1b109028b8df52d154dfaa690b197883fcc1f5db746d0 |
| [P5](evidence/P5.log) | targeted | 1 | 0.018 | 296644b1288336d92c923b5ddc7c017e80b5662610a211e8764f20c00d70cdbd |
| [P6](evidence/P6.log) | targeted | 0 | 0.023 | 4011917b4c980039eb1ee92cd6e3048aa1ca771d1fbb5181a1f7fd60ceacdf3d |
| [P7](evidence/P7.log) | targeted | 1 | 0.020 | 792bbbd71333848e28e5c2bddadbf5b05ec3e04a28461515318eeea733ec673d |
| [TYP-Event](evidence/TYP-Event.log) | targeted | 1 | 1.099 | baf059eb672c558164e1fd09a33fd95735a3c47b7e27239c94af6385a7b3d657 |
| [TYP-Historical](evidence/TYP-Historical.log) | targeted | 1 | 1.136 | 3b8d1549cea592af433d232927cbed0b355249d5f6087578c60d1bc87a145a7d |
| [SC-compile](evidence/SC-compile.log) | targeted | 0 | 4.400 | 7bbf51be693d6dc81f1212c76dac434ea1c0f7625cab4be94c6aaca369d900e6 |
| [SC-negative](evidence/SC-negative.log) | targeted | 1 | 0.040 | 52621197d83c563ece832cae182b56cfcb8f46b8e22c6aa57fbe7520898e2846 |
| [SC-positive](evidence/SC-positive.log) | targeted | 0 | 0.055 | 831f23f2b45b0d4425d7aa3f0361fb0dbc999ab49507691054cffc8da019bedb |

Gate build commands use `nix develop .#ci --quiet -c`: L3 `just build`; L4 `cabal test all -O0 --test-show-details=direct`; M1 `cabal build all --enable-tests -O0`; M4 `cabal build all -O0`; M2/M3/M5/M6/M7/M8 the L4 command; L6 `just ci`. The entry command is unchanged `./gate.sh`, with own evidence/TMPDIR and exclusively reserved gate staging names.

Targeted compile commands use `cabal exec -O0 -- ghc --make -O0` and the frozen argv flags/output directories. TYP commands additionally use `-fno-code`. P2 runs with `+RTS -N2 -RTS`; SC runs the exact named final Hspec example with default RTS settings. Shadow load paths are named by the compilation logs. There are no unjournaled compile/test attempts.
