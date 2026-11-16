# Verification receipts — S28-R1

Candidate `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`; cwd `/code/kelgroups-audit-3af3d06`. Charges: 10/12 build-class, 16/24 targeted. Raw observer records are preserved in evidence/execution-receipts.jsonl. Build intervals are observed phase wall times; targeted intervals surround the exact process invocation. Cache and free-space samples are in the JSON receipt. No CPU-time claim.

## leg3

- Class: build; exit 0; duration 17150 ms; cold cache.
- Successful positive verification.
- Raw log: [20260906T001909Z-3af3d06-leg3-build.log](evidence/20260906T001909Z-3af3d06-leg3-build.log); SHA256 `bc1ddb117913972438836e06fa78430119702f4931498350eb09aa9e4c59b7ea`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","just","build"]
```

## leg4

- Class: build; exit 0; duration 30237 ms; warm cache.
- Successful positive verification.
- Raw log: [20260906T001909Z-3af3d06-leg4-test.log](evidence/20260906T001909Z-3af3d06-leg4-test.log); SHA256 `d8b8c5f070a58fe4877e63ea84f1db8f80ae4a50092b03feeacae33e595c9583`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## M1

- Class: build; exit 1; duration 15608 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M1-build.log](evidence/20260906T001909Z-3af3d06-leg5-M1-build.log); SHA256 `546e37857a013b66251e9801cd39a802892b5773e9ca27f011c66f74881ab5e2`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","build","all","--enable-tests","-O0"]
```

## M2

- Class: build; exit 1; duration 40472 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M2-test.log](evidence/20260906T001909Z-3af3d06-leg5-M2-test.log); SHA256 `7c980f829a5005f6d59866b901697594b1e93b4961d4faf85f617d1d8dadf618`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## M3

- Class: build; exit 1; duration 32839 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M3-test.log](evidence/20260906T001909Z-3af3d06-leg5-M3-test.log); SHA256 `cb0c9a714754bf2fac4dd99c8b4dc8de59526c7ff5ea0d0167407120489dfc17`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## M4

- Class: build; exit 1; duration 9134 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M4-build.log](evidence/20260906T001909Z-3af3d06-leg5-M4-build.log); SHA256 `5cc516b742bdea84c02aefbcb3e72f7c1159b3e1744ce98f33c9700cf5a12484`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","build","all","-O0"]
```

## M5

- Class: build; exit 1; duration 34276 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M5-test.log](evidence/20260906T001909Z-3af3d06-leg5-M5-test.log); SHA256 `ee43b9725385ff30f0e4893a3e28cdeccc0897ea4b4838cf487205c2dd11b3e1`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## M6

- Class: build; exit 1; duration 35749 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M6-test.log](evidence/20260906T001909Z-3af3d06-leg5-M6-test.log); SHA256 `5e2f8fa9bd2d615eec2cc85ca6de087707a5f815042a8fc3b0bb8cf67bc5b392`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## M7

- Class: build; exit 1; duration 35785 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [20260906T001909Z-3af3d06-leg5-M7-test.log](evidence/20260906T001909Z-3af3d06-leg5-M7-test.log); SHA256 `99e1f62995d543ba3c139cb83462bf55c77fa2be465e1307932021d598a8a44d`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","test","all","-O0","--test-show-details=direct"]
```

## leg6

- Class: build; exit 0; duration 65547 ms; warm cache.
- Successful positive verification.
- Raw log: [20260906T001909Z-3af3d06-leg6-ci.log](evidence/20260906T001909Z-3af3d06-leg6-ci.log); SHA256 `cde7b1119fbbb654d302324e4f897b7e52e4f1adbf533c2327a33fd268b94d08`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","just","ci"]
```

## P1

- Class: targeted; exit 0; duration 2561 ms; warm cache.
- Successful positive verification.
- Raw log: [P1.log](evidence/P1.log); SHA256 `7e7592fffd7741eb49acdf87d9d377b3fad0aaf3d39f5883100b703dc5ca7e67`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/StoreProbe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/store","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/store","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/store/probe"]
```

## P2

- Class: targeted; exit 1; duration 28 ms; warm cache.
- F3 semantic assertion RED after 8/8 concurrency and error controls.
- Raw log: [P2.log](evidence/P2.log); SHA256 `7e9bdb49092e3d9147765ce16d7e35ea50f2ffef0e520ead2943c7d39dfc73a3`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/store/probe","+RTS","-N2","-RTS"]
```

## P3

- Class: targeted; exit 0; duration 3060 ms; warm cache.
- Successful positive verification.
- Raw log: [P3.log](evidence/P3.log); SHA256 `4ae79ce205d4c6611143a61e9671105ef1e5888199d8f1c05fc3088cd0a1477f`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/Row4Probe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4/probe"]
```

## P4

- Class: targeted; exit 0; duration 4254 ms; warm cache.
- Successful positive verification.
- Raw log: [P4.log](evidence/P4.log); SHA256 `dcd8f18f8340e297d883903fba4946818d93a4b84f29fca154da1c7fa0f0ab6d`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-i/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/row4-shadow:lib","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/Row4Probe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4-mutant","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4-mutant","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4-mutant/probe"]
```

## P5

- Class: targeted; exit 1; duration 17 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [P5.log](evidence/P5.log); SHA256 `296644b1288336d92c923b5ddc7c017e80b5662610a211e8764f20c00d70cdbd`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4-mutant/probe","witness"]
```

## P6

- Class: targeted; exit 0; duration 17 ms; warm cache.
- Successful positive verification.
- Raw log: [P6.log](evidence/P6.log); SHA256 `4011917b4c980039eb1ee92cd6e3048aa1ca771d1fbb5181a1f7fd60ceacdf3d`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4/probe","witness"]
```

## P7

- Class: targeted; exit 1; duration 22 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [P7.log](evidence/P7.log); SHA256 `bd122075c81af7dcaa92aa74ade4c1a21bb5d5a23e895ca80c44dba8f2a15229`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/row4-mutant/probe","--match","S28-1 direct-only admission"]
```

## R1-compile

- Class: targeted; exit 0; duration 2517 ms; warm cache.
- Successful positive verification.
- Raw log: [R1-compile.log](evidence/R1-compile.log); SHA256 `8eee1f52c657445c79b97977d4a04e115e873d39ea0031781e7a10335c633e61`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/TraceProbe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R1","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R1","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R1/probe"]
```

## R1-run

- Class: targeted; exit 0; duration 22 ms; warm cache.
- Successful positive verification.
- Raw log: [R1-run.log](evidence/R1-run.log); SHA256 `b73adbf3e17ee97cef7b58ea97ae786d00c8743ee603e644fc89d6b842fd1813`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R1/probe","R1"]
```

## R3-compile

- Class: targeted; exit 0; duration 2511 ms; warm cache.
- Successful positive verification.
- Raw log: [R3-compile.log](evidence/R3-compile.log); SHA256 `47439e7371dc894d68ddc3fc41b749255e7dfff693249575affe84edf651d2cd`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/TraceProbe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R3","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R3","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R3/probe"]
```

## R3-run

- Class: targeted; exit 0; duration 36 ms; warm cache.
- Successful positive verification.
- Raw log: [R3-run.log](evidence/R3-run.log); SHA256 `189d618e9d57d5666010d6a975b75f432d39e858e63e3fa420046d3627087e31`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R3/probe","R3"]
```

## R5-compile

- Class: targeted; exit 0; duration 2546 ms; warm cache.
- Successful positive verification.
- Raw log: [R5-compile.log](evidence/R5-compile.log); SHA256 `c541eb13e6755fe520cc2f052cc801896a3c9a49f9d7866520486c2f231be924`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/TraceProbe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R5","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R5","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R5/probe"]
```

## R5-run

- Class: targeted; exit 0; duration 29 ms; warm cache.
- Successful positive verification.
- Raw log: [R5-run.log](evidence/R5-run.log); SHA256 `23f08058fd47d9069732a623b532bb9ff38f4848617fb934e7da2fb508ce37e6`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/R5/probe","R5"]
```

## MAJ-compile

- Class: targeted; exit 0; duration 2512 ms; warm cache.
- Successful positive verification.
- Raw log: [MAJ-compile.log](evidence/MAJ-compile.log); SHA256 `d95a0f2eb3c547d37b9a6f794ee4f9ecc19c0df312772c311a60b1a7f6fb91c2`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/TraceProbe.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/MAJ","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/MAJ","-o","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/MAJ/probe"]
```

## MAJ-run

- Class: targeted; exit 0; duration 23 ms; warm cache.
- Successful positive verification.
- Raw log: [MAJ-run.log](evidence/MAJ-run.log); SHA256 `e47f3b4c579ed75e687e15412425b0bfc6b6e938df5e445005ff506daebf030b`.
- Exact argv (JSON, not shell-escaped):

```json
["/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/MAJ/probe","MAJ"]
```

## TypeNegative

- Class: targeted; exit 1; duration 1111 ms; warm cache.
- Intended type or semantic negative control.
- Raw log: [TypeNegative.log](evidence/TypeNegative.log); SHA256 `850adff57c921c346d809ad7ec73b6346bf9bc85b3eedc03a4a9b693bb2cf1c2`.
- Exact argv (JSON, not shell-escaped):

```json
["nix","develop",".#ci","--quiet","-c","cabal","exec","-O0","--","ghc","--make","-O0","-threaded","-XGHC2021","-XOverloadedStrings","-XDerivingStrategies","-XLambdaCase","-XStrictData","-package","kelgroups","-itest","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/TypeNegative.hs","-odir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/type-negative","-hidir","/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/handoffs/build/type-negative","-fno-code"]
```

