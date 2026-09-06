# Changelog

## [2021.13.0](https://github.com/paolino/reactivegas/compare/v2021.12.0...v2021.13.0) (2026-09-06)


### Features

* **54:** compose economic and vote events ([c50f527](https://github.com/paolino/reactivegas/commit/c50f5275a42453ebc87a0c7011b3d8470fba4006))
* **54:** compose economic and vote events ([c8c4dd8](https://github.com/paolino/reactivegas/commit/c8c4dd8903cca817c814e9f84e9ff21ceba2de0c))
* **54:** slice-A GREEN — discharge Vote.Invariants over foldVote ([757dac9](https://github.com/paolino/reactivegas/commit/757dac98aecce705e44eda6c9283a5da01b02827))
* **54:** slice-A RED — required vote machine with unproved proof obligations ([cb81a19](https://github.com/paolino/reactivegas/commit/cb81a194f499be8ca750c978d9985f4ac960ef7e))
* implement four-arm money custody ([#90](https://github.com/paolino/reactivegas/issues/90)) ([#95](https://github.com/paolino/reactivegas/issues/95)) ([890a74f](https://github.com/paolino/reactivegas/commit/890a74f1c4c34b52c55b5d941c78c94fa504e005))
* **lean:** emit versioned conformance traces ([719eb56](https://github.com/paolino/reactivegas/commit/719eb56bd5edb47265330ff3034b7b255796864d))
* **lean:** export verified Lean trace corpora ([#87](https://github.com/paolino/reactivegas/issues/87)) ([d670323](https://github.com/paolino/reactivegas/commit/d67032313acf3699cc50358a057391b88d002192))
* **lean:** prove conservation, solvency, and authorization ([0693055](https://github.com/paolino/reactivegas/commit/0693055557746cf47cfd192787cf084d83da45cd))
* **lean:** S4-B Prop/Bool mirrors, correspondence proofs and mandatory checker ([#66](https://github.com/paolino/reactivegas/issues/66) S4-B) ([189e1ed](https://github.com/paolino/reactivegas/commit/189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8))
* **lean:** submission-2 Boundary-1 design — exe build in-run, CorpusExport import, tracked-set ownership, import-reach completeness (S4-B NOTE-015) ([4d0a324](https://github.com/paolino/reactivegas/commit/4d0a324068d4ee367595adf1c68d45133cab6b12))
* **lean:** submission-2 repair — total discovery classification with fail-closed kinds, P01 promotion table (S4-B F01/v3.1) ([59309d6](https://github.com/paolino/reactivegas/commit/59309d668966206df6b01a7e9027614f79e52e5f))


### Bug Fixes

* **54:** slice-A repair — franchise no-op, partition history, named cast ([c433ff7](https://github.com/paolino/reactivegas/commit/c433ff769fc35329050411054324c19b5b100fdb))
* **57:** structurally validate vote events ([13b44bc](https://github.com/paolino/reactivegas/commit/13b44bcb89567596c8b0d953838b1500ece1f4ef))
* **build:** wire shipped corpus gate into tracked ci ([2d9e544](https://github.com/paolino/reactivegas/commit/2d9e544287cfee431c42bbdab2a189fd09b644ca))
* **lean:** align declared and running toolchains ([d7a3e05](https://github.com/paolino/reactivegas/commit/d7a3e05116f40920f3d78daf3e1818ad17c74a74))
* **lean:** align declared and running toolchains ([ebe8cdf](https://github.com/paolino/reactivegas/commit/ebe8cdf468b697cf51873edcbd0fa90142e7a161))
* **lean:** checker driver block structure — top-level defs precede run_cmd; comma+bracket tracked array literal (S4-B O1 findings) ([b667648](https://github.com/paolino/reactivegas/commit/b667648752b8fa8a7b890f115413a99ba04518dc))
* **lean:** checker driver imports CorpusGate and TraceTests closing import-reach gap (S4-B O1retry finding) ([94bb7bb](https://github.com/paolino/reactivegas/commit/94bb7bb64324a48f7361252556b4d15e45b3923f))
* **lean:** close event inversion coverage ([e6c5924](https://github.com/paolino/reactivegas/commit/e6c59242ccf9b388053626c24446faaa2d7417fd))
* **lean:** close event inversion coverage ([574bbf6](https://github.com/paolino/reactivegas/commit/574bbf68030a7aede56345c0bd1d823d869bb67b))
* **lean:** close S62-A audit findings on the production root ([9adf2e1](https://github.com/paolino/reactivegas/commit/9adf2e15592d36ec8008272bbd3c204ad9bd8daf))
* **lean:** close S62-C recut proof gaps ([2263e17](https://github.com/paolino/reactivegas/commit/2263e179cb8fa40cf0930a96f990dfb456e41a76))
* **lean:** kill reached-duplicate and serialized trace gaps ([52206d6](https://github.com/paolino/reactivegas/commit/52206d6829a31ff9f634af4de549020596829951))
* **lean:** promotion mention-check harvests hypotheses as well as conclusion (S4-B NOTE-013 defect 1) ([0f3ad01](https://github.com/paolino/reactivegas/commit/0f3ad01a447f40a23eb282ff5b4a8adc2b303ca1))
* **lean:** resolve accepted inversions by unqualified name; agree coverage instruments ([#66](https://github.com/paolino/reactivegas/issues/66) S1) ([#79](https://github.com/paolino/reactivegas/issues/79)) ([4a6cd87](https://github.com/paolino/reactivegas/commit/4a6cd87fcbc3e4a536bbc9f240f5efe5704022af))
* **lean:** total axiom gate over discovered extent with resolved-olean ownership ([#66](https://github.com/paolino/reactivegas/issues/66) S2R) ([#88](https://github.com/paolino/reactivegas/issues/88)) ([3590c00](https://github.com/paolino/reactivegas/commit/3590c0015b84fd58004bf6fb44dd18b107304c48))
* **lean:** tracked-list substitution pattern must not anchor with #-prefix (S4-B O1retry finding) ([ba62366](https://github.com/paolino/reactivegas/commit/ba62366766aeb72c988c1f5418a54907c425ac14))
* **release:** enforce exact artifact identity ([#56](https://github.com/paolino/reactivegas/issues/56)) ([2f3ed76](https://github.com/paolino/reactivegas/commit/2f3ed76243692736e71cd31431c22aaf49240c3c))

## [2021.12.0](https://github.com/paolino/reactivegas/compare/v2021.11.5...v2021.12.0) (2026-08-26)


### Features

* add GitHub Pages deployment workflow for docs ([6698347](https://github.com/paolino/reactivegas/commit/669834721137868cba171e6ccc5f7fc6ff420596))
* add lean state machine core (types, state, step, predicates) ([7ebdc42](https://github.com/paolino/reactivegas/commit/7ebdc427a7f6c6703b2fbacdf58793530c7ec857))
* add mkdocs documentation ([6655257](https://github.com/paolino/reactivegas/commit/66552576c53dec2841b4594d36f40448ce2f75c0))
* add workflow to build and upload artifacts ([1e406f1](https://github.com/paolino/reactivegas/commit/1e406f1305cc08c341d7fb2dce0638ccd603b2cc))
* enforce solvency in the lean state machine (L7) ([cb0cefd](https://github.com/paolino/reactivegas/commit/cb0cefd008c59a36ed37ff85eafc34d2f7c9d9ac))
* Lean state machine of the legacy economic laws ([771b3c0](https://github.com/paolino/reactivegas/commit/771b3c0b7ed083cf5b3c7778a02df274ab7eab83))
* prove law invariants over the lean state machine ([de01fa8](https://github.com/paolino/reactivegas/commit/de01fa817d6933370698646cd1aae3fe0438658b))
* publish the first provisional release pipeline ([#52](https://github.com/paolino/reactivegas/issues/52)) ([25ff6cd](https://github.com/paolino/reactivegas/commit/25ff6cdeddf23b97f5a57bb69c44b1d96ceddbc0)), closes [#51](https://github.com/paolino/reactivegas/issues/51)
