# Data — #70 G1

| Name | Fields | Rule |
|---|---|---|
| generation config | seeds, traces per seed, steps per trace, invalid share, wall budget | stated in the gate; fixed in CI |
| divergence report | seed, step, event, law, theorem, prefix envelope (`reactivegas.trace` v1) | emitted only on a violation |
