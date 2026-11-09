# Functions model — #66 S5

No new entry points. Changed behaviour of existing ones:

| entry point | change |
|---|---|
| `scripts/check-reactivegas-inversion-coverage` | prints `tight <theorem> = ok` for every bound inversion (14 on `d9a479c`), and fails naming the theorem whose converse does not hold |
| `scripts/check-reactivegas-inversion-coverage --negative-control` | additionally requires the converse check to reject a conclusion with a guard conjunct removed |
| `scripts/check-lean-mirrors` and its control | exclusion by declaration kind (mirror-auxiliaries slice) |
