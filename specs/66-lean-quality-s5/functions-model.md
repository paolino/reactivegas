# Functions model — #66 S5

No new entry points. Changed behaviour of existing ones:

| entry point | change |
|---|---|
| `scripts/check-reactivegas-inversion-coverage` | prints `tight <theorem> = ok` for every bound inversion (14 on `d9a479c`), and fails naming the theorem whose converse does not hold |
| `scripts/check-reactivegas-inversion-coverage --negative-control` | additionally requires the converse check to reject a conclusion with a guard conjunct removed |
| `scripts/check-lean-mirrors` and its control | exclusion by declaration kind (mirror-auxiliaries slice) |

Finite-history slice (new; names are the commit owner's, roles are fixed):

| role | arguments | result |
|---|---|---|
| integrated history validator | `θ : KelGroups.Vote.Threshold`, `auth : BackdonateAuth`, `initial : KelGroups.GroupState State`, `steps : List VoteTraceStep` | `Bool` |
| fixed-view history validator | `view : KelGroups.GroupView`, `auth : BackdonateAuth`, `initial : State`, `steps : List TraceStep` | `Bool` |
| corpus validation entry run by the hosted step | the bytes of `vote.json` and `vote.context.json` | exit status, one line per check and per negative control |
