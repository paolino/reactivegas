# Data — #70 S5-sim-a

| Name | Fields | Rule |
|---|---|---|
| core pin | `commit` (40-hex), `tree` (40-hex), `files` (unique event source) | `commit` is the last commit reachable from `HEAD` changing a path under the vocabulary roots; `tree` is that commit's tree; `files` unchanged (`lean/Reactivegas/Types.lean`); `commit` is an ancestor of `HEAD` |
