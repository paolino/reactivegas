# Build Ledger

- Audit: issue #57, S57-A, submission 2
- Candidate: `9d68abb0930bb31d9bcd1116979765e974547ffd`
- Budget: 20 build/instrument/gate invocations
- Spent: 2

| # | Kind | Command / purpose | Cache | Exit | Evidence |
|---:|---|---|---|---:|---|
| 1 | immutable gate v3 | `gate-s57-a.sh.v3 /code/reactivegas-issue-57-audit-s57-a-s2` | cold | 0 | `evidence/independent-gate-v3.log` sha256 `c5e38f000a9849cbd15ab460e5f83d6be3809e9eca75fd063566b8e590c425eb`; 105.342s; 644709 bytes; free space 153909276672 → 152180793344 bytes |
| 2 | static architecture instrument | rejected-vs-candidate F-001 seed, effect-region scan, six arms, validator `.ok` premise and call site | n/a | 0 | `evidence/probe-f001-architecture.log` sha256 `d22261ff926263e46f8be3e72817f53ecce3982e4b8683878ecac59fcbda0192`; instrument sha256 `dc737c8de52bdcc0a5447fee806ab2eb23c963ba549af48b81ffb6c0dcfae5df`; 75ms |
