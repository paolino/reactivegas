# Data model — #81

Ceiling 40 lines / 2500 bytes. No type changes (INV81-TYPES); new produced values only.

| ID | Rule |
|---|---|
| D-1 | A V-5 closure record = `{ questionId, question as it stood, verdict := .negative, cause := .renounced \| .proposerDeparted }`, appended to `closed`; the id leaves `openQuestions`. |
| D-2 | Departure closes exactly the open questions whose `proposer` equals the removed key; their relative order in `closed` follows `openQuestions` order. No other question is touched by D-2. |
| D-3 | In one departure transition `closed` grows by the D-2 records followed by the V-3 sweep records over the post view; each record carries its own cause. |
| D-4 | Renounce closes exactly the named question; the subsequent sweep runs under the unchanged view. |
| D-5 | A refused event leaves `VoteState` (and the integrated `State`) unchanged and is reported as an error. |
| D-6 | Questions already in `closed` are never re-closed or revived. |
