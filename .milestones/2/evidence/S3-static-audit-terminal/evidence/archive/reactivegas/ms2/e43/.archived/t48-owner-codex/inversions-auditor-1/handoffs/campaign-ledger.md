# T48-INVERSION-10 audit campaign ledger

- Submission: 1
- Candidate: `db76fa699d91764c5de3d1ec456d6291d7189b36`
- Base: `719eb56bd5edb47265330ff3034b7b255796864d`
- Rule: append-only; all seven mandate rows are BLOCKING.

| Invariant | Severity | State | Evidence |
|---|---|---|---|
| I10-FENCE | BLOCKING | OPEN | Pending independent declaration-inventory mutant. |
| I10-TYPES | BLOCKING | OPEN | Pending bundled exact-type weakening mutant. |
| I10-COVERAGE | BLOCKING | OPEN | Pending live reconciliation and can-fail control. |
| I10-SEMANTICS | BLOCKING | OPEN | Pending four-class bundled weakening mutant. |
| I10-ADDITIVE | BLOCKING | OPEN | Pending base-to-candidate and unrelated-declaration controls. |
| I10-DEBT | BLOCKING | OPEN | Pending inherited-debt/static negative controls. |
| I10-REGRESSION | BLOCKING | OPEN | Pending emitter/runtime/CI and matcher negative control. |

## Events

- OPEN — campaign initialized before audit conclusion; builds `0/2`.

- KILLED I10-FENCE — `auditLocaleDeclarationInventoryControl` under
  `en_GB.utf8` was rejected solely by the complete declaration inventory;
  receipt `e3e82fdb8d323eb2bb6ba980eac0d6a1fa90e95c9f91986b18c4088273375929`.
- KILLED I10-COVERAGE — `step_addUser_nearMiss_inv` drove the live reconciler
  to `ctors=18 covered=17 missing=1`;
  receipt `82d2b625533649712155139e2c17b80a899fce01eead9c5620948b99036db155`.
- KILLED I10-DEBT — `axiom auditDebtControl : True` was rejected by the
  axiom/opaque/native_decide fence;
  receipt `33da268c0759de1c700b92b6f65c89a961bbd73f72c60d9ec562c6717cb571c1`.
- KILLED I10-TYPES and I10-SEMANTICS — bundled compiling mutant weakened
  `step_addUser_inv` to author-only, `step_withdraw_inv` to affordability-only,
  `step_removeResponsabile_inv` to guard-only, and `step_backdonate_inv` to
  author/positive-only (dropping affordability, authorization, and post-state).
  The full build, 52 inherited types, live reconciliation, emitter contract,
  and CI stayed green; the exact ten-type probe made gate v4 RED. Mutant diff
  `a9f3a5d7334b756eb7c430ef13d2bab68aa697ea705ebc159dbd3a2e8802dc02`;
  gate receipt `7c9df433316ce3686f110da27b10976d1384189f5d448501701719078618558e`.
- KILLED I10-ADDITIVE — harmless `Step.lean` path escape was rejected solely by
  the one-path fence; receipt
  `5848f2cb5fa99795e82f4cee06fb9403c242554c1837e1d98591f2380fb9c275`.
- KILLED I10-REGRESSION — frozen matcher control accepted `34/34` and five
  envelopes, then rejected the `33/34` plus four-envelope mutant; instrument
  `dbe2684410b3f51a3fa27dd9f700107cda7477b4cf4f7e0a89c902032367f764`,
  receipt `ef97c1769e3cb78e96e10800f8c3bce27ccae92c5e73ad226c8628d1c2698eca`.
- SET-POINT — all seven BLOCKING rows terminal as KILLED; killed `7`, residual
  `0`, blocked `0`, open `0`; builds `2/2`.
- GREEN — exact candidate final full gate exit `0`, `failures=0`, receipt
  `faaab9a9715c11710ec16cf647193104df5c2c772a2951cf609d1c65ae78f6f6`.
