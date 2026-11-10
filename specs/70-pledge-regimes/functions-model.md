# Functions — #70 slice D

| ID | Signature | Constraint |
|---|---|---|
| F-PAGE-PLEDGES | `pagePledges(page) -> [{ c, user, amount, regime }]` | reads the built page's display only |
| F-PAGE-OFFERS | `pageOffers(page, c, user) -> [{ tag, signer, enabled, amountClass }]` | reads the dialogs' offered choices only |
| F-CORE-VERDICT | `coreVerdict(state, signer, step) -> accepted \| refusal` | the embedded core's step, no second guard |
