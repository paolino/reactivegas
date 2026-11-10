# Data — #70 slice D

No new state. Regime of a pledge of `u` in collection `c` = accepted when `c.accepted` holds a row of `u`, else
pending when `c.pending` holds one (#69 guard table, lookup order as Lean). Compared: per listed pledge (collection,
user, amount, regime label); per offered choice (event tag, collection, user, signer, amount class: raise, lower,
zero) and the core's verdict on the step it emits.
