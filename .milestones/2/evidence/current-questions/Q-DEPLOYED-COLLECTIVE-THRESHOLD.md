# Pending operator choice: deployed collective-vote policy

Asked asynchronously at 2026-09-06T09:58:23.919Z. The model retains Threshold as a parameter and has no shipped default. Verified at accepted source lean/KelGroups/Vote/Types.lean: legacyThreshold(n)=(n+1)/2, zeroThreshold(n)=0. The threshold is consulted only by collective questions, not per-designee permission questions (State.lean verdictOf).

Question: require an explicit policy at coordinator setup without a default, ship the legacy rule (2 of4), or ship strict majority (3 of4). Explicit setup policy was recommended. NO ANSWER YET. No default, implementation scope change or semantic ruling is inferred from silence. Existing #76 and #30 parameterized work continues.
