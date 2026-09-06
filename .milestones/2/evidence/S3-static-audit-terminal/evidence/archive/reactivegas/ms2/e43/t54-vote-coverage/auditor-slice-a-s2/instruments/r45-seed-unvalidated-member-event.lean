import KelGroups.Vote.Invariants

namespace AuditR45Seed

open KelGroups KelGroups.Vote

def knownBadValidate (_threshold : Threshold) (_gs : VoteState) (_signer : Key)
    (_event : VoteEvent) : Except VoteError Unit := .ok ()

-- Exact defect seed: a validator that accepts an unauthorised member event
-- must make the R-45 admissibility oracle red.
#guard
  knownBadValidate legacyThreshold emptyVoteState "stranger" (.removeMember "a") ==
    Except.error VoteError.notResponsabile

end AuditR45Seed
