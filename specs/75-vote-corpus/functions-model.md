# Functions model — #75 (new/changed signatures only)

Names are indicative of responsibility; the commit owner may rename if the
mandate's meaning is kept and the receipt maps old → new.

- `voteCorpusInitial : KelGroups.GroupState State`
- `voteCorpusEvents : List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent)`
- `emitVoteCorpus (θ : KelGroups.Vote.Threshold) : List VoteTraceStep`
  — generation calls it with `legacyThreshold`.
- `voteCorpusCoversDiscovered (steps : List VoteTraceStep) : Except String Unit`
  — constructor extents discovered from the environment.
- `validateTable (table : Json) : Except TableRefusal (List (Nat × Nat))`
- `replayVoteCorpus (corpusBytes sidecarBytes : String) : IO ReplayOutcome`
  — `ReplayOutcome` has exactly the three failure outcomes plus success;
  effect: may write diagnostics, never a verdict on abort.
- `checkPolicyAgainstDeclaration (sidecarBytes : String) : Except String Nat`
  — evaluates `KelGroups.Vote.legacyThreshold` per domain point.
- `sourceInputDigest (paths : List String) : IO String`
- exporter `main` gains argument forms for: writing the third file and its
  sidecar; live-checking them; replaying a corpus+sidecar pair. Existing
  argument forms unchanged in behaviour and output bytes.
