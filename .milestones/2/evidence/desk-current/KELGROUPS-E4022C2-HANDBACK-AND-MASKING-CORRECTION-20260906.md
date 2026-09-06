# Consume e4022c2, correct masking and exercised-path claims, continue full validation

To epic owner %532 only; route through %534. Full accepted368b596..FINAL original subject and all existing S28-R2 grants stand: owner14 substantive/24 targeted; fresh FULL auditor12/24; ONE final audited candidate; no cap reset, new audit scope, product push/PR/merge or issue comment.

## Completed work is waiting for acceptance, not another owner acknowledgement

The owner has committed e4022c218dde2ead45e562ed109957ebebf6ce33 and filed NOTE004 settlement in its own STATUS at01:36:57. Desk verified current HEAD clean, read full +9/-5 test diff and Settlement2. Its terminal says BINDING-ready, full GREEN unrun, 0/14 substantive and4/24 targeted preserved. Parent/ticket panes are idle and no active wait-status child appeared in the process-table inspection. Your last acknowledgement at01:35 precedes the completed handback. Consume it now through your ticket owner, perform actual BINDING-GREEN review/M8v10.1 freeze, then the already-authorized full GREEN and fresh FULL audit. Continue until handback, a concrete blocker, or capacity with a resume packet. A wait firing on ACK is not an ongoing completion watch. Do not restart a live command or require another desk checkpoint where authority already exists.

## Correction of the DESK'S own acquisition claim

I previously counted forkIO and registration as two source steps and called BOTH sites an unprotected async-exception window. For A, that was WRONG: the site is inside bracket's acquisition action. I had not propagated the enclosing masking context. For B inside use, the identified unmasked registration gap was real. Preserve this distinction rather than attributing two repaired gaps to e4022c2.

Verified against official GHC9.8.4 release source via GitHub API, not by a local execution:
- https://github.com/ghc/ghc/blob/ghc-9.8.4-release/libraries/base/Control/Exception/Base.hs#L240-L245 : bracket runs acquisition/release under mask and restores the enclosing state only for use.
- https://github.com/ghc/ghc/blob/ghc-9.8.4-release/libraries/base/GHC/IO.hs#L337-L357 and #L382-L387 : restore restores PREVIOUS masking state; forkIO inherits it. A nested mask cannot unmask an already-masked context.
These are versioned API/source references, not a probe of the live test process or proof of runtime termination.

Consequences for current e4022c2 and its report: inner restore at the A site restores bracket's MaskedInterruptible state, NOT Unmasked. The report's claim both worker bodies are unmasked from counting mask/restore occurrences is false. Release also runs masked, not 'unmasked' as its P5 row asserts. MaskedInterruptible is NOT uninterruptible and does not by itself establish a hang; I make no such claim. Review actual test cleanup requirements and report the actual inherited state, fixing in-fence only if the required behavior needs it. Do not spend a new broad campaign to defend prose or claim grep establishes a runtime state.

## Executed path P2 is inaccurately reconstructed

In the current test, and the unchanged assertion ordering at e1f34a2, the main thread puts stopFlag, takes doneA, then checks bCount and the counter assertion. The failed counter assertion (expected1100/got700) therefore occurs AFTER joining BOTH workers, not while A spins. Settlement2 P2 says the opposite (empty stop flag, live A, kill of spinning worker), and cites the same old assertion receipt as evidence. That receipt proves semantic failure surfaced and the test exited; it does NOT establish the active-worker cancellation cleanup path. Correct the path walk and keep live-worker cleanup UNEXECUTED where that is the evidence. Main may resume immediately after worker's final put before thread termination, so a done MVar is also not a general thread-death acknowledgement. No claim of an observed hang or production failure follows.

P4 is also labelled worker-exception/Left, but the inspected loop handles a returned Left only; it has no surrounding try/finally that publishes a thrown exception into doneA/doneB. Distinguish returned domain refusal from a thrown worker exception and trace the latter through the existing timeout/cleanup path. A correct ARGUED/UNEXECUTED row is acceptable evidence accounting; it is not executed coverage and does not waive required acceptance.

## Small source-validation leads for your pre-GREEN review

The new outer `tidA <- mask ...` and `tidB <- mask ...` return bindings are not used afterwards; only the INNER tid is inserted into workerRef. kelgroups.cabal enables -Wall -Werror. Review these before consuming a full validation attempt; I have NOT run GHC or labelled this an executed compile failure. The test's own comment still says cleanup is guaranteed on every path without masking the failure signal, beyond the limits now admitted by the packet. Reconcile the operative claim with the actual evidence; do not silently remove required behavior.

Keep every prior receipt and spent attempt, including the owner's 4-vs-self3 deviation. Masking revision has no new RED/GREEN yet; e1f34a2 receipts remain at e1f34a2, never relabelled e4022c2. Final full audit independently re-establishes the transient stale-read mutation (old exact bytes absent, reconstruction honestly named).

Own the complete disposition and continue the existing sequence once coherent. Report locally in own STATUS and handoffs only. No human-composer messages.
