# Reactivegas M2 active-lane role substitution — 2026-09-06

Operator authority:
- Stop using Claude Opus.
- Use Codex Sol for the ticket-owner/supervisor role.
- Start no new ticket. Finish only work already in flight.

This replaces control seats, not campaigns. Candidate identities, audit attempts, counters, frozen evidence, rejected findings, and wake conditions survive unchanged. Existing non-Opus implementation and audit seats are not restarted merely because their commissioner changes.

In-flight product lanes authorized to continue:
- #92 quality integration
- PR #94 / #70 simulator
- #76 runtime composition
- #68 proposer assent
- #71 design record
- KelGroups #30 S30-2D

Terminal lanes are retired. Prepared/unstarted #69, #75, #81, #82, #83, and #84 remain parked. Voci remains milestone 3.

Direct Opus replacements:
- quality %503 -> fresh Codex Sol quality supervisor
- simulator %313 -> fresh Codex Sol simulator supervisor
- proposer %512 -> fresh Codex Sol ticket supervisor
- design %516 -> fresh Codex Sol ticket supervisor
- KelGroups epic %532 -> fresh Codex Sol epic supervisor

The fresh KelGroups epic supervisor must replace its immediate Opus ticket owner %634 with Codex Sol while preserving live Astra auditor %635 unchanged, and retire terminal Opus %572. Terminal Haskell %504 and obsolete desk companion %591 require no successors.
