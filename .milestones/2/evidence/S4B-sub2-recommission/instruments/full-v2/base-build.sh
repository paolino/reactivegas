#!/usr/bin/env bash
set -euo pipefail
cd /tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2/worlds/S09/lean
exec lake build KelGroups KelGroups.Event KelGroups.Fold KelGroups.Integration KelGroups.Invariants KelGroups.State KelGroups.Tests KelGroups.Types KelGroups.Validate KelGroups.Vote.Event KelGroups.Vote.Fold KelGroups.Vote.Invariants KelGroups.Vote.State KelGroups.Vote.Tests KelGroups.Vote.Types KelGroups.Vote.Validate Reactivegas Reactivegas.Composition Reactivegas.CorpusExport Reactivegas.CorpusGate Reactivegas.Invariants Reactivegas.Predicates Reactivegas.State Reactivegas.Step Reactivegas.Trace Reactivegas.TraceTests Reactivegas.Types
