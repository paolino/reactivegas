#!/usr/bin/env bash
set -euo pipefail
cd /code/reactivegas-66-s4b-audit5/lean
exec lake build KelGroups KelGroups.Event KelGroups.Fold KelGroups.Integration KelGroups.Invariants KelGroups.Mirrors KelGroups.State KelGroups.Tests KelGroups.Types KelGroups.Validate KelGroups.Vote.Event KelGroups.Vote.Fold KelGroups.Vote.Invariants KelGroups.Vote.State KelGroups.Vote.Tests KelGroups.Vote.Types KelGroups.Vote.Validate Reactivegas Reactivegas.Composition Reactivegas.CorpusExport Reactivegas.CorpusGate Reactivegas.Invariants Reactivegas.Mirrors Reactivegas.Predicates Reactivegas.State Reactivegas.Step Reactivegas.Trace Reactivegas.TraceTests Reactivegas.Types
