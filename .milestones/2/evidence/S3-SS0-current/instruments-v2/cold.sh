#!/usr/bin/env bash
set -uo pipefail
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
cd /code/reactivegas-66-s3-ss0-scratch/lean || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LAKE: $(pwd)"
exec lake build Reactivegas.Invariants
