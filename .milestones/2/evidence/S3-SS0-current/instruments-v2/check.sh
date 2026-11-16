#!/usr/bin/env bash
set -uo pipefail
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
cd /code/reactivegas-66-s3-ss0-scratch/lean || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LEAN: $(pwd)"
exec lake env lean /tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0/instruments/Check.lean
