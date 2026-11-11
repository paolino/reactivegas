#!/usr/bin/env bash
set -uo pipefail
cd /code/reactivegas-66-s3-ss0-scratch/lean
exec lake build Reactivegas.Invariants
