#!/usr/bin/env bash
set -uo pipefail
cd /code/reactivegas-66-s3-ss0-scratch/lean
exec lake env lean /tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0/instruments/Check.lean
