#!/usr/bin/env python3
"""Auditor-local document mutants; exact v6 checker, never tracked edits."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

ROOT = Path('/tmp/reactivegas/ms2/t71-design-record/audit-b1')
TREE = Path('/code/reactivegas-issue-71-audit-b1')
OUT = ROOT / 'evidence' / 'doc-controls'
OUT.mkdir(exist_ok=True)
GATE = '877d9b9c596c216f688bf32c5f31ba29ad1b56753bb9ae083cf52007cdaf9085'
source = (TREE / 'docs/en/design/state-machine.md').read_text()

def replace(old, new):
    assert source.count(old) == 1, (old, source.count(old))
    result = source.replace(old, new)
    assert result != source and new in result
    return result

def cut(start, end):
    assert source.count(start) == source.count(end) == 1
    a, b = source.index(start), source.index(end)
    assert a < b
    result = source[:a] + source[b:]
    assert start not in result and end in result
    return result

cases = [
 ('B01-USERS', 'R71-01', replace('| field | contents |', '| field | contents |\n| `users` | invented member store |'), 1, 'still list users'),
 ('B02-EVENTS', 'R71-02', replace('**14 event constructors**', '**15 events**'), 1, 'still claim 15 events'),
 ('B03-STEP', 'R71-03', replace('def step      (view : GroupView) (s : State) (signer : Key) (app : AppEvent)', 'step : State → Event → Option State'), 1, 'stale 2-arg'),
 ('B04-TENSION', 'R71-04', cut('Two honest tensions the reader must not miss.', '## Route and the sealed base hook'), 1, 'honesty-tension'),
 ('B05-WITNESS', 'R71-05', replace('## Laws versus finite witnesses', '## Classification'), 1, 'no law/witness heading'),
 ('B06-LINK', 'R71-06', replace('**Reachability.**', '**Connection.**'), 1, 'unbound reachability'),
 ('B07-THETA', 'R71-07', replace('The vote threshold policy θ is open.', 'The vote threshold policy θ is settled.'), 1, 'theta-open sentence missing'),
 ('B08-VOCI', 'R71-08', replace('21 `Voci/` paths', 'twenty `Voci/` paths'), 1, 'stale twenty-modules count'),
 ('B09-DATES', 'R71-09', source.replace('2026-08-26', 'DATE').replace('2026-08-27', 'DATE').replace('2026-09-05', 'DATE'), 1, 'lack dated operator rulings'),
 ('B10-PENDING', 'R71-10', '\n'.join(line for line in source.split('\n') if not line.startswith('| V-5 lifecycle (#81)')), 1, 'pending anchor #81'),
 ('B11-UNCITED', 'R71-11', source + '\nAn additional `bal` claim without a citation.\n', 1, 'CLAIM-RED'),
 ('B12-CLOSURE', 'R71-12', replace('**Verdict: missing guarantee.**', 'theorem canCloseGroup_holds is proved and preserved.\n**Verdict: missing guarantee.**'), 1, 'invent a group-closure theorem'),
 ('F11-WRONG-DECL', 'R71-11', source + '\nThe substrate `KelGroups.Proposal` is cited here (`lean:Reactivegas/Types.lean:Proposal`).\n', 0, 'GATE-v6-GREEN'),
 ('F11-SOLE-MARKER', 'R71-11', replace('| `grantPermission` / `denyPermission` | `lean:pullCollection` must succeed', '| `grantPermission` / `denyPermission` | pullCollection must succeed'), 0, 'GATE-v6-GREEN'),
 ('CTRL-ORIGINAL', 'CONTROL', source, 0, 'GATE-v6-GREEN'),
]

def clean():
    assert subprocess.check_output(['git', 'status', '--porcelain=v1', '--untracked-files=no'], cwd=TREE) == b''
    assert hashlib.sha256((TREE / 'gate.sh').read_bytes()).hexdigest() == GATE
    assert subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=TREE).decode().strip() == '8e4cbb8b95ac5a2063ea39cf2d2ac6a4c1d15163'

results = []
for ident, row, document, wanted_rc, reason in cases:
    clean()
    if row != 'CONTROL':
        assert document != source
    path = OUT / (ident + '.md')
    path.write_text(document)
    assert path.read_text() == document
    env = dict(os.environ, SKIP_CI='1', REPLAYS='0', DOC_SM_OVERRIDE=str(path))
    start = time.monotonic()
    run = subprocess.run(['bash', './gate.sh'], cwd=TREE, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    log = OUT / (ident + '.log')
    log.write_bytes(run.stdout)
    clean()
    record = dict(id=ident, row=row, exit=run.returncode, expected=wanted_rc, reason=reason,
                  duration_ms=round((time.monotonic()-start)*1000),
                  sha256=hashlib.sha256(run.stdout).hexdigest(), document_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                  cache='warm' if (TREE / 'lean/.lake').exists() else 'cold')
    results.append(record)
    (OUT / 'results.json').write_text(json.dumps(results, indent=2) + '\n')
    print(json.dumps(record), flush=True)
    assert run.returncode == wanted_rc and reason.encode() in run.stdout, record
print('CONTROLS-COMPLETE 12 fresh representative kills; 2 citation survivors; 1 original positive', flush=True)
