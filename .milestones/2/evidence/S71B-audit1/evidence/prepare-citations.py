#!/usr/bin/env python3
"""Resolve observed markers to pinned source, emit compiled checks/axiom reads."""
from pathlib import Path
import re
import json
import subprocess

tree = Path('/code/reactivegas-issue-71-audit-b1')
root = Path('/tmp/reactivegas/ms2/t71-design-record/audit-b1/evidence')
decls = []
for p in sorted((tree/'lean').rglob('*.lean')):
    if '.lake' in p.parts:
        continue
    stack=[]
    scopes=[]
    for number, line in enumerate(p.read_text().splitlines(), 1):
        m=re.match(r'^namespace\s+([A-Za-z0-9_.]+)', line)
        if m:
            names=m[1].split('.')
            stack.extend(names); scopes.append(len(names)); continue
        if re.match(r'^section(?:\s|$)', line):
            scopes.append(0); continue
        m=re.match(r'^end(?:\s+([A-Za-z0-9_.]+))?\s*$', line)
        if m:
            count=scopes.pop() if scopes else 0
            if count:
                del stack[-count:]
            continue
        m=re.match(r'^(?:private\s+)?(?:noncomputable\s+)?(def|theorem|inductive|structure|abbrev|instance|class|opaque)\s+([A-Za-z0-9_.?!]+)', line)
        if m:
            name='.'.join(stack+[m[2]])
            compiled=('_private.'+str(p.relative_to(tree/'lean')).removesuffix('.lean').replace('/','.')+'.0.'+name) if line.startswith('private ') else name
            decls.append(dict(name=name, compiled=compiled, basename=m[2], kind=m[1], file=str(p.relative_to(tree/'lean')), line=number))
markers=set()
for name in ['state-machine.md', 'kelgroups-vote-machine.md']:
    markers.update(re.findall(r'lean:[A-Za-z0-9_./:-]+', (tree/'docs/en/design'/name).read_text()))
resolved=[]
for marker in sorted(markers):
    symbol=marker[5:]
    if '.lean:' in symbol:
        file, base=symbol.split(':')
        matches=[d for d in decls if d['file']==file and d['basename']==base]
    elif '.' in symbol:
        matches=[d for d in decls if d['name']==symbol]
    else:
        matches=[d for d in decls if d['basename']==symbol]
    assert len(matches)==1, (marker,matches)
    resolved.append(dict(marker=marker,**matches[0]))
assert len(resolved)>0
(root/'citation-source-map.json').write_text(json.dumps(resolved,indent=2)+'\n')
lines=['import Reactivegas', 'import KelGroups', 'import Reactivegas.TraceTests']
for d in resolved:
    lines.extend([f'-- {d["marker"]} = {d["file"]}:{d["line"]}', f'#check _root_.{d["compiled"]}'])
    if d['kind']=='theorem':
        lines.append(f'#print axioms _root_.{d["compiled"]}')
seed=Path('/tmp/reactivegas/ms2/t71-design-record/evidence/witness-driver-seed.lean').read_text()
lines.append(seed.replace('import Reactivegas.Trace\n','',1))
lines.extend(['#print _root_.Proposal', '#print KelGroups.Proposal', '#check pledge_escrow_debit', '#check conservation_preserved'])
(root/'CitationWitness.lean').write_text('\n'.join(lines)+'\n')
print(f'markers={len(resolved)} source_declarations={len(decls)} theorem_markers={sum(d["kind"]=="theorem" for d in resolved)}')
print('VM prose equality after removing newly added parenthesized markers:')
old=subprocess.check_output(['git','show','90dae99:docs/en/design/kelgroups-vote-machine.md'],cwd=tree).decode()
new=(tree/'docs/en/design/kelgroups-vote-machine.md').read_text()
strip=lambda s: re.sub(r' \(`lean:[^`]+`\)', '', s)
assert strip(old)==strip(new)
print('true; all eight additions are citation markers only')
voci=subprocess.check_output(['git','ls-tree','-rz','HEAD','--','Voci/'],cwd=tree).split(b'\0')
voci=[v.decode() for v in voci if v]
assert len(voci)==21
print('\n'.join(voci))
assert sum('Quantita.hs' in v or 'Quantità.hs' in v for v in voci)==2
