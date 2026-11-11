#!/usr/bin/env python3
"""T5 read of compiler-produced indexes from the completed T2 build; no compilation."""
import hashlib
import json
from pathlib import Path
import shutil

root=Path('/tmp/reactivegas/ms2/t71-design-record/audit-b1/evidence')
tree=Path('/code/reactivegas-issue-71-audit-b1')
copies=root/'compiled-indexes'
copies.mkdir(exist_ok=True)
definitions={}
for path in sorted((tree/'lean/.lake/build/lib/lean').rglob('*.ilean')):
    data=json.loads(path.read_text())
    for raw, value in data['references'].items():
        key=json.loads(raw)
        name=key.get('c',{}).get('n')
        if name and value.get('definition') is not None:
            definitions[name]={'module':data['module'], 'definition':value['definition'],
                               'index_sha256':hashlib.sha256(path.read_bytes()).hexdigest()}
    shutil.copyfile(path, copies/(data['module']+'.ilean'))
markers=json.loads((root/'citation-source-map.json').read_text())
observed=[]
for row in markers:
    observed.append(dict(marker=row['marker'], compiled=row['compiled'], index=definitions.get(row['compiled'])))
(root/'compiled-citation-index.json').write_text(json.dumps(observed,indent=2)+'\n')
present=sum(row['index'] is not None for row in observed)
print(f'compiler-index marker associations found={present}/{len(observed)}')
for row in observed:
    if row['index'] is None:
        print('MISSING',row)
errors={name:value for name,value in definitions.items()
        if name.startswith('KelGroups.ValidationError.') and len(name.split('.'))==3
        and len(value['definition'])>4 and value['definition'][4]=='KelGroups.ValidationError'}
print(f'compiled ValidationError constructor definitions={len(errors)}')
print(json.dumps(errors,indent=2))
(root/'validation-error-compiled-definitions.json').write_text(json.dumps(errors,indent=2)+'\n')
