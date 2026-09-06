import sys
sys.argv.append('import')
from probe import *
p=R/'CorpusExport-before.lean';p.write_bytes(subprocess.check_output(['git','show','6ec3ce3:lean/Reactivegas/CorpusExport.lean'],cwd=W))
assert b'if econPath == "check"' not in p.read_bytes()
d=P/'arity-before';d.mkdir(exist_ok=True)
for name in ['check','a','keep']:(d/name).write_text('sentinel-'+name)
before={x.name:h(x) for x in d.iterdir()}
inner='cd '+shlex.quote(str(d))+' && lean --run '+shlex.quote(str(p))+' check a'
outer='cd lean && lake env bash -c '+shlex.quote(inner)
result=run('P38-arity-control','known pre-repair arity defect, interpreted against warm imports; sentinel detector must fail',['nix','develop','--quiet','-c','bash','-c',outer],expected='accept')
after={x.name:h(x) for x in d.iterdir()}
assert before!=after and before['keep']==after['keep']
assert (d/'check').read_bytes()==(W/'lean/corpus/economic.json').read_bytes()
assert (d/'a').read_bytes()==(W/'lean/corpus/integrated.json').read_bytes()
(R/'evidence/P38-effects.json').write_text(json.dumps({'before':before,'after':after,'detector_rejects':before!=after},indent=2))
print('PRE-REPAIR DEFECT REACHED: exit=0; both destinations overwritten; no-write detector rejects')
