import sys
sys.argv.append('import')
from probe import *
mode=sys.argv[1]
fixture=P/('ci-'+mode);fixture.mkdir(exist_ok=True)
for p in (W/'lean/corpus').iterdir():shutil.copyfile(p,fixture/p.name)
command=re.findall(r'^\s+run: (nix[^\n]*just lean-corpus-verify)\s*$',(W/'.github/workflows/ci.yaml').read_text(),re.M)
assert len(command)==1
command=command[0].strip()
if mode=='econ':
    p=fixture/'economic.json';b=p.read_bytes();p.write_bytes(b+b' ');assert p.read_bytes()!=b
elif mode=='int':
    p=fixture/'integrated.json';b=p.read_bytes();p.write_bytes(b+b' ');assert p.read_bytes()!=b
elif mode=='manifest':
    p=fixture/'corpus.sha256';b=p.read_text();p.write_text('0'*64+b[64:]);assert p.read_text()!=b
base=['bwrap','--ro-bind','/','/','--dev-bind','/dev','/dev','--proc','/proc','--tmpfs','/tmp','--bind',R,R,'--ro-bind',fixture,W/'lean/corpus','--chdir',W,'--setenv','XDG_CACHE_HOME',R/'nix-cache','--setenv','XDG_RUNTIME_DIR',R/'run']
# .lake stays read-only from the root mount. A probe cannot silently rebuild.
# The exact committed command is parsed from the frozen candidate, not restated.
label={'econ':'P27-ci-econ','int':'P28-ci-int','manifest':'P29-ci-manifest','clean':'P30-ci-clean'}[mode]
result=run(label,'exact committed CI step; fixture overlay; read-only warm .lake; nested lake build corpusExport (cache-only)',base+['--','/run/current-system/sw/bin/bash','-c',command])
print(result.stdout.decode().splitlines()[-8:],flush=True)
if mode=='clean':assert result.returncode==0
else:
    assert result.returncode!=0
    reason={'econ':'economic.json','int':'integrated.json','manifest':'FAILED'}[mode]
    assert reason.encode() in result.stdout
assert b'Permission denied' not in result.stdout and b'Read-only file system' not in result.stdout
assert not re.search(rb'Built .+\([0-9]',result.stdout)
