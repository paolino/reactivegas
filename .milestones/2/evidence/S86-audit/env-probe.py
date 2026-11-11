import sys
sys.argv.append('import')
from probe import *
mode=sys.argv[1]
base=['bwrap','--ro-bind','/','/','--dev-bind','/dev','/dev','--proc','/proc','--tmpfs','/tmp','--bind',R,R,'--chdir',W,'--setenv','XDG_CACHE_HOME',R/'nix-cache','--setenv','XDG_RUNTIME_DIR',R/'run']
if mode=='omit':
    original=(W/'nix/project.nix').read_text();assert original.count('    jq\n')==1
    mutated=original.replace('    jq\n',''); p=R/'project-no-jq.nix';p.write_text(mutated)
    assert p.read_bytes()==subprocess.check_output(['git','show','6ec3ce3:nix/project.nix'],cwd=W)
    base+=['--ro-bind',p,W/'nix/project.nix']
    script='set +e; command -v jq; found=$?; echo jq_lookup_exit=$found; just lean-corpus-verify; rc=$?; echo recipe_exit=$rc; test "$found" -ne 0 && test "$rc" -ne 0'
    label='P33-jq-omission'
else:
    script='set -e; command -v jq; jq --version; just lean-corpus-verify'
    label='P32-clean-env'
args=base+['--','nix','develop','--quiet','--ignore-environment','--keep','XDG_CACHE_HOME','--keep','XDG_RUNTIME_DIR','-c','bash','-c',script]
result=run(label,'clean declared shell '+mode+'; exact recipe; own read-only warm .lake, no rebuild',args)
print(result.stdout.decode().splitlines()[-12:],flush=True)
assert result.returncode==0
assert b'Read-only file system' not in result.stdout
assert not re.search(rb'Built .+\([0-9]',result.stdout)
if mode=='omit':assert b'jq: command not found' in result.stdout and b'exit code 127' in result.stdout
else: assert re.search(rb'/nix/store/[^\n]+/bin/jq',result.stdout)
