import pathlib,re,subprocess,sys,json,hashlib
R=pathlib.Path(__file__).parent;W=pathlib.Path('/code/reactivegas-issue-86-audit-2')
mode=sys.argv[1]
original=(W/'.github/workflows/ci.yaml').read_text()
block='      - name: Verify corpus emission\n        run: nix --quiet develop --command just lean-corpus-verify\n'
assert original.count(block)==1
if mode=='removed':text=original.replace(block,'')
elif mode=='bypass':text=original.replace(block,'      - name: Verify corpus emission\n        run: ":" # lean-corpus-verify\n')
else:text=original
(R/f'workflow-{mode}.yaml').write_text(text)
match=re.findall(r'^      - name: Verify corpus emission\n        run: (.*)$',text,re.M)
if len(match)!=1:
 print('DETECTOR FAIL: expected exactly one executable corpus step; observed',len(match));sys.exit(1)
command=match[0]
fixture=R/'fixtures/ci-econ'
assert (fixture/'economic.json').read_bytes()!=(W/'lean/corpus/economic.json').read_bytes()
args=['bwrap','--ro-bind','/','/','--dev-bind','/dev','/dev','--proc','/proc','--tmpfs','/tmp','--bind',str(R),str(R),'--ro-bind',str(fixture),str(W/'lean/corpus'),'--chdir',str(W),'--setenv','XDG_CACHE_HOME',str(R/'nix-cache'),'--setenv','XDG_RUNTIME_DIR',str(R/'run'),'--','/run/current-system/sw/bin/bash','-c',command]
print('EXECUTING COMMITTED STEP:',command,flush=True)
out=subprocess.run(args,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
sys.stdout.buffer.write(out.stdout);print('STEP EXIT',out.returncode,flush=True)
if out.returncode==0:
 print('DETECTOR FAIL: corrupted fixture was accepted by step');sys.exit(1)
if b'economic.json' not in out.stdout or b'cmp:' not in out.stdout or b'Read-only file system' in out.stdout:
 print('DETECTOR BLOCKED: step failed outside intended corpus comparison');sys.exit(2)
print('DETECTOR PASS: actual step rejects corrupt fixture')
