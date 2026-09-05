import copy, hashlib, json, os, pathlib, re, shlex, shutil, subprocess, sys, time
R=pathlib.Path(__file__).parent
W=pathlib.Path('/code/reactivegas-issue-86-audit-2')
B=W/'lean/.lake/build/bin/corpusExport'
P=R/'fixtures'; P.mkdir(exist_ok=True)
E=json.loads((W/'lean/corpus/economic.json').read_text()); I=json.loads((W/'lean/corpus/integrated.json').read_text())
programs=re.findall(r"jq -e '([\s\S]*?)'",(W/'justfile').read_text())
assert len(programs)==2
for n,p in enumerate(programs): (R/f'keys-{n}.jq').write_text(p)
def h(p): return hashlib.sha256(pathlib.Path(p).read_bytes()).hexdigest()
def clean(): assert subprocess.check_output(['git','status','--porcelain'],cwd=W)==b''
def run(label,scope,args,cwd=W,expected=None,env=None):
    clean(); start=time.time(); out=subprocess.run(list(map(str,args)),cwd=cwd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,env=env)
    log=R/'evidence'/f'{label}.log'; log.write_bytes(out.stdout)
    rec=dict(id=label,scope=scope,command=shlex.join(list(map(str,args))),cwd=str(cwd),exit=out.returncode,duration_ms=round((time.time()-start)*1000),evidence=str(log),sha256=h(log),cache='warm',expected=expected)
    with (R/'commands.jsonl').open('a') as f:f.write(json.dumps(rec)+'\n')
    print(json.dumps(rec),flush=True); clean()
    if expected=='reject': assert out.returncode!=0,(label,out.stdout)
    if expected=='accept': assert out.returncode==0,(label,out.stdout)
    return out

def pair(label,e=E,i=I):
    d=P/label; d.mkdir(exist_ok=True)
    (d/'e.json').write_text(json.dumps(e));(d/'i.json').write_text(json.dumps(i))
    return d

def check(label,e,i,reason):
    d=pair(label,e,i)
    result=run(label,reason,[B,'check',d/'e.json',d/'i.json'],expected='reject')
    assert reason.encode() in result.stdout
    return d

def keycheck(label,d,idx,expected):
    return run(label,'shipped jq exact key-set program '+str(idx),['jq','-e','-f',R/f'keys-{idx}.jq',d/('e.json' if idx==0 else 'i.json')],expected=expected)

if sys.argv[1]=='core':
    # Mutated values first: these establish that the named checker can reject
    # before its positive control is trusted. Each mutation asserts application.
    e=copy.deepcopy(E); assert e['view']['members'][0]['key']!='ZZZ';e['view']['members'][0]['key']='ZZZ'
    d=check('P01-C1',e,I,'view differs from live seedView');keycheck('P02-C1-shape',d,0,'accept')
    i=copy.deepcopy(I);assert len(i['initial']['members'])>0;i['initial']['members']=[]
    check('P03-C2',E,i,'initial differs from live corpusInitial')
    e=copy.deepcopy(E);e['auth']='permissive-probe(fun _ _ => true)';assert e['auth']!=E['auth']
    check('P04-C3',e,I,'auth differs from live econAuthIdentity')
    i=copy.deepcopy(I);i['auth']='permissive-probe(fun _ _ => true)';assert i['auth']!=I['auth']
    check('P05-C4',E,i,'auth differs from live intAuthIdentity')
    run('P06-clean','positive compiled check and live extents',[B,'check',W/'lean/corpus/economic.json',W/'lean/corpus/integrated.json'],expected='accept')
    for num,args in enumerate([['check','a'],['check'],['check','a','b','c']],7):
        d=P/f'arity-{num}';d.mkdir(exist_ok=True)
        for name in ['check','a','b','c','sentinel']:(d/name).write_text('sentinel-'+name)
        before={str(p.relative_to(d)):h(p) for p in d.rglob('*') if p.is_file()}
        run(f'P{num:02}-arity','malformed check arity plus full directory byte inventory',[B]+args,cwd=d,expected='reject')
        after={str(p.relative_to(d)):h(p) for p in d.rglob('*') if p.is_file()}
        assert before==after
        (R/'evidence'/f'P{num:02}-inventory.json').write_text(json.dumps({'before':before,'after':after},indent=2))
    for num,idx,level in [(10,0,'top'),(11,1,'top'),(12,0,'nested'),(13,1,'nested')]:
        e=copy.deepcopy(E);i=copy.deepcopy(I);obj=e if idx==0 else i
        if level=='nested':obj=obj['traces' if idx==0 else 'steps'][0]
        assert 'EXTRA' not in obj;obj['EXTRA']=1
        d=pair(f'keys-{num}',e,i);keycheck(f'P{num:02}-keys',d,idx,'reject')
    d=pair('clean-keys');keycheck('P14-clean-econ-keys',d,0,'accept');keycheck('P15-clean-int-keys',d,1,'accept')
    e=copy.deepcopy(E);e['traces']=[];check('P16-empty-traces',e,I,'zero extent')
    i=copy.deepcopy(I);i['steps']=[];check('P17-empty-steps',E,i,'zero extent')
    e=copy.deepcopy(E);assert e['traces'][0]!=e['traces'][1];e['traces'][0],e['traces'][1]=e['traces'][1],e['traces'][0];check('P18-trace-swap',e,I,'element 0 differs')
    i=copy.deepcopy(I);assert i['steps'][0]!=i['steps'][1];i['steps'][0],i['steps'][1]=i['steps'][1],i['steps'][0];check('P19-step-swap',E,i,'element 0 differs')
    for num,which,kind in [(20,0,'missing'),(21,1,'missing'),(22,0,'malformed'),(23,1,'malformed')]:
        d=pair(f'failure-{num}');p=d/('e.json' if which==0 else 'i.json')
        if kind=='missing':p.unlink()
        else:p.write_text('{')
        run(f'P{num:02}-{kind}',f'{kind} input file {which} propagates failure',[B,'check',d/'e.json',d/'i.json'],expected='reject')
    d=P/'second-write';d.mkdir(exist_ok=True);(d/'first').write_text('sentinel');(d/'directory').mkdir(exist_ok=True)
    run('P24-second-write','declared non-atomic second-write failure remains observable',[B,d/'first',d/'directory'],expected='reject')
    assert (d/'first').read_bytes()==(W/'lean/corpus/economic.json').read_bytes()
    (R/'evidence/P24-effects.json').write_text(json.dumps({'first_sha256':h(d/'first'),'second_is_directory':(d/'directory').is_dir()}))
