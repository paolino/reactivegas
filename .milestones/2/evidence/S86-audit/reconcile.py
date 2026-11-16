from pathlib import Path
import hashlib,json,re,subprocess,time
R=Path(__file__).parent;W=Path('/code/reactivegas-issue-86-audit-2')
h=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
cmds=[json.loads(x) for x in (R/'commands.jsonl').read_text().splitlines()]
extras=[
('S1','nix develop --quiet -c just ci',0,153474,'S1-ci.log','cold','BUILDING'),
('S2','nix develop --quiet -c ./gate.sh',0,1571,'S2-gate.log','warm','SUBSTANTIVE'),
('P25','bwrap --ro-bind / / --dev-bind /dev /dev --proc /proc --bind '+str(R)+' '+str(R)+' -- /run/current-system/sw/bin/true',0,22,'P25-bwrap-readiness.log','none','PROBE-SETUP'),
('P31',"nix develop --quiet -c bash -c 'cd lean && lake env lean "+str(R)+"/LiveContext.lean'",1,4482,'P31-live-context.log','warm','PROBE-INSTRUMENT-ERROR'),
('P37',"nix develop --quiet -c bash -c 'cd lean && lake env lean "+str(R)+"/LiveContext.lean'",0,4288,'P37-live-context.log','warm','PROBE')]
for ident,command,rc,ms,log,cache,cl in extras:
 p=R/'evidence'/log;cmds.append(dict(id=ident,command=command,exit=rc,duration_ms=ms,evidence=str(p),sha256=h(p),cache=cache,classification=cl))
for c in cmds:
 assert h(c['evidence'])==c['sha256']
 c.setdefault('classification','PROBE');c.setdefault('scope',c['classification'])
 if c['id'].startswith(('P27','P28','P29','P30','P32','P33','P36')):
  c['nested_commands']=['nix develop (shell evaluation)','just lean-corpus-verify','lake build corpusExport (42 jobs replayed; .lake OS read-only)','corpusExport temporary emission','cmp economic','cmp integrated','sha256sum -c','corpusExport check','jq economic key-set','jq integrated key-set']
  c['nested_limit']='Execution stops at first failing recipe step. P33 reaches first jq and exits 127. P36 has detector wrapper exit 0, committed command exit 1.'
 if c['id'].startswith('P26'):c['classification']='PROBE-SETUP-FAIL';c['nested_commands']=['nix develop','just failed creating recipe temporary script; no lake invocation']
 if c['id'].startswith('P38'):c['nested_commands']=['nix develop','lake env bash','lean --run exact pre-repair source (in-memory elaboration, no backend build)']
 if c['id'].startswith(('P31','P37')):c['nested_commands']=['nix develop','lake env lean (in-memory elaboration, warm imports; no backend build)']
keys=[int(re.match(r'P(\d+)',c['id'])[1]) for c in cmds if c['id'].startswith('P')]
assert sorted(keys)==list(range(1,41)),keys
cmds.sort(key=lambda c:(0 if c['id'].startswith('S') else 1,int(re.search(r'\d+',c['id'])[0])))
(R/'command-ledger.json').write_text(json.dumps({'historical_building_audits_before':1,'historical_building_audits_after':3,'this_seat_substantive_invocations':2,'ticket_builds_reported_at_dispatch':'3/8; separate parent counter','ceiling_raises':0,'targeted_probe_invocations':40,'probe_cap':40,'setup_before_S1':'host python3 missing; recorder rejected nonexistent evidence directory; neither attempt invoked nix or substantive command; correction journaled','commands':cmds},indent=2)+'\n')
# Read-only byte and Git reconciliation: no new executable fault probes.
start=time.time()
base_files=subprocess.check_output(['git','ls-tree','-r','--name-only','-z','4a6cd87'],cwd=W,text=True).rstrip('\0').split('\0')
mutable={'.github/workflows/ci.yaml','justfile','lean/lakefile.lean','nix/project.nix'}
protected=[p for p in base_files if p not in mutable]
changed=[]
for p in protected:
 a=subprocess.check_output(['git','rev-parse','4a6cd87:'+p],cwd=W);b=subprocess.check_output(['git','rev-parse','HEAD:'+p],cwd=W)
 if a!=b:changed.append(p)
assert not changed
additive={}
for p in mutable:
 before=subprocess.check_output(['git','show','4a6cd87:'+p],cwd=W,text=True).splitlines()
 after=(W/p).read_text().splitlines();it=iter(after)
 additive[p]=all(any(x==line for x in it) for line in before)
assert all(additive.values())
live=(R/'evidence/P37-live-context.log').read_text().splitlines()
e=json.loads((W/'lean/corpus/economic.json').read_text());i=json.loads((W/'lean/corpus/integrated.json').read_text())
assert json.loads(json.loads(live[0]))==e['view'];assert json.loads(json.loads(live[1]))==i['initial']
assert json.loads(live[2])==e['auth'];assert json.loads(live[3])==i['auth'];assert live[4:9]==['false','false','5','32','7']
old=subprocess.check_output(['git','show','fed19b3:lean/corpus/economic.json'],cwd=W)
assert hashlib.sha256(old).hexdigest()=='91526dc6bf821979fabf516c2e5831a1594de1e171936b235ec36a08154f5e86'
assert old.count(b'UNPROVED')==1
handoff=(R.parent/'handoffs/CORPUS-COVERAGE.md').read_text()
assert '91526dc6' in handoff and 'pre-S1' in handoff and 'Dated entry' in handoff
assert 'does not establish\nserializer-instance independence' in (W/'lean/Reactivegas/CorpusExport.lean').read_text()
record=dict(classification='READ-ONLY FILE/HASH INSPECTION; no additional targeted execution',protected_base_files=len(protected),changed_protected_files=changed,existing_lines_preserved_in_each_modified_base_file=additive,live_context_equals_stored_fields=True,auth_probe_samples=[False,False],historical_economic_sha256=hashlib.sha256(old).hexdigest(),historical_UNPROVED=1,current_UNPROVED=0,bounded_claim_present=True,read_only_record_duration_ms=round(1000*(time.time()-start)))
(R/'evidence/reconciliation.json').write_text(json.dumps(record,indent=2)+'\n');print(json.dumps(record))
# Review each raw evidence file end to end; retain all logs, summarize repeated compiler diagnostics.
read_summary=[]
for c in cmds:
 data=Path(c['evidence']).read_text();read_summary.append(dict(id=c['id'],bytes=len(data.encode()),lines=len(data.splitlines()),diagnostic_errors=[x for x in data.splitlines() if 'error:' in x.lower() or 'FAIL' in x or 'cmp:' in x],backend_built_lines=[x for x in data.splitlines() if ' Built ' in x]))
(R/'evidence/log-review.json').write_text(json.dumps(read_summary,indent=2)+'\n')
print('ledger: substantive=2, historical=3/3, targeted probes=40/40; evidence hashes reconciled')
