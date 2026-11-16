const fs=require('node:fs'),path=require('node:path'),cp=require('node:child_process'),crypto=require('node:crypto');
const root='/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b', h=path.join(root,'handoffs'), e=path.join(h,'evidence'), cwd='/code/kelgroups-audit-3af3d06';
const status='/code/llm-settings/shared/skills/worker-protocol/scripts/status-event';
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const event=(tag,msg)=>{const r=cp.spawnSync(status,[path.join(root,'STATUS.md'),tag,msg]); if(r.status) throw Error('journal failed');};
const git=(...args)=>cp.execFileSync('git',args,{cwd,encoding:'utf8'});
const free=()=>fs.statfsSync(cwd).bavail*fs.statfsSync(cwd).bsize;
const ledger=path.join(e,'execution-receipts.jsonl');
const spent=()=>fs.existsSync(ledger)?fs.readFileSync(ledger,'utf8').trim().split('\n').filter(Boolean).map(JSON.parse):[];
const add=r=>fs.appendFileSync(ledger,JSON.stringify(r)+'\n');
const env={...process.env,G28_EVIDENCE_DIR:e,TMPDIR:path.join(e,'tmp')};
const mode=process.argv[2];
if(git('rev-parse','HEAD').trim()!=='3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4'||git('status','--porcelain').trim()) throw Error('candidate identity/hygiene');
if(sha(fs.readFileSync(path.join(cwd,'gate.sh')))!=='dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815') throw Error('gate identity');
if(mode==='gate') {
 if(spent().some(r=>r.class==='build')) throw Error('one gate envelope only');
 const fixed=['/tmp/g28m3.hs','/tmp/g28m4.hs','/tmp/g28m5.hs'];
 const reservations=[];
 for(const f of fixed){const fd=fs.openSync(f,'wx');fs.closeSync(fd);const s=fs.lstatSync(f);reservations.push({path:f,uid:s.uid,ino:s.ino,dev:s.dev});}
 fs.writeFileSync(path.join(e,'reservations.json'),JSON.stringify(reservations,null,2)+'\n',{flag:'wx'});
 const log=fs.createWriteStream(path.join(e,'gate-observer.log'),{flags:'wx'});
 let phase=null,count=0,buffer='';
 const finish=()=>{if(!phase)return;phase.duration_observed_ms=Date.now()-phase.begin;phase.free_after=free();add(phase);event('NOTE',`BUILD-END id=${phase.id} exit=${phase.exit??'unobserved'} builds=${count}/12 targeted=0/24 AUDIT-S28R1`);phase=null;};
 const start=id=>{finish();count++;if(count>10)throw Error('envelope bound');phase={id,class:'build',begin:Date.now(),free_before:free(),charge:{builds:1,targeted:0},cache:count===1?'cold':'warm'};event('NOTE',`BUILD-BEGIN id=${id} builds=${count}/12 targeted=0/24 cache=${phase.cache} AUDIT-S28R1`);};
 const line=s=>{
  if(s.startsWith('===== LEG 3:'))start('leg3');
  else if(s.startsWith('===== LEG 4:'))start('leg4');
  else if(/^--- M[1-7] /.test(s))start(s.match(/^--- (M[1-7])/)[1]);
  else if(s.startsWith('===== LEG 6:'))start('leg6');
  else if(s.startsWith('===== LEG 7:'))finish();
  if(phase){const m=s.match(/^(?:M[1-7] )?(?:build|test|ci) exit=(\d+)/);if(m)phase.exit=Number(m[1]);}
  const m=s.match(/^(M[1-7]) diff sha256=([0-9a-f]+)/);
  if(m){const diff=git('diff','--','lib','test');fs.writeFileSync(path.join(e,m[1]+'.diff'),diff,{flag:'wx'});if(sha(diff)!==m[2])throw Error('mutant snapshot mismatch');}
  if(/LEG-PASS|LEG-FAIL|KILL-QUOTE|IDENTITY-FAIL|ABORT|COMPLETE/.test(s))console.log(s);
 };
 event('NOTE','GATE-BEGIN command=./gate.sh envelope=10B reservations=owned G28_EVIDENCE_DIR=own TMPDIR=own A-01-consumed AUDIT-S28R1');
 const p=cp.spawn('./gate.sh',[],{cwd,env,stdio:['ignore','pipe','pipe']});
 fs.writeFileSync(path.join(e,'gate-process.json'),JSON.stringify({pid:p.pid,started:new Date().toISOString()},null,2)+'\n',{flag:'wx'});
 const consume=b=>{log.write(b);buffer+=b.toString();let k;while((k=buffer.indexOf('\n'))>=0){line(buffer.slice(0,k));buffer=buffer.slice(k+1);}};
 p.stdout.on('data',consume);p.stderr.on('data',consume);
 p.on('close',(code,signal)=>{if(buffer)line(buffer);finish();log.end();const clean=git('status','--porcelain');const gate=sha(fs.readFileSync(path.join(cwd,'gate.sh')));fs.writeFileSync(path.join(e,'gate-exit.json'),JSON.stringify({code,signal,builds:count,clean,gate},null,2)+'\n');event(code===0?'GATE-PASS':'GATE-FAIL',`gate-v9 exit=${code} builds=${count}/12 targeted=0/24 clean=${clean==='' } AUDIT-S28R1`);console.log(JSON.stringify({code,signal,builds:count,clean,gate}));process.exitCode=code||0;});
} else {
 const extra=fs.existsSync(path.join(h,'probe-commands-extra.json'))?JSON.parse(fs.readFileSync(path.join(h,'probe-commands-extra.json'))):[];
 const command=[...JSON.parse(fs.readFileSync(path.join(h,'probe-commands.json'))),...extra].find(c=>c.id===mode);
 if(!command)throw Error('unknown probe id');
 const history=spent();if(history.some(r=>r.id===mode))throw Error('no repeated named probe');
 const b=history.filter(r=>r.class==='build').length,t=history.filter(r=>r.class==='targeted').length+1;
 if(b>12||t>16)throw Error('budget limit');
 const logfile=path.join(e,mode+'.log');const fd=fs.openSync(logfile,'wx');
 const begin=Date.now(),before=free();
 event('NOTE',`TARGETED-BEGIN id=${mode} builds=${b}/12 targeted=${t}/24 cache=warm AUDIT-S28R1`);
 const r=cp.spawnSync(command.argv[0],command.argv.slice(1),{cwd,env,stdio:['ignore',fd,fd]});fs.closeSync(fd);
 const bytes=fs.readFileSync(logfile);const receipt={id:mode,class:'targeted',argv:command.argv,begin,duration_ms:Date.now()-begin,exit:r.status,signal:r.signal,error:r.error?.message,sha256:sha(bytes),log:logfile,bytes:bytes.length,free_before:before,free_after:free(),cache:'warm',charge:command.charge};
 add(receipt);event('NOTE',`TARGETED-END id=${mode} exit=${r.status} builds=${b}/12 targeted=${t}/24 sha256=${receipt.sha256} AUDIT-S28R1`);
 console.log(JSON.stringify(receipt));process.exitCode=r.status===null?1:r.status;
}
