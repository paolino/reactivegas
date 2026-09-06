const fs=require('fs'),p=require('path'),c=require('crypto'),cp=require('child_process'),assert=require('assert');
const R='/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b',H=p.join(R,'handoffs'),E=p.join(H,'evidence'),W='/code/kelgroups-audit-3af3d06';
const hash=f=>c.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
const write=(f,x)=>fs.writeFileSync(f,typeof x==='string'?x:JSON.stringify(x,null,2)+'\n',{flag:'wx'});
const read=f=>JSON.parse(fs.readFileSync(f,'utf8'));
const run=(cmd,args)=>cp.execFileSync(cmd,args,{cwd:W,encoding:'utf8'});
const candidate='3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4',tree='b4eb37f2187c39db4950072e309c0d125d5c27af';
assert.equal(run('git',['rev-parse','HEAD']).trim(),candidate);assert.equal(run('git',['rev-parse','HEAD^{tree}']).trim(),tree);assert.equal(run('git',['status','--porcelain']),'');
assert.equal(hash(p.join(W,'gate.sh')),'dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815');
for(const r of read(p.join(E,'probe-inputs-final.json')))assert.equal(hash(p.join(H,r.path)),r.sha256,r.path);
for(const r of read(p.join(E,'command-receipts.json')))assert.equal(hash(r.log),r.sha256,r.id);
for(const r of read(p.join(E,'input-manifest.json')))if(r.snapshot)assert.equal(hash(r.snapshot),r.sha256,r.snapshot);
const rows=fs.readFileSync(p.join(E,'execution-receipts.jsonl'),'utf8').trim().split('\n').map(JSON.parse);
assert.equal(rows.length,26);assert.equal(new Set(rows.map(r=>r.id)).size,26);assert.equal(rows.filter(r=>r.class==='build').length,10);
const fullgate=fs.readFileSync(p.join(E,'20260906T001909Z-3af3d06-gate-full.log'),'utf8');
for(const r of rows){
 r.cwd=W;
 if(r.class==='build'){
  let args,suffix;
  if(r.id==='leg3'){args=['just','build'];suffix='leg3-build.log'}
  else if(r.id==='leg6'){args=['just','ci'];suffix='leg6-ci.log'}
  else if(r.id==='M1'){args=['cabal','build','all','--enable-tests','-O0'];suffix='leg5-M1-build.log'}
  else if(r.id==='M4'){args=['cabal','build','all','-O0'];suffix='leg5-M4-build.log'}
  else {args=['cabal','test','all','-O0','--test-show-details=direct'];suffix=r.id==='leg4'?'leg4-test.log':`leg5-${r.id}-test.log`}
  r.argv=['nix','develop','.#ci','--quiet','-c',...args];r.log=p.join(E,'20260906T001909Z-3af3d06-'+suffix);r.sha256=hash(r.log);r.bytes=fs.statSync(r.log).size;
  r.argv_provenance='Frozen gate source, verified against observed gate output; observer phase times include boundary overhead.';
  assert(fullgate.includes(r.sha256),'gate log hash '+r.id);
 }else assert.equal(hash(r.log),r.sha256,r.id);
 const expected=/^M[1-7]$/.test(r.id)||['P2','P5','P7','TypeNegative'].includes(r.id)?1:0;assert.equal(r.exit,expected,r.id);
 r.interpretation=r.id==='P2'?'F3 semantic assertion RED after 8/8 concurrency and error controls':r.exit===1?'Intended type or semantic negative control':'Successful positive verification';
 if(/^M[1-7]$/.test(r.id)){r.diff=p.join(E,r.id+'.diff');r.diff_sha256=hash(r.diff);assert(fullgate.includes(r.diff_sha256),r.id+' diff hash')}
}
write(p.join(E,'verification-receipts.json'),{candidate,tree,gate_sha256:hash(p.join(W,'gate.sh')),builds:10,targeted:16,rows});
let md='# Verification receipts — S28-R1\n\nCandidate `'+candidate+'`; cwd `'+W+'`. Charges: 10/12 build-class, 16/24 targeted. Raw observer records are preserved in evidence/execution-receipts.jsonl. Build intervals are observed phase wall times; targeted intervals surround the exact process invocation. Cache and free-space samples are in the JSON receipt. No CPU-time claim.\n\n';
for(const r of rows)md+='## '+r.id+'\n\n- Class: '+r.class+'; exit '+r.exit+'; duration '+(r.duration_ms??r.duration_observed_ms)+' ms; '+r.cache+' cache.\n- '+r.interpretation+'.\n- Raw log: ['+p.basename(r.log)+'](evidence/'+p.basename(r.log)+'); SHA256 `'+r.sha256+'`.\n- Exact argv (JSON, not shell-escaped):\n\n```json\n'+JSON.stringify(r.argv)+'\n```\n\n';
write(p.join(H,'VERIFICATION-RECEIPTS.md'),md);
const snapshots=[['dist-newstyle/packagedb/ghc-9.8.4/kelgroups-0.1.0.0-inplace.conf','candidate-package.conf'],['lib/KelGroups/Store.hs','candidate-Store.hs'],['.github/workflows/ci.yml','candidate-ci.yml'],['justfile','candidate-justfile'],['flake.lock','candidate-flake.lock']];
const manifest=snapshots.map(([source,name])=>{write(p.join(E,name),fs.readFileSync(p.join(W,source),'utf8'));return{source:p.join(W,source),snapshot:name,sha256:hash(p.join(E,name))}});
write(p.join(E,'final-source-snapshots.json'),manifest);
write(p.join(E,'final-identity.json'),{time:new Date().toISOString(),candidate,tree,git_status:run('git',['status','--porcelain']),gate_sha256:hash(p.join(W,'gate.sh')),all_probe_sources_verified:true,all_preflight_logs_verified:true,all_snapshot_inputs_verified:true,all_execution_logs_verified:true,charge:{builds:0,targeted:0}});
fs.appendFileSync(p.join(R,'questions/Q-001-r5-lifecycle-fold-entrypoint.md'),'\n## Resolution — '+new Date().toISOString()+'\n\nRESOLVED by A-01/NOTE-030, received and acknowledged before START. The terminal-return wording above was a proposal in an unissued draft, not a terminal event. No CONTRACT-BLOCKED verdict was issued by this worker. The amended founding/replay command contract was fully executed; the sole terminal report is handoffs/AUDIT-REPORT.md (AUDIT-FINDINGS, F3).\n');
const frozen={time:new Date().toISOString(),report_sha256:hash(p.join(H,'AUDIT-REPORT.md')),ledger_sha256:hash(p.join(H,'REQUIREMENT-LEDGER.md')),receipts_sha256:hash(p.join(E,'verification-receipts.json'))};write(p.join(E,'report-freeze.json'),frozen);
const B=p.join(H,'build');assert(fs.lstatSync(B).isDirectory()&&!fs.lstatSync(B).isSymbolicLink());
const apparent=Number(run('du',['-sb',B]).split(/\s/)[0]),allocated=Number(run('du',['-s','-B1',B]).split(/\s/)[0]);
fs.rmSync(B,{recursive:true});assert(!fs.existsSync(B));
write(p.join(E,'retirement.json'),{time:new Date().toISOString(),report_frozen_before_retirement:frozen,retired_path:B,bytes_reclaimed:allocated,apparent_bytes:apparent,measurement:'du -s -B1 allocated bytes of exclusively owned build directory before removal; path absent afterwards. Not a global df delta.',worktree_preserved:W,worktree_apparent_bytes:Number(run('du',['-sb',W]).split(/\s/)[0]),preserved:'All probe sources, raw logs, diffs, receipts, authority snapshots, report and ledger; candidate worktree/build outputs retained.'});
const files=[];function walk(d){for(const ent of fs.readdirSync(d,{withFileTypes:true})){const f=p.join(d,ent.name);if(ent.isDirectory())walk(f);else if(ent.isFile()&&!['EVIDENCE-INVENTORY','EVIDENCE-INVENTORY.json'].includes(ent.name))files.push({path:p.relative(H,f),bytes:fs.statSync(f).size,sha256:hash(f)});else if(!ent.isFile())throw Error('Unexpected nonregular evidence '+f)}}walk(H);files.sort((a,b)=>a.path.localeCompare(b.path));
const inv={time:new Date().toISOString(),root:H,candidate,exclusions:['EVIDENCE-INVENTORY and EVIDENCE-INVENTORY.json (avoid recursive hashes)','live STATUS and mutable inbox outside handoffs'],files};
write(p.join(H,'EVIDENCE-INVENTORY.json'),inv);write(p.join(H,'EVIDENCE-INVENTORY'),'# SHA256  bytes  relative-path (all retained handoffs regular files; inventories excluded)\n'+files.map(f=>`${f.sha256}  ${f.bytes}  ${f.path}`).join('\n')+'\n');
for(const r of read(p.join(H,'EVIDENCE-INVENTORY.json')).files)assert.equal(hash(p.join(H,r.path)),r.sha256,r.path);
console.log(JSON.stringify({frozen,inventory_sha256:hash(p.join(H,'EVIDENCE-INVENTORY')),inventory_json_sha256:hash(p.join(H,'EVIDENCE-INVENTORY.json')),retained_files:files.length,retained_bytes:Number(run('du',['-sb',H]).split(/\s/)[0]),retirement:read(p.join(E,'retirement.json'))},null,2));
