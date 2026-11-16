import fs from 'node:fs';import cp from 'node:child_process';import crypto from 'node:crypto';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';const id=process.argv[2];
const plan=JSON.parse(fs.readFileSync(rt+'/instruments/full-v3/COMMANDS.json','utf8'));const row=plan.find(r=>r.id===id);if(!row)throw Error('unbound row '+id);
fs.mkdirSync(rt+'/evidence/full-v2',{recursive:true});const receipt=rt+'/evidence/full-v2/'+id+'.json';if(fs.existsSync(receipt))throw Error('no implicit retry '+id);
const prior=fs.readdirSync(rt+'/evidence/full-v2').filter(n=>/^.*\.json$/.test(n)&&!n.endsWith('.world.json')).map(n=>{try{return JSON.parse(fs.readFileSync(rt+'/evidence/full-v2/'+n,'utf8'));}catch{return {};}}).filter(x=>x.launched);
const spend={substantive:1,targeted:2};for(const r of prior)spend[r.charge]++;
if(spend[row.charge]+1>({substantive:12,targeted:80}[row.charge]))throw Error('ceiling would overrun');
const h=b=>crypto.createHash('sha256').update(b).digest('hex');
if (!fs.existsSync(rt+'/instruments/full-v3/ADMISSION.json')) throw Error('full audit not admitted');
for (const prerequisite of row.prerequisites) {
  if (['M1-S','M1R-T'].includes(prerequisite)) continue;
  const path=rt+'/evidence/full-v2/'+prerequisite+'.json';
  if (!fs.existsSync(path)) throw Error('prerequisite not launched: '+prerequisite);
  const result=JSON.parse(fs.readFileSync(path,'utf8'));
  if (!Number.isInteger(result.exit)) throw Error('prerequisite incomplete: '+prerequisite);
  if (['S01','S09','S10','BaseInventory','FinalInventory','P01-compile','P07-compile'].includes(prerequisite)&&result.exit!==0) throw Error('prerequisite failed: '+prerequisite);
}
for(const i of row.inputs||[])if(i.path&&i.sha256){if(h(fs.readFileSync(i.path))!==i.sha256)throw Error('input drift '+i.path);}
const libs=row.argv.filter(a=>a.startsWith('LEAN_PATH=')).flatMap(a=>a.slice(10).split(':'));const artifacts=[];function walk(p){if(!fs.existsSync(p))return;for(const e of fs.readdirSync(p,{withFileTypes:true})){const q=p+'/'+e.name;if(e.isDirectory())walk(q);else if(q.endsWith('.olean'))artifacts.push({path:q,sha256:h(fs.readFileSync(q))});}}for(const p of libs)walk(p);fs.writeFileSync(rt+'/evidence/full-v2/'+id+'.input-oleans.json',JSON.stringify(artifacts,null,2)+'\n');
const start=new Date().toISOString();spend[row.charge]++;
fs.appendFileSync(rt+'/STATUS.md',start+'  AUDIT-CHARGE  '+id+' class='+row.charge+' substantive='+spend.substantive+'/12 targeted='+spend.targeted+'/80\n');
const stdout=fs.openSync(rt+'/evidence/full-v2/'+id+'.stdout','wx'),stderr=fs.openSync(rt+'/evidence/full-v2/'+id+'.stderr','wx');
const pending={id,argv:row.argv,cwd:row.cwd,charge:row.charge,launched:true,start,spend,obligation:row.obligation};fs.writeFileSync(receipt,JSON.stringify(pending,null,2)+'\n');
const result=cp.spawnSync(row.argv[0],row.argv.slice(1),{cwd:row.cwd,stdio:['ignore',stdout,stderr],maxBuffer:1024});fs.closeSync(stdout);fs.closeSync(stderr);
const out=fs.readFileSync(rt+'/evidence/full-v2/'+id+'.stdout'),err=fs.readFileSync(rt+'/evidence/full-v2/'+id+'.stderr');
fs.writeFileSync(receipt,JSON.stringify({...pending,finish:new Date().toISOString(),exit:result.status,signal:result.signal,error:result.error?.message,stdoutSha256:h(out),stderrSha256:h(err)},null,2)+'\n');
console.log(JSON.stringify({id,exit:result.status,signal:result.signal,spend,stdoutBytes:out.length,stderrBytes:err.length}));
