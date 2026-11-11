import fs from 'node:fs';
import crypto from 'node:crypto';
import readline from 'node:readline';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
const src='/code/reactivegas-66-s4b-audit5';
const old=rt+'/instruments/full', fresh=rt+'/instruments/full-v2';
const hash=s=>crypto.createHash('sha256').update(s).digest('hex');
const read=p=>fs.readFileSync(p,'utf8');
const assert=(b,m)=>{if(!b)throw Error(m);};
const review=[];
assert(!fs.existsSync(rt+'/worlds')&&!fs.existsSync(rt+'/evidence/full'),'unexpected audit launch artifacts');
for(const [file,want]of [['M1/build.exit','0'],['M1/census.exit','1'],['M1R/census.exit','0']])assert(read(rt+'/evidence/'+file).trim()===want,'measurement outcome drift');
for(const file of ['instruments/MEASUREMENT.sha256','instruments/m1r/MANIFEST.sha256','instruments/full/MANIFEST.sha256','evidence/M1.sha256','evidence/M1R.sha256']){
 const lines=read(rt+'/'+file).trim().split('\n');
 for(const line of lines){const sha=line.slice(0,64),p=line.slice(66);assert(p!==file,'self manifest');assert(hash(fs.readFileSync(rt+'/'+p))===sha,'manifest drift '+p);}
 review.push(file+': '+lines.length+' complete inputs verified');
}
for(const file of ['instruments/candidate-inputs.sha256','evidence/M1/oleans.sha256']){
 const lines=read(rt+'/'+file).trim().split('\n');
 for(const line of lines)assert(hash(fs.readFileSync(src+'/'+line.slice(66)))===line.slice(0,64),'candidate/prerequisite drift');
 review.push(file+': '+lines.length+' candidate-relative inputs verified');
}
const atoms=JSON.parse(read(rt+'/planning/atoms.json'));
for(const a of atoms){
 const source=read(src+'/lean/'+a.source), actual=read(old+'/atoms/'+a.id+'.lean');
 assert(hash(source)===a.sourceSha256&&hash(actual)===a.instrumentSha256,'atom hash '+a.id);
 const start=source.indexOf('\ndef '+a.definition+' ')+1,end=source.indexOf('\n/--',start);
 assert(start>0&&end>start,'definition extent '+a.id);
 const chunk=source.slice(start,end);let index=-1;
 for(let n=0;n<=a.occurrence;n++)index=chunk.indexOf(a.old,index+1);
 assert(index>=0,'atom unavailable');
 const changed=source.slice(0,start+index)+a.replacement+source.slice(start+index+a.old.length);
 const cut=end+a.replacement.length-a.old.length;
 const ns=a.theorem.slice(0,a.theorem.lastIndexOf('.')+1);
 const expected=changed.slice(0,cut)+'\n#check '+ns+a.definition+'\n'+changed.slice(cut);
 assert(actual===expected,'extra source/proof edit '+a.id);
 review.push(a.id+': complete file equals original with one body substitution and #check; '+a.theorem+' unchanged');
}
for(const a of JSON.parse(read(old+'/selected-fragments.json'))){
 assert(hash(a.text)===a.sha256&&read(src+'/lean/'+a.file).includes(a.text),'selected bytes '+a.name);
 assert(read(old+'/P01-chain.lean').includes(a.text)||read(old+'/P07-chain.lean').includes(a.text),'unbound fragment '+a.name);
 review.push('selected '+a.name+': entire source fragment equal');
}
for(const [p,file,from,to]of [['P01','KelGroups/Types','  (lookupMember key view).isSome','  false'],['P07','Reactivegas/Step','&& col.permitted && col.pending.isEmpty','&& true && col.pending.isEmpty']]){
 const s=read(src+'/lean/'+file+'.lean');assert(s.split(from).length===2,'shadow edit not single');
 for(const path of [file,file.split('/')[1]])assert(read(old+'/shadows/'+p+'/'+path+'.lean')===s.replace(from,to),'shadow extra mutation');
 review.push(p+': full shadow source single-body edit; separate output root required');
}
assert(read(old+'/WitnessNegative.lean')===read(old+'/EconomyWitnesses.lean').replace('("sentinel", true)','("sentinel", false)'),'witness negative drift');
const inv=read(rt+'/instruments/m1r/Inventory.lean');
for(const name of ['BaseInventory','FinalInventory','EmptyInventory','UnknownInventory']){
 const s=read(old+'/'+name+'.lean');
 review.push(name+': '+hash(s)+'; entire text retained for manual algorithm/delta inspection');
}
const thin=new Map(read(rt+'/planning/M1R-expr-hashes.jsonl').trim().split('\n').map(JSON.parse).map(r=>[r.name,r]));
let declarations=0;const measured=[];
for await(const line of readline.createInterface({input:fs.createReadStream(rt+'/evidence/M1R/inventory.jsonl'),crlfDelay:Infinity})){
 const r=JSON.parse(line);if(r.record!=='declaration')continue;declarations++;
 const t=thin.get(r.name);assert(t&&t.typeHash===hash(r.typeExpr)&&t.bodyHash===hash(r.valueExpr)&&t.levelParams===r.levelParams&&t.kind===r.kind&&t.module===r.module&&t.result===r.result,'thin metadata '+r.name);
 if(r.result==='Prop'&&['defn','opaque','inductive'].includes(r.kind))measured.push(r.name);
}
assert(declarations===thin.size,'metadata truncation');
const classification=JSON.parse(read(old+'/CLASSIFICATION-ROWS.json'));
assert(classification.length===measured.length&&measured.every(n=>classification.some(r=>r.identity===n)),'classification extent');
const rows=JSON.parse(read(old+'/COMMANDS.json')), sheet=read(old+'/COMMAND-SHEET.md');
for(const row of rows){
 assert(sheet.includes('## '+row.id+' — '+row.obligation)&&sheet.includes(JSON.stringify(row.argv))&&sheet.includes(row.observation),'sheet/argv conflict '+row.id);
 for(const input of row.inputs)assert(hash(fs.readFileSync(input.path))===input.sha256,'row input drift '+row.id);
 assert(row.argv.every(a=>typeof a==='string'&&!a.includes('…')),'nonliteral argv');
 review.push(row.id+' '+row.charge+' argv='+JSON.stringify(row.argv)+' cwd='+row.cwd+' world='+row.world+' prereq='+row.prerequisites.join(',')+' observation='+row.observation);
}
const counts={substantive:1,targeted:2};for(const r of rows)counts[r.charge]++;
assert(counts.substantive<=12&&counts.targeted<=80,'full fit');
assert(!fs.existsSync(fresh),'version exists');
fs.mkdirSync(fresh);
function copy(dir,rel=''){
 for(const e of fs.readdirSync(dir,{withFileTypes:true})){
  const sub=rel+'/'+e.name;if(e.isDirectory()){fs.mkdirSync(fresh+sub);copy(dir+'/'+e.name,sub);}
  else if(e.name!=='MANIFEST.sha256'){
   const data=read(dir+'/'+e.name).replaceAll('/instruments/full/','/instruments/full-v2/').replaceAll("+'/instruments/full'","+'/instruments/full-v2'").replaceAll('/evidence/full/','/evidence/full-v2/').replaceAll("+'/evidence/full'","+'/evidence/full-v2'");
   fs.writeFileSync(fresh+sub,data);
  }
 }
}
copy(old);
fs.writeFileSync(fresh+'/REVALIDATION-STATIC.txt',review.join('\n')+'\nComplete static review counts including unchanged measurement spend: '+JSON.stringify(counts)+'\n');
console.log(JSON.stringify({originalFilesVerified:true,declarations,measuredPredicates:measured.length,counts,version:fresh}));
