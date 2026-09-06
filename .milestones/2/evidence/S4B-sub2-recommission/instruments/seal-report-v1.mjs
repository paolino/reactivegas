// Local evidence sealing only; no subprocess, Lean invocation, or remote write.
import fs from 'node:fs';
import crypto from 'node:crypto';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
process.chdir(rt);
const dir='handoffs/full-audit-v2';
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const hash=p=>sha(fs.readFileSync(p));
const read=p=>fs.readFileSync(p,'utf8');
const write=(p,s)=>fs.writeFileSync(p,s,{flag:'wx'});
const assert=(b,s)=>{if(!b)throw Error(s);};
const inv=JSON.parse(read('evidence/full-v2/FinalInventory.input-oleans.json'));
const ax=JSON.parse(read('evidence/full-v2/Axioms.input-oleans.json'));
assert(JSON.stringify(inv)===JSON.stringify(ax),'final consumer olean sets differ');
for(const r of inv)assert(hash(r.path)===r.sha256,'final olean changed '+r.path);
write(dir+'/FINAL-OLEAN-VERIFICATION.json',JSON.stringify({utc:new Date().toISOString(),scope:'Retained final inventory and axiom prerequisites; static on-disk digest inspection, no elaboration',files:inv},null,2)+'\n');
const atoms=JSON.parse(read(dir+'/ATOM-DISPOSITIONS.json'));
for(const a of atoms){
 const source=read('evidence/full-v2/'+a.id+'.stdout');
 assert(a.exit===1&&a.errors.length>0&&a.errors.every(e=>e.line>=a.proofLines[0]&&e.line<=a.proofLines[1]),'proof diagnostic scope '+a.id);
 assert(source.split('\n').some(l=>l.startsWith(a.definition+' ')||l.includes('.'+a.definition+' ')),'definition marker missing '+a.id);
}
// Preserve the mutable journal before its terminal line; no circular manifest hash.
write(dir+'/STATUS-before-COMPLETE.md',read('STATUS.md'));
write(dir+'/ASSEMBLY.json',JSON.stringify({utc:new Date().toISOString(),substantive:12,targeted:73,verdict:'AUDIT-FINDINGS',blocking:['F-001 Reach consumer-axis authority'],historicalOpen:['H-01','H-02','H-03'],reportAssembly:'instruments/report-v2.mjs',reportAssemblySha256:hash('instruments/report-v2.mjs'),staticReReviewCost:{substantive:0,targeted:0},atomsInspected:atoms.length},null,2)+'\n');
// The former current handoffs were verified and archived before replacing these views.
for(const n of ['AUDIT-REPORT.md','CAMPAIGN-LEDGER.md','ONWARD-DISCOVERIES.md','UNCOVERED-OBLIGATIONS.md','S4-CLASSIFICATION.md'])fs.copyFileSync(dir+'/'+n,'handoffs/'+n);
const files=new Set(['brief.md','instruments/report-v1.mjs','instruments/report-v2.mjs','instruments/seal-report-v1.mjs']);
function collect(root){for(const e of fs.readdirSync(root,{withFileTypes:true})){const p=root+'/'+e.name;if(e.isDirectory())collect(p);else if(e.isFile())files.add(p);}}
for(const root of ['admitted','answers','evidence','instruments','planning',dir,'handoffs/pre-full-v2'])if(fs.existsSync(root))collect(root);
for(const p of ['instruments/MEASUREMENT.sha256','instruments/candidate-inputs.sha256','instruments/modules.txt','planning/atoms.json','planning/M1R-expr-hashes.jsonl','planning/M1R-summary.json'])files.add(p);
for(const n of ['AUDIT-REPORT.md','CAMPAIGN-LEDGER.md','ONWARD-DISCOVERIES.md','UNCOVERED-OBLIGATIONS.md','S4-CLASSIFICATION.md'])files.add('handoffs/'+n);
files.delete(dir+'/MANIFEST.sha256');
const entries=[...files].sort();
write(dir+'/MANIFEST.sha256',entries.map(p=>hash(p)+'  '+p).join('\n')+'\n');
console.log(JSON.stringify({files:entries.length,manifest:hash(dir+'/MANIFEST.sha256'),report:hash(dir+'/AUDIT-REPORT.md'),finalOleanns:inv.length,spend:{substantive:12,targeted:73}}));
