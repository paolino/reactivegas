import fs from 'node:fs';import crypto from 'node:crypto';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2',f=rt+'/instruments/full-v2';
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const rows=JSON.parse(fs.readFileSync(f+'/COMMANDS.json'));
rows.find(r=>r.id==='final-reconcile').prerequisites.push('Axioms');
rows.find(r=>r.id==='final-reconcile').observation+='; full axiom theorem-name set and Event constructor projection set equal final measured identities';
for(const r of rows)for(const i of r.inputs)i.sha256=sha(i.path);
fs.writeFileSync(f+'/COMMANDS.json',JSON.stringify(rows,null,2)+'\n');
const c=JSON.parse(fs.readFileSync(f+'/CLASSIFICATION-ROWS.json'));
for(const r of c){
 if(['P01','P07'].includes(r.id))r.axis1='Proved expression equivalence (value-parametric / inline); no strict definitional identity claimed. v3.1 explicitly permits this scoped outcome; selected production-body sensitivity is a separate obligation.';
 if(r.id==='P13'){
  r.axis1='NOT-ESTABLISHED executable decision, bounded by ReachDecision synthesis attempt; lack of instance is not undecidability.';
  r.axis2='NOT-REQUIRED is a candidate claim under review, not established by missing Decidable. Test exact authority under original S4 finite-only scope; any missing standing-boundary authority remains a named finding/onward dependency, never a passed exception.';
 }
}
fs.writeFileSync(f+'/CLASSIFICATION-ROWS.json',JSON.stringify(c,null,2)+'\n');
const setup=rows.filter(r=>r.charge==='substantive').map(r=>({id:'SETUP-'+r.id,charge:'static preparation; zero compiler/build calls',argv:['node',f+'/world-prepare.mjs',r.id],cwd:rt,inputs:[{path:f+'/worlds.json',sha256:sha(f+'/worlds.json')},{path:f+'/world-prepare.mjs',sha256:sha(f+'/world-prepare.mjs')}],prerequisite:'ADMISSION.json and absent destination',output:rt+'/evidence/full-v2/'+r.id+'.world.json'}));
for(const p of ['P01','P07'])setup.push({id:'SETUP-'+p,charge:'static preparation; zero compiler/build calls',argv:['node',f+'/shadow-prepare.mjs',p],cwd:rt,inputs:[{path:f+'/shadow-prepare.mjs',sha256:sha(f+'/shadow-prepare.mjs')}],prerequisite:'ADMISSION.json, absent separate shadow path',output:rt+'/evidence/full-v2/'+p+'.empty-world.json'});
fs.writeFileSync(f+'/SETUP-COMMANDS.json',JSON.stringify(setup,null,2)+'\n');
const totals={substantive:1,targeted:2};for(const r of rows)totals[r.charge]++;
if(totals.substantive>12||totals.targeted>80)throw Error('class ceiling');
let text='# Frozen full command-to-obligation sheet v2\n\nRead REVALIDATION.md, CLASSIFICATION-ROWS.json, DECLARATION-ROWS.jsonl, WITNESS-ROWS.json and SETUP-COMMANDS.json as incorporated rows of this one sheet. Every input path/hash is fully bound in COMMANDS.json. Original mandate and all amendments remain required. Original version1 preserved; no original result inherited.\n\nVerified total cost including1/2 already spent: '+JSON.stringify(totals)+'; ceilings12/80 distinct. Ten future substantive and69 future targeted operations. No unallocated capacity is counted as coverage.\n\n';
for(const r of rows)text+='## '+r.id+' — '+r.obligation+'\n\n'+JSON.stringify(r,null,2)+'\n\n';
text+='## Original requirements and discovery relations\n\n'+fs.readFileSync(rt+'/instruments/full/COMMAND-SHEET.md','utf8').split('## Per-identity and static requirements\n\n')[1]+'\nAll source/static obligations above remain subject to independent judgment. P01/P07 classification precision corrected in v2. Reach consumer authority remains falsifiable, with unresolved authority reported rather than inferred. No test result is predicted as closure. Setup script argv is literal per SETUP-COMMANDS.json; each charged command has its own receipt and no implicit retry.\n';
fs.writeFileSync(f+'/COMMAND-SHEET.md',text);
const paths=[];function walk(p){for(const e of fs.readdirSync(p,{withFileTypes:true})){const q=p+'/'+e.name;if(e.isDirectory())walk(q);else if(e.name!=='MANIFEST.sha256'&&e.name!=='ADMISSION.json')paths.push(q);}}walk(f);
paths.push(rt+'/instruments/revalidate-v2.mjs',rt+'/instruments/full/MANIFEST.sha256',rt+'/admitted/MANIFEST.sha256',rt+'/brief.md',rt+'/instruments/candidate-inputs.sha256',rt+'/evidence/M1/oleans.sha256',rt+'/evidence/M1R.sha256');
fs.writeFileSync(f+'/MANIFEST.sha256',paths.sort().map(p=>sha(p)+'  '+p.slice(rt.length+1)).join('\n')+'\n');
console.log(JSON.stringify({manifest:sha(f+'/MANIFEST.sha256'),files:paths.length,totals}));
