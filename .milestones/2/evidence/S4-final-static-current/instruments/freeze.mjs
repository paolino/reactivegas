import fs from 'node:fs';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
const h=s=>crypto.createHash('sha256').update(s).digest('hex');
const read=p=>fs.readFileSync(p);
const write=(p,s)=>fs.writeFileSync(p,typeof s==='string'||Buffer.isBuffer(s)?s:JSON.stringify(s,null,2)+'\n');
const root='/code/reactivegas-66-s4b-final-audit';
const inputs=JSON.parse(read('evidence/candidate-inputs.json'));
for(const i of inputs)assert.equal(h(read(root+'/'+i.path)),i.sha256,i.path);
function walk(d){return fs.readdirSync(d,{withFileTypes:true}).flatMap(e=>e.isDirectory()?walk(d+'/'+e.name):[d+'/'+e.name]);}
assert.equal(walk(root).filter(p=>p.endsWith('.olean')).length,0);
const originals=JSON.parse(read('evidence/external-snapshot-index.json'));
for(const f of originals)assert.equal(h(read(f.source)),f.sha256,f.source+' changed since snapshot');
const store='/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/src/lean/';
const selected=[['Lean/Elab/BuiltinCommand.lean','toolchain-BuiltinCommand.lean.txt'],['Lean/DocString/Add.lean','toolchain-DocString-Add.lean.txt'],['Lean/DocString/Extension.lean','toolchain-DocString-Extension.lean.txt'],['Lean/Parser/Command.lean','toolchain-Parser-Command.lean.txt'],['lake/Lake/Build/Module.lean','toolchain-Lake-Module.lean.txt']];
const index=[];
for(const [s,d]of selected){const b=read(store+s);write('evidence/'+d,b);index.push({source:store+s,snapshot:'evidence/'+d,sha256:h(b)});}
for(const p of ['scripts/check-reactivegas-inversion-coverage','scripts/check-lean-axioms','scripts/check-trace-coverage-agreement','nix/lean-dependency-direction.sh','justfile','.github/workflows/ci.yaml','lean/lakefile.lean']){const b=read(root+'/'+p),d='evidence/context-'+p.replaceAll('/','_')+'.txt';write(d,b);index.push({source:root+'/'+p,snapshot:d,sha256:h(b)});}
write('evidence/source-context-index.json',index);
const session='/home/paolino/.codex/sessions/2026/09/06/rollout-2026-09-06T07-37-35-01a0756f-c8cd-7581-b8cd-48b798c0b8c6.jsonl';
const turns=read(session).toString().trim().split('\n').map(s=>JSON.parse(s)).filter(x=>x.type==='turn_context');
const t=turns.at(-1);assert.equal(t.payload.model,'gpt-6-astra');assert.equal(t.payload.effort,'high');assert.equal(t.payload.collaboration_mode.settings.model,'gpt-6-astra');assert.equal(t.payload.collaboration_mode.settings.reasoning_effort,'high');
write('evidence/final-verification.json',{utc:new Date().toISOString(),verifiedTrackedInputs:inputs.length,oleans:0,externalSnapshotsUnchanged:true,activeSessionRecord:{timestamp:t.timestamp,model:t.payload.model,effort:t.payload.effort,controlModel:t.payload.collaboration_mode.settings.model,controlEffort:t.payload.collaboration_mode.settings.reasoning_effort},projectExecutions:0,reportSha256:h(read('handoffs/AUDIT-REPORT.md')),onwardSha256:h(read('handoffs/ONWARD-DISCOVERIES.md'))});
write('evidence/STATUS-before-verdict.md',read('STATUS.md'));
const files=['brief.md','AMENDMENT-1-F001-DISTINCTION.md',...walk('admitted'),...walk('evidence'),...walk('instruments'),...walk('handoffs')].filter(p=>!['handoffs/MANIFEST.sha256','handoffs/MANIFEST-CHECK.txt'].includes(p)).sort();
assert.equal(new Set(files).size,files.length);
write('handoffs/MANIFEST.sha256',files.map(p=>h(read(p))+'  '+p+'\n').join(''));
console.log(JSON.stringify({files:files.length,manifestSha256:h(read('handoffs/MANIFEST.sha256')),reportSha256:h(read('handoffs/AUDIT-REPORT.md')),onwardSha256:h(read('handoffs/ONWARD-DISCOVERIES.md')),utc:new Date().toISOString()},null,2));
