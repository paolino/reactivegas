import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
const prior='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
const owner='/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse';
const parent='/tmp/reactivegas/ms2/e-lean-compliance';
const h=s=>crypto.createHash('sha256').update(s).digest('hex');
const read=p=>fs.readFileSync(p);
const json=p=>JSON.parse(read(p));
const save=(p,s)=>fs.writeFileSync('evidence/'+p,typeof s==='string'||Buffer.isBuffer(s)?s:JSON.stringify(s,null,2)+'\n');
function manifest(root,file) {
 const lines=read(path.join(root,file)).toString().trim().split('\n'),entries=[];
 for(const line of lines){const m=/^([0-9a-f]{64})  (.+)$/.exec(line);assert(m);const [,expected,rel]=m;assert(!path.isAbsolute(rel)&&!rel.split('/').includes('..'));assert.notEqual(rel,file);const b=read(path.join(root,rel));assert.equal(h(b),expected,rel);entries.push({path:rel,sha256:expected,bytes:b.length});}
 assert.equal(new Set(entries.map(x=>x.path)).size,entries.length);return {manifest:file,sha256:h(read(path.join(root,file))),count:entries.length,selfEntry:false,entries};
}
const a=manifest('admitted','MANIFEST.sha256');assert.equal(a.count,20);save('admitted-integrity.json',a);
const p=manifest(prior,'handoffs/full-audit-v2/MANIFEST.sha256');assert.equal(p.count,665);save('prior-integrity.json',p);
const rows=json(prior+'/handoffs/full-audit-v2/COMMAND-OUTCOMES.json');assert.equal(rows.length,82);
const verified=[];
for(const r of rows){
 const stem=prior+'/'+r.receipt.slice(0,-5);const receipt=json(stem+'.json');
 for(const k of ['id','start','finish','exit','stdoutSha256','stderrSha256'])assert.deepEqual(receipt[k],r[k],r.id+':'+k);
 for(const stream of ['stdout','stderr'])assert.equal(h(read(stem+'.'+stream)),r[stream+'Sha256'],r.id+':'+stream);
 verified.push({id:r.id,candidate:'94bb7bb64324a48f7361252556b4d15e45b3923f',charge:r.charge,start:r.start,finish:r.finish,exit:r.exit,receipt:r.receipt,stdoutSha256:r.stdoutSha256,stderrSha256:r.stderrSha256,interestingLines:read(stem+'.stdout').toString().split('\n').filter(x=>/error:|MIRROR-(MISSING|UNCOVERED|THEOREM-MISSING|RECEIPT-ABSENT|IMPORT-REACH-GAP|CHECK-OK)|AUDIT-.*(FAIL|UNKNOWN)|REACH|P07/.test(x)).slice(0,8)});
}
save('prior-command-integrity.json',verified);
const atoms=json(prior+'/handoffs/full-audit-v2/ATOM-DISPOSITIONS.json');assert.equal(atoms.length,44);
assert.equal(new Set(atoms.map(x=>x.id)).size,44);
for(const x of atoms){assert.equal(x.editCount,1);assert.equal(x.exit,1);assert(x.errors.length>0);assert(rows.some(r=>r.id===x.id));}
save('prior-atoms-retained.json',atoms);
for(const file of ['CAMPAIGN-LEDGER.md','SECOND-DEVIATION-REVIEW.md','RECEIPT-INTEGRITY.json','FINAL-OLEAN-VERIFICATION.json'])save('prior-'+file,read(prior+'/handoffs/full-audit-v2/'+file));
const snapshots=[['owner-submission-3.md',owner+'/handoffs/SUBMISSION-3.md'],['owner-final-ci.log',owner+'/handoffs/evidence/S2-CI-comment-only.log'],['owner-STATUS.txt',owner+'/STATUS.md'],['closure-map.md',parent+'/handoffs/CLOSURE-MAP.md'],['owner-d1-fit.md',parent+'/handoffs/S4-D1-FIT-VERIFICATION-AND-MY-WITHDRAWAL.md']];
const index=[];
for(const [to,from]of snapshots){const b=read(from);save(to,b);index.push({source:from,snapshot:'evidence/'+to,sha256:h(b),bytes:b.length,observedAt:new Date().toISOString()});}
save('external-snapshot-index.json',index);
const ci=read('evidence/owner-final-ci.log').toString(),lines=ci.split('\n');
save('final-ci-assessment.json',{sha256:h(ci),bytes:Buffer.byteLength(ci),lines:lines.length-1,dirtyWarnings:lines.flatMap((s,i)=>s.includes("Git tree '/code/reactivegas-66-s4b' is dirty")?[{line:i+1,text:s}]:[]),headMentions:lines.filter(s=>s.includes('04eb6c7')),exitEnvelope:lines.filter(s=>/^(EXIT|exit|status|start|finish|candidate|HEAD)[ =:]/.test(s)),markers:lines.flatMap((s,i)=>/MIRROR-CHECK-OK|MIRROR-RECEIPT-WROTE|Build completed|corpus.*OK|corpus-check:|^axiom-theorems count=|^lean-theorems declared=/.test(s)?[{line:i+1,text:s}]:[]),limit:'Owner reports exit 0. Raw combined log lacks exact SHA, start/finish and exit envelope; first two lines warn dirty source. No execution performed here.'});
console.log(JSON.stringify({admitted:a.count,priorFiles:p.count,priorCommandStreamsVerified:verified.length*2,priorAtoms:atoms.length,priorSubstantive:rows.filter(r=>r.charge==='substantive').length,priorTargeted:rows.filter(r=>r.charge==='targeted').length,retainedAdditionalM1Operations:3,finalCiDirtyWarnings:lines.filter(s=>s.includes("Git tree '/code/reactivegas-66-s4b' is dirty")).length,projectExecutions:0},null,2));
