import fs from 'node:fs';
import crypto from 'node:crypto';
import path from 'node:path';
const root = '/tmp/reactivegas/ms2/e-lean-compliance/supplement-auditor-s4b';
const out = path.join(root, 'handoffs/evidence');
const source = '/code/reactivegas-66-s4b-final-audit';
const prior = '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-final';
const lake = '/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/src/lean';
const read = p => fs.readFileSync(p);
const hash = b => crypto.createHash('sha256').update(b).digest('hex');
const write = (p, v) => fs.writeFileSync(path.join(out, p), typeof v === 'string' || Buffer.isBuffer(v) ? v : JSON.stringify(v,null,2)+'\n');
const copied = [];
function copy(p, name) { const b=read(p); write(name,b); copied.push({source:p,snapshot:name,sha256:hash(b)}); }
function checkManifest(p, base) {
  return read(p).toString().trimEnd().split('\n').map(line => {
    const m = /^([0-9a-f]{64})  (.+)$/.exec(line); if(!m) throw Error('Malformed manifest');
    const actual = hash(read(path.join(base,m[2])));
    if(actual !== m[1]) throw Error('Digest mismatch: '+m[2]);
    return {file:m[2],sha256:actual,ok:true};
  });
}
const integrity = {};
for(const [manifest,dir] of [['INPUTS-MANIFEST.sha256','inputs'],['INPUTS-AMENDMENT-1-MANIFEST.sha256','inputs-amendment-1']]) {
  const base=path.join(root,dir); integrity[manifest]=checkManifest(path.join(root,manifest),base);
  fs.mkdirSync(path.join(out,dir),{recursive:true});
  for(const row of integrity[manifest]) copy(path.join(base,row.file),dir+'/'+row.file);
  copy(path.join(root,manifest),manifest);
}
integrity.prior = checkManifest(path.join(root,'inputs/PRIOR-FULL-AUDIT-MANIFEST.sha256'),prior);
write('input-integrity.json',integrity);
for(const f of ['brief.md','inbox/AMENDMENT-1-CURRENT-STATE-AND-ADDITIVITY.md','inbox/NOTE-USAGE-LIMIT-NUDGE-20260906.md']) copy(path.join(root,f),path.basename(f));
for(const f of ['justfile','lean/lakefile.lean','lean/lean-toolchain','scripts/check-lean-mirrors','scripts/check-lean-axioms','scripts/check-reactivegas-inversion-coverage','scripts/check-lean-toolchain','scripts/check-trace-coverage-agreement','nix/lean-dependency-direction.sh','lean/Reactivegas/Mirrors.lean','lean/Reactivegas/Predicates.lean','lean/Reactivegas/Trace.lean','lean/Reactivegas/Step.lean','.github/workflows/ci.yaml']) copy(path.join(source,f),'source-'+f.replaceAll('/','_')+'.txt');
for(const f of ['lake/Lake/Build/Module.lean','lake/Lake/Build/Common.lean','lake/Lake/Build/Run.lean','lake/Lake/Build/Context.lean','lake/Lake/CLI/Main.lean','Lean/Elab/BuiltinCommand.lean','Lean/DocString/Add.lean']) copy(path.join(lake,f),'toolchain-'+f.replaceAll('/','_')+'.txt');
for(const f of ['admitted/RULING-RG-S4-REACH-20260906.md','admitted/ISSUE-66-BODY-READBACK.md','evidence/owner-d1-fit.md','evidence/comment-boundary.json','evidence/driver.after.txt','evidence/driver.before.txt','evidence/lean_Reactivegas_Mirrors.lean.before.txt','evidence/scripts_check-lean-mirrors.before.txt']) copy(path.join(prior,f),'prior-'+path.basename(f));
const tree=read(path.join(out,'candidate-tree.z')).toString().split('\0').filter(Boolean);
const blobs=tree.map(row=>{const m=/^(\d+) (\S+) ([a-f0-9]+)\t(.*)$/.exec(row);const b=read(path.join(source,m[4]));const actual=crypto.createHash('sha1').update(Buffer.from('blob '+b.length+'\0')).update(b).digest('hex');if(actual!==m[3])throw Error('Worktree blob differs: '+m[4]);return {file:m[4],mode:m[1],gitBlob:actual,sha256:hash(b)};});
write('candidate-blobs.json',blobs);
const trackedLean=blobs.filter(x=>x.file.startsWith('lean/')&&x.file.endsWith('.lean')&&x.file!=='lean/lakefile.lean');
write('tracked-modules.json',trackedLean.map(x=>x.file.slice(5,-5).replaceAll('/','.')));
const importers=trackedLean.filter(x=>/^import\s+Reactivegas\.Mirrors(?:\s|$)/m.test(read(path.join(source,x.file)).toString())).map(x=>x.file);
const results={trackedModules:trackedLean.length,trackedMirrorsImporters:importers};
for(const name of ['S2-CI-final-clean.log','SUPERSEDED-S2-CI-comment-only.log']) {
 const b=read(path.join(root,'inputs',name)); const lines=b.toString().trimEnd().split('\n');
 const numbered=lines.map((text,i)=>({line:i+1,text}));
 const progress=numbered.filter(x=>/\[\d+\/\d+\]/.test(x.text));
 const mirror=numbered.filter(x=>/^MIRROR-/.test(x.text));
 const categories={}; for(const x of numbered){const k=x.text.startsWith('axiom-theorem ')?'axiom-theorem':x.text.startsWith('axioms ')?'axioms':x.text.startsWith('axiom-module ')?'axiom-module':x.text.startsWith('warning: ')?'warning':x.text.startsWith('info: ')?'info':'other'; categories[k]=(categories[k]||0)+1;}
 results[name]={sha256:hash(b),bytes:b.length,lines:lines.length,dirty: numbered.filter(x=>/is dirty/.test(x.text)),progress,mirror,categories,axiomSummary:numbered.filter(x=>/^axiom-(sources|theorems|gate)|^axiom-module .*Mirrors/.test(x.text)),end: numbered.slice(-5)};
 const remaining=numbered.filter(x=>!/^axiom-theorem |^axioms |^axiom-module /.test(x.text));
 write(name+'.non-enumeration.txt',remaining.map(x=>x.line+'\t'+x.text).join('\n')+'\n');
}
const mirrorLines = n=>results[n].mirror.map(x=>x.text).filter(x=>!x.startsWith('MIRROR-RECEIPT-WROTE'));
results.sameMirrorEnumeration = JSON.stringify(mirrorLines('S2-CI-final-clean.log'))===JSON.stringify(mirrorLines('SUPERSEDED-S2-CI-comment-only.log'));
const nonce=1788678416820927632n;results.nonceUtc=new Date(Number(nonce/1000000n)).toISOString();
write('log-analysis.json',results);
const maps=['inputs/CLOSURE-MAP.pre-fs02-preserved.md','inputs/CLOSURE-MAP.after-fs02.md','inputs-amendment-1/CLOSURE-MAP.v2-CURRENT.md'].map(f=>({file:f,text:read(path.join(root,f)).toString()}));
const marker='# HISTORICAL RECORD — preserved, superseded where it conflicts with the above';
const histories=maps.map(x=>x.text.slice(x.text.indexOf(marker)));
const oldLine=maps[1].text.split('\n').find(x=>x.startsWith('| **S2** |'));
write('map-analysis.json',{maps:maps.map((x,i)=>({file:x.file,sha256:hash(x.text),lines:x.text.trimEnd().split('\n').length,historicalSha256:hash(histories[i])})),historicalTailIdentical:histories.every(x=>x===histories[0]),v1Preserved:read(path.join(root,'inputs/CLOSURE-MAP.after-fs02.md')).equals(read(path.join(root,'inputs-amendment-1/CLOSURE-MAP.v1-fs02-only-preserved.md'))),priorS2CurrentRow:oldLine,priorS2CurrentRowInV2History:histories[2].includes(oldLine),priorS2MarkerInV2History:histories[2].includes('submission 3'),successorCampaignMarkerInV2History:histories[2].includes('S2-SUCCESSOR-CAMPAIGN-PROPOSAL')});
write('snapshot-index.json',copied);
console.log(JSON.stringify({inputs:integrity['INPUTS-MANIFEST.sha256'].length,amended:integrity['INPUTS-AMENDMENT-1-MANIFEST.sha256'].length,prior:integrity.prior.length,blobs:blobs.length,modules:trackedLean.length,sameMirrorEnumeration:results.sameMirrorEnumeration,snapshots:copied.length}));
