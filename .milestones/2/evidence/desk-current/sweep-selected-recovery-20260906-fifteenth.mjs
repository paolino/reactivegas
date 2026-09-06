import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import cp from 'node:child_process';
const src='/tmp/reactivegas/ms2', checkout='/tmp/ms-reactivegas-2/snapshot-20260905T2100', dest=path.join(checkout,'.milestones/2');
const expected='b4e36c5c1b39c7349ecb4900054ece24dfaddeae';
const git=(...args)=>cp.execFileSync('git',args,{cwd:checkout,encoding:'utf8',maxBuffer:128*1024*1024}).trim();
const hash=b=>crypto.createHash('sha256').update(b).digest('hex');
const put=(p,b)=>{fs.mkdirSync(path.dirname(p),{recursive:true});if(fs.existsSync(p))fs.chmodSync(p,0o644);fs.writeFileSync(p,b);};
const copy=(a,b)=>put(path.join(dest,b),fs.readFileSync(a));
const copied=[];
function tree(a,b,filter=()=>true){if(!fs.existsSync(a))return;for(const e of fs.readdirSync(a,{withFileTypes:true})){if(e.name.startsWith('.')||['node_modules','dist-newstyle','target','build'].includes(e.name))continue;const s=path.join(a,e.name),d=path.join(b,e.name);if(e.isDirectory())tree(s,d,filter);else if(e.isFile()&&filter(s)&&! /\.(olean|ilean|ir|o|a|so|db|sqlite|sqlite3)$/.test(e.name)){const st=fs.statSync(s);if(st.size>12*1024*1024)throw Error('selected file exceeds review bound '+s);copy(s,d);copied.push({source:s,destination:d,bytes:st.size});}}}
const wiki='/code/reactivegas.wiki';const wg=(...args)=>cp.execFileSync('git',args,{cwd:wiki,encoding:'utf8'}).trim();
const wsha=wg('rev-parse','HEAD');if(wsha!==wg('rev-parse','origin/master'))throw Error('wiki head drift');for(const f of ['Milestone-2-Stories.json','Milestone-2.md'])if(!cp.execFileSync('git',['show','origin/master:'+f],{cwd:wiki}).equals(fs.readFileSync(path.join(wiki,f))))throw Error('wiki bytes drift');
const wdigest=hash(fs.readFileSync(path.join(wiki,'Milestone-2-Stories.json')));let resume=fs.readFileSync(path.join(src,'RESUME.md'),'utf8');resume=resume.replace(/Wiki https:\/\/github.com\/paolino\/reactivegas\/wiki\/Milestone-2 verified[0-9a-f]+; register[0-9a-f]+\./,`Wiki https://github.com/paolino/reactivegas/wiki/Milestone-2 verified${wsha}; register${wdigest}.`);fs.writeFileSync(path.join(src,'RESUME.md'),resume);
fs.appendFileSync(path.join(src,'STATUS.md'),`\n${new Date().toISOString()}  WIKI-VERIFIED  ${wsha} register${wdigest}; fresh remote page/register byte comparison PASS,34stories7groups. Terminal audit findings and current owner dispositions reflected; no new acceptance.\n`);
git('fetch','origin','milestones');if(git('rev-parse','origin/milestones')!==expected)throw Error('recovery moved; integrate instead of overwrite');
copy(path.join(src,'LEDGER.md'),'ledger.md');copy(path.join(src,'RESUME.md'),'resume/ms.md');copy(path.join(src,'STATUS.md'),'evidence/desk-STATUS-current.md');
for(const f of ['Milestone-2-Stories.json','Milestone-2.md'])copy(path.join(wiki,f),'wiki/'+f);
tree(path.join(src,'artifacts'),'evidence/desk-current',s=>fs.statSync(s).mtimeMs>=Date.parse('2026-09-05T23:21:25Z')&&/\.(md|json|mjs|txt|log)$/.test(s));
tree(path.join(src,'e-lean-compliance/commit-owner-s3-phase1/handoffs'),'evidence/S3-phase1');
tree(path.join(src,'e-lean-compliance/commit-owner-s4b-muse/handoffs'),'evidence/S4B-submission2/handoffs');
tree(path.join(src,'e-lean-compliance/commit-owner-s4b-muse/instruments'),'evidence/S4B-submission2/instruments');
tree(path.join(src,'e-lean-compliance/handoffs'),'evidence/quality-parent-current',s=>fs.statSync(s).mtimeMs>=Date.parse('2026-09-05T23:21:25Z'));
tree(path.join(src,'t-simulator-fable/handoffs'),'evidence/simulator-preparation',s=>path.basename(s).startsWith('C2-'));
tree(path.join(src,'e-kelgroups-substrate/handoffs'),'evidence/K28-epic-acceptance-and-merge',s=>fs.statSync(s).mtimeMs>=Date.parse('2026-09-06T03:15:00Z'));
tree(path.join(src,'e-kelgroups-substrate/t28-app-api/handoffs'),'evidence/K28-frozen-current',s=>fs.statSync(s).mtimeMs>=Date.parse('2026-09-05T23:21:25Z'));
for(const f of ['slim-build.log','slim-test.log','slim-ci.log'])copy(path.join(src,'e-kelgroups-substrate/t28-app-api/commit-owner-s28r1',f),'evidence/K28-SLIM/'+f);
for(const lane of ['e-lean-compliance','e-lean-compliance/commit-owner-s4b-muse','e-lean-compliance/commit-owner-s3-phase1','e-kelgroups-substrate','e-kelgroups-substrate/t28-app-api','e-kelgroups-substrate/t28-app-api/commit-owner-s28r1','t-simulator-fable']){for(const f of ['STATUS.md','RESUME.md']){const a=path.join(src,lane,f);if(fs.existsSync(a))copy(a,'evidence/current-'+lane.replaceAll('/','-')+'-'+f);}tree(path.join(src,lane,'inbox'),'evidence/current-inbox/'+lane,s=>fs.statSync(s).mtimeMs>=Date.parse('2026-09-05T23:21:25Z'));}
const patch=cp.execFileSync('git',['diff','3590c0015b84fd58004bf6fb44dd18b107304c48','b667648752b8fa8a7b890f115413a99ba04518dc','--'],{cwd:'/code/reactivegas-66-s4b'});put(path.join(dest,'evidence/S4B-submission2/unaccepted-b667648-full.patch'),patch);


// Retain the actual latest committed full S4 candidate separately from historical b667648.
const s4head=cp.execFileSync('git',['rev-parse','HEAD'],{cwd:'/code/reactivegas-66-s4b',encoding:'utf8'}).trim();
const s4patch=cp.execFileSync('git',['diff','3590c0015b84fd58004bf6fb44dd18b107304c48',s4head,'--'],{cwd:'/code/reactivegas-66-s4b'});
put(path.join(dest,'evidence/S4B-submission2/unaccepted-'+s4head+'-full.patch'),s4patch);
put(path.join(dest,'evidence/S4B-submission2/committed-identity-20260906T0430.json'),JSON.stringify({at:new Date().toISOString(),head:s4head,base:'3590c0015b84fd58004bf6fb44dd18b107304c48',patchSha256:hash(s4patch),scope:'Committed unaccepted candidate, no final validation or audit inherited.'},null,2)+'\n');

// Retain S28-R2 exact accepted source and historical evidence; landing is separately receipted.
const kroot=path.join(src,'e-kelgroups-substrate/t28-app-api');
tree(path.join(kroot,'commit-owner-s28r2'),'evidence/K28-commit-owner-s28r2');
const kwt='/code/kelgroups-issue-28';
const ksha=cp.execFileSync('git',['rev-parse','HEAD'],{cwd:kwt,encoding:'utf8'}).trim();
const kpatch=cp.execFileSync('git',['diff','368b596fef0b6d393c2ac7afc631d236c55d86d1',ksha,'--'],{cwd:kwt});
put(path.join(dest,'evidence/K28-S28R2/accepted-'+ksha+'-full.patch'),kpatch);
const kgate=fs.readFileSync(path.join(kwt,'gate.sh'));
put(path.join(dest,'evidence/K28-S28R2/gate-observed-20260906T0430.sh'),kgate);
put(path.join(dest,'evidence/K28-S28R2/identity-20260906T0430.json'),JSON.stringify({at:new Date().toISOString(),head:ksha,base:'368b596fef0b6d393c2ac7afc631d236c55d86d1',patchSha256:hash(kpatch),gateSha256:hash(kgate),scope:'Accepted candidateab25cd1 landed through PR32 as933e385 with an identical tree. This source/gate copy preserves inputs; the sealed audit and actual landing receipt establish acceptance.'},null,2)+'\n');

// Capture the frozen S3 audit by its actual manifest, not its entire extracted archive.
const s3Root=path.join(src,'e-lean-compliance/candidate-auditor-s3-phase1-static-codex');
const s3Prefix='evidence/S3-static-audit-terminal';
const s3Manifest=fs.readFileSync(path.join(s3Root,'handoffs/AUDIT-MANIFEST.sha256'),'utf8');
for(const line of s3Manifest.split('\n').filter(Boolean)){
 const m=line.match(/^([0-9a-f]{64})\s+\*?(.+)$/);if(!m)throw Error('bad S3 manifest');
 const a=path.resolve(s3Root,m[2]);if(!a.startsWith(s3Root+'/'))throw Error('manifest outside root');
 const b=fs.readFileSync(a);if(hash(b)!==m[1])throw Error('changed frozen S3 input '+m[2]);
 put(path.join(dest,s3Prefix,m[2]),b);copied.push({source:a,destination:path.join(s3Prefix,m[2]),bytes:b.length});
}
for(const f of ['brief.md','STATUS.md','handoffs/AUDIT-MANIFEST.sha256','handoffs/AUDIT-MANIFEST-CHECK.txt'])copy(path.join(s3Root,f),path.join(s3Prefix,f));
// Kelgroups terminal roots contain retained source/receipts; omit only reproducible build trees.
for(const lane of ['commit-auditor-s28r1','commit-auditor-s28r1b'])tree(path.join(src,'e-kelgroups-substrate/t28-app-api',lane),'evidence/K28-'+lane,s=>!s.endsWith('.hi')&&!s.endsWith('.dyn_hi')&&!s.endsWith('.dyn_o'));
const workingPatch=cp.execFileSync('git',['diff','HEAD','--'],{cwd:'/code/reactivegas-66-s4b'});
put(path.join(dest,'evidence/S4B-submission2/working-preparation-20260906T0430.patch'),workingPatch);
put(path.join(dest,'evidence/S4B-submission2/working-preparation-20260906T0430.json'),JSON.stringify({at:new Date().toISOString(),head:cp.execFileSync('git',['rev-parse','HEAD'],{cwd:'/code/reactivegas-66-s4b',encoding:'utf8'}).trim(),patchSha256:hash(workingPatch),scope:'Point-in-time uncommitted preparation, unvalidated and unaccepted. Not a frozen candidate.'},null,2)+'\n');

put(path.join(dest,'evidence/selected-overlay-20260906T0430.json'),JSON.stringify({at:new Date().toISOString(),copied,scope:'Selected runtime evidence overlay, source patch, status and wiki. Existing snapshot content preserved. Compiled outputs, databases, hidden paths and build directories excluded. Not a full-host backup.'},null,2)+'\n');

// Refresh actual resurrection destinations, not only evidence aliases.
copy(path.join(dest,'session.md'),'evidence/session-before-20260906T0430.md');
copy(path.join(src,'artifacts/SESSION-CURRENT-20260906T0430.md'),'session.md');
const fragments=[['e-lean-compliance/RESUME.md','e66.md'],['e-haskell-impl/handoffs/RESUME-FRAGMENT.md','e67.md'],['t-simulator-fable/RESUME.md','t70.md'],['t68-proposer-assent/RESUME.md','t68.md'],['t71-design-record/RESUME.md','t71.md'],['e-kelgroups-substrate/RESUME-FRAGMENT.md','e-kelgroups29.md']];
const fragmentReceipt=[];
for(const [source,target] of fragments){const b=fs.readFileSync(path.join(src,source));const out='resume/'+target; if(fs.existsSync(path.join(dest,out)))copy(path.join(dest,out),'evidence/resume-before-20260906T0430/'+target);put(path.join(dest,out),b);fragmentReceipt.push({source,target:out,sha256:hash(b)});}
put(path.join(dest,'evidence/resume-aggregation-20260906T0430.json'),JSON.stringify(fragmentReceipt,null,2)+'\n');


// Active independent audits: retain current protocol and owned evidence, never a terminal-verdict claim.
for(const [lane,label] of [['e-kelgroups-substrate/t28-app-api/commit-auditor-s28r2','K28-S28R2-active-audit'],['e-lean-compliance/candidate-auditor-s4b-sub2-final','S4B-sub2-active-audit'],['e-lean-compliance/candidate-auditor-s4b-sub2-final-r2','S4B-sub2-recommission']]){ const a=path.join(src,lane); for(const f of ['brief.md','STATUS.md','launch.sh'])if(fs.existsSync(path.join(a,f)))copy(path.join(a,f),'evidence/'+label+'/'+f);for(const sub of ['admitted','inbox','questions','answers','handoffs'])tree(path.join(a,sub),'evidence/'+label+'/'+sub,s=>! /\.(hi|dyn_hi|dyn_o)$/.test(s));}


// Final K28 sealed inventory: independently reverify and copy every declared file.
const finalRoot=path.join(src,'e-kelgroups-substrate/t28-app-api/commit-auditor-s28r2/handoffs');
const finalInv=JSON.parse(fs.readFileSync(path.join(finalRoot,'EVIDENCE-INVENTORY.json')));
for(const e of finalInv.entries){const a=path.resolve(finalRoot,e.path);if(!a.startsWith(finalRoot+'/'))throw Error('K28 inventory outside');const b=fs.readFileSync(a);if(b.length!==e.bytes||hash(b)!==e.sha256)throw Error('K28 final changed '+e.path);put(path.join(dest,'evidence/K28-S28R2-final-audit/handoffs',e.path),b);}
copy(path.join(finalRoot,'EVIDENCE-INVENTORY.json'),'evidence/K28-S28R2-final-audit/handoffs/EVIDENCE-INVENTORY.json');
copy(path.join(finalRoot,'../STATUS.md'),'evidence/K28-S28R2-final-audit/STATUS.md');


// Sealed S4 preflight contains current-source specimens, not executed audit acceptance.
const s4AuditRoot=path.join(src,'e-lean-compliance/candidate-auditor-s4b-sub2-final');
const s4AuditManifest=fs.readFileSync(path.join(s4AuditRoot,'EVIDENCE-MANIFEST.sha256'),'utf8');
for(const line of s4AuditManifest.trim().split('\n')){const m=line.match(/^([a-f0-9]{64})\s+\*?(.+)$/);if(!m)throw Error('bad S4 audit manifest');const a=path.resolve(s4AuditRoot,m[2]);if(!a.startsWith(s4AuditRoot+'/'))throw Error('S4 audit outside');const b=fs.readFileSync(a);if(hash(b)!==m[1])throw Error('S4 audit changed');put(path.join(dest,'evidence/S4B-sub2-terminal-preflight',m[2]),b);}
copy(path.join(s4AuditRoot,'EVIDENCE-MANIFEST.sha256'),'evidence/S4B-sub2-terminal-preflight/EVIDENCE-MANIFEST.sha256');
copy(path.join(s4AuditRoot,'STATUS.md'),'evidence/S4B-sub2-terminal-preflight/STATUS.md');

// Current M1 and instrument history, excluding build outputs; no semantic acceptance implied.
const mroot=path.join(src,'e-lean-compliance/candidate-auditor-s4b-sub2-final-r2');
tree(path.join(mroot,'instruments'),'evidence/S4B-sub2-recommission/instruments');
for(const dir of ['M1','M1R','M1-R1','M1-retry'])tree(path.join(mroot,'evidence',dir),'evidence/S4B-sub2-recommission/evidence/'+dir,s=>path.basename(s)!=='inventory.jsonl');
for(const file of fs.readdirSync(path.join(mroot,'evidence')).filter(f=>/\.sha256$/.test(f)))copy(path.join(mroot,'evidence',file),'evidence/S4B-sub2-recommission/evidence/'+file);
for(const lane of ['e-haskell-impl','e-kelgroups-substrate']){copy(path.join(src,lane,'STATUS.md'),'evidence/current-'+lane+'-STATUS.md');if(fs.existsSync(path.join(src,lane,'EPIC-MAP.md')))copy(path.join(src,lane,'EPIC-MAP.md'),'evidence/current-'+lane+'-EPIC-MAP.md');}
// The raw Expr inventory is 234905343 bytes. Preserve it losslessly compressed, with both digests.
const invReceipt=JSON.parse(fs.readFileSync(path.join(src,'artifacts/S4-M1R-inventory-20260906T0405.json')));
const zipped=fs.readFileSync(invReceipt.archive);if(hash(zipped)!==invReceipt.compressedSha256)throw Error('compressed inventory drift');
put(path.join(dest,'evidence/S4B-sub2-recommission/evidence/M1R/inventory.jsonl.gz'),zipped);
copy(path.join(src,'artifacts/S4-M1R-inventory-20260906T0405.json'),'evidence/S4B-sub2-recommission/evidence/M1R/inventory-archive-receipt.json');
tree(path.join(src,'e-kelgroups-substrate/t30-contract'),'evidence/K30-contract-preparation');
function files(d,p=''){return fs.readdirSync(d,{withFileTypes:true}).flatMap(e=>e.isDirectory()?files(path.join(d,e.name),path.join(p,e.name)):e.isFile()?[path.join(p,e.name)]:[]);}
const inventory=files(dest).filter(f=>f!=='SHA256SUMS.snapshot').sort();put(path.join(dest,'SHA256SUMS.snapshot'),inventory.map(f=>hash(fs.readFileSync(path.join(dest,f)))+'  '+f).join('\n')+'\n');
git('add','--','.milestones/2');const treeOid=git('write-tree');const commit=git('commit-tree',treeOid,'-m','milestone 2 selected evidence and resume checkpoint');git('push',`--force-with-lease=refs/heads/milestones:${expected}`,'origin',`${commit}:refs/heads/milestones`);git('fetch','origin','milestones');if(git('rev-parse','origin/milestones')!==commit)throw Error('published head mismatch');if(git('show','-s','--format=%P',commit)!=='')throw Error('snapshot not root');
const readback='/tmp/ms-reactivegas-2/readback-'+Date.now();fs.mkdirSync(readback);const archive=cp.execFileSync('git',['archive','origin/milestones','.milestones/2'],{cwd:checkout,maxBuffer:128*1024*1024});cp.execFileSync('tar',['-x','-C',readback],{input:archive});const base=path.join(readback,'.milestones/2');const manifest=fs.readFileSync(path.join(base,'SHA256SUMS.snapshot'),'utf8').trim().split('\n');for(const line of manifest){const sh=line.slice(0,64),f=line.slice(66);if(hash(fs.readFileSync(path.join(base,f)))!==sh)throw Error('readback digest mismatch '+f);}
const zlib=await import('node:zlib');
const restored=zlib.gunzipSync(fs.readFileSync(path.join(base,'evidence/S4B-sub2-recommission/evidence/M1R/inventory.jsonl.gz')));if(restored.length!==invReceipt.rawBytes||hash(restored)!==invReceipt.rawSha256)throw Error('remote inventory restoration mismatch');
const receipt={inventoryRoundtrip:true,verifiedAt:new Date().toISOString(),commit,tree:treeOid,files:manifest.length,parents:0,readback,wikiCommit:wsha,registerSha256:wdigest};fs.writeFileSync(path.join(src,'artifacts/snapshot-20260906T0430-identity.json'),JSON.stringify(receipt,null,2)+'\n');fs.appendFileSync(path.join(src,'STATUS.md'),`\n${receipt.verifiedAt}  RECOVERY-VERIFIED  ${commit} tree${treeOid} ${manifest.length}selectedfiles freshremotearchiveSHAverified zeroParents; S3terminal323-file-audit,S4preparation,K28sealed135-file-terminal-and-S4-admission-gap,C2readiness,wiki${wsha} included. No fullhostbackupclaim.\n`);console.log(JSON.stringify(receipt));
