// Read-only textual/artifact reconciliation. No Lean or candidate code execution.
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {execFileSync} from 'node:child_process';
const root=path.dirname(path.dirname(new URL(import.meta.url).pathname));
const repo='/code/reactivegas-66-s3-audit';
const read=p=>fs.readFileSync(p,'utf8');
const lines=p=>read(p).trimEnd().split('\n');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const out=(name,x)=>fs.writeFileSync(`${root}/evidence/${name}`,JSON.stringify(x,null,2)+'\n');
const counts=xs=>xs.reduce((o,x)=>(o[x]=(o[x]??0)+1,o),{});
const ids=lines(`${root}/admitted/OP10-identities.txt`);
const rows=lines(`${root}/admitted/P1A-qualified-classified.txt`).map(l=>{
 const [classification,priv,name,site,short]=l.split('|');
 const [file,line]=site.split(':');
 const source=lines(`${repo}/lean/${file}`);
 const module=file.replace(/\.lean$/,'').replaceAll('/','.');
 const escaped=(module+'.').replace(/[.*+?^${}()|[\]\\]/g,'\\$&');
 const suffix=name.replace(/[.*+?^${}()|[\]\\]/g,'\\$&');
 const pattern=new RegExp('^_private\\.'+escaped+'[0-9]+\\.'+suffix+'$');
 const matches=ids.filter(n=>priv==='yes'?pattern.test(n):n===name);
 const next=source.slice(Number(line)-1).join('\n').split(/\n(?:private )?(?:theorem|def|abbrev|inductive|structure|namespace|end)\b/)[0];
 return {classification,private:priv==='yes',name,site,short,module,matches,sourceLine:source[Number(line)-1],statement:next.split(':=')[0]};
});
out('source-compiled-map.json',rows);
const files=execFileSync('git',['ls-files','--','lean'],{cwd:repo,encoding:'utf8'}).trim().split('\n').filter(f=>f.endsWith('.lean'));
const declarations=[]; const graph={};
for(const file of files){
 const ls=lines(`${repo}/${file}`); const mod=file.replace(/^lean\//,'').replace(/\.lean$/,'').replaceAll('/','.');
 graph[mod]=ls.filter(l=>/^import /.test(l)).flatMap(l=>l.slice(7).trim().split(/\s+/));
 ls.forEach((l,i)=>{const m=l.match(/^(private\s+)?(?:theorem|lemma)\s+([^\s:{(]+)/);if(m)declarations.push({site:file.slice(5)+':'+(i+1),name:m[2],private:!!m[1]});});
}
out('source-import-graph.json',graph);
const raw=lines(`${root}/evidence/supplemental/OP10-stdout.txt`);
const emitted=raw.filter(l=>l.startsWith('axiom-theorem ')).map(l=>l.slice(14));
const axiomRows=raw.filter(l=>l.startsWith('axioms '));
const classes=lines(`${root}/evidence/supplemental/OP10-identity-classes.txt`).map(l=>{const [c,n]=l.split('|');return {class:c,name:n};});
const mapped=rows.flatMap(r=>r.matches);
const desk=JSON.parse(read(`${root}/admitted/S3-OP10-independent-identity-map-20260905.json`));
const discrepancies=rows.filter(r=>!desk.mapping.some(d=>d.name===r.name&&d.module===r.module&&JSON.stringify(d.matches)===JSON.stringify(r.matches)));
const helperText=read(`${root}/admitted/PHASE1-REPORT-R3.md`).split('**Literal per-row roster')[1].split('## §5')[0];
const helpers=[...helperText.matchAll(/`([^`]+)` \[(H-[^\]]+)\]/g)].map(m=>({name:m[1],group:m[2]}));
const filtered=helpers.filter(r=>!/^KelGroups\.Vote\.assoc(?:Adjust|Erase|Insert)_property$/.test(r.name));
const helperNames=rows.filter(r=>r.classification==='HELPER-FACT').map(r=>r.name);
out('reconciliation.json',{
 method:'Static source and retained-output reconciliation only; regex source inventory is not a fresh compiled census.',
 source:{files:files.length,modules:files.length-1,declarations:declarations.length,distinctShort:new Set(declarations.map(x=>x.name)).size,classes:counts(rows.map(r=>r.classification)),private:rows.filter(r=>r.private).length,sourceSitesMissing:declarations.filter(d=>!rows.some(r=>r.site===d.site)),badSourceLines:rows.filter(r=>!r.sourceLine.includes('theorem '+r.short)),qualifiedDistinct:new Set(rows.map(r=>r.name)).size},
 retained:{identities:ids.length,distinct:new Set(ids).size,emitted:emitted.length,emittedDistinct:new Set(emitted).size,rawMatchesIdentityFile:JSON.stringify([...new Set(emitted)].sort())===JSON.stringify([...ids].sort()),axiomRows:axiomRows.length,unexpectedAxioms:axiomRows.filter(l=>!/^axioms .+ = \[(?:(?:propext|Classical.choice|Quot.sound)(?:, )?)*\]$/.test(l)),summary:raw.filter(l=>/^axiom-(sources|theorems|duplicate|gate)/.test(l)),mappingErrors:rows.filter(r=>r.matches.length!==1),mappedDistinct:new Set(mapped).size,deskMapDiscrepancies:discrepancies,remainder:ids.filter(n=>!mapped.includes(n)).length},
 supplementalClasses:{rows:classes.length,distinct:new Set(classes.map(r=>r.name)).size,counts:counts(classes.map(r=>r.class)),missing:ids.filter(n=>!classes.some(r=>r.name===n)),extra:classes.filter(r=>!ids.includes(r.name)),sourceDisagreement:classes.filter(r=>(r.class==='SOURCE'||r.class==='SOURCE-PRIVATE')!==mapped.includes(r.name))},
 helperRoster:{original:helpers.length,afterR4Deletion:filtered.length,counts:counts(filtered.map(r=>r.group)),missing:helperNames.filter(n=>!filtered.some(r=>r.name===n)),extra:filtered.filter(r=>!helperNames.includes(r.name))},
 hashes:Object.fromEntries(['OP10-stdout.txt','OP10-stderr.txt','INDEX.md','OP10-identity-classes.txt','P1C-build2-incremental.log','P1C-build3-restore.log','P1C-scratch-variant-donate.diff'].map(f=>[f,sha(`${root}/evidence/supplemental/${f}`)]))
});
fs.writeFileSync(`${root}/evidence/source-statements.txt`,rows.map(r=>`${r.classification}|${r.name}|${r.site}\n${r.statement}\n`).join('\n'));
console.log(read(`${root}/evidence/reconciliation.json`));
