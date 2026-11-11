// Index retained archive table text, without executing any archived instrument.
import fs from 'node:fs';
import crypto from 'node:crypto';
const root= new URL('.',import.meta.url).pathname;
const names=fs.readFileSync(root+'archive-list.txt','utf8').trim().split('\n').filter(n=>n.includes('campaign-ledger')&&!n.endsWith('/')).sort();
const rows=names.map(name=>{
 const file=root+'archive/'+name, raw=fs.readFileSync(file,'utf8');
 const table=raw.split('\n').map((s,i)=>({line:i+1,s})).filter(x=>x.s.startsWith('|')||x.s.includes('\t')).map(x=>({line:x.line,cells:x.s.split(x.s.startsWith('|')?'|':'\t').map(s=>s.trim()).filter(Boolean)}));
 return {name,sha256:crypto.createHash('sha256').update(raw).digest('hex'),tableLines:table,scope:name.includes('simulator')?'simulator':name.includes('toolchain')?'toolchain':name.includes('t74-')?'Haskell exporter':name.includes('release-pipeline')?'release':name.includes('t59')||name.includes('docs-auditor')?'documentation':name.includes('t62')?'S62 mixed phases':name.includes('t57')?'S57':name.includes('t54')?'S54':name.includes('t48')?'S48':'unresolved'};
});
fs.writeFileSync(root+'receipt-archive-index.json',JSON.stringify(rows,null,2)+'\n');
fs.writeFileSync(root+'receipt-file-roster.md','# Retained campaign file inventory\n\nInspection only. Table-line counts include headers and are not receipt counts; each raw table is retained in receipt-archive-index.json. Multiple mutations per INV row must remain distinct during admissibility assessment.\n\n| File | Scope | SHA-256 |\n|---|---|---|\n'+rows.map(r=>`| ${r.name} | ${r.scope} | ${r.sha256} |`).join('\n')+'\n');
console.log(JSON.stringify(rows.map(r=>({name:r.name,scope:r.scope,rows:r.tableLines.filter(x=>!x.cells.every(c=>/^[- :]+$/.test(c))).map(x=>x.cells.slice(0,4))})),null,2));
