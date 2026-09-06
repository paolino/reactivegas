// Auditor-owned read/parse/hash inventory. No project execution, imports, or mutations.
const fs=require('fs'),path=require('path'),crypto=require('crypto');
const root=path.resolve(__dirname,'../..'), packet=path.join(root,'inputs/PACKET-UNDER-REVIEW');
const source='/code/reactivegas-66-s3-audit-final';
const json=n=>JSON.parse(fs.readFileSync(path.join(packet,n+'.json'),'utf8'));
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const write=(n,x)=>fs.writeFileSync(path.join(__dirname,n),JSON.stringify(x,null,2)+'\n');
const walk=d=>fs.readdirSync(d,{withFileTypes:true}).flatMap(e=>e.isDirectory()?walk(path.join(d,e.name)):[path.join(d,e.name)]);
const I=json('identities'),A=json('atoms'),O=json('operations'),R=json('row-outcomes'),H=json('helper-instances'),W=json('ownership'),M=json('measurement-operations');
const count=(xs,f)=>xs.reduce((a,x)=>(a[f(x)]=(a[f(x)]||0)+1,a),{});
const duplicates=xs=>Object.entries(count(xs,x=>x)).filter(([k,n])=>n>1);
const files=walk(source+'/lean').filter(f=>f.endsWith('.lean')&&!f.includes('/.lake/'));
// Mask nested block comments, line comments, and strings while preserving all newlines/offsets.
function mask(t){let r='',d=0,s=false,line=false;for(let i=0;i<t.length;i++){
 const a=t[i],b=t[i+1];
 if(line){if(a==='\n'){line=false;r+='\n';}else r+=' ';continue;}
 if(d){if(a==='/'&&b==='-'){d++;r+='  ';i++;}else if(a==='-'&&b==='/'){d--;r+='  ';i++;}else r+=a==='\n'?'\n':' ';continue;}
 if(s){if(a==='\\'){r+='  ';i++;}else if(a==='"'){s=false;r+=' ';}else r+=a==='\n'?'\n':' ';continue;}
 if(a==='-'&&b==='-'){line=true;r+='  ';i++;}else if(a==='/'&&b==='-'){d++;r+='  ';i++;}else if(a==='"'){s=true;r+=' ';}else r+=a;
 }return r;}
const lexicalControls={comments:mask('/- theorem fake /- lemma nested -/ -/\n"theorem string"\n-- lemma line\ntheorem real').match(/\b(theorem|lemma)\b/g)?.length===1,newlines:mask('/-a\nb-/\n"c\nd"').split('\n').length===4};
const decls=[],sourceHashes=[];
for(const f of files){const bytes=fs.readFileSync(f),rel=path.relative(source,f),lines=mask(bytes.toString()).split('\n'),stack=[];sourceHashes.push({file:rel,sha256:sha(bytes)});
 for(let k=0;k<lines.length;k++){let l=lines[k],m;
 if(m=l.match(/^\s*namespace\s+([\w.]+)/)){stack.push({kind:'namespace',name:m[1]});continue;}
 if(l.match(/^\s*(?:noncomputable\s+)?section(?:\s|$)/)){stack.push({kind:'section'});continue;}
 if(l.match(/^\s*end(?:\s|$)/)){stack.pop();continue;}
 if(m=l.match(/^\s*(private\s+)?(?:protected\s+)?(theorem|lemma)\s+([\w'.]+)/u)){
 const ns=stack.filter(x=>x.kind==='namespace').map(x=>x.name).join('.');
 decls.push({name:[ns,m[3]].filter(Boolean).join('.'),short:m[3],site:rel.replace(/^lean\//,'')+':'+(k+1),private:!!m[1]});
 }}
 const dst=path.join(__dirname,'source',rel);fs.mkdirSync(path.dirname(dst),{recursive:true});fs.writeFileSync(dst,bytes);
}
write('source-inventory.json',{method:'Independent nested-comment/string masking, namespace/section stack, line-start theorem/lemma declarations. No compiled inventory; helper classification reconciled separately to supplied helper identity list.',lexicalControls,sourceHashes,declarations:decls,missingFromPacket:decls.filter(d=>!I.identities.some(i=>i.name===d.name&&i.site===d.site&&i.private===d.private)),notFoundInSource:I.identities.filter(i=>!decls.some(d=>i.name===d.name&&i.site===d.site&&i.private===d.private))});
const oldDir='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s3-final-static/admitted';
const oldMap=fs.readFileSync(oldDir+'/submission-3/OPMAP-v9-requirement-verdict-grounds.txt','utf8').trim().split('\n');
const oldAtomsText=fs.readFileSync(oldDir+'/ATOMS-ledger.txt','utf8');
const oldAtoms=oldAtomsText.split('\n').filter(l=>/^\| [A-Z][\w-]* \|/.test(l)).map(l=>l.split('|')[1].trim());
const atomKeys=new Set(A.atoms.flatMap(a=>[a.successorAtomId,a.oldAtomId,a.resolvesOldId].filter(Boolean)));
const diffs=walk(packet+'/instruments/diffs').map(f=>{const lines=fs.readFileSync(f,'utf8').trimEnd().split('\n'),h=lines.findIndex(l=>l.startsWith('@@')),m=lines[h].match(/^@@ -(\d+),(\d+) \+(\d+),(\d+) @@/),body=lines.slice(h+1);return {file:path.relative(packet,f),sha256:sha(fs.readFileSync(f)),hunkHeader:lines[h],declaredOld:Number(m[2]),declaredNew:Number(m[4]),actualOld:body.filter(l=>l[0]===' '||l[0]==='-').length,actualNew:body.filter(l=>l[0]===' '||l[0]==='+').length,unprefixedLines:body.map((l,i)=>({line:h+i+2,text:l})).filter(x=>![' ','+','-','\\'].includes(x.text[0]))};});
write('diff-structure.json',diffs);
const rows=R.rows.map(r=>({oldRow:r.oldRow,identity:r.identity,originalIdentity:oldMap[r.oldRow-1]?.split('|')[1],successorOps:r.successorOps,allRefsExist:r.successorOps.every(id=>O.operations.some(o=>o.opId===id)),observationKind:r.observationKind,semanticOutcome:r.semanticOutcome,span:r.span,costKinds:r.successorOps.map(id=>O.operations.find(o=>o.opId===id)?.costKind),correctionNote:r.correctionNote}));
write('row-reconciliation-207.json',rows);
write('atom-review-151.json',A.atoms.map(a=>({id:a.successorAtomId,old:a.oldAtomId,resolves:a.resolvesOldId,status:a.status,file:a.sourceFile,span:a.lineSpan,before:a.beforeText,after:a.afterText,sourceHashMatches:fs.existsSync(source+'/'+a.sourceFile)&&sha(fs.readFileSync(source+'/'+a.sourceFile))===a.sourceSHA256,beforeTextOccurs:!!a.beforeText&&fs.existsSync(source+'/'+a.sourceFile)&&fs.readFileSync(source+'/'+a.sourceFile,'utf8').includes(a.beforeText)})));
write('operation-review-236.json',O.operations.map(o=>({id:o.opId,old:o.oldOp,atoms:o.atomIds,costKind:o.costKind,sourceFile:o.sourceFile,before:o.beforeText,after:o.afterText,observation:o.observationPath,rows:rows.filter(r=>r.successorOps.includes(o.opId)).map(r=>r.oldRow)})));
write('measurement-review-26.json',M.operations);
const receipts=fs.readFileSync(packet+'/receipts.jsonl','utf8').trim().split('\n').map(JSON.parse);
write('receipt-review-66.json',receipts.map((r,i)=>({line:i+1,id:r.receiptId,result:r.result,source:r.sourceTablePath,row:r.sourceRow,hashes:r.hashes,search:r.missingFieldSearch})));
write('census.json',{
 sourceDeclarationCount:decls.length,private:decls.filter(d=>d.private).length,
 helperCount:H.helpers.length,helperDuplicates:duplicates(H.helpers.map(h=>h.identity)),helpersNotInSource:H.helpers.filter(h=>!decls.some(d=>d.name===h.identity&&d.site===h.site)),
 originalRows:oldMap.length,successorRows:rows.length,duplicateRowIds:duplicates(rows.map(r=>r.oldRow)),missingOriginalRows:oldMap.map((_,i)=>i+1).filter(i=>!rows.some(r=>r.oldRow===i)),identityDrift:rows.filter(r=>r.identity!==r.originalIdentity),
 originalAtomCount:oldAtoms.length,unmappedOldAtoms:oldAtoms.filter(a=>!atomKeys.has(a)),successorAtoms:A.atoms.length,duplicateSuccessorAtoms:duplicates(A.atoms.map(a=>a.successorAtomId)),
 atomStatuses:count(A.atoms,a=>a.status),frozenTemplateAtoms:A.atoms.filter(a=>a.status==='FROZEN'&&a.afterText.includes('[single-atom negation/removal;')).map(a=>a.successorAtomId),
 rowObservationKinds:count(rows,r=>r.observationKind),falseAtWitness:rows.filter(r=>r.semanticOutcome==='FALSE-AT-WITNESS').length,estimatedSpans:rows.filter(r=>(r.span||'').includes('header-bound estimate')).length,
 operations:O.operations.length,operationCostKinds:count(O.operations,o=>o.costKind),duplicateOps:duplicates(O.operations.map(o=>o.opId)),
 ownershipStatuses:count(W.relations,r=>r.status),unallocatedPreserved:W.relations.filter(r=>['WITNESS','SHARED'].includes(r.atomId)&&r.status==='PRESERVED'),ownershipMissingAtomReferences:W.relations.filter(r=>r.atomId&&!atomKeys.has(r.atomId)),addedValueFlips:W.addedValueFlips,
 measurementRows:M.operations.length,measurementLayers:count(M.operations,m=>m.layer),receiptCount:receipts.length,rosterCount:receipts.filter(r=>r.result==='ROSTER-ONLY').length,emptyReceiptHashes:receipts.filter(r=>Object.keys(r.hashes||{}).length===0).map(r=>r.receiptId),
 staticOnly:'All counts are parsed records. No executions, semantic kills, compiler results, or budget totals inferred.'});
console.log('Static census written; declarations='+decls.length+' rows='+rows.length+' atoms='+A.atoms.length+' operations='+O.operations.length+' measurement rows='+M.operations.length);
