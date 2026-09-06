import fs from 'node:fs';import readline from 'node:readline';import crypto from 'node:crypto';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';const mode=process.argv[2];
const sha=s=>crypto.createHash('sha256').update(s).digest('hex');
async function load(path){const map=new Map(),mods=new Set(),pred=new Set();let summary;for await(const line of readline.createInterface({input:fs.createReadStream(path),crlfDelay:Infinity})){const r=JSON.parse(line);if(r.record==='module'){if(r.owned)mods.add(r.name);continue;}if(r.record==='summary'){if(summary)throw Error('duplicate summary');summary=r;continue;}if(r.record!=='declaration')throw Error('unknown record');if(map.has(r.name))throw Error('duplicate declaration '+r.name);map.set(r.name,{name:r.name,module:r.module,kind:r.kind,result:r.result,type:sha(r.typeExpr),value:sha(r.valueExpr),levels:r.levelParams});if(r.result==='Prop'&&['defn','opaque','inductive'].includes(r.kind))pred.add(r.name);}
return {map,mods,pred,summary};}
function assert(b,s){if(!b)throw Error(s);}
function valid(d){assert(d.summary,'missing completion summary');assert(d.map.size>0&&d.pred.size>0&&d.mods.size>0,'empty extent');assert(d.map.size===d.summary.declarations&&d.pred.size===d.summary.predicateCandidates&&d.mods.size===d.summary.ownedModules,'truncated extent');for(const r of d.map.values()){assert(d.mods.has(r.module),'declaration without owned home '+r.name);assert(r.result!=='sort-undecided','unresolved sort '+r.name);}assert(d.summary.sortUndecided===0,'unknown summary sorts');return true;}
function compare(base,candidate){for(const [n,r]of base.map){const c=candidate.map.get(n);assert(c,'missing original declaration '+n);for(const k of ['kind','type','value','levels'])assert(c[k]===r[k],'changed original '+k+' '+n);}return true;}
function control(name,fn){let caught=false;try{fn();}catch(e){caught=true;console.log('CONTROL '+name+' rejected: '+e.message);}assert(caught,'control survived '+name);}
const current=await load(rt+'/evidence/M1R/inventory.jsonl');valid(current);
if(mode==='census-controls'){
 console.log('CENSUS baseline declarations='+current.map.size+' predicates='+current.pred.size);
 control('empty',()=>valid({...current,map:new Map(),pred:new Set(),mods:new Set()}));
 const drop=new Map(current.map);drop.delete('conservation');control('one-declaration-omitted',()=>valid({...current,map:drop}));
 const unknown=new Map(current.map);unknown.set('conservation',{...unknown.get('conservation'),result:'sort-undecided'});control('unknown-sort-retained',()=>valid({...current,map:unknown}));
 const wrong=new Map(current.map);wrong.set('conservation',{...wrong.get('conservation'),module:'NotOwned'});control('false-home',()=>valid({...current,map:wrong}));
 console.log('CENSUS-CONTROLS-OK');
}else if(mode==='expr-controls'){
 compare(current,current);console.log('EXPR baseline exact self-identity');
 for(const field of ['type','value','levels','kind']){const map=new Map(current.map);map.set('conservation',{...map.get('conservation'),[field]:'known-corruption'});control('changed-'+field,()=>compare(current,{...current,map}));}
 const missing=new Map(current.map);missing.delete('conservation');control('missing-original',()=>compare(current,{...current,map:missing}));console.log('EXPR-CONTROLS-OK');
}else if(mode==='expr-compare'){
 const base=await load(rt+'/evidence/full/BaseInventory.jsonl');valid(base);compare(base,current);console.log('EXPR-PRESERVED original='+base.map.size+' candidate='+current.map.size);
}else if(mode==='final-reconcile'){
 const final=await load(rt+'/evidence/full/FinalInventory.jsonl');valid(final);compare(current,final);compare(final,current);
 const receipt=fs.readFileSync(rt+'/worlds/S10/lean/.lake/s4b-mirror-receipt','utf8').split('\n').filter(l=>l.startsWith('extent ')).map(l=>l.slice(7));assert(receipt.length===new Set(receipt).size,'duplicate extent');assert(receipt.length===final.pred.size&&receipt.every(n=>final.pred.has(n)),'mandatory/independent predicate difference');
 const wanted=fs.readFileSync(rt+'/instruments/modules.txt','utf8').trim().split('\n');assert(final.mods.size===wanted.length&&wanted.every(m=>final.mods.has(m)),'tracked/resolved modules differ');
 console.log('FINAL-RECONCILED declarations='+final.map.size+' predicates='+final.pred.size+' modules='+final.mods.size);
}else throw Error('unknown mode '+mode);
