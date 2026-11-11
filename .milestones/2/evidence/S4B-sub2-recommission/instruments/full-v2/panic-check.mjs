import fs from 'node:fs';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
const absent=(a,b)=>!a.includes('PANIC at')&&!b.includes('PANIC at');
if(process.argv[2]==='controls'){
 for(const [name,a,b,want] of [['clean','ok','',true],['stdout','PANIC at x','',false],['stderr','','PANIC at x',false],['both','PANIC at x','PANIC at y',false]]){if(absent(a,b)!==want)throw Error(name);console.log('PANIC-CONTROL '+name+' OK');}
}else if(process.argv[2]==='final'){
 for(const id of ['S10','FinalInventory','Axioms','EconomyWitnesses','GroupWitnesses','Exceptions','P01-pos','P07-pos']){const a=fs.readFileSync(rt+'/evidence/full-v2/'+id+'.stdout','utf8'),b=fs.readFileSync(rt+'/evidence/full-v2/'+id+'.stderr','utf8');if(!absent(a,b))throw Error('PANIC '+id);console.log('PANIC-ABSENT '+id+' both streams');}
}else throw Error('unbound mode');
