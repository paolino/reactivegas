import fs from 'node:fs';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
const p=process.argv[2];
if(!['P01','P07'].includes(p))throw Error('unbound shadow');
if(!fs.existsSync(rt+'/instruments/full-v2/ADMISSION.json'))throw Error('not admitted');
const path=rt+'/worlds/shadow-'+p;
if(fs.existsSync(path))throw Error('shadow already exists; cannot establish fresh isolation');
fs.mkdirSync(path+'/'+(p==='P01'?'KelGroups':'Reactivegas'),{recursive:true});
fs.writeFileSync(rt+'/evidence/full-v2/'+p+'.empty-world.json',JSON.stringify({path,created:new Date().toISOString(),files:[],otherShadowExcluded:true},null,2)+'\n');
