import fs from 'node:fs';import crypto from 'node:crypto';
const rt="/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2",src='/code/reactivegas-66-s4b-audit5/lean/.lake/build/lib/lean',dst=rt+'/worlds/shadow-P07-v5';
const h=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
if(!fs.existsSync(rt+'/instruments/full-v5/ADMISSION.json'))throw Error('not admitted');
if(fs.existsSync(dst))throw Error('overlay must be absent');
const producer=JSON.parse(fs.readFileSync(rt+'/evidence/full-v2/P07-compile.json','utf8'));if(producer.exit!==0)throw Error('missing compiled mutant prerequisite');
const selected='Reactivegas/Step.olean',mut=rt+'/worlds/shadow-P07/'+selected;
const lines=fs.readFileSync(rt+'/evidence/M1/oleans.sha256','utf8').trim().split('\n').filter(l=>l.slice(66).startsWith('lean/.lake/build/lib/lean/'));
for(const l of lines){const p=l.slice(66+'lean/.lake/build/lib/lean/'.length);if(h(src+'/'+p)!==l.slice(0,64))throw Error('clean dependency drift '+p);}
fs.cpSync(src,dst,{recursive:true});fs.copyFileSync(mut,dst+'/'+selected);
const files=lines.map(l=>{const p=l.slice(66+'lean/.lake/build/lib/lean/'.length),actual=h(dst+'/'+p),expected=p===selected?h(mut):l.slice(0,64);if(actual!==expected)throw Error('overlay drift '+p);return {path:dst+'/'+p,relative:p,sha256:actual,role:p===selected?'one mutant module':'verified clean M1-S dependency'};});
if(h(dst+'/KelGroups/Types.olean')===h(rt+'/worlds/shadow-P01/KelGroups/Types.olean'))throw Error('P01 contamination');
if(h(dst+'/'+selected)===h(src+'/'+selected))throw Error('mutant not installed');
fs.writeFileSync(rt+'/evidence/full-v2/P07-overlay-v5.json',JSON.stringify({utc:new Date().toISOString(),producer:'P07-compile',world:dst,onlyChangedModule:selected,files},null,2)+'\n');
console.log('P07-OVERLAY verified clean dependencies plus retained mutant Step, no P01 Types');
