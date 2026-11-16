import {readFileSync,writeFileSync,mkdtempSync,rmSync} from 'node:fs';
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import {join} from 'node:path';
import assert from 'node:assert/strict';
const root='/tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b';
const repo='/code/reactivegas-sim-fable-audit-c1r-s1';
const mode=process.argv[2];
const src=readFileSync(join(repo,'economics-simulator-ui-gate.mjs'),'utf8');
const transport=src.slice(0,src.indexOf('/* --- D3 fail-closed derivation'))
 .replace('const REPO = dirname(fileURLToPath(import.meta.url));','const REPO = '+JSON.stringify(repo)+';')
 + '\nexport {Browser,findChromium};';
const {Browser,findChromium}=await import('data:text/javascript;base64,'+Buffer.from(transport).toString('base64'));
const hash=x=>createHash('sha256').update(x).digest('hex');
const htmlPath=join(repo,'economics-simulator.html'), original=readFileSync(htmlPath,'utf8');
const replaceOne=(s,a,b)=>{assert.equal(s.split(a).length-1,1,'exactly one edit');return s.replace(a,b);};
const mutant=(id,body)=>{const p=join(root,'evidence',id+'.html');writeFileSync(p,body);console.log(JSON.stringify({id,path:p,sha256:hash(body),original:hash(original)}));return p;};
const run=args=>{const r=spawnSync('node',[join(repo,'economics-simulator-ui-gate.mjs'),...args],{encoding:'utf8'});console.log(JSON.stringify({args,exit:r.status,stdout:r.stdout,stderr:r.stderr}));return r;};
const profile=mkdtempSync(join(root,'evidence','profile-')),b=new Browser(findChromium(),profile);
async function page(path=htmlPath){const s=await b.page(pathToFileURL(path).href+'?audit='+mode,1280,900);await b.eval(s,'localStorage.clear();newSession();');return s;}
async function evaluate(s,fn){return b.eval(s,'('+fn.toString()+')()');}
try {
 if(mode==='geometry'){
   const s=await page(),r=await evaluate(s,()=>{
     const good=r=>{if(!r||!r.ok)throw Error('seed refused '+JSON.stringify(r));return r;};
     const rows=[],before=JSON.stringify(purchaseRingLayout(members(),[],{}).memberPos);
     const minDist=ps=>Math.min(...ps.flatMap((p,i)=>ps.slice(i+1).map(q=>Math.hypot(p.x-q.x,p.y-q.y))));
     for(let i=1;i<=10;i++){
       good(RG.runAttempt('openPurchase',{author:'anna',c:i}));
       const g=purchaseRingLayout(members(),state.collections,{});
       rows.push({n:i,minDist:i<2?null:minDist(g.placements),memberStable:before===JSON.stringify(g.memberPos),same:JSON.stringify(g)===JSON.stringify(purchaseRingLayout(members(),state.collections,{})),radii:[...new Set(g.placements.map(x=>x.radius))]});
     }
     renderAll();verifySessionShape(normalizeWrap(JSON.parse(buildExport())));
     return {rows,visiblePurchases:document.querySelectorAll('[data-obj="pile"]').length,exportAccepted:true};
   });
   console.log(JSON.stringify(r));assert.equal(r.visiblePurchases,10);
   assert(r.rows[7].minDist>=91.5,'positive eight purchases');
   assert(r.rows[9].minDist<84,'ten visible circles overlap');
   assert(r.rows.every(x=>x.memberStable&&x.same),'other geometry prerequisites retained');
 } else if(mode==='derive-bracket'||mode==='derive-dot'){
   const read=mode==='derive-dot'?'vip.dataset.vip':"vip.dataset['vip']";
   const insertion="const auditButton=document.createElement('button');auditButton.id='audit-vip';auditButton.setAttribute('data-vip','01');auditButton.textContent='Vai alla persona';document.body.appendChild(auditButton);auditButton.addEventListener('click',()=>{const vip=auditButton;go({view:'person',u:Number("+read+"),hat:'member'});});\n";
   const marker='// Exposed for scripted verification (harmless in normal use).';
   const p=mutant(mode,replaceOne(original,marker,insertion+marker)),derived=run(['--derive-only',p]),s=await page(p);
   const r=await evaluate(s,()=>{const a=runBaseTask('admitMember',{author:'anna',target:'01',_label:'01'});if(!a.ok)throw Error('seed refused');document.getElementById('audit-vip').click();return {member:members().includes('01'),nav:JSON.parse(JSON.stringify(nav())),visible:!!document.getElementById('audit-vip')};});
   console.log(JSON.stringify(r));assert(r.member&&r.visible);assert.notEqual(r.nav.u,'01');assert.equal(derived.status,mode==='derive-dot'?1:0);
 } else if(mode==='authority'){
   const s=await page(),r=await evaluate(s,()=>{
     const a=runBaseTask('admitMember',{author:'anna',target:'bruno',_label:'Bruno'});if(!a.ok)throw Error('admit refused');
     const pre=JSON.parse(JSON.stringify(curAggregate()));
     const ordinary=applyIntegrated(pre,'bruno',{app:{openPurchase:{c:99}}});
     const forgedEvent={app:{openPurchase:{c:99,author:'anna'}}};
     const forged=applyIntegrated(pre,'bruno',forgedEvent);
     if(forged.refused)throw Error('forged refused '+forged.refused);
     const wrap=JSON.parse(buildExport());
     wrap.integrated.steps.push({input:pre,signer:'bruno',event:forgedEvent,result:{tag:'applied',aggregate:forged.gs}});
     const normalized=normalizeWrap(wrap);
     verifySessionShape(normalized);
     adoptSession(normalized.env,normalized.labels);
     return {brunoAdmin:isAdminView('bruno',curView()),ordinary,forgedReferente:forged.gs.appFold.collections[0].referente,
       acceptedImport:true,adopted:state.collections.find(c=>c.id===99),wrap};
   });
   writeFileSync(join(root,'evidence','authority-forged-session.json'),JSON.stringify(r.wrap,null,2));
   delete r.wrap;console.log(JSON.stringify(r));assert.equal(r.brunoAdmin,false);assert.equal(r.ordinary.refused,'rejected');assert.equal(r.forgedReferente,'anna');assert(r.acceptedImport&&r.adopted);
 } else if(mode==='chrome'){
   const s=await page(),classifier=src.slice(src.indexOf('function stripProvenance('),src.indexOf('const RENDER_CLASSES'));
   const fn=()=>{
     const extract=()=>{const c=document.body.cloneNode(true);c.querySelectorAll('script,noscript,style').forEach(x=>x.remove());return c.innerText;},rows=[];
     for(const token of ['pledge','KelGroups','GroupState.members']){
       popAt(100,100,'<div class="pbox"><span class="mono">'+token+'</span></div>');
       rows.push({token,live:document.querySelector('#pop').innerText,visible:document.querySelector('#pop').getBoundingClientRect().width>0,whileOpen:classifyVocab(extract())});
       document.querySelector('#pop').innerHTML='';rows.at(-1).afterClosed=classifyVocab(extract());
     }
     return rows;
   };
   const r=await b.eval(s,'(()=>{'+classifier+'return ('+fn.toString()+')();})()');
   console.log(JSON.stringify(r));assert(r[0].visible&&r[0].whileOpen.banned.includes('pledge'));assert(!r[0].afterClosed.banned.length);assert(r[1].visible&&!r[1].whileOpen.banned.length);
 } else throw Error('unknown mode '+mode);
} finally {b.close();await new Promise(r=>setTimeout(r,300));rmSync(profile,{recursive:true,force:true});}
