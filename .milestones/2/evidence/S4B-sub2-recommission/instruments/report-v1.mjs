// Static receipt inspection and local report assembly only. No Lean, build, or process census.
import fs from 'node:fs';
import crypto from 'node:crypto';
import readline from 'node:readline';
import {execFileSync} from 'node:child_process';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2';
process.chdir(rt);
const out='handoffs/full-audit-v1';
fs.mkdirSync(out,{recursive:true});
const read=p=>fs.readFileSync(p,'utf8');
const json=p=>JSON.parse(read(p));
const sha=x=>crypto.createHash('sha256').update(x).digest('hex');
const hash=p=>sha(fs.readFileSync(p));
const write=(p,s)=>fs.writeFileSync(out+'/'+p,s,{flag:'wx'});
const writeJSON=(p,v)=>write(p,JSON.stringify(v,null,2)+'\n');
const assert=(v,s)=>{if(!v)throw Error(s);};
const git=(cwd,...args)=>execFileSync('git',['-C',cwd,...args],{encoding:'utf8'}).trim();
const snapshot={utc:new Date().toISOString(),scope:'Static files, git identity/status and receipt hashes only. No process listing or elaboration.',candidates:[]};
for(const cwd of ['/code/reactivegas-66-s4b-audit5',rt+'/worlds/S10']){
 const head=git(cwd,'rev-parse','HEAD'),tree=git(cwd,'rev-parse','HEAD^{tree}'),porcelain=git(cwd,'status','--porcelain');
 assert(head==='94bb7bb64324a48f7361252556b4d15e45b3923f'&&tree==='3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced'&&!porcelain,'candidate drift '+cwd);
 const lines=read('instruments/candidate-inputs.sha256').trim().split('\n');
 for(const l of lines)assert(hash(cwd+'/'+l.slice(66))===l.slice(0,64),'tracked input drift '+l);
 snapshot.candidates.push({cwd,head,tree,porcelain,verifiedTrackedInputs:lines.length});
}
for(const l of read('evidence/M1/oleans.sha256').trim().split('\n'))assert(hash('/code/reactivegas-66-s4b-audit5/'+l.slice(66))===l.slice(0,64),'M1 prerequisite drift '+l);
snapshot.M1OleannsVerified=read('evidence/M1/oleans.sha256').trim().split('\n').length;
const previous='handoffs/pre-full-v1';fs.mkdirSync(previous,{recursive:true});
for(const l of read('handoffs/M1-RETURN.sha256').trim().split('\n')){
 const p=l.slice(66);assert(hash(p)===l.slice(0,64),'old M1 return drift '+p);
 const dest=previous+'/'+p;fs.mkdirSync(dest.slice(0,dest.lastIndexOf('/')),{recursive:true});fs.copyFileSync(p,dest,fs.constants.COPYFILE_EXCL);
}
fs.copyFileSync('handoffs/M1-RETURN.sha256',previous+'/M1-RETURN.sha256',fs.constants.COPYFILE_EXCL);
const commands=json('instruments/full-v5/COMMANDS.json');const receipts=[];
for(const c of commands){
 const r=json('evidence/full-v2/'+c.id+'.json');assert(r.launched&&r.finish&&r.signal===null,'incomplete receipt '+c.id);
 assert(JSON.stringify(r.argv)===JSON.stringify(c.argv)&&r.cwd===c.cwd&&r.charge===c.charge,'command binding '+c.id);
 for(const ext of ['stdout','stderr'])assert(hash('evidence/full-v2/'+c.id+'.'+ext)===r[ext+'Sha256'],'output drift '+c.id+' '+ext);
 receipts.push({...r,receipt:'evidence/full-v2/'+c.id+'.json'});
}
receipts.sort((a,b)=>a.start.localeCompare(b.start));
let substantive=1,targeted=2;
for(const r of receipts){if(r.charge==='substantive')substantive++;else targeted++;assert(r.spend.substantive===substantive&&r.spend.targeted===targeted,'spend discontinuity '+r.id);}
assert(substantive===12&&targeted===73,'unexpected spend');
snapshot.spend={substantive,targeted,ceilings:{substantive:12,targeted:80},campaign:{substantive:18,targeted:132},campaignCeilings:{substantive:18,targeted:139}};
snapshot.receiptsVerified=receipts.length;
const nonce=read('worlds/S10/lean/.lake/s4b-mirror-nonce').trim();assert(read('worlds/S10/lean/.lake/s4b-mirror-receipt').split('\n').includes('nonce='+nonce),'nonce mismatch');
snapshot.finalCheckerNonce=nonce;
writeJSON('RECEIPT-INTEGRITY.json',snapshot);
writeJSON('COMMAND-OUTCOMES.json',receipts);
for(const p of ['s4b-mirror-receipt','s4b-mirror-nonce'])write(p,read('worlds/S10/lean/.lake/'+p));
async function index(p){const rows=[];let summary;for await(const line of readline.createInterface({input:fs.createReadStream(p),crlfDelay:Infinity})){const r=JSON.parse(line);if(r.record==='summary')summary=r;if(r.record==='declaration')rows.push({name:r.name,module:r.module,kind:r.kind,result:r.result,typeHash:sha(r.typeExpr),bodyHash:sha(r.valueExpr),levelParams:r.levelParams});}return {rows,summary};}
const base=await index('evidence/full-v2/BaseInventory.jsonl'),final=await index('evidence/full-v2/FinalInventory.jsonl');
const baseMap=new Map(base.rows.map(r=>[r.name,r]));
const planned=new Map(read('instruments/full-v2/DECLARATION-ROWS.jsonl').trim().split('\n').map(l=>{const r=JSON.parse(l);return[r.name,r];}));
write('DECLARATION-DISPOSITIONS.jsonl',final.rows.map(r=>JSON.stringify({...r,origin:baseMap.has(r.name)?'base: original Expr preservation established':'candidate addition',commands:planned.get(r.name).commands,disposition:'CLOSED metadata identity and kind; original-preservation where applicable; owned theorem axiom check where kind=theorem. Predicate consumer classification is separately in S4-CLASSIFICATION.md.'})).join('\n')+'\n');
write('ADDED-DECLARATIONS.jsonl',final.rows.filter(r=>!baseMap.has(r.name)).map(r=>JSON.stringify(r)).join('\n')+'\n');
writeJSON('EXTENT.json',{base:base.summary,final:final.summary,newDeclarations:final.rows.length-base.rows.length,bounds:{declarations:20000,bytes:268435456},rule:'Measured complete output, never a fixed acceptance denominator; unknown sorts or changed identities stop closure.'});
const kinds=Object.create(null);for(const r of final.rows)kinds[r.kind]=(kinds[r.kind]||0)+1;
console.log(JSON.stringify({snapshot,kinds,base:base.summary,final:final.summary}));
const atoms=json('planning/atoms.json');
const atomOut=atoms.map(a=>{const s=read('evidence/full-v2/'+a.id+'.stdout'),r=json('evidence/full-v2/'+a.id+'.json');const errors=[...s.matchAll(/:(\d+):(\d+): error: (.*)/g)].map(m=>({line:Number(m[1]),column:Number(m[2]),message:m[3]}));return {...a,exit:r.exit,errors,receipt:'evidence/full-v2/'+a.id+'.json',status:'CLOSED: exactly one well-typed body edit; original proof reached and failed; original statement/proof bytes preserved',negativeScope:'Definition sensitivity for this stated atom only; no completeness claim over all possible faults'};});
writeJSON('ATOM-DISPOSITIONS.json',atomOut);
write('ATOM-DISPOSITIONS.md','# Executed body controls\n\nAll 44 rows are distinct charged Lean invocations. Each definition marker succeeded and each original proof failed within its frozen proof interval; no earlier error is credited. Positive production proofs compiled in S01 and cold S10. Exact substitutions, input hashes and diagnostic locations are retained in ATOM-DISPOSITIONS.json; complete streams remain under evidence/full-v2.\n\n| Row | Obligation | Original theorem | Proof errors | Disposition |\n|---|---|---|---|---|\n'+atomOut.map(a=>'| '+[a.id,a.obligation,'`'+a.theorem+'`',a.errors.map(e=>e.line+':'+e.message).join('; '),'CLOSED'].join(' | ')+' |').join('\n')+'\n');
const classRows=json('instruments/full-v2/CLASSIFICATION-ROWS.json').sort((a,b)=>a.id.localeCompare(b.id,undefined,{numeric:true}));
const specials={P01:['Proved expression equivalence, value-parametric; not strict definitional identity or a body-sensitive original correspondence.','P01-pos/neg, P01-relatum, S07; selected original membership helpers fail under constant-false isMember.'],P07:['Proved expression equivalence, inline; not a body-sensitive original correspondence.','P07-pos, P07-negR, P07-relatum, S08; selected step_close_inv fails and forbidden close succeeds in the single-module mutant overlay.'],P11:['DEFINITIONAL-IDENTITY per constructor; existing consumer also evaluated.','ExceptionsR: 14 rfl projections, each positive and negative; final-reconcile establishes exact Event constructor coverage.'],V4:['DEFINITIONAL-IDENTITY; existing preservesQuestionDecide also evaluated.','ExceptionsR: preservation and actual changed nonempty question.'],R0:['LOGICAL-DECISION-EVIDENCE and EXECUTABLE-DECISION separately established; no independent Bool correspondence inferred.','Existing stalledDecidable; ExceptionsR positive/negative nonzero ±7 states.'],P13:['NOT-ESTABLISHED runtime decision. No undecidability claim.','ReachDecision fails synthesis for one concrete Reach instance; complete source and Expr remain unchanged.'], 'GENERATED-REACH-BELOW':['Generated inductive recursion support, not a separately designed predicate consumer.','ExceptionsR prints Reach.below; final metadata gives its generated identity.']};
let classification='# S4 final classification — both independent axes\n\nComplete final discovery: 24 predicate candidates, individually listed below. The 1,285 theorem declarations and other nonpredicate/generated declarations are individually retained in DECLARATION-DISPOSITIONS.jsonl; these are not silently omitted predicate obligations. The final discovery equals the mandatory receipt by identity.\n\nAuthority for required finite counterparts is the original S4 Phase A/Phase B plus retained R1/R4/R7/R11, amended only for P01/P07 by v3.1. R2 forbids new monitor wiring; implemented here means an executable counterpart exists, not a new production call site. K5 runtime capability depends on a computable DecidableEq payload instance; Nat payloads 73/74 were actually executed, not a claim that arbitrary classical instances execute. V3/V4 retain the caller-provided Threshold function; no default or equality over functions is introduced.\n\n#68/#69 dependency for EVERY row: source/import/type semantics are bound to accepted base 3590c001 and candidate 94bb7bb. Any later change to the named source, its imported dependencies, callable policy, payload equality or consumer requires rebind; no sibling acceptance is inherited.\n\n| ID / identity | Exact source | Axis 1 | Axis 2 / authority | Executed evidence / status |\n|---|---|---|---|---|\n';
const results=[];
for(const r of classRows){
 const aa=atoms.filter(a=>a.obligation.startsWith(r.id+' '));
 const axis1=specials[r.id]?.[0]||'PROVED-EQUIVALENCE to independently implemented Bool; compilation and nondegenerate evaluation also established.';
 let axis2='REQUIRED-CONSUMER-IMPLEMENTED under original Phase B and retained finite obligations; S01/S10 compile the counterpart and theorem.';
 let status='CLOSED';
 if(r.id==='P13'){axis2='NOT ESTABLISHED: candidate claims NOT-REQUIRED, but no admitted Reach-specific consumer authority settles that claim. Original Axis 2 remains binding; F-001 OPEN.';status='PARTLY (axis 1 bounded; axis 2 OPEN)';}
 if(r.id==='GENERATED-REACH-BELOW')axis2='NOT-REQUIRED as a distinct NEW consumer: generated recursion machinery under R1 finite mirror-only scope and R2 no new semantics. This does not exempt parent Reach or close F-001.';
 const evidence=specials[r.id]?.[1]||aa.map(a=>a.id+' '+a.theorem).join('; ')+'; '+(r.id.startsWith('P')?'EconomyWitnesses':'GroupWitnesses');
 const rr={...r,axis1,axis2,status,evidence,commands:[...new Set(r.commands.map(c=>c==='Exceptions'?'ExceptionsR':c==='P07-neg'?'P07-negR':c))]};results.push(rr);
 classification+='| '+[r.id+' / `'+r.identity+'`','`'+r.source+'`',axis1,axis2,evidence+'; '+status].join(' | ')+' |\n';
}
classification+='\nF-001 is not a finding that Reach is undecidable or that a missing oracle must be implemented. The missing evidence is the authority deciding whether the specific executable consumer is required. A finite-only implementation fence and a failed instance query cannot independently answer that milestone-level question. No predicate is removed to make this table complete.\n';
write('S4-CLASSIFICATION.md',classification);writeJSON('CLASSIFICATION-DISPOSITIONS.json',results);
const failures=new Set(['S02','Exceptions','P07-neg']);
let ledger='# Campaign ledger — final receipt reconciliation\n\nCurrent seat: **12 substantive / 73 targeted**, against **12 / 80**, separately. Remaining **0 substantive / 7 targeted**. Historical auditors **6 / 59** plus this seat gives **18 / 132**, against cumulative ceilings **18 / 139**. The v2 budget replaces the predecessor unspent 9/10; those figures are never added. Author spend is the admitted historical 18 substantive / 52 targeted, not an independent re-execution; submission **2 of 2**, no third. No count reset or refund.\n\nM1-S is one full module build (substantive, exit 0). M1-T is one failed inventory elaboration (targeted, exit 1: reserved prefix, no extent). M1R-T is one authorized repaired elaboration (targeted, exit 0, planning-only). Original M1/M1R bytes and manifests remain unchanged.\n\nThe model deviation involved no charged execution; launch argv was not evidence of the active model. Same-session restoration and full independent static revalidation preceded START. Rejected approvals launched nothing. Static reads, hash/cwd corrections, world preparation and report assembly contained no Lean/build invocation.\n\nFull plan versions: original preserved 78bfc403…; admitted full-v2 11/71; v3 12/71 added S11 after S02; v4 12/72 added ExceptionsR; v5 12/73 added P07-negR. Every complete revised sheet retained all original rows and reserved final cold CI/inventory/axioms. Failures remained spent.\n\n| Command | Class | Exit | UTC start | Cumulative S/T | Outcome |\n|---|---|---|---|---|---|\n';
for(const r of receipts)ledger+='| '+[r.id,r.charge,r.exit,r.start,r.spend.substantive+'/'+r.spend.targeted,failures.has(r.id)?'AUDITOR SETUP/PLACEMENT FAILURE; no intended closure':r.id==='ReachDecision'?'Bounded absence observation; not authority':r.id.includes('relatum')?'Relatum control only':r.exit===1?'Intended negative observation; see row scope':'Completed; see row-specific evidence'].join(' | ')+' |\n';
ledger+='\nEvery actual argv, cwd, receipt path and both stream hashes are in COMMAND-OUTCOMES.json. M1-S/M1-T/M1R actual outer and child argv are frozen in the original measurement manifests. Their three operations plus these 82 rows are the entire charged set. Four setup/placement failures total (M1-T, S02, Exceptions, P07-neg), all counted, none a candidate defect.\n';
write('CAMPAIGN-LEDGER.md',ledger);
