import fs from 'node:fs';import crypto from 'node:crypto';
const rt='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2',root='/code/reactivegas-66-s4b-audit5',dest=rt+'/instruments/full';
const read=p=>fs.readFileSync(root+'/lean/'+p,'utf8');const hash=s=>crypto.createHash('sha256').update(s).digest('hex');
function save(p,s){fs.mkdirSync((dest+'/'+p).slice(0,(dest+'/'+p).lastIndexOf('/')),{recursive:true});fs.writeFileSync(dest+'/'+p,s);}
function decl(src,name){const re=new RegExp('^(?:private )?(?:theorem|def|abbrev) '+name.replace(/[.*+?^${}()|[\]\\]/g,'\\$&')+'(?: |\\n)','m');const m=re.exec(src);if(!m)throw Error('missing declaration '+name);const tail=src.slice(m.index);const end=tail.indexOf('\n\n');if(end<0)throw Error('no decl end '+name);return tail.slice(0,end)+'\n';}
const r=read('Reactivegas/Mirrors.lean'),inv=read('Reactivegas/Invariants.lean'),pred=read('Reactivegas/Predicates.lean'),typ=read('Reactivegas/Types.lean');
const fragments=[];function take(src,file,n){const text=decl(src,n);fragments.push({file,name:n,sha256:hash(text),text});return text;}
let p01='import KelGroups.Types\n\n';for(const n of ['assocLookup_some_mem_nodupfree','assocLookup_some_of_mem_nodupfree'])p01+=take(r,'Reactivegas/Mirrors.lean',n)+'\n';p01+=take(typ,'Reactivegas/Types.lean','comuneId')+'\n'+take(pred,'Reactivegas/Predicates.lean','comune_not_a_member')+'\n';for(const n of ['view_mem_of_isMember','isMember_of_view_mem','comune_not_a_member_corr'])p01+=take(r,'Reactivegas/Mirrors.lean',n)+'\n';p01+=`def witnessMember : KelGroups.Member := ⟨"u", "u@audit", [.appRole "buyer"]⟩
def witnessView : KelGroups.GroupView := ⟨[("u", witnessMember), ("u", ⟨"u", "second@audit", []⟩)]⟩
#eval IO.println s!"P01-WITNESS present={KelGroups.GroupView.isMember "u" witnessView} absent={KelGroups.GroupView.isMember "absent" witnessView} duplicates={witnessView.members.length}"
`;save('P01-chain.lean',p01);
let p07='import KelGroups.Types\nimport Reactivegas.State\nimport Reactivegas.Step\n\nvariable {view : KelGroups.GroupView}\nvariable {auth : BackdonateAuth}\n\n';for(const n of ['option_bind_inv','demand_eq_true_of_some','bool_and_left','bool_and_right','eq_nil_of_isEmpty'])p07+=take(inv,'Reactivegas/Invariants.lean',n)+'\n';p07+=take(pred,'Reactivegas/Predicates.lean','permissionToClose')+'\n';for(const n of ['close_guard_inv','step_close_inv','close_permission_to_close'])p07+=take(inv,'Reactivegas/Invariants.lean',n)+'\n';p07+=take(r,'Reactivegas/Mirrors.lean','permissionToClose_corr')+'\n';
p07+=`def witnessAdmin : KelGroups.Member := ⟨"a", "a@audit", [.adminRole .publicAdmin]⟩
def witnessView : KelGroups.GroupView := ⟨[("a", witnessAdmin)]⟩
def selected (perm : Bool) : Collection := ⟨7, "a", perm, [⟨"u", 23⟩], []⟩
def unrelated : Collection := ⟨9, "other", false, [], [⟨"v", 5⟩]⟩
def witnessState (perm : Bool) : State := { State.empty with conti := [("u", 11)], casse := [("a", 40)], collections := [unrelated, selected perm] }
def closeResult (perm : Bool) := stepEvent witnessView (witnessState perm) (.closePurchase "a" 7) (fun _ _ => false)
#eval IO.println s!"P07-WITNESS selectedBinding={decide (pullCollection 7 (witnessState true).collections = some (selected true, [unrelated]))} authorized={ (closeResult true).isSome} forbidden={ (closeResult false).isSome} preservesOther={decide ((closeResult true).map (fun s => s.collections) = some [unrelated])} amount={decide ((closeResult true).map (fun s => bal s.casse "a") = some 17)}"
`;save('P07-chain.lean',p07);
save('selected-fragments.json',JSON.stringify(fragments,null,2)+'\n');
let p1rel='import KelGroups.Types\n'+decl(typ,'comuneId')+decl(pred,'comune_not_a_member').replace('¬ KelGroups.GroupView.isMember','KelGroups.GroupView.isMember')+decl(r,'comune_not_a_member_corr');save('P01-relatum.lean',p1rel);
let p7rel='import Reactivegas.Step\n'+decl(pred,'permissionToClose').replace('col.permitted ∧','True ∧')+decl(r,'permissionToClose_corr');save('P07-relatum.lean',p7rel);
const ts=read('KelGroups/Types.lean');if(!ts.includes('  (lookupMember key view).isSome'))throw Error('P01 body missing');save('shadows/P01/Types.lean',ts.replace('  (lookupMember key view).isSome','  false'));
const ss=read('Reactivegas/Step.lean');if(!ss.includes('&& col.permitted && col.pending.isEmpty'))throw Error('P07 body missing');save('shadows/P07/Step.lean',ss.replace('&& col.permitted && col.pending.isEmpty','&& true && col.pending.isEmpty'));
const worlds=[
{id:'S01',type:'clean',obligation:'C1 clean mandatory just lean',cmd:'just lean'},
{id:'S02',type:'counterpart-opaque',obligation:'C2 introduced owned predicate missing counterpart; opaque predicate classification, separately observed',cmd:'just lean',edits:[{path:'lean/Reactivegas/Predicates.lean',append:'\n\ndef auditMissingCounterpart (n : Nat) : Prop := n = 73\nopaque auditOpaquePredicate : Prop := True\n'}]},
{id:'S03',type:'theorem',obligation:'C3 introduced owned predicate with counterpart present and theorem absent',cmd:'just lean',edits:[{path:'lean/Reactivegas/Predicates.lean',append:'\n\ndef auditMissingTheorem (n : Nat) : Prop := n = 73\ndef auditCounterpart (n : Nat) : Bool := decide (n = 73)\n'},{path:'scripts/check-lean-mirrors',old:'def s4bCorrTable : Array (String × Option String × String) := #[',replacement:'def s4bCorrTable : Array (String × Option String × String) := #[\n  ("auditMissingTheorem", some "auditCounterpart", "auditMissingTheorem_corr"),'}]},
{id:'S04',type:'future-module',obligation:'tracked future owned module omitted from checker import closure',cmd:'just lean',edits:[{path:'lean/Reactivegas/AuditFuture.lean',create:'import Reactivegas.Types\ndef auditFutureValue : Nat := 73\n',track:true}]},
{id:'S05',type:'classifier-omit',obligation:'explicit theorem-kind arm omitted: permanent classifier must name actual unclassified declarations',cmd:'just lean',edits:[{path:'scripts/check-lean-mirrors',old:'    | .thmInfo _ => exclThm := exclThm + 1',replacement:'    -- auditor removes exactly the theorem-info classification arm'}]},
{id:'S06',type:'checker-noop',obligation:'C4 present executable checker unconditional success; mandatory receipt must reject',cmd:'just lean',edits:[{path:'scripts/check-lean-mirrors',create:'#!/usr/bin/env bash\nexit 0\n'}]},
{id:'S07',type:'P01-body',obligation:'P01 body original whole mandatory path, separate from selected helpers',cmd:'just lean',edits:[{path:'lean/KelGroups/Types.lean',old:'  (lookupMember key view).isSome',replacement:'  false'}]},
{id:'S08',type:'P07-body',obligation:'P07 close permission body original whole mandatory path',cmd:'just lean',edits:[{path:'lean/Reactivegas/Step.lean',old:'&& col.permitted && col.pending.isEmpty',replacement:'&& true && col.pending.isEmpty'}]},
{id:'S09',type:'base',obligation:'cold accepted-base build for compiled expression preservation comparison',cmd:'bash '+dest+'/base-build.sh',base:'3590c0015b84fd58004bf6fb44dd18b107304c48'},
{id:'S10',type:'final',obligation:'C26 restored final candidate cold full CI; prerequisite to final inventory/axiom trust',cmd:'just ci'}
];save('worlds.json',JSON.stringify(worlds,null,2)+'\n');
const mods=fs.readFileSync(rt+'/instruments/modules.txt','utf8').trim().split('\n').filter(m=>!m.endsWith('.Mirrors'));save('base-build.sh','#!/usr/bin/env bash\nset -euo pipefail\ncd '+rt+'/worlds/S09/lean\nexec lake build '+mods.join(' ')+'\n');save('base-modules.txt',mods.join('\n')+'\n');
console.log(JSON.stringify({selectedFragments:fragments.length,worlds:worlds.length,baseModuleInputs:mods.length}));
