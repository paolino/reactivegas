import fs from 'node:fs';import crypto from 'node:crypto';
const root='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r3';const src=fs.readFileSync('lean/Reactivegas/Mirrors.lean','utf8');
for(const [id,thm,def] of [['P01','comune_not_a_member_corr','def comune_not_a_member (view : KelGroups.GroupView) : Prop := KelGroups.GroupView.isMember comuneId view\n'],['P07','permissionToClose_corr','def permissionToClose (col : Collection) : Prop := col.permitted ∨ col.pending = []\n']]){
 const begin=src.indexOf(`theorem ${thm} `),end=src.indexOf('\n/--',begin);const theorem=src.slice(begin,end);
 fs.writeFileSync(`${root}/mutants/${id}.lean`,`import Reactivegas.Step\n-- Definitional relatum control only; not executable-expression-body strength.\n${def}\n${theorem}\n`);
 fs.writeFileSync(`${root}/evidence/${id}-theorem.sha256`,crypto.createHash('sha256').update(theorem).digest('hex')+`  ${thm} original statement and proof bytes\n`);
}
