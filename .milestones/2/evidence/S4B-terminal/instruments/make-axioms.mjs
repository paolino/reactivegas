import fs from 'node:fs';
const root='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r3';
const muts=JSON.parse(fs.readFileSync(`${root}/instruments/mutations.json`));const names=[...new Set(muts.map(m=>(m.file.includes('KelGroups')?'KelGroups.'+(m.def.startsWith('question')||m.def.startsWith('sweep')||m.def.startsWith('vote')?'Vote.':''):'')+m.thm)), 'comune_not_a_member_corr','permissionToClose_corr','productionWellFormed_proj'];
fs.writeFileSync(`${root}/instruments/Axioms.lean`,'import Reactivegas.Mirrors\nimport KelGroups.Mirrors\n'+names.map(n=>`#check ${n}\n#print axioms ${n}`).join('\n')+'\n');console.log(names.length+' public theorem checks/axiom probes');
