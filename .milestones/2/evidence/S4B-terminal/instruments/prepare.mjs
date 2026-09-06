import fs from 'node:fs';
const root='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r3';
const rows=[];
function add(file,def,thm,atoms){for(const [atom,from,to] of atoms) rows.push({id:`M${String(rows.length+1).padStart(2,'0')}`,file,def,thm,atom,from,to});}
const r='lean/Reactivegas/Mirrors.lean',k='lean/KelGroups/Mirrors.lean';
add(r,'conservationB','conservation_corr', [['cassa','sumBal s.casse','(sumBal s.casse + 1)'],['conti','sumBal s.conti','(sumBal s.conti + 1)'],['escrow','escrowSum s.collections','(escrowSum s.collections + 1)']]);
add(r,'solventB','solvent_corr',[['member-domain','view.members.map Prod.fst','([] : List KelGroups.Key)'],['balance','bal s.conti u ≥ 0','bal s.conti u > 0'],['pledge-domain','col.accepted ++ col.pending','col.accepted'],['pledge-amount','0 ≤ p.amount','0 < p.amount']]);
add(r,'insolventB','insolvent_corr',[['existential','.any (fun u','.all (fun u']]);
add(r,'uniquePledgesB','uniquePledges_corr',[['user-binding','p.user = q.user','p.user ≠ q.user'],['pledge-equality','decide (p = q)','decide (p ≠ q)']]);
add(r,'allUniquePledgesB','allUniquePledges_corr',[['composition','=> uniquePledgesB col','=> !uniquePledgesB col']]);
add(r,'escrowHeldB','escrowHeld_corr',[['actor','splitUser u col.pending','splitUser "" col.pending'],['amount','amt = v','amt = v + 1'],['absence','| none => false','| none => true']]);
add(r,'governanceEnactsB','governanceEnacts_corr',[['actor','c.referente ≠ u','c.referente = u'],['all','.collections.all','.collections.any']]);
add(r,'doubleEntryB','doubleEntry_corr',[['conto','bal s.conti u + v','bal s.conti u + v + 1'],['cassa','bal s.casse a + v','bal s.casse a + v + 1']]);
add(r,'canCloseGroupB','canCloseGroup_corr',[['member-balance','bal s.conti u = 0','bal s.conti u ≠ 0'],['collections','s.collections = []','s.collections ≠ []'],['cassa-balance','bal s.casse r = 0','bal s.casse r ≠ 0']]);
add(k,'pendingWellFormedB','pendingWellFormed_corr',[['nodup','decide (pending.approvals.Nodup)','!decide (pending.approvals.Nodup)'],['proposer','pending.proposer ∈ pending.approvals','pending.proposer ∉ pending.approvals']]);
add(k,'membersCoherentB','membersCoherent_corr',[['key','e.2.key = e.1','e.2.key ≠ e.1']]);
add(k,'pendingCoherentB','pendingCoherent_corr',[['composition','=> pendingWellFormedB e.2','=> !pendingWellFormedB e.2']]);
add(k,'wellFormedB','wellFormed_corr',[['members-nodup','decide ((gs.members.map Prod.fst).Nodup)','!decide ((gs.members.map Prod.fst).Nodup)'],['pending-nodup','decide ((gs.pendingProposals.map Prod.fst).Nodup)','!decide ((gs.pendingProposals.map Prod.fst).Nodup)'],['member-coherence','membersCoherentB gs','!membersCoherentB gs'],['pending-coherence','pendingCoherentB gs','!pendingCoherentB gs']]);
add(k,'enactsB','enacts_corr',[['enactment','.enactment.isSome','.enactment.isNone'],['state','state = result','state ≠ result']]);
add(k,'questionCleanB','questionClean_corr',[['assents','decide (q.assents.Nodup)','!decide (q.assents.Nodup)'],['dissents','decide (q.dissents.Nodup)','!decide (q.dissents.Nodup)'],['disjoint','k ∉ q.dissents','k ∈ q.dissents']]);
add(k,'sweepReadyB','sweepReady_corr',[['open-nodup','decide ((gs.openQuestions.map Prod.fst).Nodup)','!decide ((gs.openQuestions.map Prod.fst).Nodup)'],['closed-nodup','decide ((gs.closed.map (·.questionId)).Nodup)','!decide ((gs.closed.map (·.questionId)).Nodup)'],['disjoint','qid ∉ gs.closed.map','qid ∈ gs.closed.map'],['open-clean','| some q => questionCleanB q','| some q => !questionCleanB q'],['closed-clean','fun c => questionCleanB c.question','fun c => !questionCleanB c.question'],['closed-verdict','c.verdict ≠ Verdict.open','c.verdict = Verdict.open'],['lookup','assocLookup qid gs.openQuestions','assocLookup "" gs.openQuestions']]);
add(k,'voteWellFormedB','voteWellFormed_corr',[['sweep','  sweepReadyB view gs &&','  !sweepReadyB view gs &&'],['open-verdict','verdictOf θ view q = Verdict.open','verdictOf θ view q ≠ Verdict.open'],['threshold','verdictOf θ view q','verdictOf (fun _ => 0) view q']]);
for(const row of rows){const src=fs.readFileSync(row.file,'utf8');const start=src.indexOf(`\ndef ${row.def} `)+1;if(start===0)throw Error(row.def);const end=src.indexOf('\n/--',start);const block=src.slice(start,end);if(block.split(row.from).length!==2)throw Error(`${row.id}: match not unique`);row.line=src.slice(0,start+block.indexOf(row.from)).split('\n').length;}
fs.writeFileSync(`${root}/instruments/mutations.json`,JSON.stringify(rows,null,2)+'\n');
console.log(`Prepared ${rows.length} single-definition mutations, each anchor unique; no elaboration executed.`);
