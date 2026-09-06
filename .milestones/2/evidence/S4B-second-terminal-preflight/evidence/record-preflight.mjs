import fs from 'node:fs';
import { spawnSync } from 'node:child_process';
import crypto from 'node:crypto';
const root = '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r2';
const cwd = '/code/reactivegas-66-s4b-audit2';
const checks = [
  ['manifest', 'sha256sum', ['-c', 'MANIFEST.sha256'], `${root}/admitted`],
  ['clock', 'date', ['-u', '+%FT%TZ'], cwd],
  ['HEAD', 'git', ['rev-parse', 'HEAD'], cwd],
  ['detached', 'git', ['symbolic-ref', '-q', 'HEAD'], cwd],
  ['status', 'git', ['status', '--porcelain=v1'], cwd],
  ['oleans', 'find', ['.', '-type', 'f', '-name', '*.olean', '-print'], cwd],
  ['seats', 'tmux', ['list-panes', '-a', '-F', '#{pane_id} #{session_name}:#{window_index} #{window_name} pid=#{pane_pid} cmd=#{pane_current_command} cwd=#{pane_current_path}'], cwd],
  ['processes', 'ps', ['-o', 'pid,ppid,pgid,lstart,args', '-p', '2503883,1493708'], cwd],
  ['worktree-bytes', 'du', ['-sb', cwd], cwd],
];
const out = [];
for (const [id, command, args, workdir] of checks) {
  const r = spawnSync(command, args, {cwd:workdir, encoding:'utf8'});
  const stdout = id === 'seats' ? r.stdout.split('\n').filter(x => /^(%503|%547|%563) /.test(x)).join('\n')+'\n' : r.stdout;
  out.push({id, command, args, cwd:workdir, exit:r.status, stdout, stderr:r.stderr, error:r.error?.message, method:'inspection'});
}
for (const pid of [2503883,1493708]) {
  out.push({id:`proc-${pid}`, argv:fs.readFileSync(`/proc/${pid}/cmdline`,'utf8').split('\0').filter(Boolean), cwd:fs.readlinkSync(`/proc/${pid}/cwd`), method:'inspection'});
}
out.push({id:'pane', value:process.env.TMUX_PANE, method:'inspection'});
fs.writeFileSync(`${root}/evidence/preflight.json`, JSON.stringify(out,null,2)+'\n');
const names = ['brief.md','inbox-NOTE-001-ledger-and-scope-corrections.md',...fs.readdirSync(`${root}/admitted`).map(x=>`admitted/${x}`)];
fs.writeFileSync(`${root}/evidence/INPUTS.sha256`, names.map(n=>`${crypto.createHash('sha256').update(fs.readFileSync(`${root}/${n}`)).digest('hex')}  ${n}`).join('\n')+'\n');
const original = [
 ['S4-A01','Keep correspondence and consumer classification as independent axes'],
 ['S4-A02','DEFINITIONAL-IDENTITY requires cited reduction'],
 ['S4-A03','PROVED-EQUIVALENCE requires a named theorem for the independent Bool'],
 ['S4-A04','LOGICAL-DECISION-EVIDENCE records the instance without claiming correspondence or runtime capability'],
 ['S4-A05','EXECUTABLE-DECISION requires successful compilation and evaluation'],
 ['S4-A06','Existing mirror with missing correspondence owes a theorem and no duplicate mirror'],
 ['S4-A07','Missing required mirror owes both counterpart and correctness theorem'],
 ['S4-A08','NOT-EXECUTABLE claims carry bounded evidence'],
 ['S4-A09','NOT-ESTABLISHED records attempts and what would settle the identity'],
 ['S4-A10','UNDECIDABLE requires proof or cited authority; failed synthesis does not suffice for Reach'],
 ['S4-A11','REQUIRED-CONSUMER-IMPLEMENTED is justified by contract and existing implementation'],
 ['S4-A12','REQUIRED-CONSUMER-UNIMPLEMENTED remains a finding justified by contract'],
 ['S4-A13','NOT-REQUIRED is justified by authority rather than developer preference or absent caller'],
 ['S4-A14','Classification is not forced into an unsupported category'],
 ['S4-A15','Each identity names affected source paths and 68/69 rebind dependencies'],
 ['S4-A16','Historical counts 17 and 23 are not seeds, bounds, quotas or validators'],
 ['S4-A17','No manufactured decidability or mirror quota'],
 ['S4-A18','Phase A classification evidence is an input and confers no implementation acceptance'],
 ['S4-B01','Implement only finite obligations established as owed by Phase A and current authority'],
 ['S4-AUD01','Fresh independent auditor family is codex or grok, never muse or claude'],
 ['S4-AUD02','Validate both classification axes and proofs against external contract authority'],
 ['S4-AUD03','Misclassification cannot remove required or missing correspondence identities'],
 ['S4-AUD04','Set and preserve numeric substantive and targeted ceilings; failed calls count'],
 ['S4-F01','No model, original theorem statement, or statement-strengthening changes'],
 ['S4-F02','Only issue 71 writes docs/en/design; semantic questions route to the desk'],
 ['S4-F03','Local-only delivery and no human-pane writes'],
 ['S4-COMP01','Every required-unimplemented or missing-correspondence row retains an owned repair or dependency on the completion map'],
 ['S4-COMP02','Open questions are outcomes and never closures of required work'],
];
const v1 = fs.readFileSync(`${root}/admitted/INSTRUMENT-v1-SUPERSEDED.md`,'utf8');
const requirements = v1.split('\n').filter(l=>/^\| R\d+ \|/.test(l)).map(l=>{const a=l.split(' | ');return [a[0].slice(2),a[1]]});
const controls = [
 ['C1','Clean actual mandatory path returns zero'],['C2','New owned predicate without counterpart rejected and named through mandatory path'],
 ['C3','New owned predicate without theorem rejected and named through mandatory path'],
 ['C4','Ineffective checker or invocation while present detected by permanent mandatory mechanism with correct attribution; not exit 127'],
 ...Array.from({length:19},(_,i)=>[`C${i+5}`,`Separate well-typed executable-definition/input mutant for correspondence claim ${i+1}; original statement preserved and intended original theorem fails without first-failure masking`]),
 ['C24','Final-tree proof axioms permitted'],['C25','Final-tree totality and no PANIC at in either stream'],['C26','Restored full just ci at final accepted base returns zero'],
 ['V2-ID','Mechanically derive actual required identities and reconcile; nineteen is not an allowlist'],
 ['V2-DEFEQ','Definitional-equality sensitivity limits accurately stated; theorem mutation is not expression-body strength'],
 ['V2-BASE','Preserve full owned diff and controls across landed-base integration with incoming byte accounting'],
 ['V2-EVID','No unexecuted or wrong-reason control closes a row; parse/type/missing-tool failures are setup failures'],
 ['N1-COUNT','Full actual command classification; whole-module wrappers substantive, failed and warm calls retained'],
 ['N4-BASE','Accepted base is 3590c0015b84fd58004bf6fb44dd18b107304c48; re-establish affected final-candidate evidence'],
 ['B-MUT','Retain every raw mutant and final restoration evidence inside typed mutation fence'],
 ['B-PLAN','Enumerate actual full command set within carried 8/60 before START'],
 ['B-LABEL','Each conclusion labelled new-execution, unchanged-input with byte identity, or inspection'],
 ['N001-LEDGER','Own ledger distinct from read-only owner spend input; all rows initialized before START'],
 ['N001-COUNTERS','Carry S4-B-only counters; do not erase or substitute S4-A or other ticket histories'],
 ['N001-PREV','Predecessor terminal contract verdict preserved; semantic verdict unjudged'],
];
const rows=[...original,...requirements,...controls];
const head = `# S4-B audit campaign ledger\n\nCandidate: 189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8. Base: 3590c0015b84fd58004bf6fb44dd18b107304c48. Submission 1/2.\n\nAuditor carried and current spend: **0/8 substantive, 0/60 targeted**, TOTAL across both submissions. Owner spend input: 8/8 substantive, 42/60 targeted (packet claim, not independently audited). S4-A is separate: 3/3 and 15/20 (NOTE-001 claim). One S4-B ceiling increase; complete ticket-wide count unbound.\n\nInitialized pre-START from original S4 requirements, R1-R18 as incorporated by operative v2, C1-C26, and normative amendments/brief. These are obligation rows, not a discovered predicate inventory or distinct-mutant count. C5-C23 are the claimed control slots and may not bound the actual required identities. R10/R11 use v2 corrections; R18 uses amended ceilings.\n\nAll semantic verdicts remain OPEN. Campaign state BLOCKED records the dispatch precondition Q-001, not a semantic failure. No row below has an establishing semantic command. Observation for each: no dependent execution; authority preflight stopped by questions/Q-001-required-dispatch-bindings.md. Method is inspection. Administrative evidence is reported separately in AUDIT-REPORT.\n\n| Row | Obligation | Severity | Verdict | State | Substantive spent | Targeted spent | Mutants attempted/killed | Establishing command / observation | Method |\n|---|---|---|---|---|---:|---:|---|---|---|\n`;
const body=rows.map(([id,claim])=>`| ${id} | ${claim.replaceAll('|','/')} | BLOCKING | OPEN | BLOCKED | 0 | 0 | 0/0 | None; pre-START Q-001 | inspection |`).join('\n');
fs.writeFileSync(`${root}/handoffs/CAMPAIGN-LEDGER.md`,head+body+`\n\nRows: ${rows.length}; killed 0; residual 0; blocked ${rows.length}; experiments 0. Campaign has not begun and is not semantically closed. No full command set or numeric shortfall is claimed.\n`);
console.log(JSON.stringify({rows:rows.length,checks:out.length,spend:{substantive:0,targeted:0}}));
