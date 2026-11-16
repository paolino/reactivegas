import fs from 'node:fs';
import crypto from 'node:crypto';
import path from 'node:path';

// Static preflight only. This program never invokes Lean, Nix, or a build.
const root = '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final';
const candidate = '/code/reactivegas-66-s4b-audit4';
const prior = '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r3';
const sha = x => crypto.createHash('sha256').update(x).digest('hex');
const write = (name, body) => fs.writeFileSync(path.join(root, name), body, {flag: 'wx'});
const operators = fs.readFileSync(`${prior}/instruments/mutations.json`, 'utf8');
const rows = JSON.parse(operators);
write('instruments/historical-operators-input.json', operators);
const sourceInputs = {};
const commands = [];
const firstPerTheorem = new Map();
for (const row of rows) {
  const source = fs.readFileSync(`${candidate}/${row.file}`, 'utf8');
  sourceInputs[row.file] = sha(source);
  const anchor = `\ndef ${row.def} `;
  if (source.split(anchor).length !== 2) throw new Error(`Definition anchor: ${row.id}`);
  const start = source.indexOf(anchor) + 1;
  const end = source.indexOf('\n/--', start);
  if (end < 0) throw new Error(`Definition boundary: ${row.id}`);
  const block = source.slice(start, end);
  if (block.split(row.from).length !== 2) throw new Error(`Non-single edit: ${row.id}`);
  const changed = block.replace(row.from, row.to);
  const mutant = source.slice(0, start) + changed + source.slice(end);
  const target = `\ntheorem ${row.thm} `;
  const theoremStart = source.indexOf(target, end) + 1;
  if (!theoremStart) throw new Error(`Missing target: ${row.id}`);
  const filename = `${root}/mutants/${row.id}.lean`;
  write(`mutants/${row.id}.lean`, mutant);
  if (mutant.slice(start + changed.length) !== source.slice(end)) throw new Error(row.id);
  const command = {
    id: row.id, charge: 'targeted', state: 'NOT-RUN', cwd: `${candidate}/lean`,
    argv: ['nix', 'develop', '--quiet', '-c', 'env',
      `LEAN_PATH=${candidate}/lean/.lake/build/lib/lean`,
      'lean', '-DautoImplicit=false', filename],
    input: filename, inputSha256: sha(mutant), productionPath: row.file,
    productionSha256: sha(source), definition: row.def, atom: row.atom,
    edit: {from: row.from, to: row.to}, theorem: row.thm,
    sourceDefinitionLine: source.slice(0, start).split('\n').length,
    originalTheoremLine: source.slice(0, theoremStart).split('\n').length,
    preservedSuffixSha256: sha(source.slice(end)),
    prerequisites: 'Fresh clean candidate build; hash-bound clean dependency manifest; no mutant overlays.',
    requiredObservation: 'Definition elaborates; preserved original theorem fails for the named semantic change. Parse/import/type/setup errors are not kills.',
    receiptIfAuthorized: `${root}/handoffs/evidence/${row.id}.log`
  };
  commands.push(command);
  if (!firstPerTheorem.has(row.thm)) firstPerTheorem.set(row.thm, command);
}
const checker = fs.readFileSync(`${candidate}/scripts/check-lean-mirrors`, 'utf8');
const claimedBlock = checker.split('def s4bCorrTable')[1].split('/-- Separately')[0];
const claimedMirrors = [...claimedBlock.matchAll(/some "([^"]+)"/g)]
  .map(x => x[1]).filter(x => x !== 'KelGroups.GroupView.isMember').map(x => x.split('.').at(-1));
const actualMirrors = [...new Set(commands.map(x => x.definition))].sort();
if (JSON.stringify([...claimedMirrors].sort()) !== JSON.stringify(actualMirrors)) {
  throw new Error('Source-level table/definition/operator mapping is incomplete');
}
write('handoffs/COMMANDS.json', JSON.stringify({
  status: 'PREFLIGHT-BLOCKED-NOT-ADMITTED',
  candidate: '94bb7bb64324a48f7361252556b4d15e45b3923f',
  base: '3590c0015b84fd58004bf6fb44dd18b107304c48',
  operatorInput: {path: `${prior}/instruments/mutations.json`, sha256: sha(operators),
    scope: 'Operator descriptions only; no historical outcomes or compiled artifacts inherited.'},
  sourceInputs, checkerSha256: sha(checker),
  observationScope: 'Static command binding only; not a compiled census or demonstrated control.',
  newAllowance: {substantive: 9, targeted: 10},
  spent: {substantive: 0, targeted: 0},
  selectedPerTheoremCommands: [...firstPerTheorem.values()].map(x => x.id),
  commandSubsetCount: commands.length,
  commandSubsetShortfall: commands.length - 10,
  perTheoremSubsetCount: firstPerTheorem.size,
  perTheoremSubsetShortfall: firstPerTheorem.size - 10,
  commands
}, null, 2) + '\n');
const quote = s => `'${s.replaceAll("'", "'\\''")}'`;
let md = '# Pre-admission command fit — NOT EXECUTED\n\n';
md += 'Candidate `94bb7bb64324a48f7361252556b4d15e45b3923f`; base `3590c0015b84fd58004bf6fb44dd18b107304c48`; full seven-commit range.\n\n';
md += 'The concrete mutation subplan below has ' + commands.length + ' separate single-file Lean invocations against current-candidate scratch inputs. Its targeted shortfall alone is ' + (commands.length - 10) + '. No invocation is authorized or executed by this document.\n\n';
md += 'Even the diagnostic one-per-theorem subset (' + [...firstPerTheorem.values()].map(x => x.id).join(', ') + ') contains ' + firstPerTheorem.size + ' calls, exceeding 10 by ' + (firstPerTheorem.size - 10) + '. This comparison is NOT a proposed narrowing of the full atom mandate.\n\n';
md += 'All commands have cwd `' + candidate + '/lean`. `LEAN_PATH` names only the future fresh clean candidate library, which does not yet exist. Current source inputs, exact single edits, preserved theorem/proof suffixes, expected diagnostics, output receipt paths, and full argv arrays are bound in `COMMANDS.json`. Each mutant source is retained. These are preparation artifacts, not executable evidence.\n\n';
md += '| ID | Definition / atom | Original theorem | State |\n|---|---|---|---|\n';
for (const c of commands) md += `| ${c.id} | ${c.definition} / ${c.atom} | ${c.theorem} | NOT-RUN |\n`;
md += '\nExact argv, one separately charged invocation per paragraph:\n\n';
for (const c of commands) md += '```sh\n' + c.argv.map(quote).join(' ') + '\n```\n\n';
md += '## Fit limits and remaining full mandate\n\n';
md += 'The 44 operators were read as descriptions from the prior auditor and rebound to current source bytes, one exact production-body edit each. No previous KILLED state, raw diagnostic, compiled dependency, or acceptance was transferred. All 17 current source-level new-mirror table entries reconcile with these definitions; this is not the required compiled denominator and makes no completeness claim about undiscovered declarations.\n\n';
md += 'The blocker does not depend on treating 44 as a global minimum. It is the cost of these concrete independently isolated commands. The operator one-per-theorem subset already exceeds the targeted allowance. Process sharing is permitted only when separate intended outcomes are actually reached and retained. Putting 44 Lean invocations under one shell does not make them one operation; combining edits into one mutant forfeits single-variable isolation. No compiled, unmasked multi-world batch instrument is bound in this packet. Designing and validating a different instrument is not an established cost saving.\n\n';
md += 'Still required beyond this subset: clean mandatory baseline; newly introduced counterpart-absent and theorem-absent controls; repaired opaque/module discovery and classifier-own-diagnostic control; present-but-disabled checker/nonce enforcement; P01 and P07 real production-path controls; clean final full CI and final axioms/totality; both classification axes with nonempty positive and can-fail negative controls; exact statement/proof bindings and nondegenerate witnesses; P01/P07 accurate relatum scope plus independent compile/positive/negative body-chain probes and clean/defective close witnesses. No part is waived, downgraded or inherited.\n\n';
md += 'A conventional substantive schedule would use nine whole-path invocations (baseline, counterpart absent, theorem absent, opaque/module discovery, classifier omission, P01 body, P07 body, checker-noop, final CI). Its mutations and integration probes are not frozen here: the already concrete targeted subplan fails admission before they are needed. P01/P07 shadow compile/negative/positive calls would add six targeted operations under the governing layer rule. Further census/witness/axiom and instrument controls have not been declared free or bundled into an imaginary exact total. Thus 34 is a lower bound on the shortfall of this 44-call subplan, not the asserted final full-audit deficit.\n\n';
md += 'Return AUDIT-CONTRACT-BLOCKED before START. Actual new spend: 0 substantive / 0 targeted. Historical 6/59 is preserved separately; cumulative ceilings 15/69 confer only the new 9/10. No 54-operation allowance exists.\n';
write('handoffs/COMMAND-FIT.md', md);
console.log(JSON.stringify({staticOnly: true, mutations: commands.length, identities: firstPerTheorem.size,
  targetedAllowance: 10, subsetGap: commands.length - 10, onePerIdentityGap: firstPerTheorem.size - 10,
  newSpend: {substantive: 0, targeted: 0}}));
