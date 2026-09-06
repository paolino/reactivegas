const fs = require('node:fs');
const path = require('node:path');
const cp = require('node:child_process');
const crypto = require('node:crypto');
const root = '/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b';
const parent = path.dirname(root);
const cwd = '/code/kelgroups-audit-3af3d06';
const evidence = path.join(root, 'handoffs/evidence');
fs.mkdirSync(evidence, { recursive: true });
fs.mkdirSync(path.join(root, 'questions'), { recursive: true });
const sha = b => crypto.createHash('sha256').update(b).digest('hex');
const write = (p, text) => fs.writeFileSync(p, text, { flag: 'wx' });
const commands = [
  ['seat', 'tmux', ['display-message', '-p', '-t', process.env.TMUX_PANE, '#{session_name}:#{window_index} #{pane_id} #{pane_pid}']],
  ['ambient-seat', 'tmux', ['display-message', '-p', '#S:#I']],
  ['ticket-panes', 'tmux', ['list-panes', '-t', process.env.TMUX_PANE, '-F', '#{pane_id} #{session_name}:#{window_index} #{pane_pid} #{pane_current_command}']],
  ['argv', 'ps', ['-p', '1113460,2583152,2708047', '-o', 'pid,ppid,lstart,args']],
  ['family', '/code/llm-settings/shared/skills/tmux-orchestrator/scripts/alternate-authoritative-cli', ['--seat', 'commit-auditor', 'muse', 'grok', 'claude']],
  ['candidate', 'git', ['rev-parse', 'HEAD', 'HEAD^{tree}', 'HEAD^']],
  ['detached', 'git', ['symbolic-ref', '-q', 'HEAD']],
  ['hygiene', 'git', ['status', '--porcelain']],
  ['base-ancestry', 'git', ['merge-base', '--is-ancestor', '368b596fef0b6d393c2ac7afc631d236c55d86d1', 'HEAD']],
  ['disk', 'du', ['-sb', cwd]],
  ['free-space', 'df', ['-B1', cwd]],
];
const receipts = [];
for (const [id, cmd, args] of commands) {
  const start = new Date().toISOString();
  const t = performance.now();
  const r = cp.spawnSync(cmd, args, { cwd, encoding: 'utf8' });
  const output = `STDOUT\n${r.stdout || ''}\nSTDERR\n${r.stderr || ''}`;
  const log = path.join(evidence, `preflight-${id}.log`);
  write(log, output);
  receipts.push({ id, command: [cmd, ...args], cwd, start, duration_ms: Math.round(performance.now()-t), exit: r.status, error: r.error?.message, log, sha256: sha(output), charge: { builds: 0, targeted: 0 }, purpose: 'read-only admission or contract identity; no semantic test' });
}
write(path.join(evidence, 'command-receipts.json'), JSON.stringify(receipts, null, 2)+'\n');
const inputs = [
  ['brief.md', path.join(root, 'brief.md')],
  ['launch.sh', path.join(root, 'launch.sh')],
  ['mandate-v2.md', path.join(parent, 'handoffs/S28-R1-COMMAND-PLAN-v2.md')],
  ['founding-contract-r5.md', path.join(parent, 'handoffs/S28-1-CONTRACT-r5.md')],
  ['owner-submission.md', path.join(parent, 'commit-owner-s28r1/SUBMISSION.md')],
  ['prior-s28-report.md', path.join(parent, 'commit-auditor-s28b/handoffs/AUDIT-REPORT.md')],
  ['prior-s28-ledger.md', path.join(parent, 'commit-auditor-s28b/handoffs/REQUIREMENT-LEDGER.md')],
  ['prior-s28r1-report.md', path.join(parent, 'commit-auditor-s28r1/handoffs/AUDIT-REPORT.md')],
  ['prior-s28r1-question.md', path.join(parent, 'commit-auditor-s28r1/questions/Q-001-auditor-seat-placement.md')],
  ['gate.sh', path.join(cwd, 'gate.sh')],
  ['Fold.hs', path.join(cwd, 'lib/KelGroups/Fold.hs')],
  ['Validate.hs', path.join(cwd, 'lib/KelGroups/Validate.hs')],
  ['State.hs', path.join(cwd, 'lib/KelGroups/State.hs')],
  ['Event.hs', path.join(cwd, 'lib/KelGroups/Event.hs')],
  ['S28DemoApp.hs', path.join(cwd, 'test/S28DemoApp.hs')],
  ['S28AppApiSpec.hs', path.join(cwd, 'test/S28AppApiSpec.hs')],
  ['auditor-SKILL.md', '/home/paolino/.codex/skills/auditor/SKILL.md'],
  ['commit-auditor-SKILL.md', '/home/paolino/.codex/skills/commit-auditor/SKILL.md'],
  ['tmux-SKILL.md', '/home/paolino/.codex/skills/tmux-orchestrator/SKILL.md'],
];
for (const name of fs.readdirSync(path.join(parent, 'inbox')).filter(n => /^NOTE-0(24|25|26|28|29)-/.test(n))) inputs.push([name, path.join(parent, 'inbox', name)]);
const manifest = [];
for (const [name, source] of inputs) {
  const bytes = fs.readFileSync(source);
  const target = path.join(evidence, `input-${name}`);
  write(target, bytes);
  manifest.push({ source, snapshot: target, bytes: bytes.length, sha256: sha(bytes) });
}
// Owner artifacts are identity-bound only; they are not independent verification.
for (const source of [
  path.join(parent, 'handoffs/evidence/20260905T231211Z-3af3d06-gate-full.log'),
  ...['build', 'test', 'ci'].map(n => path.join(parent, `commit-owner-s28r1/slim-${n}.log`)),
  path.join(parent, 'STATUS.md'),
]) {
  const bytes = fs.readFileSync(source);
  manifest.push({ source, bytes: bytes.length, sha256: sha(bytes), treatment: 'owner evidence identity only; no inherited verdict' });
}
write(path.join(evidence, 'input-manifest.json'), JSON.stringify(manifest, null, 2)+'\n');
const gate = fs.readFileSync(path.join(cwd, 'gate.sh'), 'utf8');
const identity = { full: sha(gate), normalized: sha(gate.replace(/^GATE_SHA256=".*"/m, 'GATE_SHA256=""')) };
write(path.join(evidence, 'gate-identity.json'), JSON.stringify(identity, null, 2)+'\n');
console.log(JSON.stringify({ receipts: receipts.length, inputs: manifest.length, gate: identity, builds: 0, targeted: 0 }));
