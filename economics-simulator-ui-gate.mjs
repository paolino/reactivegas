#!/usr/bin/env node
/*
 * economics-simulator-ui-gate.mjs — shipped C-KEY / C-CHROME test surface.
 *
 * What it is: the in-repo executable harness for the two successor-campaign
 * classes. It derives the control extent from the actual page source, drives
 * every derived control through REAL interaction in headless Chromium (never
 * by reading source), reconciles derived controls to exercised witnesses,
 * and checks the enumerated render classes with the evidence still present.
 *
 * What it is not: the acceptance gate. The ticket owner authors and freezes
 * the versioned acceptance instrument (gate-v15); this file is production
 * work the acceptance gate invokes. Thresholds, pins and row severity live
 * there, not here.
 *
 * Usage:
 *   node economics-simulator-ui-gate.mjs [--repo DIR] [--html PATH]
 *       Full run: derive, drive, reconcile. EXIT 0 GREEN, EXIT 1 RED.
 *   node economics-simulator-ui-gate.mjs --omit <control-id>
 *       Omission control: discard that control's witnesses; the SINGLE
 *       ordinary reconciliation must go red naming it. RG_OMIT_NOOP=1
 *       neuters the discard (debug): then --omit must go GREEN, proving
 *       the RED came from the discard and not from the flag.
 *   node economics-simulator-ui-gate.mjs --derive-only [PATH]
 *       Fail-closed derivation without a browser: scan dataset reads,
 *       classify, print inventory. RED on unclassified/changed reads.
 *   node economics-simulator-ui-gate.mjs --vocab-only [--expect-red WORD]
 *       Actual checking path (live extract + classifyVocab) without the
 *       journey. With --expect-red, fires (exit 0) iff WORD is flagged.
 *   node economics-simulator-ui-gate.mjs --selftest
 *       Proves the instrument can fail: (a) full run GREEN; (b) --omit K-2
 *       RED; (c) intentional-coercion mutant RED on actual interaction;
 *       (d) proof sentence in BOTH states (provato live, enunciato flipped).
 *   node economics-simulator-ui-gate.mjs --html SCRATCH --expect-enunciato
 *       Live-sentence check inverted for the flipped-receipt scratch run.
 *   node economics-simulator-ui-gate.mjs --html P [--expect-enunciato] --sentence-only
 *       Fast targeted probe: boot the page and check ONLY the H-1 proof
 *       sentence (both-states flips without running the whole journey).
 *
 * Campaign accounting (contract counting rule): a FULL run of this file is a
 * full suite run = ONE substantive invocation. --omit / --expect-enunciato /
 * single mutant or flip probes are targeted invocations. --selftest drives
 * one full child plus four targeted probes.
 */

import { spawn, execFileSync } from 'node:child_process';
import { readFileSync, mkdtempSync, rmSync, existsSync,
  readdirSync, copyFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));
const argVal = flag => {
  const i = process.argv.indexOf(flag);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : null;
};
const hasFlag = f => process.argv.includes(f);
const HTML = argVal('--html') || join(argVal('--repo') || REPO, 'economics-simulator.html');
const EXPECT_ENUNCIATO = hasFlag('--expect-enunciato');
const SENTENCE_ONLY = hasFlag('--sentence-only');
const OMIT = argVal('--omit');

function rmQuiet(p) {
  try { rmSync(p, { recursive: true, force: true }); } catch { /* housekeeping */ }
}
const sleep = ms => new Promise(r => setTimeout(r, ms));

/* --- chromium discovery (same shape as the sibling gates) ---------------- */

function findChromium() {
  if (process.env.RG_CHROMIUM && existsSync(process.env.RG_CHROMIUM))
    return process.env.RG_CHROMIUM;
  for (const name of ['chromium', 'chromium-browser', 'google-chrome']) {
    try {
      const p = execFileSync('which', [name], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
      if (p) return p;
    } catch { /* keep looking */ }
  }
  const store = '/nix/store';
  try {
    const hits = [];
    for (const d of readdirSync(store)) {
      if (d.includes('playwright-chromium-headless-shell'))
        hits.unshift(join(store, d, 'chrome-linux', 'headless_shell'));
      else if (d.includes('playwright-chromium') && !d.includes('patch'))
        hits.push(join(store, d, 'chrome-linux', 'chrome'));
    }
    for (const p of hits) if (existsSync(p)) return p;
  } catch { /* no nix store */ }
  throw new Error('nessun chromium disponibile (RG_CHROMIUM, PATH, /nix/store)');
}

/* --- minimal CDP-over-pipe client ------------------------------------------ */

class Browser {
  constructor(bin, profile) {
    this.child = spawn(bin, ['--headless', '--no-sandbox', '--disable-gpu',
      '--remote-debugging-pipe', '--user-data-dir=' + profile, 'about:blank'],
      { stdio: ['ignore', 'pipe', 'pipe', 'pipe', 'pipe'] });
    this.buf = '';
    this.nextId = 1;
    this.pending = new Map();
    this.events = [];
    this.child.stdio[4].setEncoding('utf8');
    this.child.stdio[4].on('data', d => {
      this.buf += d;
      let i;
      while ((i = this.buf.indexOf('\0')) >= 0) {
        const msg = JSON.parse(this.buf.slice(0, i));
        this.buf = this.buf.slice(i + 1);
        if (msg.id && this.pending.has(msg.id)) {
          this.pending.get(msg.id)(msg);
          this.pending.delete(msg.id);
        } else this.events.push(msg);
      }
    });
  }
  send(method, params, sessionId) {
    const id = this.nextId++;
    return new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error('CDP timeout: ' + method)), 30000);
      this.pending.set(id, m => {
        clearTimeout(t);
        m.error ? rej(new Error(method + ': ' + JSON.stringify(m.error))) : res(m.result);
      });
      this.child.stdio[3].write(JSON.stringify({ id, method, params: params || {}, sessionId }) + '\0');
    });
  }
  async page(url, width, height) {
    const t = await this.send('Target.createTarget', { url: 'about:blank' });
    const a = await this.send('Target.attachToTarget', { targetId: t.targetId, flatten: true });
    const s = a.sessionId;
    await this.send('Runtime.enable', {}, s);
    await this.send('Page.enable', {}, s);
    await this.send('Log.enable', {}, s);
    await this.send('Emulation.setDeviceMetricsOverride',
      { width, height, deviceScaleFactor: 1, mobile: width < 500 }, s);
    await this.send('Page.navigate', { url }, s);
    await sleep(1100);
    return s;
  }
  async eval(sessionId, expression) {
    const r = await this.send('Runtime.evaluate',
      { expression, awaitPromise: true, returnByValue: true }, sessionId);
    if (r.exceptionDetails)
      throw new Error('eval nel browser: ' +
        (r.exceptionDetails.exception && r.exceptionDetails.exception.description ||
         r.exceptionDetails.text));
    return r.result.value;
  }
  close() { try { this.child.kill(); } catch { /* gone */ } }
}

/* --- D3 fail-closed derivation: the extent is COMPUTED from the page. ---
   deriveExtent(src) scans every `.dataset.X` read in the page source and
   classifies each against KNOWN_READS. An UNKNOWN read (an added
   key-bearing control the table never heard of) or a CHANGED read-site
   count is RED — fail-closed in the direction C-KEY names. The witness
   registry maps derived controls to exercised witnesses; driving stays
   hand-written work, discovery does not. K-12's sink is an input element,
   derived from its element + param patterns. K-14 is proven unreachable by
   the same scan: no emission and no read. --- */

const KNOWN_READS = {
  // read name: [expected read sites, covering witness controls]
  gotoPerson: [1, ['K-1']],
  u:          [1, ['K-2']],
  id:         [5, ['K-3', 'K-4']],
  pledgeU:    [1, ['K-5']],
  obj:        [1, ['K-6', 'K-7']],
  task:       [1, ['K-9']],
  bgapprove:  [1, ['K-10']],
  bgsigner:   [1, ['K-10']],
  kgcast:     [1, ['K-11']],
  kgsigner:   [1, ['K-11']],
  kgpropose:  [2, ['K-4']],
  cf:         [1, ['K-13']],
  gotoColl:   [1, ['K-8']],
  pledgeC:    [1, ['K-5']],
  crumb:      [1, ['K-15']],
  act:        [2, []],
  hat:        [1, []],
  kgballot:   [1, []],
  l:          [1, []],
  claims:     [2, []],
  offClaims:  [1, []],
  teachx:     [1, []],
};
const INPUT_SINKS = [
  { id: 'K-12', desc: 'name input -> event target key', src: ['id="un-n"', 'flow.params.target'] },
];
const K14 = { id: 'K-14',
  desc: 'data-kgadmit/data-kgremove: selector only — scan proves no emission and no read' };
const CONTROL_DESC = {
  'K-1': 'data-goto-person -> nav().u',
  'K-2': 'data-act data-u -> event user',
  'K-3': 'task-flow chip -> event author/target/from/c/user',
  'K-4': 'vote-signer chip (+propose) -> runVoteEvent signer',
  'K-5': 'data-pledge row -> nav().pu + nav().c',
  'K-6': 'data-obj member/conto/cassa -> nav().u',
  'K-7': 'data-obj pile -> nav().c (numeric)',
  'K-8': 'data-goto-coll -> nav().c (numeric)',
  'K-9': 'data-task + nav preset -> event author/user/c',
  'K-10': 'data-bgapprove -> base signer + proposal',
  'K-11': 'data-kgcast -> vote signer + ballot',
  'K-12': 'name input -> event target key',
  'K-13': 'data-cf conto/cassa -> display id (adjacent hardening)',
  'K-15': 'data-crumb -> nav index (numeric)',
};

function deriveExtent(src) {
  const problems = [];
  const found = {};
  const count = name => { found[name] = (found[name] || 0) + 1; };
  // every spelling of a dataset read, on any receiver: dot notation,
  // quoted-bracket notation, bare or dotted. A non-literal key cannot be
  // classified statically and is RED fail-closed (registered explicitly
  // or not at all). String mentions of a read shape count as reads: the
  // direction is fail-closed by design, and today the page has none.
  for (const m of src.matchAll(/\.dataset\.([A-Za-z]+)/g)) count(m[1]);
  for (const m of src.matchAll(/(?<![\w$])dataset\[\s*['"]([A-Za-z]+)['"]\s*\]/g)) count(m[1]);
  for (const m of src.matchAll(/(?<![\w$])dataset\[(?!\s*['"])[^\]]+\]/g))
    problems.push(`accesso dataset dinamico, chiave non classificabile staticamente: «${m[0].trim().slice(0, 48)}» — registrare esplicitamente`);
  for (const [name, n] of Object.entries(found)) {
    const k = KNOWN_READS[name];
    if (!k) problems.push(`lettura dataset non classificata: .dataset.${name} ×${n} — estendere l'harness, non ignorarla`);
    else if (k[0] !== n) problems.push(`sito di lettura mutato per .dataset.${name}: attesi ${k[0]}, trovati ${n}`);
  }
  for (const s of INPUT_SINKS)
    for (const p of s.src)
      if (!src.includes(p)) problems.push(`sorgente di ${s.id} assente: «${p}» — aggiornare l'harness, non ignorarlo`);
  const k14emit = ['data-kgadmit="', 'data-kgremove="'].some(p => src.includes(p));
  const k14read = ['dataset.kgadmit', 'dataset.kgremove'].some(p => src.includes(p));
  if (k14emit || k14read) problems.push('K-14 non più irraggiungibile: emissione o lettura rilevata — serve un testimone');
  const required = new Set(INPUT_SINKS.map(s => s.id));
  for (const ctrls of Object.values(KNOWN_READS).map(k => k[1]))
    for (const c of ctrls) required.add(c);
  return { problems, required: [...required].sort(), reads: found };
}

/* --- D1 actual checking path: pure, single-escaped, unit-executable. ---
   stripProvenance sets aside precisely-shaped machine tokens; classifyVocab
   runs the banned/legacy verdicts on what remains. run() and --vocab-only
   feed it the live-extracted copy; the D1 demo feeds it fabricated copies. */
function stripProvenance(t) {
  return t
    .replace(/\.[A-Za-z][\w]*/g, '')
    .replace(/[A-Za-z_][\w]*(\.[\w]+){2,}/g, '')
    .replace(/[\w.-]+\.(mjs|lean|json|trace)(v\d+)?/gi, '')
    .replace(/\b[0-9a-f]{7,}\b/gi, '');
}
function classifyVocab(copyText) {
  const v = stripProvenance(copyText);
  const banned = [];
  if (/colletta/i.test(v)) banned.push('colletta');
  if (/\bpledge\b/i.test(v)) banned.push('pledge');
  return { banned, legacy: /impegn/i.test(v) && /acquist/i.test(v) };
}
const RENDER_CLASSES = ['H-1 cards', 'H-2 dialogs/pop', 'H-3 refusals', 'H-4 toasts', 'H-5 feed', 'H-6 strips'];

const failures = [];
const witnessed = new Set();
const red = why => { failures.push(why); console.error('RED-ROW: ' + why); };
/* D2: the discard changes the ORDINARY collected evidence; the single
   ordinary reconciliation below decides. RG_OMIT_NOOP=1 neuters the discard
   (debug only): with it set, --omit must go GREEN, proving the RED came
   from the discard and not from the flag. */
const OMIT_NOOP = process.env.RG_OMIT_NOOP === '1';
const witness = (id, what) => {
  if (OMIT === id && !OMIT_NOOP) { console.log(`omit: witness ${id} (${what}) scartato`); return; }
  witnessed.add(id);
  console.log(`witness ${id}: ${what}`);
};

/* click through the real handler: dispatched MouseEvent on the live node */
const CLICK = sel => `
  (() => {
    const el = document.querySelector(${JSON.stringify(sel)});
    if (!el) throw new Error('controllo mancante: ' + ${JSON.stringify(sel)});
    if (el.disabled) throw new Error('controllo disabilitato: ' + ${JSON.stringify(sel)});
    el.scrollIntoView({ block: 'nearest' });
    const r = el.getBoundingClientRect();
    el.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true,
      clientX: r.x + r.width / 2, clientY: r.y + r.height / 2 }));
    return 'clicked ' + ${JSON.stringify(sel)};
  })()`;

async function run() {
  const src = readFileSync(HTML, 'utf8');

  /* ---- derive: the extent is computed from the page source (D3) ---- */
  const ext = deriveExtent(src);
  for (const p of ext.problems) red(p);
  console.log(`derived ${ext.required.length} controls from ${Object.keys(ext.reads).length} dataset reads + input sinks in ${HTML}`);
  console.log(`K-14 scan-proven unreachable (no emission, no read)`);

  const bin = findChromium();
  const profile = mkdtempSync(join(tmpdir(), 'ui-gate-'));
  const b = new Browser(bin, profile);
  try {
    const s = await b.page(pathToFileURL(HTML).href + '?ui-gate=1', 1280, 900);
    const ev = expr => b.eval(s, `(${expr})()`);
    const click = async sel => { await b.eval(s, CLICK(sel)); await sleep(120); };

    const snap = () => ev(`() => ({
      members: members().slice(), admins: admins().slice(),
      nav: JSON.parse(JSON.stringify(nav())),
      pendingBase: JSON.parse(JSON.stringify(bg.pendingBase)),
      colls: state.collections.map(c => ({ id: c.id, t: typeof c.id, referente: c.referente,
        permitted: c.permitted,
        pend: c.pending.map(p => ({ u: p.user, t: typeof p.user })),
        acc: c.accepted.map(p => ({ u: p.user, t: typeof p.user })) })),
    })`);
    const t = (cond, why) => { if (!cond) red(why); };

    /* ---- boot assumptions, stated not smuggled ---- */
    const boot = await snap();
    t(JSON.stringify(boot.members) === '["anna"]' && JSON.stringify(boot.admins) === '["anna"]',
      `avvio inatteso: members=${JSON.stringify(boot.members)} admins=${JSON.stringify(boot.admins)}`);

    /* ---- H-6a: teaching strip on arrival ---- */
    t(await ev(`() => !!document.querySelector('[data-teach="benvenuto"]')`),
      'H-6: striscia di benvenuto assente su profilo fresco');
    console.log('witness H-6a: benvenuto strip on arrival');
    await click('[data-teachx="benvenuto"]');
    t(await ev(`() => !document.querySelector('[data-teach="benvenuto"]')`),
      'H-6: la chiusura esplicita non ritira la striscia');

    /* ---- admit 01 / Zoe / 07 through the real name control ---- */
    const admit = async (name, key) => {
      await click('#gtasks [data-task="admitMember"]');
      await ev(`() => {
        const n = document.getElementById('un-n');
        if (!n) throw new Error('campo nome assente');
        n.value = ${JSON.stringify(name)};
        n.dispatchEvent(new Event('input', { bubbles: true }));
        return document.getElementById('un-ok').disabled ? 'blocked' : 'ok';
      }`).then(r => { if (r !== 'ok') red(`ammissione di ${name} bloccata dal controllo: ${r}`); });
      await click('#un-ok');
      const popTx = await ev(`() => (document.querySelector('#pop .pbox') || {}).innerText || ''`);
      t(/persona\?|registra\?|responsabile/.test(popTx), `H-2: prompt di scelta assente nel dialogo (admit ${name})`);
      await click('#pop .chip[data-id="anna"]');
      const m = await snap();
      t(m.members.includes(key) && typeof key === 'string',
        `K-12: chiave ${key} non in members dopo ammissione reale (${JSON.stringify(m.members)})`);
      witness('K-12', `admitMember reale ${name} -> chiave ${key}`);
      witness('K-3', `chip autore anna per ammissione ${key}`);
    };
    await admit('01', '01');
    await admit('Zoë', 'zoë');
    await admit('07', '07');
    console.log('witness H-2: dialog prompt with Italian choice text');
    const labels = await ev(`() => JSON.parse(JSON.stringify(userLabels))`);
    t(labels['01'] === '01' && labels['zoë'] === 'Zoë',
      `etichette non preservate: ${JSON.stringify(labels)}`);

    /* ---- elect 01 and 07 through real target/author chips ---- */
    for (const who of ['01', '07']) {
      await click('#gtasks [data-task="elect"]');
      await click(`#pop .chip[data-id="${who}"]`);
      await click('#pop .chip[data-id="anna"]');
      const m = await snap();
      t(m.admins.includes(who), `K-3: elezione di ${who} non applicata (${JSON.stringify(m.admins)})`);
      witness('K-3', `chip bersaglio+autore per elezione ${who}`);
    }

    /* ---- K-6: SVG member node -> person view, key exact ---- */
    await click('[data-obj="member"][data-id="anna"]');
    let m = await snap();
    t(m.nav.view === 'person' && m.nav.u === 'anna' && typeof m.nav.u === 'string',
      `K-6: vista persona anna non raggiunta (${JSON.stringify(m.nav)})`);
    witness('K-6', 'data-obj member anna -> nav().u stringa');

    /* ---- deposit anna -> 01 from the cassiere hat (preset.author path) ---- */
    await click('[data-hat="cassiere"]');
    await click('#hat-cassiere [data-task="deposit"]');
    await click('#pop .chip[data-id="01"]');
    witness('K-3', 'chip utente 01 per accredito (ruolo user)');
    witness('K-9', 'preset.author dalla vista persona-cassiere');
    await ev(`() => {
      const n = document.getElementById('am-n');
      n.value = '50'; n.dispatchEvent(new Event('input', { bubbles: true }));
    }`);
    await click('#am-ok');
    m = await snap();
    const conto01 = await ev(`() => bal(state.conti, '01')`);
    t(conto01 === 50, `accredito a 01 non applicato (conto=${conto01})`);
    t(await ev(`() => !!document.querySelector('.toast')`), 'H-4: brindisi assente dopo evento applicato');
    console.log('witness H-4: toast after applied deposit');

    /* ---- open Olio with referente anna (author preset by cassiere hat) ---- */
    await click('#hat-cassiere [data-task="openPurchase"]');
    await click('#pop [data-l="Olio"]');
    m = await snap();
    t(m.colls.length === 1 && m.colls[0].id === 1 && m.colls[0].t === 'number' &&
      m.colls[0].referente === 'anna',
      `K-3/N: acquisto non creato con CollId numerico e referente anna (${JSON.stringify(m.colls)})`);
    witness('K-3', 'apertura acquisto con autore dal contesto (chip coll numerico)');

    /* ---- K-8: referente-di linkchip -> collection, id numeric ---- */
    await click('[data-goto-coll="1"]');
    m = await snap();
    t(m.nav.view === 'collection' && m.nav.c === 1 && typeof m.nav.c === 'number',
      `K-8: vista acquisto non raggiunta con id numerico (${JSON.stringify(m.nav)})`);
    witness('K-8', 'data-goto-coll -> nav().c numerico');

    /* ---- H-3a: spent close affordance -> proof disclosure, not a task ---- */
    const closeBtn = await ev(`() => {
      const el = document.querySelector('#refpanel [data-task="closePurchase"]');
      return el ? el.className : null;
    }`);
    t(closeBtn && closeBtn.includes('off'), 'H-3: chiusura senza permesso non spenta');
    await click('#refpanel [data-task="closePurchase"]');
    t(await ev(`() => !!document.querySelector('#pop .pbox')`),
      'H-3: affordance spenta non apre la prova del rifiuto');
    t(await ev(`() => !!document.querySelector('[data-teach="guardie"]')`),
      'H-3: striscia guardie assente dopo rifiuto pre-macchina');
    console.log('witness H-3a: spent affordance -> claim pop + guardie strip');
    await click('#pop .xbtn2');

    /* ---- K-9: pledge from person-01 member hat (preset.user path) ---- */
    await click('[data-crumb="0"]');
    await click('[data-obj="member"][data-id="01"]');
    m = await snap();
    t(m.nav.view === 'person' && m.nav.u === '01' && typeof m.nav.u === 'string',
      `K-6: vista persona 01 con chiave esatta non raggiunta (${JSON.stringify(m.nav)})`);
    witness('K-6', 'data-obj member 01 -> nav().u "01" (zero iniziale)');
    await click('#hat-member [data-task="pledge"]');
    witness('K-9', 'preset.user dalla vista persona-membro');
    await click('#pop .chip[data-id="1"]');
    await click('#pop .chip[data-id="anna"]');
    await ev(`() => {
      const n = document.getElementById('am-n');
      n.value = '10'; n.dispatchEvent(new Event('input', { bubbles: true }));
    }`);
    await click('#am-ok');
    m = await snap();
    t(m.colls[0].pend.length === 1 && m.colls[0].pend[0].u === '01' &&
      m.colls[0].pend[0].t === 'string',
      `K-9: impegno di 01 non registrato con chiave stringa (${JSON.stringify(m.colls)})`);

    /* ---- back to the collection via the pile node (K-7, numeric) ---- */
    await click('[data-crumb="0"]');
    await click('[data-obj="pile"][data-id="1"]');
    m = await snap();
    t(m.nav.view === 'collection' && m.nav.c === 1 && typeof m.nav.c === 'number',
      `K-7: nodo pila non apre l'acquisto con id numerico (${JSON.stringify(m.nav)})`);
    witness('K-7', 'data-obj pile -> nav().c numerico');

    /* ---- K-2: referente queue accept for digit-shaped key ---- */
    await click('#refpanel [data-act="accept"][data-u="01"]');
    m = await snap();
    t(m.colls[0].pend.length === 0 && m.colls[0].acc.length === 1 &&
      m.colls[0].acc[0].u === '01' && m.colls[0].acc[0].t === 'string',
      `K-2: accettazione non ha conservato la chiave "01" (${JSON.stringify(m.colls)})`);
    witness('K-2', 'data-act accept data-u 01 -> event user "01"');

    /* ---- K-5: pledge row -> pledge view (pu string, c numeric) ---- */
    await click('.row[data-pledge-c="1"]');
    m = await snap();
    t(m.nav.view === 'pledge' && m.nav.pu === '01' && typeof m.nav.pu === 'string' &&
      m.nav.c === 1 && typeof m.nav.c === 'number',
      `K-5: riga impegno non porta a pu stringa + c numerico (${JSON.stringify(m.nav)})`);
    witness('K-5', 'data-pledge row -> nav().pu "01" + nav().c numerico');

    /* ---- K-1: goto-person from the pledge view ---- */
    await click('[data-goto-person="01"]');
    m = await snap();
    t(m.nav.view === 'person' && m.nav.u === '01' && typeof m.nav.u === 'string',
      `K-1: data-goto-person non conserva "01" (${JSON.stringify(m.nav)})`);
    t(await ev(`() => document.querySelector('#view .who').textContent.includes('01')`),
      'K-1: vista persona potata dopo il click (pruneNav ha scartato la chiave)');
    witness('K-1', 'data-goto-person 01 -> nav().u "01", vista non potata');

    /* ---- K-10: departure proposal + string-signer approval ---- */
    await click('[data-crumb="0"]');
    await click('#gtasks [data-task="departure"]');
    await click('#pop .chip[data-id="zoë"]');
    await click('#pop .chip[data-id="anna"]');
    m = await snap();
    t(m.pendingBase.length === 1,
      `K-10: proposta di uscita non registrata (${JSON.stringify(m.pendingBase)})`);
    const apprBtn = await ev(`() => !!document.querySelector('[data-bgapprove][data-bgsigner="01"]')`);
    t(apprBtn, 'K-10: pulsante di assenso per 01 assente (soglia? franchise?)');
    await click('[data-bgapprove][data-bgsigner="01"]');
    m = await snap();
    t(!m.members.includes('zoë') && m.pendingBase.length === 0,
      `K-10: assenso di 01 non ha deliberato l'uscita (${JSON.stringify(m.members)})`);
    witness('K-10', 'data-bgapprove signer 01 stringa -> uscita deliberata');

    /* ---- K-4 + K-11: vote question + string-signer casts ---- */
    await click('[data-key="pile:1"]');
    await click('#view [data-kgpropose]');
    await click('#pop .chip[data-id="01"]');
    witness('K-4', 'chip firmatario 01 per apertura domanda');
    let open = await ev(`() => state.votes.openQuestions.length`);
    t(open === 1, 'K-4: domanda di voto non aperta');
    const qid = await ev(`() => state.votes.openQuestions[0][0]`);
    await click(`#view [data-kgcast="${qid}"][data-kgsigner="01"][data-kgballot="assent"]`);
    await click(`#view [data-kgcast="${qid}"][data-kgsigner="anna"][data-kgballot="assent"]`);
    const verdict = await ev(`() => state.votes.closed.length ?
      state.votes.closed[state.votes.closed.length - 1].verdict : 'open'`);
    t(verdict === 'positive', `K-11: doppio sì non ha chiuso POSITIVA (verdetto=${verdict})`);
    const assents = await ev(`() => { const r = state.votes.closed[state.votes.closed.length - 1];
      return r.question.assents; }`);
    t(assents.includes('01'), `K-11: assenso di 01 non registrato come stringa (${JSON.stringify(assents)})`);
    witness('K-11', 'data-kgcast firmatari 01+anna -> verdetto positivo');
    t(await ev(`() => !!document.querySelector('[data-teach="voto"]')`) === false,
      'H-6: striscia voto ancora mostrata a domanda chiusa');
    console.log('witness H-6c: voto strip tracks open-question condition');

    /* ---- grant bridge, then close: feed rows + done card ---- */
    await click('#view [data-task="grantPermission"]');
    await click('#ext-apply');
    m = await snap();
    t(m.colls[0].permitted === true, 'ponte grant non applicato');
    await click('#view [data-task="closePurchase"]');
    const doneRows = await ev(`() => document.querySelectorAll('#done-body .done').length`);
    t(doneRows === 1, `H-5: riga completati assente dopo chiusura (n=${doneRows})`);
    console.log('witness H-5b: done card row after close');
    t(await ev(`() => !!document.querySelector('[data-teach="chiusa"]')`),
      'H-6: striscia chiusa assente dopo chiusura');
    console.log('witness H-6d: chiusa strip after close');
    t(await ev(`() => !!document.querySelector('.toast')`), 'H-4: brindisi assente dopo chiusura');
    const feedTxt = await ev(`() => document.getElementById('log').innerText`);
    t(/accetta l'impegno di 01/.test(feedTxt) && /chiude/.test(feedTxt),
      'H-5: voci attese assenti nel diario');
    console.log('witness H-5c: feed entries carry the journey');
    /* ---- K-13: counterfactual chip keeps the string key ---- */
    await click('#inv-chip');
    await click('[data-cf="conto:01"]');
    const cfTx = await ev(`() => document.querySelector('#pop').innerText`);
    t(/conto di 01/.test(cfTx), `K-13: contro-fattuale non nomina 'conto di 01' (${JSON.stringify(cfTx.slice(0, 120))})`);
    t(!/conto di 1[^0-9]/.test(cfTx), 'K-13: chiave coartata a numero nel contro-fattuale');
    witness('K-13', 'data-cf conto:01 -> display id "01"');
    await click('#pop .xbtn2');

    /* ---- K-15: crumb navigation by numeric index (re-descend first: ----
    /* ---- the close pruned collection views off the stack) ---- */
    await click('[data-obj="member"][data-id="01"]');
    m = await snap();
    t(m.nav.view === 'person' && m.nav.u === '01',
      `K-15: discesa su persona 01 fallita (${JSON.stringify(m.nav)})`);
    await click('[data-crumb="0"]');
    m = await snap();
    t(m.nav.view === 'group', `K-15: briciola 0 non torna al gruppo (${JSON.stringify(m.nav)})`);
    witness('K-15', 'data-crumb numeric index -> group');

    /* ---- H-1: governance card, no internal identifiers, sentence tracks receipt ---- */
    const h2 = await ev(`() => (document.querySelector('#govcard h2') || {}).innerText || ''`);
    t(h2 && !/KelGroups|GroupState/.test(h2),
      `H-1: intestazione con vocabolo interno (${JSON.stringify(h2)})`);
    const status = await ev(`() => (document.getElementById('gov-status') || {}).innerText || ''`);
    if (EXPECT_ENUNCIATO) {
      t(/enunciate, non dimostrate/.test(status),
        `H-1: ricevuta con enunciato ma frase non voltata (${JSON.stringify(status.slice(0, 160))})`);
    } else {
      t(/provate/.test(status) && !/enunciate/.test(status),
        `H-1: frase non derivata dalla ricevuta (${JSON.stringify(status.slice(0, 160))})`);
    }
    console.log(`witness H-1: govcard heading + proof sentence (${EXPECT_ENUNCIATO ? 'enunciato' : 'provato'} state)`);

    /* ---- full-text vocabulary scan through the actual checking path ---- */
    const copyText = await ev(`() => {
      const c = document.body.cloneNode(true);
      c.querySelectorAll('script, noscript, style').forEach(el => el.remove());
      return c.innerText;
    }`);
    const vc = classifyVocab(copyText);
    t(!vc.banned.length, 'H-vocab: termine vietato nel testo visibile: ' + vc.banned.join(','));
    t(vc.legacy,
      'H-vocab: vocabolario legacy assente dal testo visibile');
    console.log('witness H-vocab: full-text scan without .mono/#pop/.toast erasure');

    /* ---- reconcile: ONE ordinary check over the derived extent (D2) ---- */
    const missing = ext.required.filter(id => !witnessed.has(id));
    if (missing.length)
      red(`copertura incompleta: ${missing.map(id => id + ' ' + (CONTROL_DESC[id] || '?')).join('; ')}`);
  } finally {
    b.close();
    rmQuiet(profile);
  }

  if (failures.length) {
    console.error(`RED: ui-gate ${HTML} — ${failures.length} righe`);
    for (const f of failures) console.error(' - ' + f);
    process.exitCode = 1;
    return;
  }
  console.log('GREEN: ui-gate ' + HTML + ' — ' + witnessed.size + '/' + ext.required.length +
    ' controlli derivati testimoniati; classi ' + RENDER_CLASSES.join(', ') +
    '; K-14 irraggiungibile nominato; CollId numerici');
}

/* --- sentence-only: fast H-1 probe for both-states flips --- */

async function sentenceOnly() {
  const bin = findChromium();
  const profile = mkdtempSync(join(tmpdir(), 'ui-gate-sent-'));
  const b = new Browser(bin, profile);
  try {
    const s = await b.page(pathToFileURL(HTML).href + '?ui-gate=sentence', 1280, 900);
    const status = await b.eval(s, `(() => (document.getElementById('gov-status') || {}).innerText || '')()`);
    const h2 = await b.eval(s, `(() => (document.querySelector('#govcard h2') || {}).innerText || '')()`);
    if (/KelGroups|GroupState/.test(h2)) {
      console.error('RED-ROW: H-1 heading carries internal vocabulary: ' + JSON.stringify(h2));
      process.exitCode = 1; return;
    }
    const ok = EXPECT_ENUNCIATO
      ? /enunciate, non dimostrate/.test(status)
      : (/provate/.test(status) && !/enunciate/.test(status));
    if (!ok) {
      console.error('RED-ROW: H-1 sentence does not track receipt (' +
        (EXPECT_ENUNCIATO ? 'enunciato' : 'provato') + '): ' + JSON.stringify(status.slice(0, 200)));
      process.exitCode = 1; return;
    }
    console.log('GREEN: sentence-only ' + HTML + ' (' + (EXPECT_ENUNCIATO ? 'enunciato' : 'provato') + ')');
  } finally {
    b.close();
    rmQuiet(profile);
  }
}

/* --- fast modes: no journey, targeted-class --- */

async function deriveOnly() {
  const dArg = argVal('--derive-only');
  const target = (dArg && !dArg.startsWith('-')) ? dArg : HTML;
  const src = readFileSync(target, 'utf8');
  const ext = deriveExtent(src);
  console.log(`reads: ${Object.entries(ext.reads).map(([k, n]) => `${k}×${n}`).join(' ')}`);
  console.log(`required: ${ext.required.join(',')}`);
  if (ext.problems.length) {
    for (const p of ext.problems) console.error('RED-ROW: ' + p);
    console.error(`DERIVE-RED: ${ext.problems.length} problemi in ${target}`);
    process.exitCode = 1; return;
  }
  console.log(`DERIVE-GREEN: ${target} — ${ext.required.length} controlli da ${Object.keys(ext.reads).length} letture`);
}

async function vocabOnly() {
  const wantRed = argVal('--expect-red');
  const bin = findChromium();
  const profile = mkdtempSync(join(tmpdir(), 'ui-gate-vocab-'));
  const b = new Browser(bin, profile);
  try {
    const s = await b.page(pathToFileURL(HTML).href + '?ui-gate=vocab', 1280, 900);
    const copyText = await b.eval(s, `(() => {
      const c = document.body.cloneNode(true);
      c.querySelectorAll('script, noscript, style').forEach(el => el.remove());
      return c.innerText;
    })()`);
    const vc = classifyVocab(copyText);
    /* banned-words only: legacy presence needs a driven journey and is
       asserted by the full run + the page selftest, not by a boot probe */
    if (wantRed) {
      if (vc.banned.includes(wantRed)) {
        console.log(`VOCAB-RED as required: actual checking path flags «${wantRed}»`);
        return;
      }
      console.error(`RED-ROW: checking path does NOT flag «${wantRed}» (banned=[${vc.banned}])`);
      process.exitCode = 1; return;
    }
    if (vc.banned.length) {
      console.error('RED-ROW: vocab banned=[' + vc.banned.join(',') + ']');
      process.exitCode = 1; return;
    }
    console.log('VOCAB-GREEN (no banned words): ' + HTML);
  } finally {
    b.close();
    rmQuiet(profile);
  }
}

/* --- selftest: prove every required control can fail --- */

async function selftest() {
  const self = process.argv[1];
  const node = process.execPath;
  const runChild = (args, env) => new Promise(resolve => {
    const p = spawn(node, [self, ...args],
      { stdio: ['ignore', 'pipe', 'pipe'], env: { ...process.env, ...(env || {}) } });
    let out = '', err = '';
    p.stdout.on('data', d => { out += d; });
    p.stderr.on('data', d => { err += d; });
    p.on('close', code => resolve({ code, out, err }));
  });
  let bad = 0;
  const check = (name, cond, detail) => {
    console.log((cond ? 'SELFTEST-OK ' : 'SELFTEST-RED ') + name);
    if (!cond) { bad++; console.error('  ' + (detail || '')); }
  };

  const green = await runChild([]);
  check('full run GREEN', green.code === 0, green.err.slice(-2000));

  const omit = await runChild(['--omit', 'K-2']);
  check('omission --omit K-2 RED naming K-2 via ordinary reconcile',
    omit.code !== 0 && /K-2/.test(omit.out + omit.err) && /copertura incompleta/.test(omit.out + omit.err),
    'exit=' + omit.code + ' ' + (omit.out + omit.err).slice(-1500));
  const noop = await runChild(['--omit', 'K-2'], { RG_OMIT_NOOP: '1' });
  check('neutered discard fails the assurance (GREEN)', noop.code === 0,
    'exit=' + noop.code + ' ' + (noop.out + noop.err).slice(-1500));

  const scratch = mkdtempSync(join(tmpdir(), 'ui-gate-selftest-'));
  try {
    const mut = join(scratch, 'mut-coercion.html');
    copyFileSync(HTML, mut);
    let mt = readFileSync(mut, 'utf8');
    const anchor = 'const c = d.c, u = act.dataset.u;';
    if (!mt.includes(anchor)) {
      check('coercion mutant planted', false, 'ancora di mutazione assente — harness e pagina divergono');
    } else {
      mt = mt.replace(anchor, 'const c = d.c, u = Number(act.dataset.u);');
      const { writeFileSync: w } = await import('node:fs');
      w(mut, mt);
      const mr = await runChild(['--html', mut]);
      check('coercion mutant RED on interaction', mr.code !== 0 && /K-2/.test(mr.out + mr.err),
        'exit=' + mr.code + ' ' + (mr.out + mr.err).slice(-1500));
    }
    const flip = join(scratch, 'flip-enunciato.html');
    copyFileSync(HTML, flip);
    let ft = readFileSync(flip, 'utf8');
    const fanchor = "'conservation_preserved': 'provato'";
    if (!ft.includes(fanchor)) {
      check('enunciato flip planted', false, 'voce di ricevuta assente');
    } else {
      ft = ft.replace(fanchor, "'conservation_preserved': 'enunciato'");
      const { writeFileSync: w2 } = await import('node:fs');
      w2(flip, ft);
      const fr = await runChild(['--html', flip, '--expect-enunciato']);
      check('enunciato state GREEN with flipped sentence', fr.code === 0,
        'exit=' + fr.code + ' ' + (fr.out + fr.err).slice(-2000));
      const fr2 = await runChild(['--html', flip]);
      check('flipped receipt RED under provato expectation', fr2.code !== 0,
        'exit=' + fr2.code + ' la frase non segue la ricevuta');
    }
    /* D1: actual checking path REDs on ordinary banned copy */
    const ban = join(scratch, 'banned-copy.html');
    copyFileSync(HTML, ban);
    let bt = readFileSync(ban, 'utf8');
    const bins = '<div id="vocabprobe">un pledge nel copy ordinario</div>';
    if (!bt.includes('</body>')) {
      check('banned copy planted', false, 'chiusura body assente');
    } else {
      bt = bt.replace('</body>', `${bins}</body>`);
      const { writeFileSync: w3 } = await import('node:fs');
      w3(ban, bt);
      const br = await runChild(['--html', ban, '--vocab-only', '--expect-red', 'pledge']);
      check('checking path flags ordinary banned copy', br.code === 0,
        'exit=' + br.code + ' ' + (br.out + br.err).slice(-1500));
      const bv = await runChild(['--html', ban, '--vocab-only']);
      check('banned copy RED without expectation', bv.code !== 0,
        'exit=' + bv.code + ' il percorso non vede il vocabolo');
    }
    /* D3: added reachable key-bearing control absent from the table is detected */
    const add = join(scratch, 'added-control.html');
    copyFileSync(HTML, add);
    let at = readFileSync(add, 'utf8');
    const aanchor = "if (gp) { go({ view: 'person', u: gp.dataset.gotoPerson, hat: 'member' }); return; }";
    if (!at.includes(aanchor)) {
      check('added control planted', false, 'ancora di innesto assente');
    } else {
      at = at.replace(aanchor, `${aanchor}\n  const vx = t.closest('[data-goto-vip]');\n  if (vx) { go({ view: 'person', u: vx.dataset.vip, hat: 'member' }); return; }`);
      const { writeFileSync: w4 } = await import('node:fs');
      w4(add, at);
      const ar = await runChild(['--derive-only', add]);
      check('added control detected (unclassified read RED)', ar.code !== 0 && /non classificata/.test(ar.out + ar.err),
        'exit=' + ar.code + ' ' + (ar.out + ar.err).slice(-1500));
    }
    const dg = await runChild(['--derive-only']);
    check('derive-only GREEN on production page', dg.code === 0,
      'exit=' + dg.code + ' ' + (dg.out + dg.err).slice(-1500));
  } finally {
    rmQuiet(scratch);
  }
  if (bad) { console.error(`SELFTEST-RED: ${bad} controlli`); process.exitCode = 1; return; }
  console.log('SELFTEST-GREEN: full, omission both ways, mutant, both proof states, vocab path, derivation');
}

const deriveOnlyOn = hasFlag('--derive-only');
const vocabOnlyFlag = hasFlag('--vocab-only');
const mode = hasFlag('--selftest') ? 'selftest'
  : (deriveOnlyOn ? 'derive' : (vocabOnlyFlag ? 'vocab' : (SENTENCE_ONLY ? 'sentence' : 'run')));
await (mode === 'selftest' ? selftest()
  : (mode === 'derive' ? deriveOnly()
  : (mode === 'vocab' ? vocabOnly() : (mode === 'sentence' ? sentenceOnly() : run()))));