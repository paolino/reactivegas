#!/usr/bin/env node
/*
 * economics-simulator-teaching-gate.mjs — the Giuseppe teaching gate
 * (NOTE-032 + NOTE-033): proves on the REAL page, driven through its actual
 * controls in headless Chromium, that every one of the six receding teaching
 * strips (benvenuto, impegno, guardie, cassa, chiusa, voto)
 *
 *   1. carries a buyer-register surface sentence (no programmer exposition
 *      above the fold: conteggi/soglia/guardie/stato/log/evento/macchina
 *      Lean/(n+1)/2/citations), with `voto` explicitly saying Giuseppe may
 *      answer NO;
 *   2. appears exactly while its real-world condition applies — never merely
 *      because a tab or view is open;
 *   3. hides without learning when the condition ceases, and reappears when
 *      the condition returns;
 *   4. is permanently dismissed only by its explicit ✕ (surviving navigation
 *      and persisted replay), with `benvenuto` as the one documented
 *      arrival exception (view-triggered, retired on first meaningful use,
 *      proven persistent across reload);
 *   5. for `cassa`, the exact operator regression: closing a purchase leaves
 *      the relevant cassa negative AND the explanation on screen in the very
 *      same rendered result.
 *
 * The six-row lifecycle report is MECHANICAL: every row is assembled from
 * DOM snapshots the browser returned during the drive, each stamped with
 * this run's random nonce. A missing, empty, all-skipped, hand-authored, or
 * fewer-than-six report is RED (see --selftest, which also proves the
 * browser checks fail on sabotaged pages: strips suppressed, and the old
 * retire-on-close defect reintroduced).
 *
 * The gate also runs the page's own ?selftest=1 at desktop (1280) and
 * narrow (390) width, requiring SELFTEST PASS, zero console/page errors, no
 * horizontal overflow, and no non-document network request (the single-file
 * page performs zero external subresource requests).
 *
 * Chromium is located via RG_CHROMIUM, PATH, or the nix store's
 * playwright-chromium builds, and driven dependency-free over the CDP
 * --remote-debugging-pipe (JSON messages NUL-separated on fds 3/4).
 *
 * Usage:
 *   node economics-simulator-teaching-gate.mjs             # production
 *   node economics-simulator-teaching-gate.mjs --selftest  # negative controls
 */

import { spawn, execFileSync } from 'node:child_process';
import { readFileSync, writeFileSync, mkdtempSync, rmSync, existsSync,
  readdirSync } from 'node:fs';
import { createHash, randomBytes } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));
const HTML = join(REPO, 'economics-simulator.html');
const STRIPS = ['benvenuto', 'impegno', 'guardie', 'cassa', 'chiusa', 'voto'];

/* --- chromium discovery ---------------------------------------------------- */

function findChromium() {
  if (process.env.RG_CHROMIUM && existsSync(process.env.RG_CHROMIUM))
    return process.env.RG_CHROMIUM;
  for (const name of ['chromium', 'chromium-browser', 'google-chrome']) {
    try {
      const p = execFileSync('which', [name], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
      if (p) return p;
    } catch (e) { /* keep looking */ }
  }
  const store = '/nix/store';
  const hits = [];
  try {
    for (const d of readdirSync(store)) {
      if (d.includes('playwright-chromium-headless-shell'))
        hits.unshift(join(store, d, 'chrome-linux', 'headless_shell'));
      else if (d.includes('playwright-chromium') && !d.includes('patch'))
        hits.push(join(store, d, 'chrome-linux', 'chrome'));
    }
  } catch (e) { /* no nix store */ }
  for (const p of hits) if (existsSync(p)) return p;
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
    await this.send('Network.enable', {}, s);
    await this.send('Emulation.setDeviceMetricsOverride',
      { width, height, deviceScaleFactor: 1, mobile: width < 500 }, s);
    await this.send('Page.navigate', { url }, s);
    await sleep(900);
    return s;
  }
  errorsFor(sessionId) {
    return this.events.filter(e => e.sessionId === sessionId && (
      e.method === 'Runtime.exceptionThrown' ||
      (e.method === 'Runtime.consoleAPICalled' && e.params.type === 'error') ||
      (e.method === 'Log.entryAdded' && e.params.entry.level === 'error')));
  }
  requestsFor(sessionId) {
    return this.events.filter(e => e.sessionId === sessionId &&
      e.method === 'Network.requestWillBeSent').map(e => e.params.request.url);
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
  close() { try { this.child.kill(); } catch (e) { /* gone */ } }
}

const sleep = ms => new Promise(r => setTimeout(r, ms));

/* --- injected journey helpers ---------------------------------------------- */

const HELPERS = nonce => `
window.__RG_TG_NONCE = ${JSON.stringify(nonce)};
window.__rgClick = async sel => {
  const el = document.querySelector(sel);
  if (!el) throw new Error('controllo mancante: ' + sel);
  const r = el.getBoundingClientRect();
  el.dispatchEvent(new MouseEvent('click', { bubbles: true,
    clientX: r.x + r.width / 2, clientY: r.y + r.height / 2 }));
  await new Promise(x => setTimeout(x, 90));
};
window.__rgAmount = async v => {
  const n = document.getElementById('am-n');
  if (!n) throw new Error('controllo importo assente');
  n.value = String(v);
  n.dispatchEvent(new Event('input', { bubbles: true }));
  await __rgClick('#am-ok');
};
window.__rgSnap = phase => ({
  nonce: window.__RG_TG_NONCE, phase,
  strips: Object.fromEntries(${JSON.stringify(STRIPS)}.map(id =>
    [id, !!document.querySelector('[data-teach="' + id + '"]')])),
  texts: Object.fromEntries([...document.querySelectorAll('[data-teach]')].map(el =>
    [el.dataset.teach, el.textContent.replace(/\\s+/g, ' ').trim()])),
  casseNeg: window.RG.state.casse.filter(([, v]) => v < 0),
  openQuestions: window.RG.kg.openQuestions.length,
  view: location.hash || 'n/a',
});
window.__rgDismiss = async id => {
  const x = document.querySelector('[data-teachx="' + id + '"]');
  if (!x) return false;
  const r = x.getBoundingClientRect();
  x.dispatchEvent(new MouseEvent('click', { bubbles: true,
    clientX: r.x + 1, clientY: r.y + 1 }));
  await new Promise(z => setTimeout(z, 90));
  return true;
};
'helpers-ok';
`;

/* one journey step: run body, then snapshot under `phase` */
async function step(b, s, snaps, phase, body) {
  const v = await b.eval(s, `(async () => { ${body}; return __rgSnap(${JSON.stringify(phase)}); })()`);
  snaps.push(v);
  return v;
}

/* --- the real-control journey ---------------------------------------------- */

async function runJourney(b, url, nonce) {
  const s = await b.page(url, 1280, 900);
  await b.eval(s, `localStorage.clear(); 'cleared'`);
  await b.send('Page.navigate', { url }, s);
  await sleep(900);
  await b.eval(s, HELPERS(nonce));
  const snaps = [];

  // ------ benvenuto: the documented arrival exception ----------------------
  await step(b, s, snaps, 'benvenuto.appear', ``);
  await step(b, s, snaps, 'benvenuto.firstUse', `
    await __rgClick('[data-key="ghost:1"]');
    await __rgClick('#pop .chip[data-id="0"]');`);           // Bruno entra
  // persistence across a real reload (arrival again, already used)
  await b.send('Page.navigate', { url }, s);
  await sleep(900);
  await b.eval(s, HELPERS(nonce));
  await step(b, s, snaps, 'benvenuto.reloadPersist', ``);

  // ------ shared setup: elect Bruno, fund, open Olio -----------------------
  await b.eval(s, `(async () => {
    await __rgClick('#gtasks [data-task="electResponsabile"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgClick('[data-key="member:0"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="deposit"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgAmount(100);
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Olio"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('#govcard [data-kgadmit]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgClick('#govcard [data-kgadmit]');
    await __rgClick('#pop .chip[data-id="1"]');
    return 'setup-ok';
  })()`);

  // ------ voto: while a real question is open ------------------------------
  await step(b, s, snaps, 'voto.appear', `
    await __rgClick('[data-key="pile:1"]');
    await __rgClick('#view [data-kgpropose]');
    await __rgClick('#pop .chip[data-id="anna"]');`);
  await step(b, s, snaps, 'voto.leave', `
    await __rgClick('#view [data-kgcast="permesso:1"][data-kgsigner="anna"][data-kgballot="assent"]');`);
  await step(b, s, snaps, 'voto.recreate', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Vino"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:2"]');
    await __rgClick('#view [data-kgpropose]');
    await __rgClick('#pop .chip[data-id="anna"]');`);
  const votoDis = await b.eval(s, `__rgDismiss('voto')`);
  await step(b, s, snaps, 'voto.dismiss', ``);
  await step(b, s, snaps, 'voto.recreateAfterDismiss', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Farina"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:3"]');
    await __rgClick('#view [data-kgpropose]');
    await __rgClick('#pop .chip[data-id="anna"]');`);

  // ------ impegno: while money is actually held ----------------------------
  await step(b, s, snaps, 'impegno.appear', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:1"]');
    await __rgClick('#view [data-task="pledge"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgAmount(40);`);
  await step(b, s, snaps, 'impegno.leave', `
    await __rgClick('#refpanel [data-act="refuse"][data-u="1"]');`);
  await step(b, s, snaps, 'impegno.recreate', `
    await __rgClick('#view [data-task="pledge"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgAmount(40);`);
  const impegnoDis = await b.eval(s, `__rgDismiss('impegno')`);
  await step(b, s, snaps, 'impegno.dismiss', ``);
  await step(b, s, snaps, 'impegno.recreateAfterDismiss', `
    await __rgClick('#refpanel [data-act="refuse"][data-u="1"]');
    await __rgClick('#view [data-task="pledge"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgAmount(40);
    await __rgClick('#refpanel [data-act="accept"][data-u="1"]');`);

  // ------ guardie: after meeting a real refusal ----------------------------
  // the page's affordances refuse BEFORE the machine: a spent (off) task is
  // the refusal surface Giuseppe meets; clicking it is the encounter
  await step(b, s, snaps, 'guardie.appear', `
    await __rgClick('#refpanel [data-task="closePurchase"]');
    const pop = document.getElementById('pop');
    if (pop && !pop.hidden) { const x = pop.querySelector('.xbtn2'); if (x) { x.click(); await new Promise(z => setTimeout(z, 60)); } }`);
  await step(b, s, snaps, 'guardie.leave', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:0"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="deposit"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgAmount(5);
    await __rgClick('#crumbs [data-crumb="0"]');`);
  await step(b, s, snaps, 'guardie.recreate', `
    await __rgClick('[data-key="pile:1"]');
    await __rgClick('#refpanel [data-task="closePurchase"]');
    const pop = document.getElementById('pop');
    if (pop && !pop.hidden) { const x = pop.querySelector('.xbtn2'); if (x) { x.click(); await new Promise(z => setTimeout(z, 60)); } }`);
  const guardieDis = await b.eval(s, `__rgDismiss('guardie')`);
  await step(b, s, snaps, 'guardie.dismiss', ``);
  await step(b, s, snaps, 'guardie.recreateAfterDismiss', `
    await __rgClick('#refpanel [data-task="closePurchase"]');
    const pop = document.getElementById('pop');
    if (pop && !pop.hidden) { const x = pop.querySelector('.xbtn2'); if (x) { x.click(); await new Promise(z => setTimeout(z, 60)); } }`);

  // ------ cassa: the exact operator regression -----------------------------
  // grant from the positive verdict, close: cassa Bruno goes negative and
  // the explanation must be on screen IN THE SAME RENDERED RESULT
  await step(b, s, snaps, 'cassa.appear', `
    await __rgClick('#view [data-task="grantPermission"]');
    await __rgClick('#ext-apply');
    await __rgClick('#refpanel [data-task="closePurchase"]');`);
  await step(b, s, snaps, 'cassa.leave', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:1"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="transferCassa"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgAmount(50);`);
  await step(b, s, snaps, 'cassa.recreate', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:0"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="transferCassa"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgAmount(50);`);
  const cassaDis = await b.eval(s, `__rgDismiss('cassa')`);
  await step(b, s, snaps, 'cassa.dismiss', ``);
  await step(b, s, snaps, 'cassa.recreateAfterDismiss', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:1"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="transferCassa"]');
    await __rgClick('#pop .chip[data-id="0"]');
    await __rgAmount(50);
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:0"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="transferCassa"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgAmount(50);`);

  // ------ chiusa: in the post-close context --------------------------------
  await step(b, s, snaps, 'chiusa.appear', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Caffè"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:4"]');
    await __rgClick('#refpanel [data-task="failPurchase"]');`);
  await step(b, s, snaps, 'chiusa.leave', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="member:0"]');
    await __rgClick('[data-hat="cassiere"]');
    await __rgClick('#hat-cassiere [data-task="deposit"]');
    await __rgClick('#pop .chip[data-id="1"]');
    await __rgAmount(5);
    await __rgClick('#crumbs [data-crumb="0"]');`);
  await step(b, s, snaps, 'chiusa.recreate', `
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Miele"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:5"]');
    await __rgClick('#refpanel [data-task="failPurchase"]');`);
  const chiusaDis = await b.eval(s, `__rgDismiss('chiusa')`);
  await step(b, s, snaps, 'chiusa.dismiss', ``);
  await step(b, s, snaps, 'chiusa.recreateAfterDismiss', `
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="cassa:1"]');
    await __rgClick('#hat-cassiere [data-task="openPurchase"]');
    await __rgClick('#pop [data-l="Pasta"]');
    await __rgClick('#crumbs [data-crumb="0"]');
    await __rgClick('[data-key="pile:6"]');
    await __rgClick('#refpanel [data-task="failPurchase"]');`);

  const dismissed = { voto: votoDis, impegno: impegnoDis, guardie: guardieDis,
    cassa: cassaDis, chiusa: chiusaDis };
  return { session: s, snaps, dismissed,
    errors: b.errorsFor(s), requests: b.requestsFor(s) };
}

/* --- report assembly + validation ------------------------------------------ */

const PHASES = ['appear', 'leave', 'recreate', 'dismiss', 'recreateAfterDismiss'];
const EXPECT = { appear: true, leave: false, recreate: true,
  dismiss: false, recreateAfterDismiss: false };

function buildReport(run, nonce) {
  const byPhase = {};
  for (const sn of run.snaps) byPhase[sn.phase] = sn;
  const rows = {};
  for (const id of STRIPS) {
    if (id === 'benvenuto') {
      rows[id] = { exception: 'arrivo: mostrata all\'arrivo, ritirata al primo uso reale, persistente oltre il reload',
        phases: ['appear', 'firstUse', 'reloadPersist'].map(p => {
          const sn = byPhase['benvenuto.' + p];
          return sn && { phase: p, nonce: sn.nonce, visible: sn.strips.benvenuto,
            expected: p === 'appear' };
        }) };
      continue;
    }
    rows[id] = { dismissedClicked: run.dismissed[id] === true,
      phases: PHASES.map(p => {
        const sn = byPhase[id + '.' + p];
        return sn && { phase: p, nonce: sn.nonce, visible: sn.strips[id],
          expected: EXPECT[p], text: sn.texts[id] || null,
          casseNeg: id === 'cassa' ? sn.casseNeg : undefined };
      }) };
  }
  return { nonce, rows, errors: run.errors.length, requests: run.requests };
}

/* Buyer-register scan for the six surface sentences (NOTE-032): programmer
   exposition may not appear above the fold. */
const FORBIDDEN_SURFACE = [/contegg/i, /soglia/i, /guardi[ae]/i, /\bstato\b/i,
  /\blog\b/i, /\bevent[oi]\b/i, /macchina lean/i, /\(n\+1\)\/2/, /\.lean\b/,
  /teorema/i, /dichiarazion/i, /invariant/i];

export function validateReport(rep, nonce) {
  const bad = [];
  if (!rep || !rep.rows) return ['report assente o vuoto'];
  const ids = Object.keys(rep.rows);
  if (ids.length < 6) bad.push(`righe insufficienti: ${ids.length} < 6`);
  for (const id of STRIPS) {
    const row = rep.rows[id];
    if (!row) { bad.push('riga mancante: ' + id); continue; }
    const phases = (row.phases || []).filter(Boolean);
    const want = id === 'benvenuto' ? 3 : 5;
    if (phases.length < want) { bad.push(`${id}: fasi osservate ${phases.length} < ${want} (riga saltata o parziale)`); continue; }
    for (const ph of phases) {
      if (ph.nonce !== nonce)
        bad.push(`${id}.${ph.phase}: nonce non del run (report non meccanico)`);
      if (ph.visible !== ph.expected)
        bad.push(`${id}.${ph.phase}: attesa visibile=${ph.expected}, osservata=${ph.visible}`);
    }
    if (id !== 'benvenuto' && row.dismissedClicked !== true)
      bad.push(`${id}: la ✕ non era cliccabile quando attesa (striscia assente al momento del congedo)`);
    const shown = phases.find(p => p.visible && p.text);
    if (id !== 'benvenuto') {
      if (!shown) bad.push(`${id}: nessun testo di superficie osservato`);
      else {
        for (const re of FORBIDDEN_SURFACE)
          if (re.test(shown.text.replace(/⊢.*$/, '')))
            bad.push(`${id}: registro sbagliato in superficie (${re}): «${shown.text.slice(0, 80)}»`);
        if (id === 'voto' && !/\bno\b/i.test(shown.text))
          bad.push('voto: la frase non dice che si può rispondere NO');
      }
    }
  }
  // the exact operator regression: negative cassa AND visible strip together
  const ca = rep.rows.cassa && (rep.rows.cassa.phases || []).filter(Boolean)
    .find(p => p.phase === 'appear');
  if (!ca) bad.push('regressione cassa: fase mancante');
  else {
    if (!ca.casseNeg || !ca.casseNeg.length)
      bad.push('regressione cassa: nessuna cassa negativa dopo la chiusura (flusso non riuscito)');
    if (ca.visible !== true)
      bad.push('regressione cassa: cassa negativa senza spiegazione a schermo nello stesso render');
  }
  if (rep.errors > 0) bad.push(`errori console/pagina durante il giro: ${rep.errors}`);
  const external = (rep.requests || []).filter(u => !u.startsWith('file://'));
  if (external.length) bad.push('richieste non-documento: ' + external.join(', '));
  return bad;
}

/* --- the page's own selftest at a given width ------------------------------ */

async function runPageSelftest(b, url, width) {
  const s = await b.page(url + '?selftest=1', width, 900);
  const title = await b.eval(s, `new Promise(res => {
    const t0 = Date.now();
    const iv = setInterval(() => {
      if (document.title.startsWith('SELFTEST') || Date.now() - t0 > 90000) {
        clearInterval(iv); res(document.title);
      }
    }, 250);
  })`);
  const overflow = await b.eval(s,
    `document.documentElement.scrollWidth > document.documentElement.clientWidth`);
  const errors = b.errorsFor(s).length;
  const external = b.requestsFor(s).filter(u => !u.startsWith('file://'));
  return { width, title, overflow, errors, external };
}

/* --- one full production evaluation ---------------------------------------- */

async function runGate(opts) {
  const htmlPath = opts.html || HTML;
  const nonce = randomBytes(12).toString('hex');
  const profile = mkdtempSync(join(tmpdir(), 'rg-teach-profile-'));
  const b = new Browser(findChromium(), profile);
  try {
    await sleep(400);
    const url = 'file://' + htmlPath;
    const run = await runJourney(b, url, nonce);
    const rep = buildReport(run, nonce);
    const reasons = validateReport(rep, nonce);
    let selftests = [];
    if (!opts.skipWidths && !reasons.length) {
      for (const w of [1280, 390]) {
        const r = await runPageSelftest(b, url, w);
        selftests.push(r);
        if (r.title !== 'SELFTEST PASS — Reactivegas')
          reasons.push(`selftest della pagina a ${w}px: ${r.title}`);
        if (r.overflow) reasons.push(`overflow orizzontale a ${w}px`);
        if (r.errors) reasons.push(`errori console a ${w}px: ${r.errors}`);
        if (r.external.length) reasons.push(`richieste esterne a ${w}px`);
      }
    }
    return { ok: !reasons.length, reasons, report: rep, selftests, nonce };
  } finally {
    b.close();
    rmSync(profile, { recursive: true, force: true });
  }
}

function printRows(rep) {
  for (const id of STRIPS) {
    const row = rep.rows[id];
    if (!row) { console.log(`  ${id}: RIGA MANCANTE`); continue; }
    const seq = (row.phases || []).filter(Boolean)
      .map(p => `${p.phase}=${p.visible ? 'V' : '·'}`).join(' ');
    const shown = (row.phases || []).filter(Boolean).find(p => p.visible && p.text);
    console.log(`  ${id}: ${seq}${row.exception ? ' (eccezione arrivo)' : ''}`);
    if (shown) console.log(`    «${shown.text.slice(0, 140)}»`);
  }
}

/* --- selftest --------------------------------------------------------------- */

async function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');
  // 1) report-validation controls: fabricated/partial/hand-authored reports
  const nonce = 'deadbeefdeadbeefdeadbeef';
  const goodPhase = (id, p) => ({ phase: p, nonce, visible: EXPECT[p], text:
    id === 'voto' ? 'puoi dire no' : 'frase' });
  const goodRow = id => id === 'benvenuto'
    ? { exception: 'x', phases: [
        { phase: 'appear', nonce, visible: true, expected: true },
        { phase: 'firstUse', nonce, visible: false, expected: false },
        { phase: 'reloadPersist', nonce, visible: false, expected: false }] }
    : { dismissedClicked: true, phases: PHASES.map(p => ({ ...goodPhase(id, p),
        expected: EXPECT[p],
        casseNeg: id === 'cassa' && p === 'appear' ? [[1, -40]] : undefined })) };
  const goodRep = () => ({ nonce, errors: 0, requests: [],
    rows: Object.fromEntries(STRIPS.map(id => [id, goodRow(id)])) });
  const fabControls = [
    ['report vuoto', {}, /report assente|righe insufficienti|riga mancante/],
    ['cinque righe', (() => { const r = goodRep(); delete r.rows.voto; return r; })(),
      /riga mancante: voto/],
    ['riga saltata (fasi parziali)', (() => { const r = goodRep();
      r.rows.cassa.phases = r.rows.cassa.phases.slice(0, 2); return r; })(),
      /cassa: fasi osservate 2 < 5/],
    ['report scritto a mano (nonce estraneo)', (() => { const r = goodRep();
      r.rows.voto.phases[0].nonce = 'altro'; return r; })(),
      /nonce non del run/],
    ['regressione cassa senza spiegazione', (() => { const r = goodRep();
      r.rows.cassa.phases[0].visible = false; return r; })(),
      /senza spiegazione a schermo|attesa visibile=true/],
  ];
  for (const [name, rep, expect] of fabControls) {
    const bad = validateReport(rep, nonce);
    if (!bad.length) {
      console.error(`SELFTEST RED: controllo «${name}» ACCETTATO`);
      return 1;
    }
    if (!expect.test(bad.join('\n'))) {
      console.error(`SELFTEST RED: «${name}» respinto per il motivo sbagliato: ${bad.join('; ').slice(0, 200)}`);
      return 1;
    }
    console.log(`controllo negativo «${name}»: RED come atteso — ${bad[0].slice(0, 100)}`);
  }
  // 2) sabotage browser controls on scratch copies of the real page
  const sabotages = [
    {
      name: 'strisce soppresse nella pagina',
      mutate: d => d.replace('${teachBarHtml()}', ''),
      expect: /attesa visibile=true, osservata=false|nessun testo di superficie/,
    },
    {
      name: 'ritiro-alla-causa reintrodotto (difetto NOTE-033)',
      mutate: d => d.replace("if (tag === 'closePurchase') { teachLearn('cassa'); }",
        '').replace('    if (tag === \'pledge\') teachLearn(\'impegno\');', '')
        .replace('  if (res.ok) {\n    recordApplied(tag, args, res);',
          "  if (res.ok) {\n    recordApplied(tag, args, res);\n    if (tag === 'closePurchase') teachDismiss('cassa');"),
      expect: /cassa negativa senza spiegazione|cassa\.appear/,
    },
  ];
  for (const c of sabotages) {
    const mutated = c.mutate(doc);
    if (mutated === doc) {
      console.error(`SELFTEST RED: controllo «${c.name}» non ha mutato la pagina`);
      return 1;
    }
    const p = join(work, 'sab-' + c.name.replace(/[^a-z]+/gi, '-') + '.html');
    writeFileSync(p, mutated);
    const r = await runGate({ html: p, skipWidths: true });
    if (r.ok) {
      console.error(`SELFTEST RED: controllo «${c.name}» ACCETTATO dal gate`);
      return 1;
    }
    if (!c.expect.test(r.reasons.join('\n'))) {
      console.error(`SELFTEST RED: «${c.name}» RED per il motivo sbagliato:\n` +
        r.reasons.join('\n').slice(0, 300));
      return 1;
    }
    console.log(`controllo negativo «${c.name}»: RED come atteso — ${r.reasons[0].slice(0, 110)}`);
  }
  const green = await runGate({});
  if (!green.ok) {
    console.error('SELFTEST RED: la produzione non torna GREEN:\n' + green.reasons.join('\n'));
    return 1;
  }
  console.log(`selftest GREEN: ${fabControls.length + sabotages.length} controlli negativi RED per il motivo atteso; produzione GREEN`);
  return 0;
}

/* --- CLI -------------------------------------------------------------------- */

const isMain = process.argv[1] &&
  fileURLToPath(import.meta.url) === process.argv[1];
if (isMain) {
  const work = mkdtempSync(join(tmpdir(), 'rg-teach-gate-'));
  let code = 1;
  try {
    if (process.argv.includes('--selftest')) {
      code = await selftest(work);
    } else {
      const r = await runGate({});
      if (r.ok) {
        console.log(`GREEN: 6/6 strisce con ciclo di vita provato sui controlli reali ` +
          `(nonce ${r.nonce.slice(0, 8)}…); regressione cassa coperta; ` +
          `selftest pagina PASS a ${r.selftests.map(x => x.width + 'px').join(' e ')}; ` +
          `zero errori, zero richieste esterne`);
        printRows(r.report);
        code = 0;
      } else {
        console.error(`RED: ${r.reasons.length} problemi`);
        r.reasons.forEach(x => console.error(' - ' + x));
        printRows(r.report);
        code = 1;
      }
    }
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
  process.exit(code);
}
