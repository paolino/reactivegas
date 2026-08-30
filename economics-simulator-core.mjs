/*
 * economics-simulator-core.mjs — THE machine core of the Reactivegas
 * simulator: the single source of the three Lean-machine transcriptions
 * (economic Step.lean, KelGroups base proposals, KelGroups.Vote), the
 * frozen trace verifiers and Lean-emitted fixtures, the claim manifest and
 * its receipt, and the composition routing/governance walk of the accepted
 * pin.
 *
 * Consumed by BOTH production surfaces, with no second transcription:
 *   - economics-simulator.html embeds these exact slices between
 *     @@CORE:<id>@@ markers; economics-simulator-build.mjs regenerates the
 *     page from this file, and the scenario gate REDs when the embedded
 *     copy is stale or forked (byte compare per slice);
 *   - economics-simulator-scenario-gate.mjs imports this module directly.
 *
 * The boundary is a pure functional surface (state in → result out, no DOM,
 * no storage, no clock): a future core.wasm implementing the same exported
 * contract can replace this file without changing the page or the runner.
 * The only host-provided hooks are the toy naming pools defined here.
 */

/* @@CORE:names@@ */
const FOUNDER = 0;
const NAMES = ['Anna', 'Bruno', 'Elena', 'Carlo', 'Dora', 'Enzo',
               'Febe', 'Gaia', 'Hugo', 'Irma', 'Luca', 'Mara'];
const nm = u => NAMES[u] !== undefined ? NAMES[u] : '?';
const PRESETS = ['Olio', 'Vino', 'Farina', 'Caffè', 'Miele', 'Pasta', 'Riso', 'Sale'];
let colLabels = {};                       // CollId -> toy label
const lbl = c => colLabels[c] !== undefined ? colLabels[c] : '?';

/* @@CORE:names:END@@ */

/* @@CORE:machine@@ */
/* --- State.lean helpers ------------------------------------------------ */

// State.init r := ⟨[r], [r], [], [], []⟩
function initState(r) {
  return { users: [r], responsabili: [r], conti: [], casse: [], collections: [] };
}

// bal: first matching entry, absent means zero
function bal(m, u) {
  for (const [k, v] of m) if (k === u) return v;
  return 0;
}

// bump: add d to the first entry of u, appending a fresh entry when absent
function bump(m, u, d) {
  const out = [];
  let done = false;
  for (const [k, v] of m) {
    if (!done && k === u) { out.push([k, v + d]); done = true; }
    else out.push([k, v]);
  }
  if (!done) out.push([u, d]);
  return out;
}

function sumBal(m) { return m.reduce((a, [, v]) => a + v, 0); }
function sumPledges(l) { return l.reduce((a, p) => a + p.amount, 0); }
function escrowOf(c) { return sumPledges(c.accepted) + sumPledges(c.pending); }
function escrowSum(cols) { return cols.reduce((a, c) => a + escrowOf(c), 0); }

// splitUser: first pledge of u, and the list without it; null when absent
function splitUser(u, l) {
  const i = l.findIndex(p => p.user === u);
  if (i < 0) return null;
  return [l[i].amount, l.slice(0, i).concat(l.slice(i + 1))];
}

// refundAll: foldl bump
function refundAll(m, l) {
  return l.reduce((acc, p) => bump(acc, p.user, p.amount), m);
}

// pullCollection: the collection with id c, and the list without it
function pullCollection(c, cols) {
  const i = cols.findIndex(x => x.id === c);
  if (i < 0) return null;
  return [cols[i], cols.slice(0, i).concat(cols.slice(i + 1))];
}

// stripCollections: remove collections whose referente is r; collect their
// pledges (accepted ++ pending, in list order) for refunding
function stripCollections(r, cols) {
  const rest = [], ps = [];
  for (const c of cols) {
    if (c.referente === r) { ps.push(...c.accepted, ...c.pending); }
    else rest.push(c);
  }
  return [rest, ps];
}

function isResponsabile(s, u) { return s.responsabili.includes(u); }

/* Reserved non-member comune account inside conti (issue #48). Lean's
   comuneId is 0, which is this toy's FOUNDER, so the executable key is
   a sentinel that cannot collide with a member identity. It is never a
   standalone State field. */
const COMUNE_ID = -1;

/* --- Guard labels (canonical Lean code + toy-side why text) ------------ */

const g = (code, why, law) => ({ code, why, law: law || null });
const AUTH = a => g('isResponsabile s a', `${nm(a)} non è responsabile`, 'AUTH');
const PULL = c => g('pullCollection c = none', `nessun acquisto aperto «${lbl(c)}»`);

/* --- Step.lean: the rejecting transition, with named guards ------------
   attempt(s,e) → {ok:true, state, flow} | {ok:false, failed:[guard]}
   The accept/reject decision is exactly the Lean conjunction; `failed`
   lists every failing guard that is evaluable (guards depending on a
   failed pullCollection/splitUser cannot be evaluated and are skipped). */

function attempt(s, e) {
  const fails = [];
  const need = (cond, guard) => { if (!cond) fails.push(guard); return cond; };
  const rej = () => ({ ok: false, failed: fails });

  switch (e.tag) {

    case 'addUser': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(!s.users.includes(e.target),
        g('!s.users.contains u', `${nm(e.target)} è già nel gruppo`));
      if (fails.length) return rej();
      return { ok: true, flow: `${nm(e.target)} entra nel gruppo`,
        state: { ...s, users: [...s.users, e.target] } };
    }

    case 'electResponsabile': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(s.users.includes(e.target),
        g('s.users.contains u', `${nm(e.target)} non è nel gruppo`));
      need(!isResponsabile(s, e.target),
        g('!isResponsabile s u', `${nm(e.target)} è già responsabile`));
      if (fails.length) return rej();
      return { ok: true, flow: `★ ${nm(e.target)} è responsabile`,
        state: { ...s, responsabili: [e.target, ...s.responsabili] } };
    }

    case 'removeResponsabile': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(isResponsabile(s, e.target),
        g('isResponsabile s u', `${nm(e.target)} non è responsabile`));
      if (fails.length) return rej();
      const [rest, ps] = stripCollections(e.target, s.collections);
      const i = s.responsabili.indexOf(e.target);
      const resps = s.responsabili.slice(0, i).concat(s.responsabili.slice(i + 1));
      const flow = ps.length
        ? `★✕ ${nm(e.target)} · suoi acquisti chiusi, rimborsi: ` +
          ps.map(p => `conto ${nm(p.user)} +${p.amount}`).join(', ')
        : `★✕ ${nm(e.target)} · nessun acquisto da chiudere`;
      return { ok: true, flow,
        state: { ...s, responsabili: resps, conti: refundAll(s.conti, ps), collections: rest } };
    }

    case 'openPurchase': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(!s.collections.some(x => x.id === e.c),
        g('!(s.collections.any (x.id == c))', `acquisto «${lbl(e.c)}» già aperto`));
      if (fails.length) return rej();
      return { ok: true,
        flow: `apre l'acquisto «${lbl(e.c)}» — referente ${nm(e.author)}, permesso: no, impegni 0`,
        state: { ...s, collections:
          [{ id: e.c, referente: e.author, permitted: false, accepted: [], pending: [] },
           ...s.collections] } };
    }

    case 'grantPermission': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      return { ok: true, flow: `«${lbl(e.c)}»: assenso del gruppo — permesso di chiusura concesso (L2 ✓)`,
        state: { ...s, collections: [{ ...col, permitted: true }, ...rest] } };
    }

    case 'denyPermission': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const ps = [...col.accepted, ...col.pending];
      return { ok: true,
        flow: `acquisto «${lbl(e.c)}» negato e chiuso · rimborsi: ` +
          (ps.length ? ps.map(p => `conto ${nm(p.user)} +${p.amount}`).join(', ') : 'nessuno'),
        state: { ...s, conti: refundAll(s.conti, ps), collections: rest } };
    }

    case 'deposit': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(s.users.includes(e.user),
        g('s.users.contains u', `${nm(e.user)} non è nel gruppo`));
      need(e.author !== e.user,
        g('a != u', 'autore e utente devono essere diversi'));
      need(0 <= e.v, g('0 ≤ v', `importo negativo (${e.v})`));
      if (fails.length) return rej();
      return { ok: true, flow: `accredito: conto ${nm(e.user)} +${e.v} · cassa ${nm(e.author)} +${e.v}`,
        state: { ...s, conti: bump(s.conti, e.user, e.v), casse: bump(s.casse, e.author, e.v) } };
    }

    case 'withdraw': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(s.users.includes(e.user),
        g('s.users.contains u', `${nm(e.user)} non è nel gruppo`));
      need(e.author !== e.user,
        g('a != u', 'autore e utente devono essere diversi'));
      need(bal(s.conti, e.user) >= e.v,
        g('bal s.conti u ≥ v',
          `credito insufficiente: conto ${nm(e.user)} = ${bal(s.conti, e.user)} < ${e.v}`,
          'COVERED (L7)'));
      if (fails.length) return rej();
      return { ok: true, flow: `prelievo: conto ${nm(e.user)} −${e.v} · cassa ${nm(e.author)} −${e.v}`,
        state: { ...s, conti: bump(s.conti, e.user, -e.v), casse: bump(s.casse, e.author, -e.v) } };
    }

    case 'transferCassa': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(isResponsabile(s, e.from_),
        g('isResponsabile s f', `${nm(e.from_)} non è responsabile`, 'AUTH'));
      need(e.author !== e.from_,
        g('a != f', 'autore e mittente devono essere diversi'));
      need(e.v > 0, g('v > 0', `l'importo deve essere positivo (${e.v})`));
      if (fails.length) return rej();
      return { ok: true, flow: `cassa ${nm(e.from_)} −${e.v} → cassa ${nm(e.author)} +${e.v}`,
        state: { ...s, casse: bump(bump(s.casse, e.from_, -e.v), e.author, e.v) } };
    }

    case 'pledge': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(s.users.includes(e.user),
        g('s.users.contains u', `${nm(e.user)} non è nel gruppo`));
      if (pulled) {
        const [col] = pulled;
        need(!col.accepted.some(p => p.user === e.user),
          g('!(col.accepted.any (p.user == u))',
            `${nm(e.user)} ha già un impegno accettato in «${lbl(e.c)}»`, 'L8'));
        need(!col.pending.some(p => p.user === e.user),
          g('!(col.pending.any (p.user == u))',
            `${nm(e.user)} ha già un impegno pendente in «${lbl(e.c)}»`, 'L8'));
      }
      need(0 < e.v, g('0 < v', `l'impegno deve essere positivo (${e.v})`, 'COVERED (L7)'));
      need(bal(s.conti, e.user) >= e.v,
        g('bal s.conti u ≥ v',
          `credito insufficiente: conto ${nm(e.user)} = ${bal(s.conti, e.user)} < ${e.v}`,
          'COVERED (L7)'));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      return { ok: true,
        flow: `impegno: conto ${nm(e.user)} −${e.v} → impegni di «${lbl(e.c)}» (pendente)`,
        state: { ...s, conti: bump(s.conti, e.user, -e.v),
          collections: [{ ...col, pending: [{ user: e.user, amount: e.v }, ...col.pending] },
                        ...rest] } };
    }

    case 'acceptPledge': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      let split = null;
      if (pulled) {
        split = splitUser(e.user, pulled[0].pending);
        need(split !== null,
          g('splitUser u col.pending = none',
            `${nm(e.user)} non ha un impegno pendente in «${lbl(e.c)}»`));
        need(pulled[0].referente === e.author,
          g('col.referente == a',
            `solo il referente (${nm(pulled[0].referente)}) può accettare`, 'REFERENTE'));
      }
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const [v, pend2] = split;
      return { ok: true,
        flow: `«${lbl(e.c)}»: impegno di ${nm(e.user)} (${v}) pendente → accettato`,
        state: { ...s, collections:
          [{ ...col, pending: pend2, accepted: [{ user: e.user, amount: v }, ...col.accepted] },
           ...rest] } };
    }

    case 'refusePledge': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      let split = null;
      if (pulled) {
        split = splitUser(e.user, pulled[0].pending);
        need(split !== null,
          g('splitUser u col.pending = none',
            `${nm(e.user)} non ha un impegno pendente in «${lbl(e.c)}»`));
        need(pulled[0].referente === e.author,
          g('col.referente == a',
            `solo il referente (${nm(pulled[0].referente)}) può rifiutare`, 'REFERENTE'));
      }
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const [v, pend2] = split;
      return { ok: true,
        flow: `rimborso: impegni di «${lbl(e.c)}» −${v} → conto ${nm(e.user)} +${v}`,
        state: { ...s, conti: bump(s.conti, e.user, v),
          collections: [{ ...col, pending: pend2 }, ...rest] } };
    }

    case 'correctPledge': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      let split = null;
      if (pulled) {
        split = splitUser(e.user, pulled[0].accepted);
        need(split !== null,
          g('splitUser u col.accepted = none',
            `${nm(e.user)} non ha un impegno accettato in «${lbl(e.c)}»`));
        need(pulled[0].referente === e.author,
          g('col.referente == a',
            `solo il referente (${nm(pulled[0].referente)}) può correggere`, 'REFERENTE'));
      }
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(0 <= e.v, g("0 ≤ v'", `il nuovo importo non può essere negativo (${e.v})`, 'COVERED (L7)'));
      if (split) {
        const v = split[0];
        need(bal(s.conti, e.user) + (v - e.v) >= 0,
          g("bal s.conti u + (v − v') ≥ 0",
            `il conguaglio manderebbe conto ${nm(e.user)} a ${bal(s.conti, e.user) + (v - e.v)} < 0`,
            'COVERED (L7)'));
      }
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const [v, acc2] = split;
      const d = v - e.v;
      return { ok: true,
        flow: `impegno di ${nm(e.user)}: ${v} → ${e.v} · conto ${nm(e.user)} ` +
          (d >= 0 ? `+${d}` : `${d}`),
        state: { ...s, conti: bump(s.conti, e.user, d),
          collections: [{ ...col, accepted: [{ user: e.user, amount: e.v }, ...acc2] },
                        ...rest] } };
    }

    case 'closePurchase': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (pulled) {
        const [col] = pulled;
        need(col.referente === e.author,
          g('col.referente == a',
            `solo il referente (${nm(col.referente)}) può chiudere`, 'REFERENTE'));
        need(col.permitted,
          g('col.permitted', 'il gruppo non ha concesso il permesso di chiusura', 'PERMITTED (L2)'));
        need(col.pending.length === 0,
          g('col.pending.isEmpty',
            `restano ${col.pending.length} impegni pendenti`, 'NO-PENDING (L4)'));
      }
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const tot = sumPledges(col.accepted);
      return { ok: true,
        flow: `acquisto «${lbl(e.c)}» chiuso: cassa ${nm(col.referente)} −${tot} · impegni −${tot} (spesi)`,
        state: { ...s, casse: bump(s.casse, col.referente, -tot), collections: rest } };
    }

    case 'failPurchase': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(s, e.author), AUTH(e.author));
      if (pulled) {
        const [col] = pulled;
        need(col.referente === e.author,
          g('col.referente == a',
            `solo il referente (${nm(col.referente)}) può farla fallire`, 'REFERENTE'));
        need(col.pending.length === 0,
          g('col.pending.isEmpty',
            `restano ${col.pending.length} impegni pendenti`, 'NO-PENDING (L4)'));
      }
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const ps = [...col.accepted, ...col.pending];
      return { ok: true,
        flow: `acquisto «${lbl(e.c)}» fallito · rimborsi: ` +
          (ps.length ? ps.map(p => `conto ${nm(p.user)} +${p.amount}`).join(', ') : 'nessuno'),
        state: { ...s, conti: refundAll(s.conti, ps), collections: rest } };
    }

    case 'donate': {
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(0 < e.v, g('0 < v', `l'importo deve essere positivo (${e.v})`));
      if (fails.length) return rej();
      return { ok: true,
        flow: `donazione: cassa ${nm(e.author)} +${e.v} · comune +${e.v}`,
        state: { ...s, casse: bump(s.casse, e.author, e.v),
          conti: bump(s.conti, COMUNE_ID, e.v) } };
    }

    case 'backdonate': {
      const n = s.users.length;
      need(isResponsabile(s, e.author), AUTH(e.author));
      need(0 < e.w, g('0 < w', `la quota deve essere positiva (${e.w})`));
      need(bal(s.conti, COMUNE_ID) >= n * e.w,
        g('comuneBal s ≥ n * w',
          `comune insufficiente: ${bal(s.conti, COMUNE_ID)} < ${n * e.w}`));
      if (fails.length) return rej();
      let conti = bump(s.conti, COMUNE_ID, -(n * e.w));
      for (const u of s.users) conti = bump(conti, u, e.w);
      return { ok: true,
        flow: `redistribuzione: comune −${n * e.w} · ${n} conti +${e.w}`,
        state: { ...s, conti } };
    }
  }
  throw new Error('unknown event tag: ' + e.tag);
}

/* @@CORE:machine:END@@ */

/* @@CORE:events@@ */
const EV = {
  addUser:            { it: 'Fai entrare una persona', roles: ['targetGhost', 'author'] },
  electResponsabile:  { it: 'Eleggi responsabile',     roles: ['targetMember', 'author'] },
  removeResponsabile: { it: 'Revoca responsabile',     roles: ['targetResp', 'author'] },
  openPurchase:       { it: 'Apri un acquisto',        roles: ['label', 'author'] },
  // interface outcomes: the vote happens in the OTHER machine (KelGroups.Vote)
  // and only its closed verdict crosses the unproved bridge into this one;
  // the responsabile carrier required by Step.lean stays inside the adapter
  grantPermission:    { it: 'Assenso: permesso concesso', roles: ['coll'], external: true },
  denyPermission:     { it: 'Dissenso: acquisto negato',  roles: ['coll'], external: true },
  deposit:            { it: 'Accredito',               roles: ['user', 'author'], amount: true },
  withdraw:           { it: 'Prelievo',                roles: ['user', 'author'], amount: true },
  transferCassa:      { it: 'Giro di cassa',           roles: ['from', 'author'], amount: true },
  pledge:             { it: 'Impegno su un acquisto',  roles: ['user', 'coll', 'author'], amount: true },
  acceptPledge:       { it: 'Accetta impegno',         roles: ['coll', 'pendUser', 'author'] },
  refusePledge:       { it: 'Rifiuta impegno',         roles: ['coll', 'pendUser', 'author'] },
  correctPledge:      { it: 'Correggi impegno',        roles: ['coll', 'accUser', 'author'], amount: true },
  closePurchase:      { it: 'Chiudi acquisto',         roles: ['coll', 'author'] },
  failPurchase:       { it: 'Fallisci acquisto',       roles: ['coll', 'author'] },
};

/* @@CORE:events:END@@ */

/* @@CORE:claims@@ */
/* --- Claim manifest: every interface assertion bound to a Lean proof -----
   One row per claim the UI makes. kind: 'teorema' (proved property),
   'definizione' (definitional effect of the cited declaration), or
   'NON PROVATO' (no Lean backing — marked locally where the user meets it).
   file:line points at the declaration in the accepted source snapshot and is
   verified mechanically (CHECK_RECEIPT below + external resolution receipt).
   The browser executes a JavaScript transcription: the binding is mechanical
   (this manifest + Lean-generated trace conformance), not Lean-in-browser —
   that honest limit is itself row 'js-transcription'. */

const CLAIMS = {
  'auth':              { c: 'Ogni evento è dichiarato da un responsabile eletto (AUTH)', k: 'teorema', d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 452 },
  'referente':         { c: "Accettare, rifiutare, correggere, chiudere o fallire: solo il referente dell'acquisto", k: 'teorema', d: 'auth_referente_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 148 },
  'permitted':         { c: 'La chiusura positiva richiede il permesso del gruppo (L2)', k: 'teorema', d: 'close_permission_to_close', f: 'lean/Reactivegas/Invariants.lean', l: 543 },
  'nopending':         { c: 'La chiusura richiede zero impegni pendenti', k: 'teorema', d: 'close_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 154 },
  'nopending-fail':    { c: 'Anche il fallimento richiede zero impegni pendenti', k: 'teorema', d: 'fail_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 164 },
  'covered':           { c: 'Un addebito che manderebbe un conto sotto zero è rifiutato (COVERED, L7)', k: 'teorema', d: 'solvent_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 760 },
  'covered-guards':    { c: "Le guardie dell'impegno (autorità, unicità, copertura) sono esattamente quelle Lean", k: 'teorema', d: 'pledge_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 130 },
  'l8':                { c: 'Un impegno duplicato nello stesso acquisto è rifiutato (L8)', k: 'teorema', d: 'pledge_rejected_when_member', f: 'lean/Reactivegas/Invariants.lean', l: 1066 },
  'l8-preserved':      { c: "L'unicità degli impegni si conserva a ogni evento", k: 'teorema', d: 'pledge_preserves_allUnique', f: 'lean/Reactivegas/Invariants.lean', l: 1094 },
  'exists-coll':       { c: 'Un evento su un acquisto inesistente è rifiutato', k: 'definizione', d: 'pullCollection', f: 'lean/Reactivegas/State.lean', l: 83 },
  'exists-impegno':    { c: "Accettare/rifiutare/correggere richiede che l'impegno esista", k: 'definizione', d: 'splitUser', f: 'lean/Reactivegas/State.lean', l: 65 },
  'membership':        { c: 'Accrediti, prelievi e impegni solo per persone del gruppo; ingresso solo se nuovo', k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'atomic':            { c: 'Un tentativo è un passo atomico: rifiutato = stato invariato', k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'conservation':      { c: 'Σ casse − Σ conti − Σ impegni = 0 dopo ogni evento (L6)', k: 'teorema', d: 'conservation_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 316 },
  'solvency':          { c: 'Nessun conto va sotto zero in nessuno stato raggiungibile (L7)', k: 'teorema', d: 'not_insolvent_of_reach', f: 'lean/Reactivegas/Invariants.lean', l: 1009 },
  'solvency-reach':    { c: 'La solvibilità vale dallo stato iniziale lungo ogni esecuzione', k: 'teorema', d: 'reach_solvent', f: 'lean/Reactivegas/Invariants.lean', l: 1003 },
  'nonneg-impegni':    { c: 'Ogni impegno resta non negativo (L7)', k: 'teorema', d: 'solvent_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 760 },
  'unique':            { c: 'In ogni acquisto, al più un impegno per persona (L8)', k: 'definizione', d: 'uniquePledges', f: 'lean/Reactivegas/Predicates.lean', l: 29 },
  'accredito':         { c: "L'accredito muove insieme conto della persona e cassa del cassiere (+v, +v)", k: 'teorema', d: 'deposit_double_entry', f: 'lean/Reactivegas/Invariants.lean', l: 593 },
  'prelievo':          { c: 'Il prelievo è speculare: conto −v e cassa −v insieme', k: 'teorema', d: 'withdraw_double_entry', f: 'lean/Reactivegas/Invariants.lean', l: 603 },
  'giro':              { c: "Il giro sposta v dalla cassa del mittente a quella dell'autore", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'impegno-escrow':    { c: "All'impegno il denaro lascia subito il conto ed entra negli impegni dell'acquisto; il totale del gruppo non cambia (L3)", k: 'teorema', d: 'pledge_escrow_debit', f: 'lean/Reactivegas/Invariants.lean', l: 555 },
  'accept-effect':     { c: "L'accettazione sposta l'impegno da pendente ad accettato conservando l'unicità", k: 'teorema', d: 'uniquePledges_pend_cons', f: 'lean/Reactivegas/Invariants.lean', l: 1053 },
  'refuse-refund':     { c: "Il rifiuto rimborsa l'impegno pendente sul conto della persona", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'correct-conguaglio':{ c: 'La correzione conguaglia la differenza sul conto e non può mandarlo sotto zero', k: 'teorema', d: 'step_correct_inv', f: 'lean/Reactivegas/Invariants.lean', l: 260 },
  'close-payout':      { c: "La chiusura addebita l'intero raccolto sulla cassa del referente, e solo lì (L4)", k: 'teorema', d: 'close_spends_referente', f: 'lean/Reactivegas/Invariants.lean', l: 575 },
  'close-gone':        { c: "L'acquisto chiuso, negato o fallito sparisce dallo stato: la storia vive solo nel log", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'cassa-negativa':    { c: 'La cassa può andare sotto zero (ha pagato il fornitore): la solvibilità copre i conti, non le casse', k: 'definizione', d: 'solvent', f: 'lean/Reactivegas/Predicates.lean', l: 18 },
  'remove-refunds':    { c: 'Revocare un responsabile chiude i suoi acquisti: nessun acquisto resta orfano', k: 'teorema', d: 'governance_enacts_remove', f: 'lean/Reactivegas/Invariants.lean', l: 528 },
  'remove-refund-sum': { c: "Alla revoca il totale rimborsato è esattamente l'escrow degli acquisti chiusi", k: 'teorema', d: 'stripCollections_sum', f: 'lean/Reactivegas/State.lean', l: 331 },
  'open-referente':    { c: "Chi apre l'acquisto ne è il referente", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 27 },
  'inv-pledge':        { c: "Il rifiuto di un impegno è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_pledge_inv', f: 'lean/Reactivegas/Invariants.lean', l: 203 },
  'inv-accept':        { c: "Il rifiuto di un'accettazione è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_accept_inv', f: 'lean/Reactivegas/Invariants.lean', l: 223 },
  'inv-refuse':        { c: "Il rifiuto di un rifiuto-impegno è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_refuse_inv', f: 'lean/Reactivegas/Invariants.lean', l: 241 },
  'inv-grant':         { c: "Il rifiuto di un assenso è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_grant_inv', f: 'lean/Reactivegas/Invariants.lean', l: 173 },
  'inv-deny':          { c: "Il rifiuto di un dissenso è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_deny_inv', f: 'lean/Reactivegas/Invariants.lean', l: 187 },
  'refusal-unproved':  { c: "Questo rifiuto non ha ancora un lemma di inversione accettato nella sorgente Lean verificata (li completa #48): la spiegazione del rifiuto è NON PROVATA", k: 'NON PROVATO', d: null, f: null, l: null },
  'vote-wellformed':   { c: 'Ogni stato raggiunto dal fold di voto è ben formato', k: 'teorema', d: 'KelGroups.Vote.foldVote_wellFormed', f: 'lean/KelGroups/Vote/Invariants.lean', l: 92 },
  'vote-clean':        { c: 'Una posizione per persona: sì e no mai insieme, mai duplicati — la fuga «vota no» è sempre disponibile', k: 'teorema', d: 'KelGroups.Vote.ballots_nodup_disjoint', f: 'lean/KelGroups/Vote/Invariants.lean', l: 101 },
  'vote-nostale':      { c: 'Nessuna domanda aperta ha già un verdetto: lo sweep chiude tutto ciò che è deciso, nello stesso passo', k: 'teorema', d: 'KelGroups.Vote.open_questions_are_open', f: 'lean/KelGroups/Vote/Invariants.lean', l: 112 },
  'vote-partition':    { c: 'Aperte e chiuse partizionano le domande: nessuna sparisce in silenzio e ogni record chiuso ha un verdetto deciso (invariante escrow)', k: 'teorema', d: 'KelGroups.Vote.questions_partition', f: 'lean/KelGroups/Vote/Invariants.lean', l: 125 },
  'vote-noexpiry':     { c: 'Nessuna scadenza: non esiste un campo temporale che possa chiudere una domanda', k: 'teorema', d: 'KelGroups.Vote.no_expiry', f: 'lean/KelGroups/Vote/Invariants.lean', l: 141 },
  'vote-franchise':    { c: 'Ogni voto registrato fu espresso da chi era responsabile al momento del voto', k: 'teorema', d: 'KelGroups.Vote.franchise_of_tallies', f: 'lean/KelGroups/Vote/Invariants.lean', l: 178 },
  'vote-policyfree':   { c: 'Il verdetto dipende dalla soglia solo attraverso il suo valore alla dimensione attuale dell’elettorato', k: 'teorema', d: 'KelGroups.Vote.verdictOf_threshold_congr', f: 'lean/KelGroups/Vote/Invariants.lean', l: 70 },
  'vote-verdict':      { c: 'Il verdetto è deciso in un solo punto: sì contro soglia, poi no contro la stessa soglia, altrimenti aperta', k: 'definizione', d: 'KelGroups.Vote.verdictOf', f: 'lean/KelGroups/Vote/State.lean', l: 81 },
  'vote-threshold-exhibit': { c: 'La soglia scelta dal toy è l’esibizione legacy (n+1)/2 — parametro esplicito, mai un default: con quattro votanti bastano due sì (o due no: il primo lato a soglia chiude)', k: 'definizione', d: 'KelGroups.Vote.legacyThreshold', f: 'lean/KelGroups/Vote/Types.lean', l: 44 },
  'vote-place':        { c: 'Cambiare voto sposta la posizione; ripetere lo stesso voto non conta mai doppio', k: 'definizione', d: 'KelGroups.Vote.placeBallot', f: 'lean/KelGroups/Vote/Fold.lean', l: 46 },
  'vote-sweep':        { c: 'Dopo OGNI evento lo sweep rivaluta ogni domanda aperta contro l’elettorato attuale e chiude ciò che è deciso', k: 'definizione', d: 'KelGroups.Vote.sweepClosures', f: 'lean/KelGroups/Vote/Fold.lean', l: 59 },
  'vote-apply':        { c: 'Un passo del fold è: effetto dell’evento, poi sweep incondizionato', k: 'definizione', d: 'KelGroups.Vote.applyVoteEvent', f: 'lean/KelGroups/Vote/Fold.lean', l: 77 },
  'vote-validate':     { c: 'Ogni rifiuto del voto è un errore distinto della macchina (notResponsabile, questionNotFound, …)', k: 'definizione', d: 'KelGroups.Vote.validateVoteEvent', f: 'lean/KelGroups/Vote/Validate.lean', l: 37 },
  'vote-open-empty':   { c: 'Una domanda si apre con conteggi VUOTI: chi propone non è contato come sì (divergenza deliberata dal legacy)', k: 'definizione', d: 'KelGroups.Vote.applyVoteEvent', f: 'lean/KelGroups/Vote/Fold.lean', l: 77 },
  'vote-admit-plain':  { c: 'Ammettere un membro è un evento semplice, mai una domanda e mai un voto (R-66)', k: 'definizione', d: 'KelGroups.Vote.VoteEvent', f: 'lean/KelGroups/Vote/Event.lean', l: 14 },
  'vote-permission-kind': { c: 'Una domanda-permesso ha esattamente un designato e solo il SUO voto la decide', k: 'definizione', d: 'KelGroups.Vote.QuestionKind', f: 'lean/KelGroups/Vote/Types.lean', l: 63 },
  'vote-closure-cause':{ c: 'Ogni chiusura dichiara la sua causa: soglia raggiunta, o cambio di elettorato che fa passare un conteggio ormai stantio', k: 'definizione', d: 'KelGroups.Vote.closureCause', f: 'lean/KelGroups/Vote/State.lean', l: 105 },
  'comp-routing':      { c: 'Instradamento TOTALE e senza wildcard dei 18 costruttori accettati: voteDerived e = true ↔ route e ≠ direct — provato al pin accettato della composizione (commit non ancora in questo albero)', k: 'teorema', d: 'Reactivegas.Composition.voteDerived_iff_not_direct', f: 'lean/Reactivegas/Composition.lean', l: 88, g: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c' },
  'comp-base-threshold': { c: 'Canale base: per un Enactment REALE di applyEventDetailed la cui proposta è nel vocabolario FEDELE (changeRoles, removeMember; introduceMember escluso per costruzione), gli assensi registrati raggiunsero la maggioranza dello stato precedente — CONDIZIONATO alla proposta fedele; l’evento economico e l’enactment restano parametri separati: NESSUN join, nessuna equivalenza di macchine', k: 'teorema', d: 'Reactivegas.Composition.baseEnacted_threshold_met', f: 'lean/Reactivegas/Composition.lean', l: 111, g: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c' },
  'comp-app-verdict':  { c: 'Canale app: l’eliminazione del verdetto è ESAUSTIVA — un ClosureRecord permette un evento esattamente quando chiuse positive o negative; open non permette nulla', k: 'teorema', d: 'Reactivegas.Composition.appDecided_verdict_exhaustive', f: 'lean/Reactivegas/Composition.lean', l: 142, g: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c' },
  'comp-witness':      { c: 'Testimone di raggiungibilità (anti-vacuità) del canale app: usa zeroThreshold, quindi NON è MAI evidenza della forza reale della soglia', k: 'definizione', d: 'Reactivegas.Composition.productionVerdictWitness', f: 'lean/Reactivegas/Composition.lean', l: 163, g: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c' },
  'kg-threshold':      { c: 'Canale base (trascritto qui): una delibera avviene solo al raggiungimento della maggioranza di assensi', k: 'teorema', d: 'enact_implies_threshold_met', f: 'lean/KelGroups/Invariants.lean', l: 572 },
  'kg-nodup':          { c: 'Canale base: gli assensi non contano mai doppio — la lista è senza duplicati', k: 'teorema', d: 'approvals_nodup', f: 'lean/KelGroups/Invariants.lean', l: 560 },
  'kg-proposer':       { c: 'Canale base: chi propone è già nel conteggio degli assensi', k: 'teorema', d: 'proposer_mem_approvals', f: 'lean/KelGroups/Invariants.lean', l: 566 },
  'kg-majority-def':   { c: 'Canale base: la maggioranza è definita come (admin+1)/2', k: 'definizione', d: 'KelGroups.majority', f: 'lean/KelGroups/State.lean', l: 32 },
  'kg-validate':       { c: 'Canale base: ogni rifiuto è un errore di validazione della macchina (notAnAdmin, alreadyApproved, …)', k: 'definizione', d: 'KelGroups.validateEvent', f: 'lean/KelGroups/Validate.lean', l: 132 },
  'kg-approve-guard':  { c: 'Canale base: approvare richiede un admin, una proposta esistente e nessun assenso precedente dello stesso firmatario', k: 'definizione', d: 'KelGroups.validateApproval', f: 'lean/KelGroups/Validate.lean', l: 115 },
  'kg-enact-effect':   { c: 'Canale base: la delibera applica la proposta ai membri e rimuove la pendente, in un passo', k: 'definizione', d: 'KelGroups.finishEnact', f: 'lean/KelGroups/Fold.lean', l: 18 },
  'kg-apply':          { c: 'Canale base: proporre inserisce la pendente con il proponente già assenziente e tenta subito la delibera', k: 'definizione', d: 'KelGroups.applyEventDetailed', f: 'lean/KelGroups/Fold.lean', l: 75 },
  'ev-remove-member':  { c: 'removeMember esiste nella macchina economica accettata al pin (#48) ed è instradato baseEnacted (vocabolario fedele); questo snapshot del simulatore non lo esegue ancora', k: 'NON PROVATO', d: null, f: null, l: null },
  'ev-donate':         { c: 'donate è direct: alza insieme la cassa dell\'autore e il conto comune riservato (non-membro in conti) di +v; rifiuta autore non responsabile e v non positivo. Nessun teorema di donazione è ancora proved (sorry #48)', k: 'NON PROVATO', d: null, f: null, l: null },
  'ev-backdonate':     { c: 'backdonate è appDecided: quota uguale w a ogni membro e −n*w dal comune; attempt non inventa backdonateAuthorized (sorry); il governo rifiuta senza ponte evento-voto (NON PROVATO)', k: 'NON PROVATO', d: null, f: null, l: null },
  'kg-setinsert':      { c: 'L’inserimento di posizione è idempotente per costruzione (substrato condiviso)', k: 'definizione', d: 'KelGroups.setInsert', f: 'lean/KelGroups/Types.lean', l: 46 },
  'kg-majority':       { c: 'Aritmetica della formula (n+1)/2: 0,1,1,2,2,3 per 0–5 — provata nella macchina fusa sullo stesso calcolo scelto qui come esibizione', k: 'teorema', d: 'majority_table', f: 'lean/KelGroups/Invariants.lean', l: 597 },
  'kg-tie':            { c: 'Con un numero pari la formula (n+1)/2 non è stretta: 2·soglia ≤ n — provato nella macchina fusa sullo stesso calcolo', k: 'teorema', d: 'majority_not_strict_on_even', f: 'lean/KelGroups/Invariants.lean', l: 606 },
  'join-vote-econ':    { c: 'Il PONTE fra verdetto di voto e permesso economico NON è provato: nessun teorema garantisce che Reactivegas consumi solo grantPermission derivati da un verdetto KelGroups.Vote (in attesa di #54 slice 2 / #48 backdonation)', k: 'NON PROVATO', d: null, f: null, l: null },
  'vote-model-status': { c: 'Questo è il modello di voto RICHIESTO (#54 slice A): verdetti sì/no/aperta, dissenso, permessi per-persona sono modellati; le sue prove sono ENUNCIATE, NON DIMOSTRATE (sorry in corso di scarico). Il sottoinsieme fuso propose/approve è superato per il simulatore', k: 'NON PROVATO', d: null, f: null, l: null },
  'done-list':         { c: "L'elenco «acquisti completati» è ricostruzione del toy: la macchina non conserva gli acquisti chiusi", k: 'NON PROVATO', d: null, f: null, l: null },
  'toy-cap':           { c: 'Il limite 500 su accrediti e giri è del toy: la macchina Lean non ha limite superiore', k: 'NON PROVATO', d: null, f: null, l: null },
  'names':             { c: 'Nomi ed etichette sono decorazione del toy, fuori dallo stato macchina', k: 'NON PROVATO', d: null, f: null, l: null },
  'js-transcription':  { c: 'Il browser esegue una trascrizione JavaScript: il legame con Lean è meccanico (manifesto + tracce generate da Lean), non esecuzione Lean', k: 'NON PROVATO', d: null, f: null, l: null },
};

/* Bound mechanically by the committed gate at the repository root:
   `node economics-simulator-claim-gate.mjs` (add `--selftest` for the three
   negative controls). `decls` must equal the citation set the gate extracts
   from CLAIMS above (no second hand-copied list), and `sha` must equal the
   sha256 of the output of the #check driver the gate GENERATES from that
   extraction and runs fresh in the repository lake environment. The gate
   exits nonzero on any dangling citation, hash drift, or binding mismatch. */
const CHECK_RECEIPT = {
  sha: '149a7cc9332635bf5e25649943b3d9658472df52ff00fc00a39736f5ccee2814',
  decls: ['auth_referente_guard_inv', 'close_guard_inv', 'close_permission_to_close',
    'close_spends_referente', 'conservation_preserved', 'deposit_double_entry',
    'fail_guard_inv', 'governance_enacts_remove', 'not_insolvent_of_reach',
    'pledge_escrow_debit', 'pledge_guard_inv', 'pledge_preserves_allUnique',
    'pledge_rejected_when_member', 'pullCollection', 'reach_solvent', 'solvent',
    'solvent_preserved', 'splitUser', 'step', 'step_accept_inv', 'step_authorized',
    'step_correct_inv', 'step_deny_inv', 'step_grant_inv', 'step_pledge_inv',
    'step_refuse_inv', 'stripCollections_sum', 'uniquePledges',
    'uniquePledges_pend_cons', 'withdraw_double_entry',
    'majority_table', 'majority_not_strict_on_even', 'KelGroups.setInsert',
    'KelGroups.Vote.foldVote_wellFormed', 'KelGroups.Vote.ballots_nodup_disjoint',
    'KelGroups.Vote.open_questions_are_open', 'KelGroups.Vote.questions_partition',
    'KelGroups.Vote.no_expiry', 'KelGroups.Vote.franchise_of_tallies',
    'KelGroups.Vote.verdictOf_threshold_congr', 'KelGroups.Vote.verdictOf',
    'KelGroups.Vote.legacyThreshold', 'KelGroups.Vote.placeBallot',
    'KelGroups.Vote.sweepClosures', 'KelGroups.Vote.applyVoteEvent',
    'KelGroups.Vote.validateVoteEvent', 'KelGroups.Vote.VoteEvent',
    'KelGroups.Vote.QuestionKind', 'KelGroups.Vote.closureCause',
    'enact_implies_threshold_met', 'approvals_nodup', 'proposer_mem_approvals',
    'KelGroups.majority', 'KelGroups.validateEvent', 'KelGroups.validateApproval',
    'KelGroups.finishEnact', 'KelGroups.applyEventDetailed'],
  /* The accepted composition pin: an immutable COMMIT, never a branch. The
     gate resolves it fresh, requires exactly this tree, verifies the cited
     declarations' file:line inside the pinned source, and re-derives each
     status by elaborating the pinned module in a scratch checkout on every
     run (#print axioms; the #guard witnesses fail the build if false). The
     three theorems are separate statements about separate premises — never
     one join, never machine equivalence, never proof that every economic
     event originated from a vote. */
  composition: {
    commit: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c',
    tree: '641107474766534915f67651311b6bdcf1d1a574',
    decls: {
      'Reactivegas.Composition.voteDerived_iff_not_direct': 'provato',
      'Reactivegas.Composition.baseEnacted_threshold_met': 'provato',
      'Reactivegas.Composition.appDecided_verdict_exhaustive': 'provato',
      'Reactivegas.Composition.productionVerdictWitness': 'provato',
    },
  },
  /* Derived, never hand-assigned: for every citation above the gate runs
     `#print axioms <decl>` fresh and classifies it `provato` (no sorryAx)
     or `enunciato` (depends on sorryAx: stated, not yet proved). This map
     must equal that fresh derivation or the gate exits RED. The UI renders
     the three states (provato / enunciato, non dimostrato / NON PROVATO)
     from here — a sorry-backed citation can never render as proved. */
  axioms: {
    'KelGroups.Vote.QuestionKind': 'provato', 'KelGroups.Vote.VoteEvent': 'provato',
    'KelGroups.Vote.applyVoteEvent': 'provato',
    'KelGroups.Vote.ballots_nodup_disjoint': 'enunciato',
    'KelGroups.Vote.closureCause': 'provato',
    'KelGroups.Vote.foldVote_wellFormed': 'enunciato',
    'KelGroups.Vote.franchise_of_tallies': 'enunciato',
    'KelGroups.Vote.legacyThreshold': 'provato', 'KelGroups.Vote.no_expiry': 'enunciato',
    'KelGroups.Vote.open_questions_are_open': 'enunciato',
    'KelGroups.Vote.placeBallot': 'provato',
    'KelGroups.Vote.questions_partition': 'enunciato',
    'KelGroups.Vote.sweepClosures': 'provato',
    'KelGroups.Vote.validateVoteEvent': 'provato', 'KelGroups.Vote.verdictOf': 'provato',
    'KelGroups.Vote.verdictOf_threshold_congr': 'enunciato',
    'KelGroups.setInsert': 'provato', 'auth_referente_guard_inv': 'provato',
    'enact_implies_threshold_met': 'provato', 'approvals_nodup': 'provato',
    'proposer_mem_approvals': 'provato', 'KelGroups.majority': 'provato',
    'KelGroups.validateEvent': 'provato', 'KelGroups.validateApproval': 'provato',
    'KelGroups.finishEnact': 'provato', 'KelGroups.applyEventDetailed': 'provato',
    'close_guard_inv': 'provato', 'close_permission_to_close': 'provato',
    'close_spends_referente': 'provato', 'conservation_preserved': 'provato',
    'deposit_double_entry': 'provato', 'fail_guard_inv': 'provato',
    'governance_enacts_remove': 'provato', 'majority_not_strict_on_even': 'provato',
    'majority_table': 'provato', 'not_insolvent_of_reach': 'provato',
    'pledge_escrow_debit': 'provato', 'pledge_guard_inv': 'provato',
    'pledge_preserves_allUnique': 'provato', 'pledge_rejected_when_member': 'provato',
    'pullCollection': 'provato', 'reach_solvent': 'provato', 'solvent': 'provato',
    'solvent_preserved': 'provato', 'splitUser': 'provato', 'step': 'provato',
    'step_accept_inv': 'provato', 'step_authorized': 'provato',
    'step_correct_inv': 'provato', 'step_deny_inv': 'provato',
    'step_grant_inv': 'provato', 'step_pledge_inv': 'provato',
    'step_refuse_inv': 'provato', 'stripCollections_sum': 'provato',
    'uniquePledges': 'provato', 'uniquePledges_pend_cons': 'provato',
    'withdraw_double_entry': 'provato',
  },
  sources: {
    'lean/Reactivegas/Types.lean': 'd4c1f54ce6a86bb49f708ae3cf5b1a1911a29bc630b4884841910bab778c7615',
    'lean/Reactivegas/State.lean': '23461c9d3b3cf4f686a7e459fff0645ce71f4924d0e1bb6cc5c6f81ca4ee8f2d',
    'lean/Reactivegas/Step.lean': '682192f4c12bb4fa8b76f9b74445c8b465382b0db8e845722c3293fed5c5851b',
    'lean/Reactivegas/Predicates.lean': 'aedaa51e3944f94a860c87e1eb9f6caea2b69ed7dc8aff4e66a25a58c2a4ccbd',
    'lean/Reactivegas/Invariants.lean': 'f9964580396daebb2e8df292a084d045e883ed6ffdb00cdb7b5d824052d99462',
    'lean/KelGroups/Types.lean': 'bfc2b40f18d73bde56455b0aa4dc4b6d78bf34eb396a3827f22e4020b7c5be79',
    'lean/KelGroups/State.lean': 'f0fd3e2a70e40da909cb1f804b0d68f42b7f6c6c898dca19daeb6048e4e041ea',
    'lean/KelGroups/Fold.lean': 'c6cbb818705db481b5f6fd8469e7a7b279fe2e6f9a9675f4cb262df570d679cf',
    'lean/KelGroups/Validate.lean': '23084e484f5bdf9f28d07644b4778e844ed2e821bcb7e88aaab7e650f4f9e68d',
    'lean/KelGroups/Invariants.lean': '77f01acca48ec29e5b7eb251cf1225c5d6a86e2e7d22cf919c12f8b731bf6080',
    'lean/KelGroups/Event.lean': '086575d5103fe69bb6dc8a7a91b3214dded89112b5b9edaac5f1616d056c00df',
    'lean/KelGroups/Tests.lean': '9e2c9b9dc8c33a17b63c73a65de10e011831cba0b3325e27ef2b40cf84d6e8fa',
    'lean/KelGroups.lean': '6c8e10e7c1dc1486f90f27571d633a48051a4cc8742d1a120ad981a5b78e05ce',
    'lean/KelGroups/Vote/Types.lean': 'dc6227f8c785b566aa08c3baab35deae5bc58d6ae420d1cf4e8df923b97fedc8',
    'lean/KelGroups/Vote/Event.lean': 'b7a5aeb4fed0da18d066282f25b860dd45431821851bd8ce4226f39ecfca7f45',
    'lean/KelGroups/Vote/State.lean': 'c7126be8045854a0fab42bf72d4f11dd4405d7a1fabe3b0e2eee4dcc4c098187',
    'lean/KelGroups/Vote/Fold.lean': '90a48f45b60547e691c49013b130824f47f41cc73f7b1141b687cd5c95dabafa',
    'lean/KelGroups/Vote/Validate.lean': '85179a25245028459bfd3cc28a0a1a63201e768c17602b8d5b9253d8169e7d7f',
    'lean/KelGroups/Vote/Invariants.lean': '199e849eb126cb18a681cae1528b3892fbc804e704a948d4fa281f05479272f3',
    'lean/KelGroups/Vote/Tests.lean': '2f561dd3b1655a58b39b8772cec726a646fe479477f65610ff99ce3f21d43e37',
  },
};

// guard badge label → claim id
const GUARD_CLAIMS = {
  'AUTH': 'auth', 'REFERENTE': 'referente', 'PERMITTED (L2)': 'permitted',
  'NO-PENDING (L4)': 'nopending', 'COVERED (L7)': 'covered', 'L8': 'l8',
};
// event constructor → effect claim ids. This is the CLAIM COVERAGE table
// over the ACCEPTED pin's 18 constructors, not just this snapshot's 15:
// claimAudit and the committed gate require every pinned constructor to
// carry rows, with route-appropriate composition rows on the non-direct
// ones (an omitted constructor or a hand-trimmed route is RED).
const TAG_CLAIMS = {
  addUser: ['membership'],
  electResponsabile: ['auth', 'comp-routing', 'comp-base-threshold', 'kg-threshold', 'join-vote-econ'],
  removeResponsabile: ['remove-refunds', 'remove-refund-sum', 'comp-routing', 'comp-base-threshold', 'kg-threshold', 'join-vote-econ'],
  removeMember: ['ev-remove-member', 'comp-routing', 'comp-base-threshold', 'join-vote-econ'],
  openPurchase: ['open-referente'],
  grantPermission: ['join-vote-econ', 'comp-routing', 'comp-app-verdict'],
  denyPermission: ['join-vote-econ', 'comp-routing', 'comp-app-verdict', 'close-gone'],
  deposit: ['accredito'], withdraw: ['prelievo'], transferCassa: ['giro', 'conservation'],
  donate: ['ev-donate'],
  backdonate: ['ev-backdonate', 'comp-routing', 'comp-app-verdict', 'join-vote-econ'],
  pledge: ['impegno-escrow'], acceptPledge: ['accept-effect'],
  refusePledge: ['refuse-refund'], correctPledge: ['correct-conguaglio'],
  closePurchase: ['close-payout', 'cassa-negativa', 'close-gone'],
  failPurchase: ['close-gone'],
};

/* Three derived proof states, never hand-assigned per row:
   'npv'  — NON PROVATO: no Lean declaration at all;
   'enun' — enunciato, non dimostrato: the citation exists and typechecks but
            `#print axioms` shows it depends on sorryAx (CHECK_RECEIPT.axioms,
            derived fresh by the committed gate on every run);
   'ok'   — provato: the citation exists and is sorry-free.
   A citation missing from the axioms map counts as 'enun', never 'ok':
   inability to classify must not render as proved (claimAudit flags it). */
function proofState(id) {
  const r = CLAIMS[id];
  if (!r || r.k === 'NON PROVATO' || !r.d) return 'npv';
  // rows carrying `g` cite the accepted composition PIN, whose status the
  // gate re-derives by elaborating the pinned module fresh
  if (r.g) return CHECK_RECEIPT.composition.decls[r.d] === 'provato' ? 'ok' : 'enun';
  return CHECK_RECEIPT.axioms[r.d] === 'provato' ? 'ok' : 'enun';
}

/* @@CORE:claims:END@@ */

/* @@CORE:audit@@ */
/* selftest gate: the manifest cannot be fictional or unmarked */
function claimAudit() {
  const bad = [];
  const KINDS = ['teorema', 'definizione', 'NON PROVATO'];
  for (const [id, r] of Object.entries(CLAIMS)) {
    if (!r.c || !KINDS.includes(r.k)) { bad.push(id + ': forma non valida'); continue; }
    if (r.k === 'NON PROVATO') {
      if (r.d !== null) bad.push(id + ': NON PROVATO con dichiarazione');
    } else if (r.g) {
      // a pinned-commit citation: bound to the accepted composition pin
      if (r.g !== CHECK_RECEIPT.composition.commit)
        bad.push(id + ': pin diverso dalla composizione accettata');
      if (!r.d || !['provato', 'enunciato'].includes(CHECK_RECEIPT.composition.decls[r.d]))
        bad.push(id + ': stato non derivato al pin per ' + r.d);
      if (!r.f || !Number.isInteger(r.l) || r.l <= 0)
        bad.push(id + ': sorgente/riga non validi');
    } else {
      if (!r.d || !CHECK_RECEIPT.decls.includes(r.d))
        bad.push(id + ': dichiarazione senza ricevuta di verifica');
      if (!r.f || !Number.isInteger(r.l) || r.l <= 0 || !CHECK_RECEIPT.sources[r.f])
        bad.push(id + ': sorgente/riga non validi');
      if (!['provato', 'enunciato'].includes(CHECK_RECEIPT.axioms[r.d]))
        bad.push(id + ': stato assiomi non derivato per ' + r.d);
    }
  }
  // the axioms map may not invent entries the citation set does not contain
  for (const d of Object.keys(CHECK_RECEIPT.axioms))
    if (!CHECK_RECEIPT.decls.includes(d))
      bad.push('assiomi per dichiarazione non citata: ' + d);
  // exhaustive constructor coverage over the ACCEPTED pin's route table:
  // every constructor carries claim rows; every non-direct constructor
  // carries the routing theorem, its route-form theorem, and the honest
  // unproved-join row; the witness row must keep its zeroThreshold caveat
  for (const [tag2, route] of Object.entries(EVENT_ROUTES)) {
    const ids2 = TAG_CLAIMS[tag2] || [];
    if (!ids2.length) { bad.push('costruttore senza righe di manifesto: ' + tag2); continue; }
    if (route !== 'direct') {
      const need = ['comp-routing', 'join-vote-econ',
        route === 'baseEnacted' ? 'comp-base-threshold' : 'comp-app-verdict'];
      for (const nid of need)
        if (!ids2.includes(nid)) bad.push(`costruttore ${tag2} (${route}) senza riga ${nid}`);
    }
  }
  for (const tag2 of Object.keys(TAG_CLAIMS))
    if (!EVENT_ROUTES[tag2]) bad.push('riga di copertura per costruttore non instradato: ' + tag2);
  for (const tag2 of Object.keys(EV))
    if (!EVENT_ROUTES[tag2]) bad.push('evento della macchina fuori dall\'instradamento accettato: ' + tag2);
  if (!CLAIMS['comp-witness'].c.includes('zeroThreshold'))
    bad.push('riga del testimone senza l\'avvertenza zeroThreshold');
  for (const g of Object.values(GUARD_CLAIMS))
    if (!CLAIMS[g]) bad.push('guardia senza riga: ' + g);
  for (const ids of Object.values(TAG_CLAIMS))
    for (const i of ids) if (!CLAIMS[i]) bad.push('tag senza riga: ' + i);
  // derived refusal coverage: a binding without a verified inversion theorem
  // is a claim about Lean that Lean was not asked to confirm
  for (const [tag, rowId] of Object.entries(REFUSAL_PROOFS)) {
    const row = CLAIMS[rowId];
    if (!EV[tag]) bad.push('inversione per evento inesistente: ' + tag);
    if (!row || row.k !== 'teorema' || !row.d || !CHECK_RECEIPT.decls.includes(row.d))
      bad.push('inversione non verificata per: ' + tag);
  }
  const inv = refusalInventory();
  if (inv.proven.length + inv.missing.length !== inv.events.length)
    bad.push('inventario inversioni incoerente');
  return bad;
}

/* @@CORE:audit:END@@ */

/* @@CORE:laws@@ */
function lawViolations(s) {
  const out = [];
  // conservation: sumBal casse − sumBal conti − escrowSum collections = 0
  if (sumBal(s.casse) - sumBal(s.conti) - escrowSum(s.collections) !== 0)
    out.push('conservation (L6)');
  // solvent, first conjunct: ∀ u, bal conti u ≥ 0 (absent entries are 0)
  if (s.conti.some(([, v]) => v < 0)) out.push('solvent (L7): conto negativo');
  for (const col of s.collections) {
    const all = [...col.accepted, ...col.pending];
    // solvent, second conjunct: every pledged amount non-negative
    if (all.some(p => p.amount < 0)) out.push(`solvent (L7): impegno negativo in «${lbl(col.id)}»`);
    // uniquePledges: p.user = q.user → p = q
    outer:
    for (let i = 0; i < all.length; i++)
      for (let j = i + 1; j < all.length; j++)
        if (all[i].user === all[j].user && all[i].amount !== all[j].amount) {
          out.push(`uniquePledges (L8) in «${lbl(col.id)}»`);
          break outer;
        }
  }
  return out;
}

/* @@CORE:laws:END@@ */

/* @@CORE:trace@@ */
/* --- Trace interchange v1 (FROZEN: reactivegas.trace, TRACE-FORMAT-v1) ----
   LEAN_TRACES_V1 below is the verbatim output of the COMMITTED producer
   lean/TraceDriverV1.lean: the two seed envelopes emitted by Lean `ToJson`
   instances over the authoritative `step` of lean/Reactivegas/Step.lean.
   Reproduce from a clean checkout with `cd lean && lake env lean
   TraceDriverV1.lean`; the committed verifier at the repository root,
   `node economics-simulator-trace-gate.mjs`, regenerates it fresh, compares
   byte/structure against this fixture, and replays every envelope through
   THIS page's own production `traceConformance` (add `--selftest` for the
   negative controls).
   Raw output sha256:
   f4722f098216841a3212bacc7ec965960d1d1412d8a08f080565b91277498608
   Each step carries its explicit input state and an applied result computed
   by Lean; nothing here is hand-written or JS-generated. The seed corpus
   contains applied steps only: the typed diagnostic evaluator that would emit
   refusal guard ids rides in #48 and is not fabricated here. Trace A
   exercises removeResponsabile with two live collections (strip + refund of
   accepted and pending); trace B exercises correctPledge in both directions,
   closePurchase driving a cassa negative, and denyPermission refunding
   accepted and pending together. */

const LEAN_TRACES_V1 = {"A":{"initial":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0]},"schema":"reactivegas.trace","steps":[{"event":{"addUser":{"author":0,"target":1}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1]},"tag":"applied"}},{"event":{"addUser":{"author":0,"target":2}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1,2]},"tag":"applied"}},{"event":{"electResponsabile":{"author":0,"target":1}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1,2]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"deposit":{"author":0,"user":1,"v":100}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100]],"collections":[],"conti":[[1,100]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"deposit":{"author":1,"user":2,"v":80}},"input":{"casse":[[0,100]],"collections":[],"conti":[[1,100]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[],"conti":[[1,100],[2,80]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"openPurchase":{"author":1,"c":10}},"input":{"casse":[[0,100],[1,80]],"collections":[],"conti":[[1,100],[2,80]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":10,"pending":[],"permitted":false,"referente":1}],"conti":[[1,100],[2,80]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":0,"c":10,"user":2,"v":30}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":10,"pending":[],"permitted":false,"referente":1}],"conti":[[1,100],[2,80]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":10,"pending":[{"amount":30,"user":2}],"permitted":false,"referente":1}],"conti":[[1,100],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"acceptPledge":{"author":1,"c":10,"user":2}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":10,"pending":[{"amount":30,"user":2}],"permitted":false,"referente":1}],"conti":[[1,100],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[],"permitted":false,"referente":1}],"conti":[[1,100],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":1,"c":10,"user":1,"v":40}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[],"permitted":false,"referente":1}],"conti":[[1,100],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"openPurchase":{"author":1,"c":11}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":11,"pending":[],"permitted":false,"referente":1},{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":0,"c":11,"user":2,"v":20}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":11,"pending":[],"permitted":false,"referente":1},{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":2}],"permitted":false,"referente":1},{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,30]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"removeResponsabile":{"author":0,"target":1}},"input":{"casse":[[0,100],[1,80]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":2}],"permitted":false,"referente":1},{"accepted":[{"amount":30,"user":2}],"id":10,"pending":[{"amount":40,"user":1}],"permitted":false,"referente":1}],"conti":[[1,60],[2,30]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,100],[1,80]],"collections":[],"conti":[[1,100],[2,80]],"responsabili":[0],"users":[0,1,2]},"tag":"applied"}}],"version":1},"B":{"initial":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0]},"schema":"reactivegas.trace","steps":[{"event":{"addUser":{"author":0,"target":1}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1]},"tag":"applied"}},{"event":{"electResponsabile":{"author":0,"target":1}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[0],"users":[0,1]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1]},"tag":"applied"}},{"event":{"addUser":{"author":0,"target":2}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1]},"result":{"state":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"deposit":{"author":0,"user":2,"v":50}},"input":{"casse":[],"collections":[],"conti":[],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[],"conti":[[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"openPurchase":{"author":1,"c":7}},"input":{"casse":[[0,50]],"collections":[],"conti":[[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,50]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":0,"c":7,"user":2,"v":20}},"input":{"casse":[[0,50]],"collections":[{"accepted":[],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,50]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[],"id":7,"pending":[{"amount":20,"user":2}],"permitted":false,"referente":1}],"conti":[[2,30]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"acceptPledge":{"author":1,"c":7,"user":2}},"input":{"casse":[[0,50]],"collections":[{"accepted":[],"id":7,"pending":[{"amount":20,"user":2}],"permitted":false,"referente":1}],"conti":[[2,30]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":20,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,30]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"correctPledge":{"author":1,"c":7,"user":2,"v":35}},"input":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":20,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,30]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":35,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,15]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"correctPledge":{"author":1,"c":7,"user":2,"v":5}},"input":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":35,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,15]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":5,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"grantPermission":{"author":0,"c":7}},"input":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":5,"user":2}],"id":7,"pending":[],"permitted":false,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":5,"user":2}],"id":7,"pending":[],"permitted":true,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"closePurchase":{"author":1,"c":7}},"input":{"casse":[[0,50]],"collections":[{"accepted":[{"amount":5,"user":2}],"id":7,"pending":[],"permitted":true,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,-5]],"collections":[],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"openPurchase":{"author":1,"c":8}},"input":{"casse":[[0,50],[1,-5]],"collections":[],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":1,"c":8,"user":2,"v":10}},"input":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,45]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[],"id":8,"pending":[{"amount":10,"user":2}],"permitted":false,"referente":1}],"conti":[[2,35]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"acceptPledge":{"author":1,"c":8,"user":2}},"input":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[],"id":8,"pending":[{"amount":10,"user":2}],"permitted":false,"referente":1}],"conti":[[2,35]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,35]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"deposit":{"author":1,"user":0,"v":25}},"input":{"casse":[[0,50],[1,-5]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,35]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,20]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,35],[0,25]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"pledge":{"author":0,"c":8,"user":0,"v":15}},"input":{"casse":[[0,50],[1,20]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[],"permitted":false,"referente":1}],"conti":[[2,35],[0,25]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,20]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[{"amount":15,"user":0}],"permitted":false,"referente":1}],"conti":[[2,35],[0,10]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}},{"event":{"denyPermission":{"author":0,"c":8}},"input":{"casse":[[0,50],[1,20]],"collections":[{"accepted":[{"amount":10,"user":2}],"id":8,"pending":[{"amount":15,"user":0}],"permitted":false,"referente":1}],"conti":[[2,35],[0,10]],"responsabili":[1,0],"users":[0,1,2]},"result":{"state":{"casse":[[0,50],[1,20]],"collections":[],"conti":[[2,45],[0,25]],"responsabili":[1,0],"users":[0,1,2]},"tag":"applied"}}],"version":1}};

/* Lean ToJson event shape {"tag":{...fields}} ↔ machine event {tag, ...} */
const leanEventOf = ej => {
  const tag = Object.keys(ej)[0];
  return { tag, ...ej[tag] };
};
const leanEventJson = e => {
  const { tag, ...fields } = e;
  return { [tag]: fields };
};

const canonState = s => JSON.stringify({
  users: s.users, responsabili: s.responsabili, conti: s.conti, casse: s.casse,
  collections: s.collections.map(c => ({ id: c.id, referente: c.referente,
    permitted: c.permitted,
    accepted: c.accepted.map(p => ({ user: p.user, amount: p.amount })),
    pending: c.pending.map(p => ({ user: p.user, amount: p.amount })) })) });

/* Refusal explanations: DERIVED coverage, never a frozen list. An event's
   refusal is proved iff its entry below binds a manifest row of kind
   'teorema' whose declaration is mechanically verified in the Lean source
   snapshot (CHECK_RECEIPT). Every event without such a verified binding
   renders NON PROVATO locally — including any constructor a future Lean
   amendment adds (#48's accepted surface carries events this snapshot does
   not; they are discovered by inventory, not by a copied list). Never assume
   future proof names. */
const REFUSAL_PROOFS = {
  pledge: 'inv-pledge', acceptPledge: 'inv-accept', refusePledge: 'inv-refuse',
  correctPledge: 'correct-conguaglio', closePurchase: 'nopending',
  failPurchase: 'nopending-fail', grantPermission: 'inv-grant',
  denyPermission: 'inv-deny',
};

/* is this event's refusal explanation bound to a verified inversion theorem?
   A sorry-backed (enunciato) theorem never counts: proved means proved. */
function refusalProven(tag) {
  const row = CLAIMS[REFUSAL_PROOFS[tag]];
  return !!(row && row.k === 'teorema' && row.d && CHECK_RECEIPT.decls.includes(row.d)
    && CHECK_RECEIPT.axioms[row.d] === 'provato');
}
/* claim ids to show for a refusal/guard encounter on this event */
function refusalClaims(tag) {
  return refusalProven(tag) ? [REFUSAL_PROOFS[tag]] : ['refusal-unproved'];
}
/* live inventory: event constructors vs verified inversion bindings */
function refusalInventory() {
  const events = Object.keys(EV);
  const proven = events.filter(refusalProven);
  const missing = events.filter(t => !refusalProven(t));
  return { events, proven, missing };
}

/* v1 consumer: validate + replay one envelope through the JS transcription.
   Enforces every frozen-contract rejection: schema, version, shape,
   input continuity from `initial` (stored inputs are never authority),
   outcome match, post-state match, and refusal-guard validity. Returns
   { steps, states } or throws with a precise reason. */
function verifyTraceV1(env, opts) {
  opts = opts || {};
  const short = x => { const j = typeof x === 'string' ? x : JSON.stringify(x);
    return j.length > 220 ? j.slice(0, 220) + '…' : j; };
  const fail = m => { throw new Error('trace: ' + m); };
  if (!env || typeof env !== 'object') fail('non è un oggetto');
  if (env.schema !== 'reactivegas.trace') fail('schema sconosciuto');
  if (env.version !== 1) fail('versione non supportata: ' + env.version);
  if (!env.initial || !Array.isArray(env.steps)) fail('forma non valida');
  let s = env.initial;
  if (canonState(s) !== canonState(initState(FOUNDER)))
    fail('stato iniziale non è State.init 0 — atteso=' + short(canonState(initState(FOUNDER))) +
      ' osservato=' + short(canonState(s)));
  const states = [JSON.parse(canonState(s))];
  const refusedNotes = [];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || !st.event || !st.result) fail(`passo ${i}: forma non valida`);
    if (canonState(st.input) !== canonState(s))
      fail(`passo ${i}: input discontinuo — derivato=${short(canonState(s))} memorizzato=${short(canonState(st.input))}`);
    const e = leanEventOf(st.event);
    if (!EV[e.tag]) fail(`passo ${i}: evento sconosciuto ${short(st.event)}`);
    const res = attempt(JSON.parse(canonState(s)), e);
    if (st.result.tag === 'applied') {
      if (!res.ok)
        fail(`passo ${i} (.${e.tag}): registrato applicato, la macchina rifiuta — guardie: ` +
          short(res.failed.map(g => g.code).join(' ∧ ')));
      if (canonState(res.state) !== canonState(st.result.state))
        fail(`passo ${i} (.${e.tag}): post-stato divergente — atteso=${short(canonState(st.result.state))} osservato=${short(canonState(res.state))}`);
      const viol = lawViolations(res.state);
      if (viol.length) fail(`passo ${i}: ${viol.join(' · ')}`);
      s = res.state;
    } else if (st.result.tag === 'refused') {
      if (opts.appliedOnly) fail(`passo ${i}: rifiuto in un log di sessione`);
      if (res.ok) fail(`passo ${i} (.${e.tag}): registrato rifiutato, la macchina applica`);
      const gu = st.result.guard;
      if (!gu || typeof gu.id !== 'string') fail(`passo ${i}: guard mancante`);
      if (refusalProven(e.tag) && gu.declaration !== CLAIMS[REFUSAL_PROOFS[e.tag]].d)
        fail(`passo ${i}: GuardId/dichiarazione senza teorema di inversione accettato — ` +
          `dichiarata=${short(gu.declaration)} attesa=${CLAIMS[REFUSAL_PROOFS[e.tag]].d}`);
      // «entrambi rifiutati» non è conformità: senza il valutatore diagnostico
      // provato di #48 non esiste un guard-id autorevole da confrontare, quindi
      // la verifica del motivo resta dichiaratamente NON PROVATA, mai silenziosa
      refusedNotes.push(`passo ${i} (.${e.tag}): guard-id «${gu.id}» non verificabile in questo snapshot (NON PROVATO, #48)`);
    } else fail(`passo ${i}: result.tag sconosciuto «${short(st.result.tag)}»`);
    states.push(JSON.parse(canonState(s)));
  });
  return { steps: env.steps.length, states,
    refusedUnverified: refusedNotes.length, refusedNotes };
}

/* Subtle-drift net: replay the Lean seed envelopes through the JS `attempt`
   and compare every post-state. Fails loudly on any divergence — and on an
   EMPTY corpus: a conformance check that passes because it found no traces
   is the failure this goal exists to prevent. The corpus below is the LOCAL
   applied-only seed generated from this checkout's Step.lean; it is not the
   forthcoming accepted #48 emitter corpus, and refused-step guard-id
   verification stays NON PROVATO until that lands. */
function traceConformance() {
  const names = Object.keys(LEAN_TRACES_V1);
  let steps = 0, refusedUnverified = 0;
  for (const name of names) {
    try {
      const r = verifyTraceV1(LEAN_TRACES_V1[name]);
      steps += r.steps;
      refusedUnverified += r.refusedUnverified;
    } catch (e) {
      throw new Error(name + ' — ' + e.message);
    }
  }
  if (!names.length || steps === 0)
    throw new Error('corpus di conformità vuoto: nessuna traccia, nessuna prova');
  return { steps, refusedUnverified, corpus: 'seme-locale-solo-applicati' };
}

/* @@CORE:trace:END@@ */

/* @@CORE:vote-a@@ */
/* --- Required Vote machine core: transcription of lean/KelGroups/Vote/ ----
   Second machine, MODEL-FAITHFUL to the required vote model (#54 slice A,
   REQUIRED-OF-SUBSTRATE): three-way verdicts with `open` as a real outcome,
   dissent, per-person permission questions, explicit threshold parameter,
   one-position-per-responsabile ballots, the unconditional recompute-and-
   close sweep, and append-only closure records. Its contractual theorems are
   currently STATED WITH UNPROVED BODIES (sorry): the manifest renders them
   «enunciato, non dimostrato», never «provato» — the three-state
   classification is derived from `#print axioms` by the committed claim
   gate, not asserted here. Toy decoration (names, question phrasing) stays
   outside. The economic↔vote JOIN remains first-class NON PROVATO
   ('join-vote-econ'). */

/* the threshold is a machine PARAMETER (R-46); the toy's explicit choice is
   the named legacy exhibit (n+1)/2 — declared, never a hidden default */
const VT_THRESHOLDS = {
  legacyThreshold: n => Math.floor((n + 1) / 2),
  zeroThreshold: () => 0,
};
const VT_THRESHOLD_NAME = 'legacyThreshold';
const vtTheta = VT_THRESHOLDS[VT_THRESHOLD_NAME];

const vtEmpty = () => ({ members: [], openQuestions: [], closed: [] });
const vtLookup = (k, l) => { for (const [c, v] of l) if (c === k) return v; return null; };
const vtErase = (k, l) => { const i = l.findIndex(e => e[0] === k);
  return i < 0 ? l.slice() : l.slice(0, i).concat(l.slice(i + 1)); };
const vtInsert = (k, v, l) => [[k, v], ...vtErase(k, l)];
const vtAdjust = (k, f, l) => { const i = l.findIndex(e => e[0] === k);
  if (i < 0) return l.slice(); const out = l.slice(); out[i] = [k, f(out[i][1])]; return out; };
const vtSetInsert = (v, l) => l.includes(v) ? l.slice() : [v, ...l];
const vtListErase = (v, l) => { const i = l.indexOf(v);
  return i < 0 ? l.slice() : l.slice(0, i).concat(l.slice(i + 1)); };
const vtHasAdmin = roles => roles.some(r => 'adminRole' in r);
const vtFranchise = gs => gs.members.filter(([, m]) => vtHasAdmin(m.roles)).map(([k]) => k);
const vtFranchiseSize = gs => vtFranchise(gs).length;
const vtIsResp = (k, gs) => { const m = vtLookup(k, gs.members); return !!m && vtHasAdmin(m.roles); };

/* a QuestionKind is either the string 'collective' (fieldless ctor) or
   { permission: { designee } }; this is the one place that distinction lives */
const vtPermKind = k =>
  (k !== null && typeof k === 'object' && k.permission) ? k.permission : null;

/* verdictOf — the single verdict site (State.lean): collective tallies are
   compared to θ(franchise), assents first; a permission verdict is decided
   ONLY by the designee's own recorded ballot */
function vtVerdictOf(gs, q) {
  const perm = vtPermKind(q.kind);
  if (perm) {
    const d = perm.designee;
    if (q.assents.includes(d)) return 'positive';
    if (q.dissents.includes(d)) return 'negative';
    return 'open';
  }
  const required = vtTheta(vtFranchiseSize(gs));
  if (q.assents.length >= required) return 'positive';
  if (q.dissents.length >= required) return 'negative';
  return 'open';
}

function vtClosureCause(gs, q, verdict) {
  if (verdict === 'positive')
    return q.assents.every(k => vtIsResp(k, gs)) ? 'tally' : 'franchiseChange';
  if (verdict === 'negative')
    return q.dissents.every(k => vtIsResp(k, gs)) ? 'tally' : 'franchiseChange';
  return 'tally';
}

/* placeBallot — one position per responsabile: inserting into one list
   erases from the other; re-casting the same position changes neither */
function vtPlaceBallot(voter, ballot, q) {
  if (ballot === 'assent')
    return { ...q, assents: vtSetInsert(voter, q.assents),
      dissents: vtListErase(voter, q.dissents) };
  return { ...q, dissents: vtSetInsert(voter, q.dissents),
    assents: vtListErase(voter, q.assents) };
}

/* sweepClosures — unconditional on every step: close every open question
   whose verdict under the CURRENT franchise is decided; removal plus an
   appended closure record, as one operation */
function vtSweep(gs) {
  const resolved = [], remaining = [];
  for (const [qid, q] of gs.openQuestions) {
    const v = vtVerdictOf(gs, q);
    if (v === 'open') remaining.push([qid, q]);
    else resolved.push({ questionId: qid, question: q, verdict: v,
      cause: vtClosureCause(gs, q, v) });
  }
  return { ...gs, openQuestions: remaining, closed: gs.closed.concat(resolved) };
}

function vtApply(gs, signer, ev) {
  let effected = gs;
  if ('openQuestion' in ev) {
    const { questionId, kind } = ev.openQuestion;
    if (vtLookup(questionId, gs.openQuestions) === null &&
        !gs.closed.some(r => r.questionId === questionId))
      effected = { ...gs, openQuestions: vtInsert(questionId,
        { kind, proposer: signer, assents: [], dissents: [] }, gs.openQuestions) };
  } else if ('cast' in ev) {
    const { questionId, ballot } = ev.cast;
    if (vtIsResp(signer, gs)) {
      const q = vtLookup(questionId, gs.openQuestions);
      if (q) effected = { ...gs, openQuestions:
        vtInsert(questionId, vtPlaceBallot(signer, ballot, q), gs.openQuestions) };
    }
  } else if ('renounce' in ev) {
    /* slice-A no-op */
  } else if ('admitMember' in ev) {
    const { key, email, roles } = ev.admitMember;
    effected = { ...gs, members: vtInsert(key, { key, email, roles }, gs.members) };
  } else if ('removeMember' in ev) {
    effected = { ...gs, members: vtErase(ev.removeMember.key, gs.members) };
  } else if ('setRoles' in ev) {
    const { key, roles } = ev.setRoles;
    effected = { ...gs, members: vtAdjust(key, m => ({ ...m, roles }), gs.members) };
  }
  const swept = vtSweep(effected);
  return { state: swept, closedDelta: swept.closed.length - gs.closed.length };
}

/* validateVoteEvent transcription — returns null or the VoteError ctor name,
   the SAME identifiers Lean emits and the corpus compares exactly */
function vtValidate(gs, signer, ev) {
  if ('openQuestion' in ev)
    return vtIsResp(signer, gs) ? null : 'notResponsabile';
  if ('cast' in ev) {
    if (!vtIsResp(signer, gs)) return 'notResponsabile';
    return vtLookup(ev.cast.questionId, gs.openQuestions) ? null : 'questionNotFound';
  }
  if ('renounce' in ev)
    return vtLookup(ev.renounce.questionId, gs.openQuestions) ? null : 'questionNotFound';
  return null;   // admitMember / removeMember / setRoles: plain state events
}

const vtCanonRole = r => 'adminRole' in r
  ? { adminRole: { admin: r.adminRole.admin } } : { appRole: { name: r.appRole.name } };
const vtCanonKind = k => { const p = vtPermKind(k);
  return p ? { permission: { designee: p.designee } } : 'collective'; };
const vtCanonQ = q => ({ kind: vtCanonKind(q.kind), proposer: q.proposer,
  assents: q.assents.slice(), dissents: q.dissents.slice() });
const canonVoteState = gs => JSON.stringify({
  members: gs.members.map(([k, m]) => [k,
    { key: m.key, email: m.email, roles: m.roles.map(vtCanonRole) }]),
  openQuestions: gs.openQuestions.map(([qid, q]) => [qid, vtCanonQ(q)]),
  closed: gs.closed.map(r => ({ questionId: r.questionId, question: vtCanonQ(r.question),
    verdict: r.verdict, cause: r.cause })) });

/* crude net — transcription of the STATED invariants (their Lean proofs are
   currently «enunciato, non dimostrato»; this runtime check detects
   transcription drift, it does not claim the proofs): QuestionClean,
   partition, no-stale-open */
/* @@CORE:vote-a:END@@ */

/* @@CORE:vote-b@@ */
function kgLawViolations(gs) {
  const out = [];
  const clean = (q, where) => {
    if (new Set(q.assents).size !== q.assents.length ||
        new Set(q.dissents).size !== q.dissents.length)
      out.push(`QuestionClean nodup violato (${where})`);
    if (q.assents.some(k => q.dissents.includes(k)))
      out.push(`QuestionClean disgiunzione violata (${where})`);
  };
  for (const [qid, q] of gs.openQuestions) {
    clean(q, qid);
    if (vtVerdictOf(gs, q) !== 'open')
      out.push(`domanda aperta con verdetto deciso (${qid}) — sweep mancato`);
  }
  const openIds = gs.openQuestions.map(([qid]) => qid);
  const closedIds = gs.closed.map(r => r.questionId);
  if (new Set(openIds).size !== openIds.length) out.push('id aperti duplicati');
  if (new Set(closedIds).size !== closedIds.length) out.push('id chiusi duplicati');
  if (openIds.some(i => closedIds.includes(i))) out.push('partizione aperte/chiuse violata');
  for (const r of gs.closed) {
    clean(r.question, r.questionId + ' (chiusa)');
    if (r.verdict === 'open') out.push(`record di chiusura con verdetto open (${r.questionId})`);
  }
  return out;
}
/* @@CORE:vote-b:END@@ */

/* @@CORE:vote-c@@ */
function verifyKelTraceV1(env, opts) {
  opts = opts || {};
  const short = x => { const j = typeof x === 'string' ? x : JSON.stringify(x);
    return j.length > 200 ? j.slice(0, 200) + '…' : j; };
  const fail = m => { throw new Error('voto: ' + m); };
  if (!env || typeof env !== 'object') fail('non è un oggetto');
  if (env.schema !== 'kelgroups-vote.trace') fail('schema sconosciuto');
  if (env.version !== 1) fail('versione non supportata: ' + env.version);
  if (!VT_THRESHOLDS[env.threshold]) fail('soglia sconosciuta: ' + env.threshold);
  if (env.threshold !== VT_THRESHOLD_NAME)
    fail('soglia diversa dal parametro scelto dal toy: ' + env.threshold);
  if (!env.initial || !Array.isArray(env.steps)) fail('forma non valida');
  let s = env.initial;
  if (canonVoteState(s) !== canonVoteState(vtEmpty()))
    fail('stato iniziale non è emptyVoteState');
  const states = [JSON.parse(canonVoteState(s))];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || typeof st.signer !== 'string' || !st.event || !st.result)
      fail(`passo ${i}: forma non valida`);
    if (canonVoteState(st.input) !== canonVoteState(s))
      fail(`passo ${i}: input discontinuo — derivato=${short(canonVoteState(s))} memorizzato=${short(canonVoteState(st.input))}`);
    const verr = vtValidate(JSON.parse(canonVoteState(s)), st.signer, st.event);
    if (st.result.tag === 'applied') {
      if (verr) fail(`passo ${i}: registrato applicato, la validazione rifiuta (${verr})`);
      const det = vtApply(JSON.parse(canonVoteState(s)), st.signer, st.event);
      if (canonVoteState(det.state) !== canonVoteState(st.result.state))
        fail(`passo ${i}: post-stato divergente — atteso=${short(canonVoteState(st.result.state))} osservato=${short(canonVoteState(det.state))}`);
      if (typeof st.result.closedCount === 'number' &&
          det.state.closed.length !== st.result.closedCount)
        fail(`passo ${i}: conteggio chiusure divergente — atteso=${st.result.closedCount} osservato=${det.state.closed.length}`);
      const viol = kgLawViolations(det.state);
      if (viol.length) fail(`passo ${i}: ${viol.join(' · ')}`);
      s = det.state;
    } else if (st.result.tag === 'refused') {
      if (opts.appliedOnly) fail(`passo ${i}: rifiuto in un log di sessione`);
      if (!verr) fail(`passo ${i}: registrato rifiutato, la validazione applica`);
      const errId = typeof st.result.error === 'string'
        ? st.result.error : Object.keys(st.result.error || {})[0];
      if (errId !== verr)
        fail(`passo ${i}: errore di validazione divergente — atteso=${errId} osservato=${verr}`);
    } else fail(`passo ${i}: result.tag sconosciuto`);
    states.push(JSON.parse(canonVoteState(s)));
  });
  return { steps: env.steps.length, states };
}

function kelTraceConformance() {
  const names = Object.keys(VOTE_TRACES_V1);
  let steps = 0;
  for (const name of names) {
    try { steps += verifyKelTraceV1(VOTE_TRACES_V1[name]).steps; }
    catch (e) { throw new Error(name + ' — ' + e.message); }
  }
  if (!names.length || steps === 0)
    throw new Error('corpus di voto vuoto: nessuna traccia, nessuna prova');
  return { steps, corpus: 'seme-locale-vote-richiesto' };
}

/* outer-module compatibility names (session log, presentation, restore) */
const kgEmptyState = vtEmpty;
const canonKelState = canonVoteState;
const kgApplyDetailed = (gs, signer, ev) => vtApply(gs, signer, ev);

/* VOTE_TRACES_V1: verbatim output of the COMMITTED producer
   lean/KelTraceDriverV1.lean over the authoritative KelGroups.Vote fold
   (foldVote step semantics, legacyThreshold as the declared parameter).
   Reproduce with `cd lean && lake env lean KelTraceDriverV1.lean`; the
   committed verifier `node economics-simulator-vote-trace-gate.mjs`
   regenerates it fresh, compares with this fixture, and replays both
   through THIS page's production Vote transcription.
   Vote raw output sha256:
   28682a9cddb724cadc258ebbb2309ff62553a145edf228fbb38898cbeb6abce8
   Local schema kelgroups-vote.trace v1 (no shared proved schema with the
   frozen economic contract is claimed). Seed: franchise via plain member
   events (R-66), empty-tally opening (no proposer auto-assent), refused
   casts (notResponsabile, questionNotFound), position switch closing at
   threshold, idempotent re-cast, dissent-driven NEGATIVE verdict,
   per-person permission decided only by the designee, franchise-change
   closure over a stale tally, no-op renounce, and an OPEN (undecided)
   question in the final state. */
const VOTE_TRACES_V1 = {"V":{"initial":{"closed":[],"members":[],"openQuestions":[]},"schema":"kelgroups-vote.trace","steps":[{"event":{"admitMember":{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}},"input":{"closed":[],"members":[],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"anna"},{"event":{"admitMember":{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}},"input":{"closed":[],"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"anna"},{"event":{"admitMember":{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}},"input":{"closed":[],"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"anna"},{"event":{"admitMember":{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}},"input":{"closed":[],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"anna"},{"event":{"openQuestion":{"kind":"collective","questionId":"q:permesso-olio"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"error":"notResponsabile","tag":"refused"},"signer":"dora"},{"event":{"cast":{"ballot":"assent","questionId":"q:nessuna"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"error":"questionNotFound","tag":"refused"},"signer":"anna"},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":0,"state":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"cast":{"ballot":"dissent","questionId":"q:permesso-olio"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":0,"state":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":["bruno"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"bruno"},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":["bruno"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"bruno"},{"event":{"openQuestion":{"kind":"collective","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"elena"},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"elena"},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"carlo"},{"event":{"openQuestion":{"kind":{"permission":{"designee":"bruno"}},"questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:incarico",{"assents":[],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"cast":{"ballot":"assent","questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:incarico",{"assents":[],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:incarico",{"assents":["elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"tag":"applied"},"signer":"elena"},{"event":{"cast":{"ballot":"assent","questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:incarico",{"assents":["elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"bruno"},{"event":{"openQuestion":{"kind":"collective","questionId":"q:magazzino"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"cast":{"ballot":"assent","questionId":"q:magazzino"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"carlo"},{"event":{"removeMember":{"key":"carlo"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"removeMember":{"key":"bruno"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:magazzino",{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":4,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"},{"cause":"franchiseChange","question":{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:magazzino","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"tag":"applied"},"signer":"anna"},{"event":{"openQuestion":{"kind":"collective","questionId":"q:aperta"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"},{"cause":"franchiseChange","question":{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:magazzino","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[]},"result":{"closedCount":4,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"},{"cause":"franchiseChange","question":{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:magazzino","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"},{"event":{"renounce":{"questionId":"q:aperta"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"},{"cause":"franchiseChange","question":{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:magazzino","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":4,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"},{"cause":"franchiseChange","question":{"assents":["carlo"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:magazzino","verdict":"positive"}],"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna"}],"threshold":"legacyThreshold","version":1}};

/* @@CORE:vote-c:END@@ */

/* @@CORE:base-a@@ */
const bgEmpty = () => ({ members: [], pendingProposals: [] });
/* @@CORE:base-a:END@@ */

/* @@CORE:base-b@@ */
/* State.lean (merged): counts, majority, membership */
const bgAdminCount = gs => gs.members.reduce((n, [, m]) => n + (vtHasAdmin(m.roles) ? 1 : 0), 0);
const bgMajority = gs => Math.floor((bgAdminCount(gs) + 1) / 2);   // KelGroups.majority
const bgIsAdmin = (k, gs) => { const m = vtLookup(k, gs.members); return !!m && vtHasAdmin(m.roles); };
const bgIsMember = (k, gs) => vtLookup(k, gs.members) !== null;
const bgBootstrap = gs => bgAdminCount(gs) === 0;                  // authMode = .bootstrap

/* a Proposal is exactly one of the three Lean constructors */
const bgPropKind = p => 'introduceMember' in p ? 'introduceMember'
  : ('removeMember' in p ? 'removeMember' : 'changeRoles');

/* Composition.baseProposalFaithful (at the pin): only changeRoles and
   removeMember enactments carry base-enacted economic evidence */
const bgProposalFaithful = p => bgPropKind(p) !== 'introduceMember';

/* digest is a machine PARAMETER (Proposal → ProposalId); the toy picks a
   readable deterministic digest. Re-proposing the same digest replaces the
   pending entry (assocInsert), exactly as Fold.lean does. */
const bgDigest = p => {
  const k = bgPropKind(p);
  if (k === 'introduceMember') return 'entra:' + p.introduceMember.key;
  if (k === 'removeMember') return 'esce:' + p.removeMember.key;
  return 'ruoli:' + p.changeRoles.key + ':' +
    (p.changeRoles.roles.some(r => 'adminRole' in r) ? 'admin' : 'app');
};

/* KelGroups.enact */
const bgEnact = (gs, p) => {
  const k = bgPropKind(p);
  if (k === 'introduceMember') {
    const { key, email, roles } = p.introduceMember;
    return { ...gs, members: vtInsert(key, { key, email, roles }, gs.members) };
  }
  if (k === 'removeMember')
    return { ...gs, members: vtErase(p.removeMember.key, gs.members) };
  const { key, roles } = p.changeRoles;
  return { ...gs, members: vtAdjust(key, m => ({ ...m, roles }), gs.members) };
};

/* KelGroups.tryEnactDetailed: majority is read on the state that already
   contains the pending proposal, members unchanged by the pending entry */
function bgTryEnactDetailed(gs, pid) {
  const pending = vtLookup(pid, gs.pendingProposals);
  if (!pending) return { state: gs, enactment: null };
  if (pending.approvals.length >= bgMajority(gs)) {
    const enacted = bgEnact(gs, pending.proposal);
    return {
      state: { ...enacted, pendingProposals: vtErase(pid, enacted.pendingProposals) },
      enactment: { proposalId: pid, pending, preState: gs },
    };
  }
  return { state: gs, enactment: null };
}

/* KelGroups.applyEventDetailed, base events only (the toy's α is unit):
   propose inserts the pending with the proposer already approving and tries
   the enactment at once; approve setInserts and tries again */
function bgApply(gs, signer, ev) {
  if ('propose' in ev) {
    const pid = bgDigest(ev.propose.proposal);
    const pending = { proposal: ev.propose.proposal, proposer: signer, approvals: [signer] };
    const proposed = { ...gs, pendingProposals: vtInsert(pid, pending, gs.pendingProposals) };
    return bgTryEnactDetailed(proposed, pid);
  }
  const pid = ev.approve.proposalId;
  const pending = vtLookup(pid, gs.pendingProposals);
  if (!pending) return { state: gs, enactment: null };
  const approved = { ...pending, approvals: vtSetInsert(signer, pending.approvals) };
  const updated = { ...gs, pendingProposals: vtInsert(pid, approved, gs.pendingProposals) };
  return bgTryEnactDetailed(updated, pid);
}

/* KelGroups.validateEvent (base), with the toy's parameters: validKey = ⊤
   and an empty GroupConfig (no app-role preconditions, so the changeRoles
   role checks are vacuously ok — exactly the Lean code with roleDefs []).
   Returns null or the Lean ValidationError constructor name. */
function bgValidate(gs, signer, ev) {
  if ('propose' in ev) {
    const p = ev.propose.proposal;
    if (bgBootstrap(gs)) {
      if (bgPropKind(p) === 'introduceMember')
        return p.introduceMember.roles.some(r => 'adminRole' in r) ? null : 'bootstrapRequiresAdmin';
      return 'bootstrapRequiresAdmin';
    }
    if (!bgIsAdmin(signer, gs)) return 'notAnAdmin';
    const k = bgPropKind(p);
    if (k === 'introduceMember')
      return bgIsMember(p.introduceMember.key, gs) ? 'memberAlreadyExists' : null;
    if (k === 'removeMember')
      return bgIsMember(p.removeMember.key, gs) ? null : 'memberNotFound';
    return bgIsMember(p.changeRoles.key, gs) ? null : 'memberNotFound';
  }
  if (!bgIsAdmin(signer, gs)) return 'notAnAdmin';
  const pending = vtLookup(ev.approve.proposalId, gs.pendingProposals);
  if (!pending) return 'proposalNotFound';
  return pending.approvals.includes(signer) ? 'alreadyApproved' : null;
}

const bgCanonProposal = p => {
  const k = bgPropKind(p);
  if (k === 'introduceMember') return { introduceMember: {
    key: p.introduceMember.key, email: p.introduceMember.email,
    roles: p.introduceMember.roles.map(vtCanonRole) } };
  if (k === 'removeMember') return { removeMember: { key: p.removeMember.key } };
  return { changeRoles: { key: p.changeRoles.key,
    roles: p.changeRoles.roles.map(vtCanonRole) } };
};
const canonBaseState = gs => JSON.stringify({
  members: gs.members.map(([k, m]) => [k,
    { key: m.key, email: m.email, roles: m.roles.map(vtCanonRole) }]),
  pendingProposals: gs.pendingProposals.map(([pid, pp]) => [pid,
    { proposal: bgCanonProposal(pp.proposal), proposer: pp.proposer,
      approvals: pp.approvals.slice() }]) });

/* Crude-drift net for the base channel: approvals_nodup and
   proposer_mem_approvals, transcribed and asserted after every applied
   event. A violation means this transcription diverged from Fold.lean. */
/* @@CORE:base-b:END@@ */

/* @@CORE:base-c@@ */
function bgLawViolations(gs) {
  const out = [];
  for (const [pid, pp] of gs.pendingProposals) {
    if (new Set(pp.approvals).size !== pp.approvals.length)
      out.push(`approvals_nodup: «${pid}»`);
    if (!pp.approvals.includes(pp.proposer))
      out.push(`proposer_mem_approvals: «${pid}»`);
  }
  return out;
}

/* v1 consumer for the base stream (local schema kelgroups-base.trace):
   replay + verify each stored step against this transcription. */
function verifyBaseTraceV1(env, opts) {
  opts = opts || {};
  const fail = m => { throw new Error('base: ' + m); };
  if (!env || typeof env !== 'object') fail('non è un oggetto');
  if (env.schema !== 'kelgroups-base.trace') fail('schema sconosciuto');
  if (env.version !== 1) fail('versione non supportata');
  if (!env.initial || !Array.isArray(env.steps)) fail('forma non valida');
  let s = env.initial;
  if (canonBaseState(s) !== canonBaseState(bgEmpty()))
    fail('stato iniziale non vuoto');
  const states = [JSON.parse(canonBaseState(s))];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || !st.event || !st.result || !st.signer)
      fail(`passo ${i}: forma non valida`);
    if (canonBaseState(st.input) !== canonBaseState(s))
      fail(`passo ${i}: input discontinuo`);
    const verr = bgValidate(s, st.signer, st.event);
    if (st.result.tag === 'applied') {
      if (verr) fail(`passo ${i}: registrato applicato, la validazione rifiuta (${verr})`);
      const det = bgApply(s, st.signer, st.event);
      if (canonBaseState(det.state) !== canonBaseState(st.result.state))
        fail(`passo ${i}: post-stato divergente`);
      if (!!st.result.enacted !== !!det.enactment)
        fail(`passo ${i}: presenza enactment divergente`);
      s = det.state;
    } else if (st.result.tag === 'refused') {
      if (opts.appliedOnly) fail(`passo ${i}: rifiuto in un log di sessione`);
      if (!verr) fail(`passo ${i}: registrato rifiutato, la validazione applica`);
      if (st.result.error !== verr)
        fail(`passo ${i}: errore divergente — atteso=${st.result.error} osservato=${verr}`);
    } else fail(`passo ${i}: result.tag sconosciuto`);
    states.push(JSON.parse(canonBaseState(s)));
  });
  return { steps: env.steps.length, states };
}

/* --- Composition routing (accepted pin) -----------------------------------
   The 18-constructor route table of Composition.lean at the accepted pin.
   The committed claim gate re-derives this table by parsing the pinned
   source fresh on every run and REDs on any divergence (route-list drift),
   so this literal cannot silently drift from the accepted classifier.
   removeMember remains unexecuted (retired-by-#62 / R62-08). */
const EVENT_ROUTES = {
  addUser: 'direct', electResponsabile: 'baseEnacted',
  removeResponsabile: 'baseEnacted', removeMember: 'baseEnacted',
  openPurchase: 'direct', grantPermission: 'appDecided',
  denyPermission: 'appDecided', deposit: 'direct', withdraw: 'direct',
  transferCassa: 'direct', donate: 'direct', backdonate: 'appDecided',
  pledge: 'direct', acceptPledge: 'direct', refusePledge: 'direct',
  correctPledge: 'direct', closePurchase: 'direct', failPurchase: 'direct',
};

/* Temporary dated exemption: #62 / R62-08 will delete these four Event
   constructors. Coverage subtracts exactly these keys and must not treat
   the exemption as permission to implement them now. */
const EVENT_RETIREMENTS = {
  addUser: { status: 'retired-by-#62', issue: '#62',
    requirement: 'R62-08', declared: '2026-08-30' },
  electResponsabile: { status: 'retired-by-#62', issue: '#62',
    requirement: 'R62-08', declared: '2026-08-30' },
  removeMember: { status: 'retired-by-#62', issue: '#62',
    requirement: 'R62-08', declared: '2026-08-30' },
  removeResponsabile: { status: 'retired-by-#62', issue: '#62',
    requirement: 'R62-08', declared: '2026-08-30' },
};

const voteDerivedTag = tag => EVENT_ROUTES[tag] !== undefined && EVENT_ROUTES[tag] !== 'direct';

/* --- Governance credits: the model-level rejection ------------------------
   A vote-derived economic event is applied ONLY against route-appropriate
   evidence: a faithful base Enactment (baseEnacted) or a closed vote
   verdict (appDecided). Base credits are consumable exactly once; app
   evidence is the closure record itself (a question id can close at most
   once, so existence is exactly-once by construction). Without evidence the
   toy REFUSES the event before the machine ever sees it — electing four
   responsabili without one vote is no longer expressible. */

/* the economic event a faithful enactment authorizes, or null */
function bgCreditOf(enactment) {
  const p = enactment.pending.proposal;
  if (!bgProposalFaithful(p)) return null;
  if (bgPropKind(p) === 'removeMember')
    return { kind: 'removeMember', key: p.removeMember.key, pid: enactment.proposalId };
  const admin = p.changeRoles.roles.some(r => 'adminRole' in r);
  return { kind: admin ? 'electResponsabile' : 'removeResponsabile',
    key: p.changeRoles.key, pid: enactment.proposalId };
}

/* toy-side name↔key mapping shared by the governance walk: user ids map to
   the fixed decoration pool; keys are the lowercase names */
const kgKey = u => nm(u).toLowerCase();
const kgName = key => { const i = NAMES.findIndex(n => n.toLowerCase() === key);
  return i >= 0 ? NAMES[i] : key; };
const kgUid = key => { const i = NAMES.findIndex(n => n.toLowerCase() === key);
  return i >= 0 ? i : null; };
const permQid = c => 'permesso:' + c;

/* Governance over the combined seq (model-level, also on import/restore):
   walk the three verified streams in order, minting base credits from
   faithful enactments and requiring route-appropriate evidence before any
   vote-derived economic event. Throws on the first violation, so a log in
   which four responsabili were elected without one vote is REFUSED. */
function verifyGovernedSeq(n) {
  let bgs = bgEmpty(), kgs = vtEmpty();
  const credits = [];
  let ei = 0, ki = 0, bi = 0;
  for (const m of n.seq) {
    if (m === 'b') {
      const st = n.baseEnv.steps[bi++];
      const det = bgApply(bgs, st.signer, st.event);
      bgs = det.state;
      if (det.enactment) {
        const cr = bgCreditOf(det.enactment);
        if (cr) credits.push(cr);
      }
    } else if (m === 'k') {
      const st = n.kelEnv.steps[ki++];
      kgs = vtApply(kgs, st.signer, st.event).state;
    } else {
      const st = n.env.steps[ei++];
      const e = leanEventOf(st.event);
      if (!voteDerivedTag(e.tag)) continue;
      if (EVENT_ROUTES[e.tag] === 'baseEnacted') {
        const key = kgKey(e.target);
        const i = credits.findIndex(cr => cr.kind === e.tag && cr.key === key);
        if (i < 0)
          throw new Error(`governo: ${e.tag} senza enactment fedele del canale base`);
        credits.splice(i, 1);
      } else if (e.tag === 'grantPermission' || e.tag === 'denyPermission') {
        const want = e.tag === 'grantPermission' ? 'positive' : 'negative';
        const rec = kgs.closed.find(r => r.questionId === permQid(e.c));
        if (!rec || rec.verdict !== want)
          throw new Error(`governo: ${e.tag} senza verdetto ${want === 'positive' ? 'positivo' : 'negativo'} chiuso`);
      } else if (e.tag === 'backdonate') {
        throw new Error('governo: backdonate senza ponte evento-voto provato (NON PROVATO)');
      } else {
        throw new Error(`governo: ${e.tag} appDecided senza evidenza di canale`);
      }
    }
  }
}

/* @@CORE:base-c:END@@ */

/* @@EXPORTS@@ — module surface for the scenario runner (the page uses
   the inlined slices above in script scope; a future core.wasm adapter must
   provide exactly this surface) */
export {
  initState, attempt, canonState, leanEventOf, leanEventJson,
  verifyTraceV1, traceConformance, lawViolations, sumBal, escrowSum, bal,
  EV, CLAIMS, CHECK_RECEIPT, GUARD_CLAIMS, TAG_CLAIMS, EVENT_ROUTES,
  EVENT_RETIREMENTS,
  claimAudit, proofState, refusalProven, refusalClaims, refusalInventory,
  vtEmpty, vtApply, vtValidate, canonVoteState, verifyKelTraceV1,
  kelTraceConformance, kgLawViolations, vtTheta, vtFranchiseSize,
  bgEmpty, bgApply, bgValidate, canonBaseState, verifyBaseTraceV1,
  bgLawViolations, bgProposalFaithful, bgCreditOf, bgDigest, bgMajority,
  voteDerivedTag, verifyGovernedSeq, kgKey, kgName, kgUid, permQid,
  NAMES, PRESETS, nm, lbl,
};
