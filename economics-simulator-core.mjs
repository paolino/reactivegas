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
/* Identity is the substrate KelGroups.Key (a string). Toy display names are
   decoration keyed by the key itself; the pool below only seeds suggestions. */
const NAMES = ['Anna', 'Bruno', 'Elena', 'Carlo', 'Dora', 'Enzo',
               'Febe', 'Gaia', 'Hugo', 'Irma', 'Luca', 'Mara'];
let userLabels = {};
const nm = k => userLabels[k] !== undefined ? userLabels[k]
  : (NAMES.find(n => n.toLowerCase() === k) || String(k));
const PRESETS = ['Olio', 'Vino', 'Farina', 'Caffè', 'Miele', 'Pasta', 'Riso', 'Sale'];
let colLabels = {};                       // CollId -> toy label
const lbl = c => colLabels[c] !== undefined ? colLabels[c] : '?';

/* @@CORE:names:END@@ */

/* @@CORE:machine@@ */
/* --- State.lean helpers ------------------------------------------------
   State.empty := ⟨[], [], [], emptyVoteState⟩ — conti, casse, collections,
   votes and nothing else. Membership and roles live ONLY in the canonical
   GroupView handed to every transition (Reactivegas.State after #62); the
   payload has no second store to disagree with it. */

function emptyState() {
  // votes literal = KelGroups.Vote.emptyVoteState (inlined: this runs at
  // boot, before the vote slice below is evaluated)
  return { conti: [], casse: [], collections: [],
    votes: { openQuestions: [], closed: [] } };
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

/* --- KelGroups.GroupView: the one readable membership --------------------
   The canonical projection of GroupState.members (KelGroups.groupView):
   read-only, no path back to a writable aggregate. Every guard below reads
   membership and roles from the view handed to the transition — never from
   the payload, never from a cache. */
function hasAdmin(roles) { return roles.some(r => 'adminRole' in r); }
const lookupMember = (k, members) => {
  for (const [c, m] of members) if (c === k) return m;
  return null;
};
// KelGroups.GroupView.isAdmin u view
const isAdminView = (u, view) => {
  const m = lookupMember(u, view.members);
  return !!m && hasAdmin(m.roles);
};
// KelGroups.GroupView.isMember u view
const isMemberView = (u, view) => lookupMember(u, view.members) !== null;
// Reactivegas.memberKeys: member keys of the canonical view, in store order
const memberKeys = view => view.members.map(([k]) => k);
const groupAdmins = view => view.members.filter(([, m]) => hasAdmin(m.roles)).map(([k]) => k);

/* Reactivegas.isResponsabile view u := KelGroups.GroupView.isAdmin u view */
const isResponsabile = (view, u) => isAdminView(u, view);

/* Reserved non-member comune account inside conti (issue #48).
   Reactivegas.comuneId : KelGroups.Key := "comune" — no numeric sentinel,
   no Nat↔String bridge. It is never a standalone State field. */
const COMUNE_KEY = 'comune';
const comuneBal = s => bal(s.conti, COMUNE_KEY);
/* State.lean: stalled s := comuneBal s < 0 */
const stalled = s => comuneBal(s) < 0;

/* --- The sealed base hook (Reactivegas.Step, T6223) ----------------------
   Economic consequences of one committed base membership or role change,
   derived from the exact pre/post canonical views. There is no separately
   signable cleanup: a base change and its consequences are one transition. */

// absorbConto: the leaver's conto moves to the comune with no balance gate
function absorbConto(s, key) {
  return { ...s,
    conti: bump(bump(s.conti, key, -bal(s.conti, key)), COMUNE_KEY, bal(s.conti, key)) };
}

// windUpAdmin: open collections cancelled, every pledge refunded to its
// pledger, the cassa claim moved to the comune
function windUpAdmin(s, key) {
  const [rest, ps] = stripCollections(key, s.collections);
  return { ...s,
    conti: bump(refundAll(s.conti, ps), COMUNE_KEY, -bal(s.casse, key)),
    casse: bump(s.casse, key, -bal(s.casse, key)),
    collections: rest };
}

/* economicCleanup — exhaustive over KelGroups.BaseChange; null is a refusal
   that rejects the whole base transition with it. A stalled comune refuses
   departures and admin loss until a donation cures it. An admitted member
   has no economic consequence (a zero balance is READ from bal, never
   stored); a promotion returns the payload unchanged. */
function economicCleanup(change, pre, post, s) {
  if ('memberAdmitted' in change) return s;
  if ('memberRemoved' in change) {
    const key = change.memberRemoved;
    if (stalled(s)) return null;
    return absorbConto(isAdminView(key, pre) ? windUpAdmin(s, key) : s, key);
  }
  if ('rolesChanged' in change) {
    const key = change.rolesChanged;
    if (isAdminView(key, pre) && !isAdminView(key, post)) {
      if (stalled(s)) return null;
      return windUpAdmin(s, key);
    }
    return s;
  }
  throw new Error('BaseChange sconosciuto: ' + JSON.stringify(change));
}

/* baseHook θ: economic cleanup first, then the vote recompute every base
   change owes the question set — all open questions re-evaluated against the
   POST-transition franchise (a question can close because the electorate
   changed and no ballot was cast). null is a rejection. */
function baseHook(change, pre, post, s) {
  const cleaned = economicCleanup(change, pre, post, s);
  if (cleaned === null) return null;
  return { ...cleaned, votes: vtSweep(vtTheta, post, cleaned.votes) };
}

/* --- Guard labels (canonical Lean code + toy-side why text) ------------ */

const g = (code, why, law) => ({ code, why, law: law || null });
const AUTH = a => g('isResponsabile view a', `${nm(a)} non è responsabile`, 'AUTH');
const MEMBER = u => g('GroupView.isMember u view', `${nm(u)} non è nel gruppo`);
const PULL = c => g('pullCollection c = none', `nessun acquisto aperto «${lbl(c)}»`);

/* --- Step.lean: the rejecting transition, with named guards ------------
   attempt(s,e) → {ok:true, state, flow} | {ok:false, failed:[guard]}
   The accept/reject decision is exactly the Lean conjunction; `failed`
   lists every failing guard that is evaluable (guards depending on a
   failed pullCollection/splitUser cannot be evaluated and are skipped). */

/* The toy's BackdonateAuth exhibit. Step.lean takes authorization as an
   explicit caller-supplied argument and chooses no product policy (#47); the
   toy supplies ⊤ here and refuses backdonate at the governance boundary
   instead — the event↔vote bridge is NON PROVATO and is never invented. */
const TOY_AUTH = () => true;

/* --- Step.lean: the rejecting transition, with named guards ------------
   attempt(view, s, e, auth) → {ok:true, state, flow} | {ok:false, failed:[guard]}
   The accept/reject decision is exactly the Lean conjunction; `failed`
   lists every failing guard that is evaluable (guards depending on a
   failed pullCollection/splitUser cannot be evaluated and are skipped). */

function attempt(view, s, e, auth = TOY_AUTH) {
  const fails = [];
  const need = (cond, guard) => { if (!cond) fails.push(guard); return cond; };
  const rej = () => ({ ok: false, failed: fails });

  switch (e.tag) {

    case 'openPurchase': {
      need(isResponsabile(view, e.author), AUTH(e.author));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      return { ok: true, flow: `«${lbl(e.c)}»: assenso del gruppo — permesso di chiusura concesso (L2 ✓)`,
        state: { ...s, collections: [{ ...col, permitted: true }, ...rest] } };
    }

    case 'denyPermission': {
      const pulled = pullCollection(e.c, s.collections);
      need(pulled !== null, PULL(e.c));
      need(isResponsabile(view, e.author), AUTH(e.author));
      if (fails.length) return rej();
      const [col, rest] = pulled;
      const ps = [...col.accepted, ...col.pending];
      return { ok: true,
        flow: `acquisto «${lbl(e.c)}» negato e chiuso · rimborsi: ` +
          (ps.length ? ps.map(p => `conto ${nm(p.user)} +${p.amount}`).join(', ') : 'nessuno'),
        state: { ...s, conti: refundAll(s.conti, ps), collections: rest } };
    }

    case 'deposit': {
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(isMemberView(e.user, view), MEMBER(e.user));
      need(e.author !== e.user,
        g('a != u', 'autore e utente devono essere diversi'));
      need(0 <= e.v, g('0 ≤ v', `importo negativo (${e.v})`));
      if (fails.length) return rej();
      return { ok: true, flow: `accredito: conto ${nm(e.user)} +${e.v} · cassa ${nm(e.author)} +${e.v}`,
        state: { ...s, conti: bump(s.conti, e.user, e.v), casse: bump(s.casse, e.author, e.v) } };
    }

    case 'withdraw': {
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(isMemberView(e.user, view), MEMBER(e.user));
      need(e.author !== e.user,
        g('a != u', 'autore e utente devono essere diversi'));
      need(bal(s.conti, e.user) >= e.v,
        g('bal s.conti u ≥ v',
          `credito insufficiente: conto ${nm(e.user)} = ${bal(s.conti, e.user)} < ${e.v}`,
          'COVERED (L7)'));
      need(!stalled(s), g('!(decide (stalled s))',
        `il comune è in stallo (${comuneBal(s)}) — una donazione lo cura`, 'STALL'));
      if (fails.length) return rej();
      return { ok: true, flow: `prelievo: conto ${nm(e.user)} −${e.v} · cassa ${nm(e.author)} −${e.v}`,
        state: { ...s, conti: bump(s.conti, e.user, -e.v), casse: bump(s.casse, e.author, -e.v) } };
    }

    case 'transferCassa': {
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(isResponsabile(view, e.from_),
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
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(isMemberView(e.user, view), MEMBER(e.user));
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
      need(!stalled(s), g('!(decide (stalled s))',
        `il comune è in stallo (${comuneBal(s)}) — una donazione lo cura`, 'STALL'));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(!stalled(s), g('!(decide (stalled s))',
        `il comune è in stallo (${comuneBal(s)}) — una donazione lo cura`, 'STALL'));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
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
        need(!stalled(s), g('!(decide (stalled s))',
          `il comune è in stallo (${comuneBal(s)}) — una donazione lo cura`, 'STALL'));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
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
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(0 < e.v, g('0 < v', `l'importo deve essere positivo (${e.v})`));
      if (fails.length) return rej();
      return { ok: true,
        flow: `donazione: cassa ${nm(e.author)} +${e.v} · comune +${e.v}`,
        state: { ...s, casse: bump(s.casse, e.author, e.v),
          conti: bump(s.conti, COMUNE_KEY, e.v) } };
    }

    case 'backdonate': {
      const members = memberKeys(view);
      const n = members.length;
      need(isResponsabile(view, e.author), AUTH(e.author));
      need(0 < e.w, g('0 < w', `la quota deve essere positiva (${e.w})`));
      need(comuneBal(s) >= n * e.w,
        g('comuneBal s ≥ n * w',
          `comune insufficiente: ${comuneBal(s)} < ${n * e.w}`));
      need(auth(s, e.w),
        g('auth s w', 'nessuna evidenza di voto per la redistribuzione'));
      if (fails.length) return rej();
      let conti = bump(s.conti, COMUNE_KEY, -(n * e.w));
      for (const u of members) conti = bump(conti, u, e.w);
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
  'auth':              { c: 'Ogni evento è dichiarato da un responsabile eletto (AUTH)', k: 'teorema', d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 561 },
  'referente':         { c: "Accettare, rifiutare, correggere, chiudere o fallire: solo il referente dell'acquisto", k: 'teorema', d: 'auth_referente_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 172 },
  'permitted':         { c: 'La chiusura positiva richiede il permesso del gruppo (L2)', k: 'teorema', d: 'close_permission_to_close', f: 'lean/Reactivegas/Invariants.lean', l: 647 },
  'nopending':         { c: 'La chiusura richiede zero impegni pendenti', k: 'teorema', d: 'close_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 178 },
  'nopending-fail':    { c: 'Anche il fallimento richiede zero impegni pendenti', k: 'teorema', d: 'fail_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 188 },
  'covered':           { c: 'Un addebito che manderebbe un conto sotto zero è rifiutato (COVERED, L7)', k: 'teorema', d: 'solvent_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 1164 },
  'covered-guards':    { c: "Le guardie dell'impegno (autorità, unicità, copertura) sono esattamente quelle Lean", k: 'teorema', d: 'pledge_guard_inv', f: 'lean/Reactivegas/Invariants.lean', l: 154 },
  'l8':                { c: 'Un impegno duplicato nello stesso acquisto è rifiutato (L8)', k: 'teorema', d: 'pledge_rejected_when_member', f: 'lean/Reactivegas/Invariants.lean', l: 1241 },
  'l8-preserved':      { c: "L'unicità degli impegni si conserva a ogni evento", k: 'teorema', d: 'pledge_preserves_allUnique', f: 'lean/Reactivegas/Invariants.lean', l: 1269 },
  'exists-coll':       { c: 'Un evento su un acquisto inesistente è rifiutato', k: 'definizione', d: 'pullCollection', f: 'lean/Reactivegas/State.lean', l: 103 },
  'exists-impegno':    { c: "Accettare/rifiutare/correggere richiede che l'impegno esista", k: 'definizione', d: 'splitUser', f: 'lean/Reactivegas/State.lean', l: 85 },
  'atomic':            { c: 'Un tentativo è un passo atomico: rifiutato = stato invariato', k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 44 },
  'conservation':      { c: 'Σ casse − Σ conti − Σ impegni = 0 dopo ogni evento (L6)', k: 'teorema', d: 'conservation_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 430 },
  'solvency':          { c: 'Nessun conto va sotto zero in nessuno stato raggiungibile (L7)', k: 'teorema', d: 'not_insolvent_of_reach', f: 'lean/Reactivegas/Invariants.lean', l: 1184 },
  'solvency-reach':    { c: 'La solvibilità vale dallo stato iniziale lungo ogni esecuzione', k: 'teorema', d: 'reach_solvent', f: 'lean/Reactivegas/Invariants.lean', l: 1177 },
  'nonneg-impegni':    { c: 'Ogni impegno resta non negativo (L7)', k: 'teorema', d: 'solvent_preserved', f: 'lean/Reactivegas/Invariants.lean', l: 1164 },
  'unique':            { c: 'In ogni acquisto, al più un impegno per persona (L8)', k: 'definizione', d: 'uniquePledges', f: 'lean/Reactivegas/Predicates.lean', l: 40 },
  'accredito':         { c: "L'accredito muove insieme conto della persona e cassa del cassiere (+v, +v)", k: 'teorema', d: 'deposit_double_entry', f: 'lean/Reactivegas/Invariants.lean', l: 697 },
  'prelievo':          { c: 'Il prelievo è speculare: conto −v e cassa −v insieme', k: 'teorema', d: 'withdraw_double_entry', f: 'lean/Reactivegas/Invariants.lean', l: 707 },
  'giro':              { c: "Il giro sposta v dalla cassa del mittente a quella dell'autore", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 44 },
  'impegno-escrow':    { c: "All'impegno il denaro lascia subito il conto ed entra negli impegni dell'acquisto; il totale del gruppo non cambia (L3)", k: 'teorema', d: 'pledge_escrow_debit', f: 'lean/Reactivegas/Invariants.lean', l: 659 },
  'accept-effect':     { c: "L'accettazione sposta l'impegno da pendente ad accettato conservando l'unicità", k: 'teorema', d: 'uniquePledges_pend_cons', f: 'lean/Reactivegas/Invariants.lean', l: 1228 },
  'refuse-refund':     { c: "Il rifiuto rimborsa l'impegno pendente sul conto della persona", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 44 },
  'correct-conguaglio':{ c: 'La correzione conguaglia la differenza sul conto e non può mandarlo sotto zero', k: 'teorema', d: 'step_correct_inv', f: 'lean/Reactivegas/Invariants.lean', l: 284 },
  'close-payout':      { c: "La chiusura addebita l'intero raccolto sulla cassa del referente, e solo lì (L4)", k: 'teorema', d: 'close_spends_referente', f: 'lean/Reactivegas/Invariants.lean', l: 679 },
  'close-gone':        { c: "L'acquisto chiuso, negato o fallito sparisce dallo stato: la storia vive solo nel log", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 44 },
  'cassa-negativa':    { c: 'La cassa può andare sotto zero (ha pagato il fornitore): la solvibilità copre i conti, non le casse', k: 'definizione', d: 'solvent', f: 'lean/Reactivegas/Predicates.lean', l: 30 },
  'remove-refund-sum': { c: "Alla revoca il totale rimborsato è esattamente l'escrow degli acquisti chiusi", k: 'teorema', d: 'stripCollections_sum', f: 'lean/Reactivegas/State.lean', l: 351 },
  'open-referente':    { c: "Chi apre l'acquisto ne è il referente", k: 'definizione', d: 'step', f: 'lean/Reactivegas/Step.lean', l: 44 },
  'inv-pledge':        { c: "Il rifiuto di un impegno è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_pledge_inv', f: 'lean/Reactivegas/Invariants.lean', l: 227 },
  'inv-accept':        { c: "Il rifiuto di un'accettazione è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_accept_inv', f: 'lean/Reactivegas/Invariants.lean', l: 247 },
  'inv-refuse':        { c: "Il rifiuto di un rifiuto-impegno è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_refuse_inv', f: 'lean/Reactivegas/Invariants.lean', l: 265 },
  'inv-grant':         { c: "Il rifiuto di un assenso è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_grant_inv', f: 'lean/Reactivegas/Invariants.lean', l: 197 },
  'inv-deny':          { c: "Il rifiuto di un dissenso è spiegato esattamente dalle sue guardie Lean (inversione)", k: 'teorema', d: 'step_deny_inv', f: 'lean/Reactivegas/Invariants.lean', l: 211 },
  'refusal-unproved':  { c: "Questo rifiuto non ha ancora un lemma di inversione accettato nella sorgente Lean verificata (li completa #48): la spiegazione del rifiuto è NON PROVATA", k: 'NON PROVATO', d: null, f: null, l: null },
  'vote-wellformed':   { c: 'Ogni stato raggiunto dal fold di voto è ben formato', k: 'teorema', d: 'KelGroups.Vote.foldVote_wellFormed', f: 'lean/KelGroups/Vote/Invariants.lean', l: 688 },
  'vote-clean':        { c: 'Una posizione per persona: sì e no mai insieme, mai duplicati — la fuga «vota no» è sempre disponibile', k: 'teorema', d: 'KelGroups.Vote.ballots_nodup_disjoint', f: 'lean/KelGroups/Vote/Invariants.lean', l: 802 },
  'vote-nostale':      { c: 'Nessuna domanda aperta ha già un verdetto: lo sweep chiude tutto ciò che è deciso, nello stesso passo', k: 'teorema', d: 'KelGroups.Vote.open_questions_are_open', f: 'lean/KelGroups/Vote/Invariants.lean', l: 810 },
  'vote-partition':    { c: 'Aperte e chiuse partizionano le domande: nessuna sparisce in silenzio e ogni record chiuso ha un verdetto deciso (invariante escrow)', k: 'teorema', d: 'KelGroups.Vote.questions_partition', f: 'lean/KelGroups/Vote/Invariants.lean', l: 818 },
  'vote-noexpiry':     { c: 'Nessuna scadenza: non esiste un campo temporale che possa chiudere una domanda', k: 'teorema', d: 'KelGroups.Vote.no_expiry', f: 'lean/KelGroups/Vote/Invariants.lean', l: 877 },
  'vote-franchise':    { c: 'Ogni voto registrato fu espresso da chi era responsabile al momento del voto', k: 'teorema', d: 'KelGroups.Vote.franchise_of_tallies', f: 'lean/KelGroups/Vote/Invariants.lean', l: 1115 },
  'vote-policyfree':   { c: 'Il verdetto dipende dalla soglia solo attraverso il suo valore alla dimensione attuale dell’elettorato', k: 'teorema', d: 'KelGroups.Vote.verdictOf_threshold_congr', f: 'lean/KelGroups/Vote/Invariants.lean', l: 389 },
  'vote-verdict':      { c: 'Il verdetto è deciso in un solo punto: sì contro soglia, poi no contro la stessa soglia, altrimenti aperta', k: 'definizione', d: 'KelGroups.Vote.verdictOf', f: 'lean/KelGroups/Vote/State.lean', l: 85 },
  'vote-threshold-exhibit': { c: 'La soglia scelta dal toy è l’esibizione legacy (n+1)/2 — parametro esplicito, mai un default: con quattro votanti bastano due sì (o due no: il primo lato a soglia chiude)', k: 'definizione', d: 'KelGroups.Vote.legacyThreshold', f: 'lean/KelGroups/Vote/Types.lean', l: 44 },
  'vote-place':        { c: 'Cambiare voto sposta la posizione; ripetere lo stesso voto non conta mai doppio', k: 'definizione', d: 'KelGroups.Vote.placeBallot', f: 'lean/KelGroups/Vote/Fold.lean', l: 51 },
  'vote-sweep':        { c: 'Dopo OGNI evento lo sweep rivaluta ogni domanda aperta contro l’elettorato attuale e chiude ciò che è deciso', k: 'definizione', d: 'KelGroups.Vote.sweepClosures', f: 'lean/KelGroups/Vote/Fold.lean', l: 74 },
  'vote-apply':        { c: 'Un passo del fold è: effetto dell’evento, poi sweep incondizionato', k: 'definizione', d: 'KelGroups.Vote.applyVoteEvent', f: 'lean/KelGroups/Vote/Fold.lean', l: 118 },
  'vote-validate':     { c: 'Ogni rifiuto del voto è un errore distinto della macchina (notResponsabile, questionNotFound, …)', k: 'definizione', d: 'KelGroups.Vote.validateVoteEvent', f: 'lean/KelGroups/Vote/Validate.lean', l: 54 },
  'vote-open-empty':   { c: 'Una domanda si apre con conteggi VUOTI: chi propone non è contato come sì (divergenza deliberata dal legacy)', k: 'definizione', d: 'KelGroups.Vote.applyVoteEvent', f: 'lean/KelGroups/Vote/Fold.lean', l: 118 },
  'vote-admit-plain':  { c: 'Ammettere un membro è un evento semplice, mai una domanda e mai un voto (R-66)', k: 'definizione', d: 'KelGroups.Vote.VoteEvent', f: 'lean/KelGroups/Vote/Event.lean', l: 23 },
  'vote-permission-kind': { c: 'Una domanda-permesso ha esattamente un designato e solo il SUO voto la decide', k: 'definizione', d: 'KelGroups.Vote.QuestionKind', f: 'lean/KelGroups/Vote/Types.lean', l: 63 },
  'vote-closure-cause':{ c: 'Ogni chiusura dichiara la sua causa: soglia raggiunta, o cambio di elettorato che fa passare un conteggio ormai stantio', k: 'definizione', d: 'KelGroups.Vote.closureCause', f: 'lean/KelGroups/Vote/State.lean', l: 109 },
  'comp-routing':      { c: 'Instradamento TOTALE e senza wildcard dei 14 costruttori accettati: voteDerived e = true ↔ route e ≠ direct — provato al pin accettato della composizione (commit non ancora in questo albero)', k: 'teorema', d: 'Reactivegas.Composition.voteDerived_iff_not_direct', f: 'lean/Reactivegas/Composition.lean', l: 83, g: '934de7a8df136d86a8ad2caadbda99af60e58b59' },
  'comp-base-threshold': { c: 'Canale base: per un Enactment REALE di applyEventDetailed la cui proposta è nel vocabolario FEDELE (changeRoles, removeMember; introduceMember escluso per costruzione), gli assensi registrati raggiunsero la maggioranza dello stato precedente — CONDIZIONATO alla proposta fedele; l’evento economico e l’enactment restano parametri separati: NESSUN join, nessuna equivalenza di macchine', k: 'teorema', d: 'Reactivegas.Composition.baseEnacted_threshold_met', f: 'lean/Reactivegas/Composition.lean', l: 108, g: '934de7a8df136d86a8ad2caadbda99af60e58b59' },
  'comp-app-verdict':  { c: 'Canale app: l’eliminazione del verdetto è ESAUSTIVA — un ClosureRecord permette un evento esattamente quando chiuse positive o negative; open non permette nulla', k: 'teorema', d: 'Reactivegas.Composition.appDecided_verdict_exhaustive', f: 'lean/Reactivegas/Composition.lean', l: 139, g: '934de7a8df136d86a8ad2caadbda99af60e58b59' },
  'comp-witness':      { c: 'Testimone di raggiungibilità (anti-vacuità) del canale app: usa zeroThreshold, quindi NON è MAI evidenza della forza reale della soglia', k: 'definizione', d: 'Reactivegas.Composition.productionVerdictWitness', f: 'lean/Reactivegas/Composition.lean', l: 160, g: '934de7a8df136d86a8ad2caadbda99af60e58b59' },
  'kg-threshold':      { c: 'Canale base (trascritto qui): una delibera avviene solo al raggiungimento della maggioranza di assensi', k: 'teorema', d: 'enact_implies_threshold_met', f: 'lean/KelGroups/Invariants.lean', l: 342 },
  'kg-nodup':          { c: 'Canale base: gli assensi non contano mai doppio — la lista è senza duplicati', k: 'teorema', d: 'approvals_nodup', f: 'lean/KelGroups/Invariants.lean', l: 312 },
  'kg-proposer':       { c: 'Canale base: chi propone è già nel conteggio degli assensi', k: 'teorema', d: 'proposer_mem_approvals', f: 'lean/KelGroups/Invariants.lean', l: 317 },
  'kg-majority-def':   { c: 'Canale base: la maggioranza è definita come (admin+1)/2', k: 'definizione', d: 'KelGroups.majority', f: 'lean/KelGroups/State.lean', l: 50 },
  'kg-validate':       { c: 'Canale base: ogni rifiuto è un errore di validazione della macchina (notAnAdmin, alreadyApproved, …)', k: 'definizione', d: 'KelGroups.validateEvent', f: 'lean/KelGroups/Validate.lean', l: 180 },
  'kg-approve-guard':  { c: 'Canale base: approvare richiede un admin, una proposta esistente e nessun assenso precedente dello stesso firmatario', k: 'definizione', d: 'KelGroups.validateApproval', f: 'lean/KelGroups/Validate.lean', l: 116 },
  'kg-enact-effect':   { c: 'Canale base: la delibera applica la proposta ai membri e rimuove la pendente, in un passo', k: 'definizione', d: 'KelGroups.finishEnact', f: 'lean/KelGroups/Fold.lean', l: 18 },
  'kg-apply':          { c: 'Canale base: proporre inserisce la pendente con il proponente già assenziente e tenta subito la delibera', k: 'definizione', d: 'KelGroups.applyEventDetailed', f: 'lean/KelGroups/Fold.lean', l: 75 },
  'ev-donate':         { c: 'donate è direct: alza insieme la cassa dell\'autore e il conto comune riservato (non-membro in conti) di +v; rifiuta autore non responsabile e v non positivo. Nessun teorema di donazione è ancora proved (sorry #48)', k: 'NON PROVATO', d: null, f: null, l: null },
  'ev-backdonate':     { c: 'backdonate è appDecided: quota uguale w a ogni membro e −n*w dal comune; attempt non inventa backdonateAuthorized (sorry); il governo rifiuta senza ponte evento-voto (NON PROVATO)', k: 'NON PROVATO', d: null, f: null, l: null },
  'kg-setinsert':      { c: 'L’inserimento di posizione è idempotente per costruzione (substrato condiviso)', k: 'definizione', d: 'KelGroups.setInsert', f: 'lean/KelGroups/Types.lean', l: 46 },
  'kg-majority':       { c: 'Aritmetica della formula (n+1)/2: 0,1,1,2,2,3 per 0–5 — provata nella macchina fusa sullo stesso calcolo scelto qui come esibizione', k: 'teorema', d: 'majority_table', f: 'lean/KelGroups/Invariants.lean', l: 450 },
  'kg-tie':            { c: 'Con un numero pari la formula (n+1)/2 non è stretta: 2·soglia ≤ n — provato nella macchina fusa sullo stesso calcolo', k: 'teorema', d: 'majority_not_strict_on_even', f: 'lean/KelGroups/Invariants.lean', l: 459 },
  'join-vote-econ':    { c: 'Il PONTE fra verdetto di voto e permesso economico NON è provato: nessun teorema garantisce che Reactivegas consumi solo grantPermission derivati da un verdetto KelGroups.Vote (in attesa di #54 slice 2 / #48 backdonation)', k: 'NON PROVATO', d: null, f: null, l: null },
  'vote-model-status': { c: 'Il modello di voto richiesto (#54 slice A) è dimostrato sulla macchina produttiva: verdetti sì/no/aperta, dissenso e permessi per-persona sono modellati e le loro proposizioni portano prova senza sorry (scarico #48/#65)', k: 'teorema', d: 'KelGroups.Vote.foldVote_wellFormed', f: 'lean/KelGroups/Vote/Invariants.lean', l: 688 },
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
  sha: '3b3e0cc0911c91ada9436aa1139dfb1645b6431cc4536462932c3e3466134894',
  decls: ['auth_referente_guard_inv', 'close_guard_inv', 'close_permission_to_close',
    'close_spends_referente', 'conservation_preserved', 'deposit_double_entry',
    'fail_guard_inv', 'not_insolvent_of_reach',
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
    commit: '934de7a8df136d86a8ad2caadbda99af60e58b59',
    tree: 'b306b0ce6fc57b2b7fb880a5930f9740699cc637',
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
    'KelGroups.Vote.QuestionKind': 'provato',
    'KelGroups.Vote.VoteEvent': 'provato',
    'KelGroups.Vote.applyVoteEvent': 'provato',
    'KelGroups.Vote.ballots_nodup_disjoint': 'provato',
    'KelGroups.Vote.closureCause': 'provato',
    'KelGroups.Vote.foldVote_wellFormed': 'provato',
    'KelGroups.Vote.franchise_of_tallies': 'provato',
    'KelGroups.Vote.legacyThreshold': 'provato',
    'KelGroups.Vote.no_expiry': 'provato',
    'KelGroups.Vote.open_questions_are_open': 'provato',
    'KelGroups.Vote.placeBallot': 'provato',
    'KelGroups.Vote.questions_partition': 'provato',
    'KelGroups.Vote.sweepClosures': 'provato',
    'KelGroups.Vote.validateVoteEvent': 'provato',
    'KelGroups.Vote.verdictOf': 'provato',
    'KelGroups.Vote.verdictOf_threshold_congr': 'provato',
    'KelGroups.applyEventDetailed': 'provato',
    'KelGroups.finishEnact': 'provato',
    'KelGroups.majority': 'provato',
    'KelGroups.setInsert': 'provato',
    'KelGroups.validateApproval': 'provato',
    'KelGroups.validateEvent': 'provato',
    'approvals_nodup': 'provato',
    'auth_referente_guard_inv': 'provato',
    'close_guard_inv': 'provato',
    'close_permission_to_close': 'provato',
    'close_spends_referente': 'provato',
    'conservation_preserved': 'provato',
    'deposit_double_entry': 'provato',
    'enact_implies_threshold_met': 'provato',
    'fail_guard_inv': 'provato',
    'majority_not_strict_on_even': 'provato',
    'majority_table': 'provato',
    'not_insolvent_of_reach': 'provato',
    'pledge_escrow_debit': 'provato',
    'pledge_guard_inv': 'provato',
    'pledge_preserves_allUnique': 'provato',
    'pledge_rejected_when_member': 'provato',
    'proposer_mem_approvals': 'provato',
    'pullCollection': 'provato',
    'reach_solvent': 'provato',
    'solvent': 'provato',
    'solvent_preserved': 'provato',
    'splitUser': 'provato',
    'step': 'provato',
    'step_accept_inv': 'provato',
    'step_authorized': 'provato',
    'step_correct_inv': 'provato',
    'step_deny_inv': 'provato',
    'step_grant_inv': 'provato',
    'step_pledge_inv': 'provato',
    'step_refuse_inv': 'provato',
    'stripCollections_sum': 'provato',
    'uniquePledges': 'provato',
    'uniquePledges_pend_cons': 'provato',
    'withdraw_double_entry': 'provato',
  },
  sources: {
    'lean/Reactivegas/Types.lean': 'fb01d7d74bda2aa598fa83ea056bc4a53aaf10de21a240b6469bb38b88edaa1f',
    'lean/Reactivegas/State.lean': 'de2651bf189c3a30258250e4dd06eb27441fa60390e61256395249ffef8c01ef',
    'lean/Reactivegas/Step.lean': 'f498490c0a3eca78692d51119d6298115c456f79ce3526bf9210892a658f46fb',
    'lean/Reactivegas/Predicates.lean': '6ae3a61dea55f9c81b45de2e2898d416b17f441605561a4b96e93ed5324e9729',
    'lean/Reactivegas/Invariants.lean': '0ffbbfc7cfb61265ab7ae767ba3e7f2004ccad69653b86ec178cf017264667d1',
    'lean/Reactivegas/Composition.lean': '43b52f8b30bd4d96383bfd676660e78780a4d43436be76e74bd2ee5d6eee0743',
    'lean/KelGroups/Types.lean': '971a2a2ee774c3af63270e8f0bb8f1d5346f1da5c97f5faea1856dbe460acd07',
    'lean/KelGroups/State.lean': '25c2109e304fa9cdb084daabb267deb4a4a8a15749c9f66bae878d190fea49a6',
    'lean/KelGroups/Fold.lean': 'c6cbb818705db481b5f6fd8469e7a7b279fe2e6f9a9675f4cb262df570d679cf',
    'lean/KelGroups/Validate.lean': '109a0c5ac27cfbed634fde0abc82667b6246b5276a74a8c0091cafcce79ea36f',
    'lean/KelGroups/Invariants.lean': '86f200cb8dccd63d5d14a362e46286b7781040e2df1b214baedfb23e065a88e2',
    'lean/KelGroups/Event.lean': 'c89e135ba8ed919865eacdf7ed6c1c33953450997cbcbddbf3daf4510c61a449',
    'lean/KelGroups/Integration.lean': 'a23b27e8265024368e7b39399d4111548f9ce56ce18782550799d76199c706e2',
    'lean/KelGroups/Tests.lean': 'b9d8aa9f3d25292114f8ba5019bd55c99969ef9aecbaa2c646d1732a886f04c9',
    'lean/KelGroups.lean': 'fa7ca68cd42e9630deaa8ed0ce8943c6d0146f0ea716ea719514583f4c0a8ffc',
    'lean/KelGroups/Vote/Types.lean': 'dc6227f8c785b566aa08c3baab35deae5bc58d6ae420d1cf4e8df923b97fedc8',
    'lean/KelGroups/Vote/Event.lean': '84a186d5977f61c047d728f279ada07ad5908f77b0090942876132021e72b596',
    'lean/KelGroups/Vote/State.lean': '1a179920a0d5de725642b6254e223cfb3f61a1a06aed617cee59ee93b553214b',
    'lean/KelGroups/Vote/Fold.lean': '690d64668aac52013f03f9debde79f5f23c62825d7faae4615fb8b3a2df31e63',
    'lean/KelGroups/Vote/Validate.lean': '8d2ee979ca838b3fd4d25daca182ae77f10a00245dd6c7cc71edaddf18ae8846',
    'lean/KelGroups/Vote/Invariants.lean': 'a1c5dc24aaac89bbeb55d8459fde4f8b8d22d7224045d960edbb3826b9192da9',
    'lean/KelGroups/Vote/Tests.lean': 'd074d3943bcd2d3528c000458b7c80f0198991b73e603d98095fb757553dcec7',
  },
  /* Per-source immutable commit pins. Each pin is an origin/master ancestor
     whose blob sha256 equals the recorded source hash. Citation permalinks
     must use these SHAs, never a branch. Derived independently from the
     receipt hashes; not copied from a guessed branch tip. */
  sourcePins: {
    'lean/Reactivegas/Types.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/Reactivegas/State.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/Reactivegas/Step.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/Reactivegas/Predicates.lean': '3590c0015b84fd58004bf6fb44dd18b107304c48',
    'lean/Reactivegas/Invariants.lean': '3590c0015b84fd58004bf6fb44dd18b107304c48',
    'lean/Reactivegas/Composition.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Types.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/State.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Fold.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Validate.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Invariants.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Event.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Integration.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Tests.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Types.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Event.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/State.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Fold.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Validate.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Invariants.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
    'lean/KelGroups/Vote/Tests.lean': 'e6c59242ccf9b388053626c24446faaa2d7417fd',
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
  // solvent, first conjunct (Predicates.solvent): every MEMBER conto ≥ 0.
  // The reserved comune conto may legitimately go negative — that is the
  // stall the machine refuses to spend from, not an insolvency.
  if (s.conti.some(([k, v]) => k !== COMUNE_KEY && v < 0)) out.push('solvent (L7): conto negativo');
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
   lean/TraceDriverV1.lean: the seed envelopes emitted by Lean ToJson
   instances over the authoritative transition of lean/Reactivegas/Step.lean
   at the merged surface (State.empty, KelGroups.Key identities, stepEvent
   view s e auth, and the sealed base hook economicCleanup where the retired
   constructors used to be). Reproduce from a clean checkout with:
   `cd lean && lake env lean TraceDriverV1.lean`; the committed verifier at
   the repository root, `node economics-simulator-trace-gate.mjs`,
   regenerates it fresh, compares byte/structure against this fixture, and
   replays every envelope through THIS page's own production
   `traceConformance` (add `--selftest` for the negative controls).
   Raw output sha256:
   1bec2e6a76d33af88ca4329ed319818dc45f5fcd09fb3c4bf734a523ed0f8d99
   Each step carries its explicit input state, the canonical view the
   transition consumed (per-step, because membership moves between steps),
   and an applied result computed by Lean; base-change steps carry the
   pre/post views and run economicCleanup. Nothing here is hand-written or
   JS-generated. The seed corpus contains applied steps only. */

const LEAN_TRACES_V1 = {"A":{"initial":{"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"schema":"reactivegas-integrated.trace","steps":[{"event":{"direct":{"admitMember":{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}}},"input":{"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"memberAdmitted":"bruno"},"tag":"applied"},"signer":"anna"},{"event":{"propose":{"proposal":{"changeRoles":{"key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"rolesChanged":"bruno"},"tag":"applied"},"signer":"anna"},{"event":{"app":{"deposit":{"user":"bruno","v":100}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[],"conti":[["bruno",100]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"openPurchase":{"c":10}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[],"conti":[["bruno",100]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",100]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":10,"user":"bruno","v":30}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",100]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":10,"pending":[{"amount":30,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"acceptPledge":{"c":10,"user":"bruno"}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":10,"pending":[{"amount":30,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"openPurchase":{"c":11}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":11,"user":"bruno","v":20}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",70]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"direct":{"admitMember":{"email":"elena@toy.example","key":"elena","roles":[{"appRole":{"name":"socio"}}]}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"appRole":{"name":"socio"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"memberAdmitted":"elena"},"tag":"applied"},"signer":"anna"},{"event":{"propose":{"proposal":{"changeRoles":{"key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}}}},"input":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"appRole":{"name":"socio"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"rolesChanged":"elena"},"tag":"applied"},"signer":"anna"},{"event":{"app":{"openQuestion":{"kind":"collective","questionId":"q:sconto"}}},"input":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"cast":{"ballot":"assent","questionId":"q:sconto"}}},"input":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"propose":{"proposal":{"departure":"bruno"}}},"input":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[["depart:bruno",{"approvals":["anna"],"mutation":{"removeMember":{"key":"bruno"}},"proposer":"anna"}]]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"approve":{"proposalId":"depart:bruno"}},"input":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100]],"collections":[{"accepted":[],"id":11,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"},{"accepted":[{"amount":30,"user":"bruno"}],"id":10,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[["q:sconto",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[["depart:bruno",{"approvals":["anna"],"mutation":{"removeMember":{"key":"bruno"}},"proposer":"anna"}]]},"result":{"aggregate":{"members":[["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",100],["bruno",0]],"collections":[],"conti":[["bruno",0],["comune",100]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":{"memberRemoved":"bruno"},"tag":"applied"},"signer":"elena"}],"version":1},"B":{"initial":{"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"schema":"reactivegas-integrated.trace","steps":[{"event":{"direct":{"admitMember":{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}}},"input":{"members":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"memberAdmitted":"bruno"},"tag":"applied"},"signer":"anna"},{"event":{"propose":{"proposal":{"changeRoles":{"key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":{"rolesChanged":"bruno"},"tag":"applied"},"signer":"anna"},{"event":{"app":{"deposit":{"user":"bruno","v":50}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[],"collections":[],"conti":[],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50]],"collections":[],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"deposit":{"user":"anna","v":25}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50]],"collections":[],"conti":[["bruno",50]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[],"conti":[["bruno",50],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"openPurchase":{"c":7}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[],"conti":[["bruno",50],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":7,"user":"bruno","v":20}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",50],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[],"id":7,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",30],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"acceptPledge":{"c":7,"user":"bruno"}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[],"id":7,"pending":[{"amount":20,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",30],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":20,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",30],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"correctPledge":{"c":7,"user":"bruno","v":35}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":20,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",30],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":35,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",15],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"correctPledge":{"c":7,"user":"bruno","v":5}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":35,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",15],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"openQuestion":{"kind":"collective","questionId":"q:permesso:7"}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[],"openQuestions":[["q:permesso:7",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"cast":{"ballot":"assent","questionId":"q:permesso:7"}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[],"openQuestions":[["q:permesso:7",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"grantPermission":{"c":7}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":true,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"closePurchase":{"c":7}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",25]],"collections":[{"accepted":[{"amount":5,"user":"bruno"}],"id":7,"pending":[],"permitted":true,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"openPurchase":{"c":8}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":8,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":8,"user":"bruno","v":10}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":8,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":8,"pending":[{"amount":10,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"acceptPledge":{"c":8,"user":"bruno"}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":8,"pending":[{"amount":10,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[{"amount":10,"user":"bruno"}],"id":8,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":8,"user":"anna","v":15}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[{"amount":10,"user":"bruno"}],"id":8,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[{"amount":10,"user":"bruno"}],"id":8,"pending":[{"amount":15,"user":"anna"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",10]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"denyPermission":{"c":8}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[{"amount":10,"user":"bruno"}],"id":8,"pending":[{"amount":15,"user":"anna"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",10]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"app":{"openPurchase":{"c":9}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":9,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"bruno"},{"event":{"app":{"pledge":{"c":9,"user":"bruno","v":10}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":9,"pending":[],"permitted":false,"referente":"bruno"}],"conti":[["bruno",45],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":9,"pending":[{"amount":10,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":null,"tag":"applied"},"signer":"anna"},{"event":{"propose":{"proposal":{"changeRoles":{"key":"bruno","roles":[{"appRole":{"name":"socio"}}]}}}},"input":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",20]],"collections":[{"accepted":[],"id":9,"pending":[{"amount":10,"user":"bruno"}],"permitted":false,"referente":"bruno"}],"conti":[["bruno",35],["anna",25]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"result":{"aggregate":{"members":[["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"appRole":{"name":"socio"}}]}],["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]],"payload":{"casse":[["anna",50],["bruno",0]],"collections":[],"conti":[["bruno",45],["anna",25],["comune",-20]],"votes":{"closed":[{"cause":"tally","question":{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso:7","verdict":"positive"}],"openQuestions":[]}},"pendingBase":[]},"change":{"rolesChanged":"bruno"},"tag":"applied"},"signer":"anna"}],"version":1}};

/* Lean ToJson event shape {"tag":{...fields}} ↔ machine event {tag, ...} */
const leanEventOf = ej => {
  const tag = Object.keys(ej)[0];
  return { tag, ...ej[tag] };
};
const leanEventJson = e => {
  const { tag, ...fields } = e;
  return { [tag]: stripAuthor(fields) };
};

/* The economic stream's canonical projection: conti, casse, collections.
   The votes field is carried by the payload but NOT here — it evolves through
   the vote stream (and the base-hook sweep), which has its own envelope and
   its own continuity contract (canonVoteState). attempt() never reads it. */
const canonState = s => JSON.stringify({
  conti: s.conti, casse: s.casse,
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
  return verifyIntegratedV1(env, opts);
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
  let steps = 0;
  for (const name of names) {
    try {
      steps += verifyTraceV1(LEAN_TRACES_V1[name]).steps;
    } catch (e) {
      throw new Error(name + ' — ' + e.message);
    }
  }
  if (!names.length || steps === 0)
    throw new Error('corpus di conformità vuoto: nessuna traccia, nessuna prova');
  return { steps, corpus: 'seme-locale-solo-applicati' };
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

const vtEmpty = () => ({ openQuestions: [], closed: [] });
const vtLookup = (k, l) => { for (const [c, v] of l) if (c === k) return v; return null; };
const vtErase = (k, l) => { const i = l.findIndex(e => e[0] === k);
  return i < 0 ? l.slice() : l.slice(0, i).concat(l.slice(i + 1)); };
const vtInsert = (k, v, l) => [[k, v], ...vtErase(k, l)];
const vtAdjust = (k, f, l) => { const i = l.findIndex(e => e[0] === k);
  if (i < 0) return l.slice(); const out = l.slice(); out[i] = [k, f(out[i][1])]; return out; };
const vtSetInsert = (v, l) => l.includes(v) ? l.slice() : [v, ...l];
const vtListErase = (v, l) => { const i = l.indexOf(v);
  return i < 0 ? l.slice() : l.slice(0, i).concat(l.slice(i + 1)); };
/* franchise: read from the canonical view (R62-11) — the payload keeps no
   copy of it, so a tally can never be evaluated against stale membership */
const vtFranchise = view => view.members.filter(([, m]) => hasAdmin(m.roles)).map(([k]) => k);
const vtFranchiseSize = view => vtFranchise(view).length;
const vtIsResp = (k, view) => isAdminView(k, view);

/* a QuestionKind is either the string 'collective' (fieldless ctor) or
   { permission: { designee } }; this is the one place that distinction lives */
const vtPermKind = k =>
  (k !== null && typeof k === 'object' && k.permission) ? k.permission : null;

/* verdictOf — the single verdict site (State.lean): collective tallies are
   compared to θ(franchise(view)), assents first; a permission verdict is
   decided ONLY by the designee's own recorded ballot */
function vtVerdictOf(view, q) {
  const perm = vtPermKind(q.kind);
  if (perm) {
    const d = perm.designee;
    if (q.assents.includes(d)) return 'positive';
    if (q.dissents.includes(d)) return 'negative';
    return 'open';
  }
  const required = vtTheta(vtFranchiseSize(view));
  if (q.assents.length >= required) return 'positive';
  if (q.dissents.length >= required) return 'negative';
  return 'open';
}

function vtClosureCause(view, q, verdict) {
  if (verdict === 'positive')
    return q.assents.every(k => vtIsResp(k, view)) ? 'tally' : 'franchiseChange';
  if (verdict === 'negative')
    return q.dissents.every(k => vtIsResp(k, view)) ? 'tally' : 'franchiseChange';
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
   whose verdict under the CURRENT canonical view is decided; removal plus an
   appended closure record, as one operation */
function vtSweep(theta, view, gs) {
  const resolved = [], remaining = [];
  for (const [qid, q] of gs.openQuestions) {
    const v = vtVerdictOf(view, q);
    if (v === 'open') remaining.push([qid, q]);
    else resolved.push({ questionId: qid, question: q, verdict: v,
      cause: vtClosureCause(view, q, v) });
  }
  return { ...gs, openQuestions: remaining, closed: gs.closed.concat(resolved) };
}

/* applyVoteEventChecked: validate, effect, then the unconditional sweep —
   the view is a per-step parameter (the integrated caller hands the
   post-transition canonical view) */
function vtApply(view, gs, signer, ev) {
  let effected = gs;
  if ('openQuestion' in ev) {
    const { questionId, kind } = ev.openQuestion;
    if (vtLookup(questionId, gs.openQuestions) === null &&
        !gs.closed.some(r => r.questionId === questionId))
      effected = { ...gs, openQuestions: vtInsert(questionId,
        { kind, proposer: signer, assents: [], dissents: [] }, gs.openQuestions) };
  } else if ('cast' in ev) {
    const { questionId, ballot } = ev.cast;
    if (vtIsResp(signer, view)) {
      const q = vtLookup(questionId, gs.openQuestions);
      if (q) effected = { ...gs, openQuestions:
        vtInsert(questionId, vtPlaceBallot(signer, ballot, q), gs.openQuestions) };
    }
  } else if ('renounce' in ev) {
    /* slice-A no-op */
  }
  const swept = vtSweep(vtTheta, view, effected);
  return { state: swept, closedDelta: swept.closed.length - gs.closed.length };
}

/* validateVoteEvent transcription — returns null or the VoteError ctor name,
   the SAME identifiers Lean emits and the corpus compares exactly */
function vtValidate(view, gs, signer, ev) {
  if ('openQuestion' in ev)
    return vtIsResp(signer, view) ? null : 'notResponsabile';
  if ('cast' in ev) {
    if (!vtIsResp(signer, view)) return 'notResponsabile';
    return vtLookup(ev.cast.questionId, gs.openQuestions) ? null : 'questionNotFound';
  }
  if ('renounce' in ev)
    return vtIsResp(signer, view) ? null
      : 'notResponsabile';
  return null;
}

const vtCanonRole = r => 'adminRole' in r
  ? { adminRole: { admin: r.adminRole.admin } } : { appRole: { name: r.appRole.name } };
const vtCanonKind = k => { const p = vtPermKind(k);
  return p ? { permission: { designee: p.designee } } : 'collective'; };
const vtCanonQ = q => ({ kind: vtCanonKind(q.kind), proposer: q.proposer,
  assents: q.assents.slice(), dissents: q.dissents.slice() });
const canonVoteState = gs => JSON.stringify({
  openQuestions: gs.openQuestions.map(([qid, q]) => [qid, vtCanonQ(q)]),
  closed: gs.closed.map(r => ({ questionId: r.questionId, question: vtCanonQ(r.question),
    verdict: r.verdict, cause: r.cause })) });

/* crude net — transcription of the STATED invariants QuestionClean,
   partition, no-stale-open: this runtime check detects transcription drift
   between the Lean sources and this transcription. It does not itself
   establish any Lean proof; the cited declarations' proof states are
   whatever the claim receipt reports (CHECK_RECEIPT.axioms), which covers
   the cited declarations only, not every Lean statement. */
/* @@CORE:vote-a:END@@ */

/* @@CORE:vote-b@@ */
function kgLawViolations(view, gs) {
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
    if (vtVerdictOf(view, q) !== 'open')
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
  let gs = env.initial;
  if (canonVoteState(gs) !== canonVoteState(vtEmpty()))
    fail('stato iniziale non è emptyVoteState');
  const states = [JSON.parse(canonVoteState(gs))];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || !Array.isArray(st.view) || typeof st.signer !== 'string' ||
        !st.event || !st.result)
      fail(`passo ${i}: forma non valida`);
    if (canonVoteState(st.input) !== canonVoteState(gs))
      fail(`passo ${i}: input discontinuo — derivato=${short(canonVoteState(gs))} memorizzato=${short(canonVoteState(st.input))}`);
    const view = { members: st.view };
    const verr = vtValidate(view, JSON.parse(canonVoteState(gs)), st.signer, st.event);
    if (st.result.tag === 'applied') {
      if (verr) fail(`passo ${i}: registrato applicato, la validazione rifiuta (${verr})`);
      const det = vtApply(view, JSON.parse(canonVoteState(gs)), st.signer, st.event);
      if (canonVoteState(det.state) !== canonVoteState(st.result.state))
        fail(`passo ${i}: post-stato divergente — atteso=${short(canonVoteState(st.result.state))} osservato=${short(canonVoteState(det.state))}`);
      if (typeof st.result.closedCount === 'number' &&
          det.state.closed.length !== st.result.closedCount)
        fail(`passo ${i}: conteggio chiusure divergente — atteso=${st.result.closedCount} osservato=${det.state.closed.length}`);
      const viol = kgLawViolations(view, det.state);
      if (viol.length) fail(`passo ${i}: ${viol.join(' · ')}`);
      gs = det.state;
    } else if (st.result.tag === 'refused') {
      if (opts.appliedOnly) fail(`passo ${i}: rifiuto in un log di sessione`);
      if (!verr) fail(`passo ${i}: registrato rifiutato, la validazione applica`);
      const errId = typeof st.result.error === 'string'
        ? st.result.error : Object.keys(st.result.error || {})[0];
      if (errId !== verr)
        fail(`passo ${i}: errore di validazione divergente — atteso=${errId} osservato=${verr}`);
    } else fail(`passo ${i}: result.tag sconosciuto`);
    states.push(JSON.parse(canonVoteState(gs)));
  });
  return { steps: env.steps.length, states };
}

function kelTraceConformance() {
  const names = Object.keys(VOTE_TRACES_V1);
  let steps = 0;
  for (const name of names) {
    try { steps += verifyKelTraceV1(VOTE_TRACES_V1[name]).steps; }
    catch (e) { throw new Error(name + ' — ' + e.message + ' | stack: ' + (e.stack || '').split('\n').slice(1, 3).join(' | ')); }
  }
  if (!names.length || steps === 0)
    throw new Error('corpus di voto vuoto: nessuna traccia, nessuna prova');
  return { steps, corpus: 'seme-locale-vote-richiesto' };
}

/* VOTE_TRACES_V1: verbatim output of the COMMITTED producer
   lean/KelTraceDriverV1.lean over the authoritative KelGroups.Vote fold
   (applyVoteEvent θ view gs signer ev, legacyThreshold as the declared
   parameter). Reproduce with `cd lean && lake env lean KelTraceDriverV1.lean`;
   the committed verifier `node economics-simulator-vote-trace-gate.mjs`
   regenerates it fresh, compares with this fixture, and replays both through
   THIS page's production Vote transcription.
   Vote raw output sha256:
   b53eb59cb41916a267739f170c7cc7976239e84676fe9b0fc6c995a11944bba8
   Local schema kelgroups-vote.trace v1 (no shared proved schema with the
   frozen economic contract is claimed). The payload holds no membership
   (R62-11): the franchise is a parameter of every step, recorded per step.
   Seed: empty-tally opening (no proposer auto-assent), refused casts
   (notResponsabile, questionNotFound), position switch closing at threshold,
   idempotent re-cast, dissent-driven NEGATIVE verdict, per-person permission
   decided only by the designee, no-op renounce, and an OPEN (undecided)
   question in the final state. */

const VOTE_TRACES_V1 = {"V":{"initial":{"closed":[],"openQuestions":[]},"schema":"kelgroups-vote.trace","steps":[{"event":{"openQuestion":{"kind":"collective","questionId":"q:permesso-olio"}},"input":{"closed":[],"openQuestions":[]},"result":{"closedCount":0,"state":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"error":"notResponsabile","tag":"refused"},"signer":"dora","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:nessuna"}},"input":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"error":"questionNotFound","tag":"refused"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":0,"state":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"dissent","questionId":"q:permesso-olio"}},"input":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":0,"state":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":["bruno"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"bruno","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:permesso-olio"}},"input":{"closed":[],"openQuestions":[["q:permesso-olio",{"assents":["anna"],"dissents":["bruno"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[]},"tag":"applied"},"signer":"bruno","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"openQuestion":{"kind":"collective","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"elena","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":1,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"elena","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"dissent","questionId":"q:sconto"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"}],"openQuestions":[["q:sconto",{"assents":[],"dissents":["elena"],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[]},"tag":"applied"},"signer":"carlo","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"openQuestion":{"kind":{"permission":{"designee":"bruno"}},"questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[["q:incarico",{"assents":[],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[["q:incarico",{"assents":[],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"result":{"closedCount":2,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[["q:incarico",{"assents":["elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"tag":"applied"},"signer":"elena","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"cast":{"ballot":"assent","questionId":"q:incarico"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"}],"openQuestions":[["q:incarico",{"assents":["elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"}]]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"openQuestions":[]},"tag":"applied"},"signer":"bruno","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"openQuestion":{"kind":"collective","questionId":"q:aperta"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"openQuestions":[]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]},{"event":{"renounce":{"questionId":"q:aperta"}},"input":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"result":{"closedCount":3,"state":{"closed":[{"cause":"tally","question":{"assents":["bruno","anna"],"dissents":[],"kind":"collective","proposer":"anna"},"questionId":"q:permesso-olio","verdict":"positive"},{"cause":"tally","question":{"assents":[],"dissents":["carlo","elena"],"kind":"collective","proposer":"anna"},"questionId":"q:sconto","verdict":"negative"},{"cause":"tally","question":{"assents":["bruno","elena"],"dissents":[],"kind":{"permission":{"designee":"bruno"}},"proposer":"anna"},"questionId":"q:incarico","verdict":"positive"}],"openQuestions":[["q:aperta",{"assents":[],"dissents":[],"kind":"collective","proposer":"anna"}]]},"tag":"applied"},"signer":"anna","view":[["anna",{"email":"anna@toy.example","key":"anna","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["bruno",{"email":"bruno@toy.example","key":"bruno","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["elena",{"email":"elena@toy.example","key":"elena","roles":[{"adminRole":{"admin":"publicAdmin"}}]}],["carlo",{"email":"carlo@toy.example","key":"carlo","roles":[{"adminRole":{"admin":"publicAdmin"}}]}]]}],"threshold":"legacyThreshold","version":1}};

/* @@CORE:vote-c:END@@ */

/* @@CORE:base-a@@ */
/* The ONE aggregate-side base store of the toy: members + the pending base
   mutations awaiting their threshold (the integrated pendingBase, typed by
   the restricted vocabulary that cannot express admission). The app payload
   lives beside it and is touched ONLY by the sealed hook. */
const bgEmpty = () => ({ members: [], pendingBase: [] });
/* @@CORE:base-a:END@@ */

/* @@CORE:base-b@@ */
/* State.lean (merged): counts, majority, membership — one store */
const bgAdminCount = gs => gs.members.reduce((n, [, m]) => n + (hasAdmin(m.roles) ? 1 : 0), 0);
const bgMajority = gs => Math.floor((bgAdminCount(gs) + 1) / 2);   // KelGroups.majority
const bgIsAdmin = (k, gs) => isAdminView(k, { members: gs.members });
const bgIsMember = (k, gs) => isMemberView(k, { members: gs.members });

/* a Proposal is exactly one of the two restricted constructors
   (Reactivegas.Proposal, T6221: no admission — voted admission is not
   representable) */
const bgPropKind = p => 'departure' in p ? 'departure' : 'changeRoles';

/* proposalDigest is a machine PARAMETER (Step.lean): departure →
   'depart:<key>', changeRoles → 'roles:<key>'. Re-proposing the same digest
   replaces the pending entry (assocInsert), exactly as the substrate does. */
const bgDigest = p => 'departure' in p
  ? 'depart:' + p.departure
  : 'roles:' + p.changeRoles.key;

/* validateDirectAdmission: the sole member-insertion validator, three guards
   in a fixed order (admin signer; reserved key; already a member) */
function bgValidateAdmission(gs, signer, target) {
  if (!bgIsAdmin(signer, gs)) return 'notAnAdmin';
  if (target === COMUNE_KEY) return 'reservedKey';
  if (bgIsMember(target, gs)) return 'memberAlreadyExists';
  return null;
}
/* validateBaseMutation: admissibility of the two voted mutations */
function bgValidateMutation(gs, signer, p) {
  if (!bgIsAdmin(signer, gs)) return 'notAnAdmin';
  const key = 'departure' in p ? p.departure : p.changeRoles.key;
  return bgIsMember(key, gs) ? null : 'memberNotFound';
}
/* validateBaseApproval: an admin, a pending proposal, no prior assent */
function bgValidateApproval(gs, signer, pid) {
  if (!bgIsAdmin(signer, gs)) return 'notAnAdmin';
  const pend = vtLookup(pid, gs.pendingBase);
  if (!pend) return 'proposalNotFound';
  return pend.approvals.includes(signer) ? 'alreadyApproved' : null;
}

/* enactMutation: neither arm can introduce a key */
function bgEnactMutation(gs, p) {
  if ('departure' in p)
    return { ...gs, members: vtErase(p.departure, gs.members) };
  const { key, roles } = p.changeRoles;
  return { ...gs, members: vtAdjust(key, m => ({ ...m, roles }), gs.members) };
}
/* mutationChange: the observable BaseChange a mutation commits */
function bgMutationChange(p) {
  return 'departure' in p ? { memberRemoved: p.departure }
    : { rolesChanged: p.changeRoles.key };
}

/* commitBaseChange: the sealed hook sees the exact pre/post canonical views
   and the pre-transition payload; its output IS the payload the caller
   observes. A hook refusal rejects the whole transition with it. */
function bgCommitBaseChange(pre, post, change, payload) {
  const hooked = baseHook(change, { members: pre.members }, { members: post.members }, payload);
  if (hooked === null) return { refused: 'rejected' };
  return { gs: post, payload: hooked, change };
}

/* tryEnactBase: enact a pending mutation once its approvals reach the
   majority of the current franchise (read on the state that already holds
   the pending entry); otherwise leave it pending, no change */
function bgTryEnact(gs, pid, payload) {
  const pend = vtLookup(pid, gs.pendingBase);
  if (!pend) return { gs, payload, change: null };
  if (pend.approvals.length >= bgMajority(gs)) {
    const proposal = pend.proposal;
    const mutated = bgEnactMutation({ ...gs, pendingBase: vtErase(pid, gs.pendingBase) }, proposal);
    return bgCommitBaseChange(gs, mutated, bgMutationChange(proposal), payload);
  }
  return { gs, payload, change: null };
}

/* applyIntegratedEvent, base routes (direct admission + propose + approve):
   validation dominates the effect on every route. `payload` is the app
   payload the sealed hook runs over. */
function bgApply(gs, signer, ev, payload) {
  if ('direct' in ev) {
    const adm = ev.direct.admitMember;
    const verr = bgValidateAdmission(gs, signer, adm.key);
    if (verr) return { gs, payload, refused: verr };
    const post = { ...gs,
      members: vtInsert(adm.key, { key: adm.key, email: adm.email, roles: adm.roles }, gs.members) };
    const done = bgCommitBaseChange(gs, post, { memberAdmitted: adm.key }, payload);
    if (done.refused) return { gs, payload, refused: done.refused };
    return { ...done, enacted: true };
  }
  if ('propose' in ev) {
    const p = ev.propose.proposal;
    const verr = bgValidateMutation(gs, signer, p);
    if (verr) return { gs, payload, refused: verr };
    const pid = bgDigest(p);
    const proposed = { ...gs, pendingBase: vtInsert(pid,
      { proposal: p, proposer: signer, approvals: [signer] }, gs.pendingBase) };
    return bgTryEnact(proposed, pid, payload);
  }
  const pid = ev.approve.proposalId;
  const verr = bgValidateApproval(gs, signer, pid);
  if (verr) return { gs, payload, refused: verr };
  const pend = vtLookup(pid, gs.pendingBase);
  const approved = { ...pend, approvals: vtSetInsert(signer, pend.approvals) };
  return bgTryEnact({ ...gs, pendingBase: vtInsert(pid, approved, gs.pendingBase) }, pid, payload);
}

const bgCanonProposal = p => 'departure' in p
  ? { departure: { key: p.departure } }
  : { changeRoles: { key: p.changeRoles.key,
      roles: p.changeRoles.roles.map(vtCanonRole) } };
const canonBaseState = gs => JSON.stringify({
  members: gs.members.map(([k, m]) => [k,
    { key: m.key, email: m.email, roles: m.roles.map(vtCanonRole) }]),
  pendingBase: gs.pendingBase.map(([pid, pp]) => [pid,
    { proposal: bgCanonProposal(pp.proposal), proposer: pp.proposer,
      approvals: pp.approvals.slice() }]) });

/* Crude-drift net for the base channel: approvals_nodup and
   proposer_mem_approvals, transcribed and asserted after every applied
   event. A violation means this transcription diverged from the substrate. */
/* @@CORE:base-b:END@@ */

/* @@CORE:base-c@@ */
function bgLawViolations(gs) {
  const out = [];
  for (const [pid, pp] of gs.pendingBase) {
    if (new Set(pp.approvals).size !== pp.approvals.length)
      out.push(`approvals_nodup: «${pid}»`);
    if (!pp.approvals.includes(pp.proposer))
      out.push(`proposer_mem_approvals: «${pid}»`);
  }
  return out;
}

/* v1 consumer for the base stream (local schema kelgroups-base.trace):
   replay + verify each stored step against this transcription. The hook's
   payload consequences are verified by the ECONOMIC stream (its base-change
   steps); this stream verifies admissibility, membership and enactment. */
function verifyBaseTraceV1(env, opts) {
  opts = opts || {};
  const fail = m => { throw new Error('base: ' + m); };
  if (!env || typeof env !== 'object') fail('non è un oggetto');
  if (env.schema !== 'kelgroups-base.trace') fail('schema sconosciuto');
  if (env.version !== 1) fail('versione non supportata');
  if (!env.initial || !Array.isArray(env.steps)) fail('forma non valida');
  let s = env.initial;
  // the initial base state is the toy's GUARDED FOUNDING AGGREGATE
  // (Reactivegas.boot/productionWellFormed): the reserved comune is no
  // member, and every member entry names itself
  const badMember = s.members.some(([k, m]) => m.key !== k);
  if (badMember || bgIsMember(COMUNE_KEY, s))
    fail('stato iniziale non è un aggregato fondato ben formato');
  const states = [JSON.parse(canonBaseState(s))];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || !st.event || !st.result || !st.signer)
      fail(`passo ${i}: forma non valida`);
    if (canonBaseState(st.input) !== canonBaseState(s))
      fail(`passo ${i}: input discontinuo`);
    const verr = ('direct' in st.event)
      ? bgValidateAdmission(s, st.signer, st.event.direct.admitMember.key)
      : ('propose' in st.event)
        ? bgValidateMutation(s, st.signer, st.event.propose.proposal)
        : bgValidateApproval(s, st.signer, st.event.approve.proposalId);
    if (st.result.tag === 'applied') {
      if (verr) fail(`passo ${i}: registrato applicato, la validazione rifiuta (${verr})`);
      const det = bgApply(s, st.signer, st.event, emptyState());
      if (det.refused) fail(`passo ${i}: registrato applicato, la transizione rifiuta (${det.refused})`);
      if (canonBaseState(det.gs) !== canonBaseState(st.result.state))
        fail(`passo ${i}: post-stato divergente`);
      if (!!st.result.change !== !!det.change)
        fail(`passo ${i}: presenza del cambiamento divergente`);
      s = det.gs;
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

/* --- Composition routing (merged surface) ---------------------------------
   The 14-constructor route table of Composition.lean at the accepted pin:
   11 direct + 3 appDecided + 0 baseEnacted. The committed claim gate
   re-derives this table by parsing the pinned source fresh on every run and
   REDs on any divergence (route-list drift), so this literal cannot silently
   drift from the accepted classifier. */
const EVENT_ROUTES = {
  openPurchase: 'direct', grantPermission: 'appDecided',
  denyPermission: 'appDecided', deposit: 'direct', withdraw: 'direct',
  transferCassa: 'direct', donate: 'direct', backdonate: 'appDecided',
  pledge: 'direct', acceptPledge: 'direct', refusePledge: 'direct',
  correctPledge: 'direct', closePurchase: 'direct', failPurchase: 'direct',
};

const voteDerivedTag = tag => EVENT_ROUTES[tag] !== undefined && EVENT_ROUTES[tag] !== 'direct';

/* --- Governance walk over the combined seq (model-level, also on
   import/restore). The base credits are GONE: no economic event routes
   baseEnacted any more, so there is nothing for an enactment to authorize —
   promotion, demotion and departure ride the sealed hook inside their own
   transition. What remains governable: an appDecided economic event carries
   a closed verdict of the vote machine (the join stays NON PROVATO and is
   marked), and backdonate is refused outright. The walk also cross-checks
   ONE membership: every recorded step view must equal the walk's own
   membership at that point of the sequence. */
const canonMembers = members => JSON.stringify(members.map(([k, m]) => [k,
  { key: m.key, email: m.email, roles: m.roles.map(vtCanonRole) }]));

function verifyGovernedSeq(n) {
  let gs = n.baseEnv.initial || bgEmpty(), kgs = vtEmpty();
  let ei = 0, ki = 0, bi = 0;
  const checkView = (st, what) => {
    if (st.view !== undefined &&
        canonMembers(st.view) !== canonMembers(gs.members))
      throw new Error(`governo: ${what} registra una membership che non è quella del gruppo — una sola store esiste`);
  };
  for (const m of n.seq) {
    if (m === 'b') {
      const st = n.baseEnv.steps[bi++];
      const det = bgApply(gs, st.signer, st.event, emptyState());
      if (det.refused) throw new Error(`governo: evento di base rifiutato (${det.refused})`);
      gs = det.gs;
    } else if (m === 'k') {
      const st = n.kelEnv.steps[ki++];
      checkView(st, 'un passo di voto');
      kgs = vtApply({ members: gs.members }, kgs, st.signer, st.event).state;
    } else {
      const st = n.env.steps[ei++];
      const e = leanEventOf(st.event);
      if (e.tag === 'baseChange') {
        // a continuation of the preceding base transition: the POST view it
        // records must be exactly the walk's membership at this point
        if (st.postView !== undefined &&
            canonMembers(st.postView) !== canonMembers(gs.members))
          throw new Error('governo: un cambio di base registra una membership post che non è quella del gruppo — una sola store esiste');
        continue;
      }
      checkView(st, 'un evento economico');
      if (!voteDerivedTag(e.tag)) continue;
      if (e.tag === 'grantPermission' || e.tag === 'denyPermission') {
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

/* --- The integrated transition (ONE aggregate, ONE step) ------------------
   Transcription of KelGroups.applyIntegratedEvent over the toy aggregate
   {members, pendingBase, appFold}: validation dominates the effect on every
   route, and a committed base change runs the sealed hook EXACTLY ONCE —
   economic cleanup and the vote recompute inside the same transition, so
   there is no window in which the group has moved and its consequences have
   not, and no separately replayable hook step a log could forge, omit,
   duplicate or reorder. */

const permQid = c => 'permesso:' + c;

const canonAggregate = agg => {
  const gs = agg.payload !== undefined && agg.appFold === undefined
    ? { ...agg, appFold: agg.payload } : agg;
  return JSON.stringify({
    members: gs.members.map(([k, m]) => [k,
      { key: m.key, email: m.email, roles: m.roles.map(vtCanonRole) }]),
    pendingBase: (gs.pendingBase || []).map(([pid, pp]) => [pid,
      { proposer: pp.proposer, approvals: (pp.approvals || []).slice() }]),
    payload: { conti: gs.appFold.conti, casse: gs.appFold.casse,
      collections: gs.appFold.collections.map(c => ({ id: c.id,
        referente: c.referente, permitted: c.permitted,
        accepted: c.accepted.map(p => ({ user: p.user, amount: p.amount })),
        pending: c.pending.map(p => ({ user: p.user, amount: p.amount })) })),
      votes: JSON.parse(canonVoteState(gs.appFold.votes)) }
    });
};

const VOTE_TAGS = { openQuestion: 1, cast: 1, renounce: 1 };

/* INV-3 actor binding (F-01 class): event arguments never carry the actor.
   Agreement control against actual Lean source, two sides: (1) imported
   application-event args ↔ AppEvent (lean/Reactivegas/Types.lean:75 — no
   ctor declares `author`, so one inside payload args is foreign); (2)
   constructed machine event ↔ legacy Event (Types.lean:43 — every ctor
   declares `author`, and the signer is its only source, supplied
   separately: appFold takes signer apart from the event,
   lean/Reactivegas/Step.lean:181). payloadAuthorOf reports a payload
   author for the mismatch refusal; stripAuthor drops it so such a field
   can neither override the recorded signer at a merge site nor leak into
   stored events. Equal-author payloads are stripped, not refused: every
   honest pre-fix page export carries author==signer (old leanEventJson
   kept all fields, commitFlow always set args.author, pushIntegratedStep
   used it as the step signer), so refusal there would break roundtrip
   without privilege delta. Honest traffic never carries a mismatching one. */
const payloadAuthorOf = a =>
  (a && typeof a === 'object' &&
    Object.prototype.hasOwnProperty.call(a, 'author')) ? a.author : undefined;
const stripAuthor = a => {
  if (!a || typeof a !== 'object' || Array.isArray(a) ||
    !Object.prototype.hasOwnProperty.call(a, 'author')) return a;
  const { author: _dropped, ...rest } = a;
  return rest;
};

/* the integrated event of the toy aggregate, from the LEAN event JSON:
   {direct:{admitMember}} | {propose:{proposal}} | {approve:{proposalId}}
   | {app:{<AppEvent ctor>}} — the vote constructors openQuestion/cast/
   renounce ride the app route exactly as Reactivegas.appFold routes them. */
function applyIntegrated(gs, signer, ev) {
  if ('direct' in ev) {
    const adm = ev.direct.admitMember;
    const verr = bgValidateAdmission(gs, signer, adm.key);
    if (verr) return { refused: verr };
    const det = bgApply(gs, signer, ev, gs.appFold);
    if (det.refused) return { refused: det.refused };
    return { gs: { ...det.gs, appFold: det.payload }, payload: det.payload,
      change: det.change || null };
  }
  if ('propose' in ev || 'approve' in ev) {
    const verr = ('propose' in ev)
      ? bgValidateMutation(gs, signer, ev.propose.proposal)
      : bgValidateApproval(gs, signer, ev.approve.proposalId);
    if (verr) return { refused: verr };
    const det = bgApply(gs, signer, ev, gs.appFold);
    if (det.refused) return { refused: det.refused };
    return { gs: { ...det.gs, appFold: det.payload }, payload: det.payload,
      change: det.change || null };
  }
  if ('app' in ev) {
    const ae = ev.app;
    const tag = Object.keys(ae)[0];
    // a payload `author` mismatching the recorded signer is refused here,
    // at verification and replay alike (both run through applyIntegrated);
    // the merge below then only ever sees authorless args.
    const smuggled = payloadAuthorOf(ae[tag]);
    if (smuggled !== undefined && smuggled !== signer)
      return { refused: 'author-mismatch' };
    const args = stripAuthor(ae[tag]);
    const pre = { members: gs.members };
    if (!isMemberView(signer, pre)) return { refused: 'notAMember' };
    if (VOTE_TAGS[tag]) {
      const verr = vtValidate(pre, gs.appFold.votes, signer, ae);
      if (verr) return { refused: verr };
      const det = vtApply(pre, gs.appFold.votes, signer, ae);
      return { gs: { ...gs, appFold: { ...gs.appFold, votes: det.state } },
        payload: { ...gs.appFold, votes: det.state }, change: null };
    }
    const res = attempt(pre, gs.appFold, { tag, author: signer, ...args });
    if (!res.ok) return { refused: 'rejected', failed: res.failed };
    return { gs: { ...gs, appFold: res.state }, payload: res.state, change: null };
  }
  throw new Error('evento integrato sconosciuto: ' + JSON.stringify(ev));
}

/* the toy's founded aggregate (Reactivegas.boot): the founding admin arrives
   through the guarded initial aggregate, never by a self-admitting event */
const bootAggregate = () => {
  const anna = { key: 'anna', email: 'anna@toy.example',
    roles: [{ adminRole: { admin: 'publicAdmin' } }] };
  return { members: [['anna', anna]], pendingBase: [],
    appFold: emptyState() };
};

/* v1 consumer for the INTEGRATED stream: replay + verify each stored step
   against this transcription. The whole aggregate is the continuity
   contract: members, pending base and payload — votes included — must match
   step for step, and every accepted step is the result of a signed event.
   There is no hook-only event to accept. */
function verifyIntegratedV1(env, opts) {
  opts = opts || {};
  const short = x => { const j = typeof x === 'string' ? x : JSON.stringify(x);
    return j.length > 200 ? j.slice(0, 200) + '…' : j; };
  const fail = m => { throw new Error('integrato: ' + m); };
  if (!env || typeof env !== 'object') fail('non è un oggetto');
  if (env.schema !== 'reactivegas-integrated.trace') fail('schema sconosciuto');
  if (env.version !== 1) fail('versione non supportata: ' + env.version);
  if (!env.initial || !Array.isArray(env.steps)) fail('forma non valida');
  let gs = env.initial;
  if (gs.payload !== undefined && gs.appFold === undefined)
    gs = { ...gs, appFold: gs.payload };
  for (const req of ['members', 'pendingBase', 'appFold'])
    if (gs[req] === undefined) fail('aggregato iniziale senza ' + req);
  if (canonAggregate(gs) !== canonAggregate(bootAggregate()))
    fail('aggregato iniziale non è l’aggregato fondato — atteso=' +
      short(canonAggregate(bootAggregate())) + ' osservato=' + short(canonAggregate(gs)));
  const states = [JSON.parse(canonAggregate(gs))];
  env.steps.forEach((st, i) => {
    if (!st || !st.input || !st.event || !st.result || typeof st.signer !== 'string')
      fail(`passo ${i}: forma non valida (input, signer, event, result)`);
    for (const k of ['direct', 'propose', 'approve', 'app'])
      if (st.event[k] !== undefined && Object.keys(st.event).length !== 1)
        fail(`passo ${i}: più di un costruttore nell'evento integrato`);
    if (st.event.baseChange !== undefined)
      fail(`passo ${i}: un effetto del gancio non è un evento: deve nascere da una transizione firmata`);
    const stIn = st.input.payload !== undefined && st.input.appFold === undefined
      ? { ...st.input, appFold: st.input.payload } : st.input;
    if (canonAggregate(stIn) !== canonAggregate(gs))
      fail(`passo ${i}: input discontinuo — derivato=${short(canonAggregate(gs))} memorizzato=${short(canonAggregate(st.input))}`);
    if (st.result.tag !== 'applied')
      fail(`passo ${i}: rifiuto in un corpus di soli applicati`);
    const det = applyIntegrated(gs, st.signer, st.event);
    if (det.refused)
      fail(`passo ${i}: registrato applicato, la transizione rifiuta (${det.refused})`);
    if (canonAggregate(det.gs) !== canonAggregate(st.result.aggregate))
      fail(`passo ${i}: post-aggregato divergente — atteso=${short(canonAggregate(st.result.aggregate))} osservato=${short(canonAggregate(det.gs))}`);
    gs = det.gs;
    states.push(JSON.parse(canonAggregate(gs)));
  });
  return { steps: env.steps.length, states, final: gs };
}

/* governance over the integrated stream: an appDecided economic event
   carries its closed verdict from the SAME aggregate (the join stays NON
   PROVATO and is marked); backdonate is refused outright. The replay itself
   binds every hook effect to its signed base transition — there is no
   standalone cleanup event to accept, so forged/omitted/duplicated/
   reordered consequences cannot pass verification at all. */
function verifyGovernedIntegrated(env) {
  let gs = env.initial;
  if (gs.payload !== undefined && gs.appFold === undefined)
    gs = { ...gs, appFold: gs.payload };
  let ei = 0;
  for (const m of env.steps) {
    const st = env.steps[ei++];
    const stIn = st.input && st.input.payload !== undefined && st.input.appFold === undefined
      ? { ...st.input, appFold: st.input.payload } : st.input;
    const det = applyIntegrated(gs, st.signer, st.event);
    if (det.refused) throw new Error(`governo: evento integrato rifiutato (${det.refused})`);
    if ('app' in st.event) {
      const tag = Object.keys(st.event.app)[0];
      if (tag === 'grantPermission' || tag === 'denyPermission') {
        const want = tag === 'grantPermission' ? 'positive' : 'negative';
        const rec = gs.appFold.votes.closed.find(r => r.questionId === permQid(st.event.app[tag].c));
        if (!rec || rec.verdict !== want)
          throw new Error(`governo: ${tag} senza verdetto ${want === 'positive' ? 'positivo' : 'negativo'} chiuso`);
      } else if (tag === 'backdonate') {
        throw new Error('governo: backdonate senza ponte evento-voto provato (NON PROVATO)');
      }
    }
    gs = det.gs;
  }
}

function integratedTraceConformance() {
  const names = Object.keys(LEAN_TRACES_V1);
  let steps = 0;
  for (const name of names) {
    try { steps += verifyIntegratedV1(LEAN_TRACES_V1[name]).steps; }
    catch (e) { throw new Error(name + ' — ' + e.message); }
  }
  if (!names.length || steps === 0)
    throw new Error('corpus integrato vuoto: nessuna traccia, nessuna prova');
  return { steps, corpus: 'seme-locale-integrato-produzione' };
}
/* @@CORE:base-c:END@@ */

/* @@EXPORTS@@ — module surface for the scenario runner (the page uses
   the inlined slices above in script scope; a future core.wasm adapter must
   provide exactly this surface) */
export {
  emptyState, attempt, isResponsabile, memberKeys, economicCleanup, baseHook,
  COMUNE_KEY, comuneBal, stalled, bal, bump, canonState, leanEventOf, leanEventJson,
  verifyTraceV1, traceConformance, lawViolations, sumBal, escrowSum,
  EV, CLAIMS, CHECK_RECEIPT, GUARD_CLAIMS, TAG_CLAIMS, EVENT_ROUTES,
  claimAudit, proofState, refusalProven, refusalClaims, refusalInventory,
  vtEmpty, vtApply, vtValidate, vtSweep, vtTheta, vtFranchise, canonVoteState,
  verifyKelTraceV1, kelTraceConformance, kgLawViolations,
  bgEmpty, bgApply, bgValidateAdmission, bgValidateMutation, bgValidateApproval,
  bgMajority, bgDigest, bgPropKind, canonBaseState, verifyBaseTraceV1,
  bgLawViolations, voteDerivedTag, verifyGovernedIntegrated, permQid,
  applyIntegrated, verifyIntegratedV1, integratedTraceConformance, bootAggregate, canonAggregate,
  NAMES, PRESETS, nm, lbl, isAdminView, isMemberView, hasAdmin,
};
