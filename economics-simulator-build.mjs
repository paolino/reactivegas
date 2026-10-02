#!/usr/bin/env node
/*
 * economics-simulator-build.mjs — deterministic inliner of the ONE machine
 * core into the publishable zero-external-request single-file page.
 *
 * economics-simulator-core.mjs is the single source of the machine core;
 * economics-simulator.html embeds its slices verbatim between paired
 * markers `/* @@CORE:<id>@@ *​/ … /* @@CORE:<id>:END@@ *​/`. This script:
 *
 *   default:   regenerate the page in place from the core (splice every
 *              slice; report which slices changed);
 *   --check:   compare without writing; exit 1 listing every stale or
 *              forked slice (used by economics-simulator-scenario-gate.mjs
 *              to RED on a stale generated artifact or a forked copy).
 *
 * The slice set is discovered from the core file; a slice present in one
 * file and missing in the other is an error, so neither side can silently
 * grow a second transcription.
 */

import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = dirname(fileURLToPath(import.meta.url));
const argPath = flag => {
  const i = process.argv.indexOf(flag);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : null;
};
// --html/--core overrides exist for negative controls (checking a scratch
// copy); production callers use the defaults
const HTML = argPath('--html') || join(REPO, 'economics-simulator.html');
const CORE = argPath('--core') || join(REPO, 'economics-simulator-core.mjs');

const sliceRe = id => new RegExp(
  `/\\* @@CORE:${id}@@ \\*/\\n([\\s\\S]*?)/\\* @@CORE:${id}:END@@ \\*/`);

function slicesOf(text, what) {
  const ids = [...text.matchAll(/\/\* @@CORE:([a-z-]+)@@ \*\//g)].map(m => m[1]);
  if (!ids.length) throw new Error(`nessuna fetta @@CORE@@ in ${what}`);
  const out = {};
  for (const id of ids) {
    const m = text.match(sliceRe(id));
    if (!m) throw new Error(`fetta ${id} senza marcatore di chiusura in ${what}`);
    out[id] = m[1];
  }
  return out;
}

const core = readFileSync(CORE, 'utf8');
const html = readFileSync(HTML, 'utf8');
const coreSlices = slicesOf(core, 'economics-simulator-core.mjs');
const htmlSlices = slicesOf(html, 'economics-simulator.html');

const coreIds = Object.keys(coreSlices).sort();
const htmlIds = Object.keys(htmlSlices).sort();
if (JSON.stringify(coreIds) !== JSON.stringify(htmlIds)) {
  console.error('RED: insiemi di fette divergenti — core=[' + coreIds +
    '] pagina=[' + htmlIds + ']');
  process.exit(1);
}

const stale = coreIds.filter(id => coreSlices[id] !== htmlSlices[id]);

if (process.argv.includes('--check')) {
  if (stale.length) {
    console.error(`RED: artefatto generato stantio o biforcato — ${stale.length} ` +
      `fette divergenti dal core: ${stale.join(', ')}`);
    process.exit(1);
  }
  console.log(`GREEN: ${coreIds.length} fette del core identiche byte-per-byte ` +
    `fra economics-simulator-core.mjs e la pagina generata`);
  process.exit(0);
}

if (!stale.length) {
  console.log(`pagina già aggiornata: ${coreIds.length} fette identiche`);
  process.exit(0);
}
let out = html;
for (const id of stale)
  out = out.replace(sliceRe(id),
    `/* @@CORE:${id}@@ */\n${coreSlices[id]}/* @@CORE:${id}:END@@ */`);
writeFileSync(HTML, out);
console.log(`pagina rigenerata dal core: fette aggiornate: ${stale.join(', ')}`);
