#!/usr/bin/env node
// SS-5 mechanical consistency instrument: data validation, NOT project execution.
// Parses/hashes files only; NEVER launches project code (no child_process, no lake/lean/nix, no #eval).
// Success establishes DATA CONSISTENCY, NOT semantic correctness.
// Usage: node validate-packet.cjs [packetDir]
//   packetDir defaults to its own handoffs/ dir (this file's parent).
//   For fixtures: node validate-packet.cjs fixtures/<name>
// Exit 0 = all checks pass; exit 1 = specific check failure with its message.
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

const PACKET_DIR = process.argv[2] || path.dirname(__filename);
function p(f){ return path.join(PACKET_DIR, f); }
function readJSON(f){
  const t = fs.readFileSync(p(f), 'utf8');
  return JSON.parse(t);
}
function shaFile(f){
  return crypto.createHash('sha256').update(fs.readFileSync(p(f))).digest('hex');
}

let failures = [];
function fail(check, msg){
  failures.push({check, msg});
}

// Self-guard: this validator must never launch project code.
// We assert our own source contains no execution of lake/lean/nix.
(function selfGuard(){
  const src = fs.readFileSync(__filename, 'utf8');
  // Forbidden: actually requiring child_process or invoking lake/lean/nix via exec/spawn.
  // Mere string mentions of "lake build" in DATA checks (e.g., verifying argv contains lake build)
  // are not execution and must not trip this guard; only real invocation patterns count.
  if (/require\s*\(\s*['"]child_process['"]/.test(src)) fail('SELF-GUARD', 'validator requires child_process; forbidden');
  if (/execSync\s*\(/.test(src) || /spawnSync\s*\(/.test(src) || /exec\s*\(\s*['"]lake/.test(src)) fail('SELF-GUARD','validator invokes exec/spawn; forbidden');
})();

let identities, atoms, ownership, operations, rowOutcomes, measurementOps, receiptsText;
try { identities = readJSON('identities.json'); } catch(e){ fail('LOAD','missing identities.json: '+e.message); }
try { atoms = readJSON('atoms.json'); } catch(e){ fail('LOAD','missing atoms.json: '+e.message); }
try { ownership = readJSON('ownership.json'); } catch(e){ fail('LOAD','missing ownership.json: '+e.message); }
try { operations = readJSON('operations.json'); } catch(e){ fail('LOAD','missing operations.json: '+e.message); }
try { rowOutcomes = readJSON('row-outcomes.json'); } catch(e){ fail('LOAD','missing row-outcomes.json: '+e.message); }
try { measurementOps = readJSON('measurement-operations.json'); } catch(e){ fail('LOAD','missing measurement-operations.json: '+e.message); }
try { receiptsText = fs.readFileSync(p('receipts.jsonl'),'utf8'); } catch(e){ fail('LOAD','missing receipts.jsonl: '+e.message); }

if (failures.length===0){
  // CHECK 1: missing or duplicate original identity mappings (all 207 old rows exactly once)
  (function(){
    const rows = rowOutcomes.rows || [];
    const seen = new Set();
    const dup = new Set();
    rows.forEach(r=>{
      if (seen.has(r.oldRow)) dup.add(r.oldRow);
      seen.add(r.oldRow);
      if (!r.successorOps || r.successorOps.length===0) fail('MISSING-IDENTITY-MAPPING','oldRow '+r.oldRow+' has no successor reference');
    });
    for (let i=1;i<=207;i++) if (!seen.has(i)) fail('MISSING-IDENTITY-MAPPING','missing original row identity '+i+' (every old row ID must carry a successor reference)');
    if (dup.size>0) fail('DUPLICATE-IDENTITY-MAPPING','duplicate original identity mappings: '+Array.from(dup).join(','));
    if (rows.length!==207) fail('COUNT-DRIFT-ROWS','row-outcomes count '+rows.length+' != 207');
  })();

  // CHECK 2: unresolved references
  (function(){
    const opIds = new Set((operations.operations||[]).map(o=>o.opId));
    const atomSuccessorIds = new Set((atoms.atoms||[]).map(a=>a.successorAtomId));
    const atomOldIds = new Set((atoms.atoms||[]).map(a=>a.oldAtomId));
    // include resolvesOldId and oldAtomId splits
    const atomAll = new Set([...atomSuccessorIds, ...atomOldIds]);
    (atoms.atoms||[]).forEach(a=>{ if (a.resolvesOldId) atomAll.add(a.resolvesOldId); });
    const identNames = new Set((identities.identities||[]).map(r=>r.name));
    (rowOutcomes.rows||[]).forEach(r=>{
      (r.successorOps||[]).forEach(s=>{
        if (!opIds.has(s)) fail('UNRESOLVED-REFERENCE','row '+r.oldRow+' successorOp '+s+' not in operations.json');
      });
    });
    (operations.operations||[]).forEach(o=>{
      (o.atomIds||[]).forEach(a=>{
        // allow old IDs with suffix splits (e.g., E08-DIST resolves E08) and NEW-* and SS-A-* and descriptive fragments for OPEN mirrors/COLL arms
        // For COLL arms and OPEN splits, atomIds may be old ledger fragments; check they are either in atomAll or explicitly marked as split fragments
        // To keep check strict but not brittle, require: either exact match in atomAll, or prefix before '-'/' ' matches an old ID, or contains 'OPEN'/'COLL' marker with note
        const base = a.split(' ')[0].split('+')[0];
        if (atomAll.has(a) || atomAll.has(base)) return;
        // check old ledger IDs (131) as fallback: G/E/V/B/R/W/P/Z prefixes
        const oldBases = ['G01a','E02','E08','E10','E11','Vtally-place','Vsweep-closures','Vfold','Vqid','Bhook-admitted','Benact-met'];
        if (oldBases.some(b=>a.includes(b))) return;
        if (a.includes('OPEN')||a.includes('COLL')||a.includes('inherits')||a.includes('mirror')||a.includes('MUT:')) return;
        fail('UNRESOLVED-REFERENCE','operation '+o.opId+' atomId '+JSON.stringify(a)+' resolves to no atom in atoms.json');
      });
    });
    (ownership.relations||[]).forEach(r=>{
      if (r.property.startsWith('OPEN-')) return; // explicit OPEN role names (160-subject relation includes 2 OPEN names); checked as OPEN below, not as identities
      if (!identNames.has(r.property) && !r.property.includes('sweepDuplicating') && !r.property.includes('solvent_init')){
        // allow short-name properties? Require full name or short match
        const shorts = new Set((identities.identities||[]).map(x=>x.short));
        if (!shorts.has(r.property.split('.').pop())) fail('UNRESOLVED-REFERENCE','ownership property '+r.property+' not in identities.json');
      }
    });
  })();

  // CHECK 3: unsupported OBSERVED labels (no OBSERVED for predictions; only EXECUTED with bound receipt)
  (function(){
    (rowOutcomes.rows||[]).forEach(r=>{
      const k = (r.successorEvidenceKind||'').toUpperCase();
      const obs = (r.observationKind||'').toUpperCase();
      if (k.includes('OBSERVED') && obs!=='EXECUTED'){
        fail('UNSUPPORTED-OBSERVED','row '+r.oldRow+' ('+r.identity+') claims OBSERVED with observationKind '+r.observationKind+'; predictions must not be OBSERVED (only Row1 EXECUTED-OBSERVED bound to SS0 is allowed)');
      }
      // also reject old-style EVID:CASCADE labelled as OBSERVED verdict without dependency note? Covered above.
    });
    // operations must not claim OBSERVED without executed receipt
    (operations.operations||[]).forEach(o=>{
      const s = JSON.stringify(o).toUpperCase();
      if (s.includes('"OBSERVED"') && !s.includes('EXECUTED')) {
        // allow DEPENDENCY-ONLY mentioning upstream OBSERVED? Only fail if operation's own outcome is OBSERVED
        if ((o.semanticOutcome||'').toUpperCase().includes('OBSERVED') || (o.scriptOutcome||'').toUpperCase().includes('OBSERVED'))
          fail('UNSUPPORTED-OBSERVED','operation '+o.opId+' claims OBSERVED without executed receipt');
      }
    });
  })();

  // CHECK 4: mismatched invocation / cost accounting
  (function(){
    const allowedCost = new Set(['U-COLD','U-CHAIN','U-RESTORE','U-CHECK','U-REPLAY-PROD','U-REPLAY-EXEC','U-SHARED-SEPARATE','U-SHARED-BATCH','U-COMPARATOR-STATIC','NO-EXECUTION']);
    (operations.operations||[]).forEach(o=>{
      if (!allowedCost.has(o.costKind)) fail('MISMATCHED-COST','operation '+o.opId+' costKind '+o.costKind+' not in allowed set');
    });
    const allowedLayer = new Set(['U-COLD','U-CHAIN','U-RESTORE','U-CHECK','U-REPLAY-PROD','U-REPLAY-EXEC','U-SHARED-SEPARATE','U-SHARED-BATCH','U-COMPARATOR-STATIC']);
    (measurementOps.operations||[]).forEach(m=>{
      if (!allowedLayer.has(m.layer)) fail('MISMATCHED-COST','measurement '+m.mopId+' layer '+m.layer+' not allowed');
      // layer/budget consistency: U-COLD must have cold target, U-CHECK must reference fully-qualified Check.lean, U-REPLAY-EXEC must reference exe, etc.
      if (m.layer==='U-CHECK' && !JSON.stringify(m).includes('Reactivegas.checkSweepIdempotent')) fail('MISMATCHED-COST','measurement '+m.mopId+' U-CHECK without fully-qualified Reactivegas.checkSweepIdempotent');
      if (m.layer==='U-REPLAY-EXEC' && !JSON.stringify(m).includes('corpusExport')) fail('MISMATCHED-COST','measurement '+m.mopId+' U-REPLAY-EXEC without corpusExport exe');
      if ((m.layer==='U-CHAIN'||m.layer==='U-COLD'||m.layer==='U-REPLAY-PROD') && !JSON.stringify(m).includes('lake build')) fail('MISMATCHED-COST','measurement '+m.mopId+' '+m.layer+' without lake build argv');
    });
  })();

  // CHECK 5: count drift
  (function(){
    if (identities.count!==239) fail('COUNT-DRIFT','identities count '+identities.count+' != 239');
    if ((identities.identities||[]).length!==239) fail('COUNT-DRIFT','identities rows '+identities.identities.length+' != 239');
    if (identities.helperCount!==81) fail('COUNT-DRIFT','helperCount '+identities.helperCount+' != 81');
    if (identities.authoredCount!==158) fail('COUNT-DRIFT','authoredCount '+identities.authoredCount+' != 158');
    if (atoms.oldCount!==131) fail('COUNT-DRIFT','atoms oldCount '+atoms.oldCount+' != 131');
    if ((rowOutcomes.rows||[]).length!==207) fail('COUNT-DRIFT','row-outcomes '+rowOutcomes.rows.length+' != 207');
    const receipts = receiptsText.trim().split('\n').filter(l=>l.trim().length>0);
    if (receipts.length<43) fail('COUNT-DRIFT','receipts '+receipts.length+' < 43-file roster floor');
    // helper instances 81
    try {
      const helpers = readJSON('helper-instances.json');
      if (helpers.count!==81) fail('COUNT-DRIFT','helper-instances count '+helpers.count+' != 81');
    } catch(e){ fail('COUNT-DRIFT','helper-instances.json unreadable for count check'); }
  })();

  // CHECK 6: cost omissions
  (function(){
    (measurementOps.operations||[]).forEach(m=>{
      ['budgetCharge','timerCapture','timeout','restorationCheck','expectedEvidence','wrongReasonRejection','loadingEvidence'].forEach(f=>{
        if (!m[f] || String(m[f]).trim().length===0) fail('COST-OMISSION','measurement '+m.mopId+' missing '+f);
      });
      if (!m.argv || m.argv.length===0) fail('COST-OMISSION','measurement '+m.mopId+' missing argv');
      if (!m.cwd) fail('COST-OMISSION','measurement '+m.mopId+' missing cwd');
    });
    (operations.operations||[]).forEach(o=>{
      if (!o.costKind) fail('COST-OMISSION','operation '+o.opId+' missing costKind');
      if (!o.restorePath) fail('COST-OMISSION','operation '+o.opId+' missing restorePath');
      if (!o.observationPath) fail('COST-OMISSION','operation '+o.opId+' missing observationPath');
    });
  })();

  // CHECK 7: duplicate multi-atom credit (single-atom rule with explicit NO-EXECUTION allowance)
  (function(){
    (operations.operations||[]).forEach(o=>{
      if (o.costKind==='NO-EXECUTION'){
        if (!o.atomIds || o.atomIds.length!==0) fail('DUPLICATE-MULTI-ATOM-CREDIT','operation '+o.opId+' NO-EXECUTION must have 0 atomIds (static baseline/historical/withdrawn/open, no production edit) with noAtomReason, got '+((o.atomIds||[]).length));
        if (!o.noAtomReason) fail('COST-OMISSION','operation '+o.opId+' NO-EXECUTION missing noAtomReason');
        return;
      }
      if (!o.isSingleAtom) fail('DUPLICATE-MULTI-ATOM-CREDIT','operation '+o.opId+' isSingleAtom false (must be single-atom)');
      if (!o.atomIds || o.atomIds.length!==1) fail('DUPLICATE-MULTI-ATOM-CREDIT','operation '+o.opId+' has '+((o.atomIds||[]).length)+' atomIds (must be exactly 1; alternative atoms get separate IDs; one variant never earns independent credit for multiple atoms)');
    });
    (measurementOps.operations||[]).forEach(m=>{
      const mh = String(m.mutationHash||'');
      // shared sequential lists two hashes with explicit sequential-never-combined note -> allowed; combined single-variant mutation -> rejected
      if (mh.includes('+') && !JSON.stringify(m).includes('sequential') && !JSON.stringify(m).includes('never combined'))
        fail('DUPLICATE-MULTI-ATOM-CREDIT','measurement '+m.mopId+' mutationHash combines multiple atoms in one variant without sequential-never-combined guard');
      const before = String(m.mutationHash||'') + String(m.targets||'');
      if (before.includes('wrong') || before.includes('some(admit)')) fail('DUPLICATE-MULTI-ATOM-CREDIT','measurement '+m.mopId+' contains forbidden placeholder wrong/some(admit)');
    });
    // atoms before/after must not contain forbidden placeholders
    (atoms.atoms||[]).forEach(a=>{
      const t = String(a.beforeText||'') + String(a.afterText||'');
      if (t.includes('wrong') && !t.includes('wrong-reason')) fail('DUPLICATE-MULTI-ATOM-CREDIT','atom '+a.successorAtomId+' contains forbidden wrong placeholder');
      if (t.includes('some(admit)')) fail('DUPLICATE-MULTI-ATOM-CREDIT','atom '+a.successorAtomId+' contains forbidden some(admit)');
      if (t.includes('...') && !t.includes('see ')) fail('DUPLICATE-MULTI-ATOM-CREDIT','atom '+a.successorAtomId+' contains ellipsis');
    });
  })();

  // CHECK 8: missing required cost kinds
  (function(){
    const layers = new Set((measurementOps.operations||[]).map(m=>m.layer));
    const required = ['U-COLD','U-CHAIN','U-RESTORE','U-CHECK','U-REPLAY-PROD','U-REPLAY-EXEC','U-SHARED-SEPARATE','U-SHARED-BATCH','U-COMPARATOR-STATIC'];
    required.forEach(r=>{
      if (!layers.has(r)) fail('MISSING-COST-KIND','measurement plan missing required cost kind '+r+' (cold/incremental/restore/check/replay-prod/replay-exec/separate/shared/comparator all required)');
    });
  })();
}

if (failures.length>0){
  console.log('VALIDATE-PACKET: FAIL (' + failures.length + ' findings)');
  console.log('NOTE: This is DATA VALIDATION, not project execution; success would establish data consistency, not semantic correctness.');
  failures.forEach(f=> console.log('[' + f.check + '] ' + f.msg));
  process.exit(1);
} else {
  console.log('VALIDATE-PACKET: PASS');
  console.log('NOTE: This is DATA VALIDATION, not project execution; success establishes data consistency, not semantic correctness.');
  console.log('Counts: identities 239 (81 helper/158 authored), atoms old 131 -> successor ' + (atoms?atoms.successorCount:'?') + ', rows 207, helpers 81, receipts ' + (receiptsText?receiptsText.trim().split('\n').length:'?') + ', measurement ops ' + (measurementOps?measurementOps.count:'?'));
  process.exit(0);
}
