const fs = require("fs");
const IN = "admitted/OPMAP-v7-requirement-verdict-grounds.txt";
const OUT = "handoffs/OPMAP-v8-requirement-verdict-grounds.txt";
const lines = fs.readFileSync(IN, "utf8").split("\n");
if (lines.length !== 207 && !(lines.length === 208 && lines[207] === "")) {
  console.error("ABORT: expected 207 lines, got " + lines.length); process.exit(1);
}
const N = 207;
const out = [];
const openExt = {}; for (let k = 0; k < 31; k++) openExt[170 + k] = k + 1;
for (let ln = 1; ln <= N; ln++) {
  if (ln > 10000) { console.error("ABORT: iteration ceiling"); process.exit(1); }
  let L = lines[ln - 1];
  const cols = L.split("|");
  if (cols.length !== 5 && cols.length !== 6) { console.error("ABORT: line " + ln + " has " + cols.length + " cols"); process.exit(1); }
  let op = cols[0], req = cols[1], verdict = cols[2], col4 = cols[3], col5 = cols[4], ground = cols[5];
  const rep = function (o) { out.push(o); };
  if (ln === 89 || ln === 90) {
    verdict = "WITHDRAWN-DUPLICATE";
    col5 = "WITHDRAWN (phantom KelGroups.* namespace; sole declaration at Reactivegas/Invariants.lean:" + (ln === 89 ? "1616" : "1600") + "; real row " + (ln === 89 ? "L97" : "L98") + ")";
    ground = "GROUND:duplicate-spelling, no distinct declaration (A-ASSESSMENT-51)";
  } else if (ln === 25) {
    ground = "GROUND:(P) proof-shape sensitivity: statement excludes comuneId (Invariants.lean:890-895), donate proof rw bal_bump_ne breaks; statement stays true (Addendum-5 input, re-verified)";
    col5 = col5.replace("RED (donate-arm case)", "PREDICTED-RED (donate-arm rw-shape)");
  } else if (ln === 21) {
    ground = "GROUND:(c) statement false at witness (responsabile signer, v=0: mutated guard admits, guard conjunct (… && decide (0 < 0)) = true fails); post-state equation unaffected";
    col5 = "PREDICTED-RED (guard-conjunct falsified at v=0)";
  } else if (ln === 34) {
    ground = "GROUND:(P) proof-shape sensitivity: statement stays true under mutant; List.erase_subset applied to non-erased membership fails (assent arm); OP-25 pattern";
    col5 = col5.replace("RED (definitional)", "PREDICTED-RED (exact-term mismatch, statement true)");
  } else if (ln === 40) {
    verdict = "PREDICTED-SURVIVE";
    col5 = "PREDICTED-GREEN (statement quantifies .closed only; proof mem_append/mem_filterMap untouched by open-retention filter mutant)";
    ground = "GROUND:survive-predicted, (a) WITHDRAWN (A-ASSESSMENT-51 L40)";
  } else if (ln === 43) {
    verdict = "OBSERVED";
    col5 = "upstream KelGroups.Vote.sweepClosures_open_mem (cascade: mpr needs verdict-test shape; statement itself stays true)";
    ground = "GROUND:cascade via open_mem, (a) WITHDRAWN (A-ASSESSMENT-51 L43)";
  } else if (ln === 3) {
    verdict = "OBSERVED";
    col5 = "upstream the 14 step_*_inv inversion lemmas (statement authorizedStep ignores both states; effect mutants keep guards; RED only via consumed lemmas)";
    ground = "GROUND:cascade via 14 inversion lemmas, (a) WITHDRAWN (A-ASSESSMENT-51 L3)";
  } else if (ln === 2) {
    col5 = "PREDICTED-RED per run (14 arms, carriers in A-ASSESSMENT-51 L2), NOT observed; no Phase-2 execution has occurred";
  } else if (ln === 61 || ln === 63) {
    verdict = "PREDICTED-SURVIVE";
    col5 = ln === 61
      ? "PREDICTED-GREEN (hypothesis-driven simp never unfolds validate body; vacuous under mutant)"
      : "PREDICTED-GREEN (cast path Validate.lean:60-64 untouched by :58 openQuestion-arm mutant)";
    ground = "GROUND:survive-predicted, (a) WITHDRAWN (A-ASSESSMENT-51 " + (ln === 61 ? "L61" : "L63") + ")";
  } else if (ln === 94) {
    col5 = "OBSERVED (upstream KelGroups.tryEnactDetailed_enactment_threshold_met, consumed at KelGroups/Invariants.lean:357 and :366)";
  }
  if (verdict === "KILL") col5 = col5.replace(/(?<!PREDICTED-)\bRED\b/g, "PREDICTED-RED");
  if (ln === 201) col5 += " [corroborated t54-PARTITION ea9882087e28/7846a04f5984; exact-atom identity unestablished, not RECEIPT-BOUND]";
  col4 = col4.replace("Validate.lean:146:if-isAdmin", "Validate.lean:145:if-isAdmin")
             .replace("Validate.lean:147:if-target-reserved", "Validate.lean:146:if-target-reserved")
             .replace("Validate.lean:148:if-isMember", "Validate.lean:147:if-isMember")
             .replace("KelGroups/State.lean:34:members", "KelGroups/State.lean:35:members");
  if (ln === 86) col4 = col4.replace("Integration.lean:app-branch:members", "Integration.lean:210:app-branch:members");
  if (ln === 88) col4 = col4.replace("Integration.lean:app-branch:change", "Integration.lean:210:app-branch:change");
  if (ln === 125) col4 = "ELAB:Reactivegas/Step.lean:470";
  if (ln === 147) col5 = "structural (boot elimination; constructive proof Invariants.lean:877, not a decide witness/check)";
  if (ln === 166) col5 = "cascade-consumer of KelGroups.enact_implies_threshold_met (proof term <hfaithful, ...>); still ELAB-STATIC, not a routing equation";
  if (verdict === "OPEN-KILL" && openExt[ln]) ground = ground + " [EXTENT:OPEN-EXTENTS-31#" + openExt[ln] + "]";
  rep(ground === undefined ? [op, req, verdict, col4, col5].join("|") : [op, req, verdict, col4, col5, ground].join("|"));
}
fs.writeFileSync(OUT, out.join("\n") + "\n");
console.log("wrote " + out.length + " lines");
