const fs = require("fs");
const IN = "admitted/OPMAP-v7-requirement-verdict-grounds.txt";
const OUT = "handoffs/submission-3/OPMAP-v9-requirement-verdict-grounds.txt";
const lines = fs.readFileSync(IN, "utf8").split("\n");
if (lines.length !== 207 && !(lines.length === 208 && lines[207] === "")) {
  console.error("ABORT: expected 207 lines, got " + lines.length); process.exit(1);
}
const N = 207, out = [], errors = [];
const openExt = {}; for (let k = 0; k < 31; k++) openExt[170 + k] = k + 1;
const PROOFFAIL = {25:1, 34:1, 64:1, 65:1, 66:1, 67:1, 68:1, 87:1, 91:1, 92:1, 93:1, 95:1, 96:1};
// v7 lines 69,70 (OP-52) and 84,85 (OP-58) reclassified SURVIVE by owned source reads (submission 3).
const SURVIVE2 = {
  69: "PREDICTED-GREEN (induction unifies against mutant definition; single-step results stay well-formed; statement true)",
  70: "PREDICTED-GREEN (as L69: cons-case exact matches mutant foldl unfolding; statement true)",
  84: "PREDICTED-GREEN (proof never consumes the >= shape: splits + commitBaseChange_ok + noConfusion all survive >; absence unaffected)",
  85: "PREDICTED-GREEN (proof never consumes the >= shape; hook still runs whenever change is reported)"
};
// OPEN lines converting to PREDICTED-KILL(c): ln -> [atoms-col4, red-text, ground-suffix]
const FLIP = {
  172: ["MUT:Step.lean:298-303:baseHook-sweep-post→pre + MUT:Vote/Fold.lean:76:filter-pred→fun-true",
    "PREDICTED-RED (enacted votes != post-sweep under OP-63; open retained under OP-40)",
    "GROUND:(c) check value flips (openQuestions==[] fails); REACHABLE under OP-63 (break :1600 after check :1517), IMPORT-BLOCKED under OP-40 [EXTENT:OPEN-EXTENTS-31#3]"],
  173: ["MUT:Step.lean:254-259:windUpAdmin-collections-kept + MUT:Integration.lean:143:appFold-swap (OP-57/57B)",
    "PREDICTED-RED (cleanup/balance assertions fail)",
    "GROUND:(c) check value flips (collections==[] under OP-67G; bal/comune assertions under OP-57/57B); PROOF-BLOCKED (:637/:647 break first) [EXTENT:OPEN-EXTENTS-31#4]"],
  174: ["MUT:Step.lean:298-303:baseHook-sweep-post→pre",
    "PREDICTED-RED (result votes == sweep@pre != sweep@post)",
    "GROUND:(c) check value flips; REACHABLE (break :1600 after check :1541) [EXTENT:OPEN-EXTENTS-31#5]"],
  175: ["MUT:Step.lean:90-92:drop-members-foldl-distr (OP-23 atom)",
    "PREDICTED-RED (alice==1 and bob==1 fail, both stay 0)",
    "GROUND:(c) check value flips (static semantic argument, not executed kill); PROOF-BLOCKED (:410 breaks first) [EXTENT:OPEN-EXTENTS-31#6]"],
  177: ["MUT:Validate.lean:145:admin→True (OP-54) + :146:reserved→False (OP-55) + :147:member→False (OP-56)",
    "PREDICTED-RED (exact-identity refusals become admissions)",
    "GROUND:(c) check value flips, one atom per sub-check; PROOF-BLOCKED (:628 breaks first) [EXTENT:OPEN-EXTENTS-31#8]"],
  178: ["inherits OPEN-EXTENTS-31#8 atoms via checkDirectAdmissionOnly conjunct",
    "PREDICTED-RED (inherited conjunction flip)",
    "GROUND:(c) inherited check-value flip; PROOF-BLOCKED [EXTENT:OPEN-EXTENTS-31#9]"],
  179: ["MUT:Vote/Validate.lean:58:openQuestion-gate→ok (OP-49)",
    "PREDICTED-RED (bob openQuestion succeeds, error-conjuncts fail)",
    "GROUND:(c) check value flips; IMPORT-BLOCKED (Vote.Invariants breaks) [EXTENT:OPEN-EXTENTS-31#10]"],
  180: ["MUT:Vote/Validate.lean:58:openQuestion-gate→ok (OP-49)",
    "PREDICTED-RED (bob/bypass openQuestions succeed)",
    "GROUND:(c) check value flips; IMPORT-BLOCKED [EXTENT:OPEN-EXTENTS-31#11]"],
  182: ["MUT:Integration.lean:210:app-branch:members→[] (OP-58B)",
    "PREDICTED-RED (members==mixedGroup.members fails)",
    "GROUND:(c) check value flips; PROOF-BLOCKED [EXTENT:OPEN-EXTENTS-31#13]"],
  185: ["MUT:Vote/Validate.lean:58 (OP-49) + MUT:Validate.lean:145 (OP-54)",
    "PREDICTED-RED (inherited conjunction flips)",
    "GROUND:(c) inherited check-value flips; IMPORT-BLOCKED/PROOF-BLOCKED [EXTENT:OPEN-EXTENTS-31#16]"],
  186: ["MUT:Step.lean:298-303 (OP-63) + MUT:Vote/Fold.lean:76 (OP-40)",
    "PREDICTED-RED (inherited conjunction flips)",
    "GROUND:(c) inherited check-value flips [EXTENT:OPEN-EXTENTS-31#17]"],
  187: ["MUT:Step.lean:298-303 (OP-63) + MUT:Vote/Fold.lean:76 (OP-40)",
    "PREDICTED-RED (q retained open: !opens.contains q fails)",
    "GROUND:(c) check value flips; PROOF-BLOCKED under OP-63 (:1600 first), IMPORT-BLOCKED under OP-40 [EXTENT:OPEN-EXTENTS-31#18]"],
  188: ["MUT:Vote/Fold.lean:76:filter-pred→fun-true (OP-40)",
    "PREDICTED-RED (closed qp retained open: isNone fails)",
    "GROUND:(c) check value flips; IMPORT-BLOCKED [EXTENT:OPEN-EXTENTS-31#19]"],
  189: ["MUT:Vote/Fold.lean:76 (OP-40) via checkI57PolicyFree conjunct",
    "PREDICTED-RED (inherited conjunction flip)",
    "GROUND:(c) inherited check-value flip; IMPORT-BLOCKED [EXTENT:OPEN-EXTENTS-31#20]"],
  191: ["MUT:Step.lean:298-303 (OP-63) via checkBaseRecomputeReachable conjunct",
    "PREDICTED-RED (inherited conjunction flip)",
    "GROUND:(c) inherited check-value flip; PROOF-BLOCKED [EXTENT:OPEN-EXTENTS-31#22]"],
  193: ["MUT:Vote/Fold.lean:76:filter-pred→fun-true (OP-40)",
    "PREDICTED-RED (retained q re-closes: duplicate record, idempotence equation fails)",
    "GROUND:(c) check value flips; IMPORT-BLOCKED [EXTENT:OPEN-EXTENTS-31#24]"],
  195: ["inherits line-175 atom (mirror)",
    "PREDICTED-RED (mirror of Reactivegas flip)", "GROUND:(c) mirror check-value flip [EXTENT:OPEN-EXTENTS-31#26]"],
  196: ["inherits line-172 atoms (mirror)",
    "PREDICTED-RED (mirror of Reactivegas flip)", "GROUND:(c) mirror check-value flip [EXTENT:OPEN-EXTENTS-31#27]"],
  197: ["inherits line-173 atoms (mirror)",
    "PREDICTED-RED (mirror of Reactivegas flip)", "GROUND:(c) mirror check-value flip [EXTENT:OPEN-EXTENTS-31#28]"],
  198: ["inherits line-174 atom (mirror)",
    "PREDICTED-RED (mirror of Reactivegas flip)", "GROUND:(c) mirror check-value flip [EXTENT:OPEN-EXTENTS-31#29]"],
  200: ["inherits line-193 atom (mirror)",
    "PREDICTED-RED (mirror of Reactivegas flip)", "GROUND:(c) mirror check-value flip [EXTENT:OPEN-EXTENTS-31#31]"]
};
for (let ln = 1; ln <= N; ln++) {
  if (ln > 10000) { console.error("ABORT: iteration ceiling"); process.exit(1); }
  const cols = lines[ln - 1].split("|");
  if (cols.length !== 5 && cols.length !== 6) { console.error("ABORT: line " + ln + " cols " + cols.length); process.exit(1); }
  let op = cols[0], req = cols[1], verdict = cols[2], col4 = cols[3], col5 = cols[4], ground = cols[5];
  let evid = "N-A";
  if (ln === 89 || ln === 90) {
    verdict = "WITHDRAWN-DUPLICATE";
    col5 = "WITHDRAWN (phantom KelGroups.* namespace; sole declaration at Reactivegas/Invariants.lean:" + (ln === 89 ? "1616" : "1600") + "; real row " + (ln === 89 ? "L97" : "L98") + ")";
    ground = "GROUND:duplicate-spelling, no distinct declaration (A-ASSESSMENT-51)";
  } else if (ln === 25) {
    ground = "GROUND:(P) proof-shape sensitivity: statement excludes comuneId (Invariants.lean:890-895), donate proof rw bal_bump_ne breaks; statement stays true (Addendum-5 input, re-verified)";
    col5 = col5.replace("RED (donate-arm case)", "PREDICTED-RED (donate-arm rw-shape)");
  } else if (ln === 21) {
    ground = "GROUND:(c) statement false at witness (responsabile signer, v=0: mutated guard admits, guard conjunct fails); post-state equation unaffected";
    col5 = "PREDICTED-RED (guard-conjunct falsified at v=0)";
  } else if (ln === 34) {
    ground = "GROUND:(P) proof-shape sensitivity: statement stays true under mutant; List.erase_subset applied to non-erased membership fails (assent arm); OP-25 pattern";
    col5 = col5.replace("RED (definitional)", "PREDICTED-RED (exact-term mismatch, statement true)");
  } else if (ln === 40) {
    verdict = "PREDICTED-SURVIVE";
    col5 = "PREDICTED-GREEN (statement quantifies .closed only; proof untouched by open-retention filter mutant)";
    ground = "GROUND:survive-predicted, (a) WITHDRAWN (A-ASSESSMENT-51 L40)";
  } else if (ln === 43) {
    verdict = "OBSERVED";
    col5 = "upstream KelGroups.Vote.sweepClosures_open_mem (cascade: mpr needs verdict-test shape; statement itself stays true)";
    ground = "GROUND:cascade via open_mem, (a) WITHDRAWN (A-ASSESSMENT-51 L43)";
  } else if (ln === 3) {
    verdict = "OBSERVED";
    col5 = "upstream the 14 step_*_inv inversion lemmas (authorizedStep ignores both states; RED only via consumed lemmas)";
    ground = "GROUND:cascade via 14 inversion lemmas, (a) WITHDRAWN (A-ASSESSMENT-51 L3)";
  } else if (ln === 2) {
    col5 = "PREDICTED-RED per run (10 arms THEOREM-FAIL: deny/deposit/withdraw/transfer/backdonate/pledge/refuse/correct/close/fail; 4 arms PROOF-FAIL: openPurchase/grant/donate-under-OP-22/accept; carriers A-ASSESSMENT-51 L2r3), NOT observed; no Phase-2 execution has occurred";
    ground = "GROUND:(a/c) per-arm split MIXED (A-ASSESSMENT-51 L2r3)";
  } else if (ln === 61 || ln === 63) {
    verdict = "PREDICTED-SURVIVE";
    col5 = ln === 61
      ? "PREDICTED-GREEN (hypothesis-driven simp never unfolds validate body; vacuous under mutant)"
      : "PREDICTED-GREEN (cast path Validate.lean:60-64 untouched by :58 openQuestion-arm mutant)";
    ground = "GROUND:survive-predicted, (a) WITHDRAWN (A-ASSESSMENT-51 " + (ln === 61 ? "L61" : "L63") + ")";
  } else if (ln === 94) {
    col5 = "OBSERVED (upstream KelGroups.tryEnactDetailed_enactment_threshold_met, consumed at KelGroups/Invariants.lean:357 and :366)";
  } else if (SURVIVE2[ln]) {
    verdict = "PREDICTED-SURVIVE";
    col5 = SURVIVE2[ln];
    ground = "GROUND:survive-predicted, (P) WITHDRAWN by owned source read (submission 3 appendix R3)";
  } else if (FLIP[ln]) {
    verdict = "KILL";
    col4 = FLIP[ln][0]; col5 = FLIP[ln][1]; ground = FLIP[ln][2];
  }
  if (verdict === "KILL") col5 = col5.replace(/(?<!PREDICTED-)\bRED\b/g, "PREDICTED-RED");
  if (ln === 201) col5 += " [corroborated t54-PARTITION ea9882087e28/7846a04f5984; exact-atom identity unestablished, not RECEIPT-BOUND]";
  if (ln === 148 || ln === 150 || ln === 151 || ln === 152) col5 += " [corroborated S4-B-O5 mechanism (Step.lean:446/449/471 decide-RED) at its actual identity only, not this row's receipt]";
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
  // EVID-KIND seventh column: actual receipts / predicted theorem failure / predicted proof-script failure / static cascade.
  if (verdict === "KILL") {
    if (ln === 2) evid = "MIXED";
    else if (PROOFFAIL[ln] || ln === 25 || ln === 34) evid = "PROOF-FAIL";
    else evid = "THEOREM-FAIL";
  } else if (verdict === "OBSERVED") evid = "CASCADE";
  else if (verdict === "OPEN-KILL") evid = "NONE";
  const row = ground === undefined ? [op, req, verdict, col4, col5] : [op, req, verdict, col4, col5, ground];
  row.push("EVID:" + evid);
  out.push(row.join("|"));
}
fs.writeFileSync(OUT, out.join("\n") + "\n");
console.log("wrote " + out.length + " lines");
