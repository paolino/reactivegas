const fs = require("fs");
const files = { RI: "Reactivegas/Invariants.lean", RS: "Reactivegas/Step.lean", TT: "Reactivegas/TraceTests.lean" };
const src = {};
for (const k in files) src[k] = fs.readFileSync(files[k], "utf8").split("\n");
function findDef(name) {
  for (const fk in src) {
    const L = src[fk];
    const i = L.findIndex(function (l) { return l.indexOf("def " + name) === 0; });
    if (i >= 0) return { fk: fk, line: i + 1 };
  }
  return null;
}
const MAXSPAN = 400;
const UNPARSED = [];
function bodyOf(fk, startIdx, what) {
  const L = src[fk];
  let end = startIdx + 1;
  let guard = 0;
  while (end < L.length && (L[end].trim() === "" || L[end][0] === " " || L[end][0] === "\t" || L[end].indexOf("--") === 0)) {
    end++;
    if (++guard > MAXSPAN) { UNPARSED.push(what + ": body exceeds " + MAXSPAN + " lines, ABORTED (not silently skipped)"); return null; }
  }
  return L.slice(startIdx, end).join("\n");
}
function idents(body) {
  const m = body.match(/[A-Za-z_][A-Za-z0-9_']*/g) || [];
  return Array.from(new Set(m));
}
const checks = ["checkDirectAdmissionOnly","checkBaseCleanupReachable","checkV3BaseReachable",
  "checkBaseRecomputeReachable","checkSweepIdempotent","checkSweepIdempotentMutant",
  "checkIntegratedTheoremWitness","checkCanonicalEconomy","checkExhaustiveInventories",
  "checkI57Boundary","checkI57Exhaustive","checkI57Noop","checkI57Auth",
  "checkI57R45","checkI57Partition","checkI57Disjoint","checkI57DisjointMutant",
  "checkI57NoStale","checkI57Franchise","checkI57FranchiseMutant",
  "checkI57PolicyFree","checkI57PolicyFreeMutant","checkI57NoExpiry",
  "checkAdmissionPreservation","checkComuneThresholdSanity","checkAppMembersPreservationMutant"];
const prodPat = /^(step|stepEvent|stepDetailed|apply|voteApply|applyVoteEvent|applyVoteEventChecked|effectedState|sweepClosures|sweepStep|foldVote|foldFrom|validateVoteEvent|validateDirectAdmission|validateBaseMutation|validateProposal|baseHook|windUpAdmin|economicCleanup|commitBaseChange|tryEnactBase|tryEnact|enact|boot|seedCorpus|preservationDonate|placeBallot|enactMutation|admitMemberInto|applyIntegratedEvent|foldIntegrated)$/;
for (const name of checks) {
  const d = findDef(name);
  if (!d) { console.log(name + " NOT-FOUND"); continue; }
  const body = bodyOf(d.fk, d.line - 1, name);
  if (body === null) { console.log("### " + name + " UNPARSED (see UNPARSED list)"); continue; }
  const ids = idents(body).filter(function (w) {
    return w !== name && w !== "def" && w !== "Bool" && w !== "true" && w !== "false" && w !== "match" && w !== "with" && w !== "let" && w !== "some" && w !== "none" && w !== "ok" && w !== "error" && w !== "fun" && w !== "if" && w !== "then" && w !== "else";
  });
  const directProds = ids.filter(function (w) { return prodPat.test(w); });
  const helpers = ids.filter(function (w) { return /^(check|s62b|v3|mixed|probe|preservation|seed)/i.test(w) && w !== name; });
  console.log("### " + name + " @" + d.fk + ":" + d.line);
  console.log("  direct-prod-refs: " + (directProds.join(", ") || "(none)"));
  console.log("  helper-fixture-refs: " + (helpers.join(", ") || "(none)"));
  console.log("  helper-count-total: " + helpers.length + (helpers.length > 12 ? " (showing first 12 ONLY, remainder listed unparsed below)" : ""));
  if (helpers.length > 12) { for (const h of helpers.slice(12)) UNPARSED.push(name + ": helper beyond display cap, not expanded: " + h); }
  for (const h of helpers.slice(0, 12)) {
    const hd = findDef(h);
    if (!hd) { console.log("    " + h + " -> (no def in RI/RS/TT: literal/fixture/param or defined elsewhere, NOT expanded)"); continue; }
    const hb = bodyOf(hd.fk, hd.line - 1, name + "->" + h);
    if (hb === null) { console.log("    " + h + " UNPARSED (see UNPARSED list)"); continue; }
    const hp = idents(hb).filter(function (w) { return prodPat.test(w); });
    console.log("    " + h + " @" + hd.fk + ":" + hd.line + " prod=" + (hp.join(", ") || "(none)"));
  }
}
if (typeof UNPARSED !== "undefined") { console.log("UNPARSED-COUNT: " + UNPARSED.length); for (const u of UNPARSED) console.log("UNPARSED: " + u); }
