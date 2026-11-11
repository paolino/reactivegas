const fs = require("fs");
const files = { RI: "Reactivegas/Invariants.lean", RS: "Reactivegas/Step.lean", TT: "Reactivegas/TraceTests.lean" };
const src = {};
for (const k in files) src[k] = fs.readFileSync(files[k], "utf8").split("\n");
const checks = [
  ["RI","checkDirectAdmissionOnly"],["RI","checkBaseCleanupReachable"],["RI","checkV3BaseReachable"],
  ["RI","checkBaseRecomputeReachable"],["RI","checkSweepIdempotent"],["RI","checkSweepIdempotentMutant"],
  ["RI","checkIntegratedTheoremWitness"],["RI","checkCanonicalEconomy"],["RI","checkExhaustiveInventories"],
  ["RI","checkI57Boundary"],["RI","checkI57Exhaustive"],["RI","checkI57Noop"],["RI","checkI57Auth"],
  ["RI","checkI57R45"],["RI","checkI57Partition"],["RI","checkI57Disjoint"],["RI","checkI57DisjointMutant"],
  ["RI","checkI57NoStale"],["RI","checkI57Franchise"],["RI","checkI57FranchiseMutant"],
  ["RI","checkI57PolicyFree"],["RI","checkI57PolicyFreeMutant"],["RI","checkI57NoExpiry"],
  ["RI","checkAdmissionPreservation"],["RI","checkComuneThresholdSanity"],
  ["RS","checkAppMembersPreservationMutant"],["TT","checkAppMembersPreservationMutant"],
  ["TT","checkIntegratedTheoremWitness"]
];
const prods = ["stepEvent","stepDetailed","voteApply","sweepClosures","foldVote","foldFrom","validateVoteEvent",
  "validateDirectAdmission","baseHook","windUpAdmin","economicCleanup","commitBaseChange","tryEnactBase","tryEnact",
  "effectedState","applyVoteEvent","placeBallot","boot","seedCorpus","seedDenyPermissionRefunds",
  "preservationDonate","applyIntegratedEvent","apply ","checkAppMembersPreservation","decide","Reactivegas.apply"];
for (const pair of checks) {
  const fk = pair[0], name = pair[1];
  const L = src[fk];
  const i = L.findIndex(function (l) { return l.indexOf("def " + name) === 0; });
  if (i < 0) { console.log(fk + " " + name + " NOT-FOUND"); continue; }
  let end = i + 1;
  while (end < L.length && (L[end].trim() === "" || L[end][0] === " " || L[end][0] === "\t" || L[end].indexOf("--") === 0)) end++;
  const body = L.slice(i, end).join("\n");
  const hits = [];
  for (const p of prods) { if (body.indexOf(p) >= 0 && hits.indexOf(p) < 0) hits.push(p); }
  console.log(fk + ":" + (i + 1) + " " + name + " nlines=" + (end - i) + " refs=" + hits.join("|"));
}
