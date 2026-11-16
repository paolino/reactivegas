const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const root = '/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b';
const h = path.join(root,'handoffs');
const old = path.join(path.dirname(root),'commit-auditor-s28b/handoffs/evidence');
const write = (p,b) => fs.writeFileSync(p,b,{flag:'wx'});
fs.mkdirSync(path.join(h,'build'),{recursive:true});
fs.mkdirSync(path.join(h,'evidence/tmp'),{recursive:true});
let src = fs.readFileSync(path.join(old,'StoreProbe.hs'),'utf8');
src = src.replace('-- This barrier runs after the production readState and before the production INSERT.', '-- Original candidate: barrier after readState. Repaired candidate forces rendering before its lock/read.');
src = src.replace('[(1,2),(3,7),(11,19),(101,307)]', '[(1,2),(3,7),(11,19),(101,307),(5,11),(42,43),(1000,7),(0,999)]');
src = src.replace('++ "/4"', '++ "/8"');
src = src.replace('  let ok = accepted && consistent', `  live <- S.readState store
  ordered <- S.readEventsFrom store 1
  seqs <- query_ (S.storeConn store) "SELECT seq_no FROM events ORDER BY id" :: IO [Only Int]
  let events = traverse (\\se -> (S.seSigner se,) <$> decodeStrict (S.seEventBytes se)) ordered
      option1 = [("admin-key-1",E.IEApp (DemoAdd a)),("admin-key-1",E.IEApp (DemoAdd b))]
      option2 = [("admin-key-1",E.IEApp (DemoAdd b)),("admin-key-1",E.IEApp (DemoAdd a))]
      fullReplay = fmap (F.foldIntegratedFrom demoIntegration foundingDemo) events
      orderedOK = (events == Just option1 || events == Just option2) && seqs == [Only 1,Only 2] && fullReplay == Just live
  putStrLn $ "ORDER-REPLAY values=" ++ show (a,b) ++ " events=" ++ show events ++ " seqs=" ++ show seqs ++ " fullState=" ++ show (fullReplay == Just live)
  let ok = orderedOK && accepted && consistent`);
src = src.replace('  S.closeKEL store\n  results <- forM', `  execute_ (S.storeConn store) "DROP TRIGGER audit_refuse"
  invalid <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd (-7)))
  invalidCounts <- readCounts store
  unless (not (isRight invalid) && invalidCounts == counts) (error "REFUSAL-CONTROL failed")
  continued <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd 4))
  afterContinue <- readCounts store
  unless (isRight continued && afterContinue == (7,3,3,7,3)) (error "POST-SQL-ERROR lock-release failed")
  putStrLn $ "REFUSAL-CONTROL unchanged=True POST-SQL-ERROR counts=" ++ show afterContinue
  S.closeKEL store
  results <- forM`);
write(path.join(h,'StoreProbe.hs'),src);
write(path.join(h,'Row4Probe.hs'),fs.readFileSync(path.join(old,'Row4Probe.hs')));
const fold = fs.readFileSync('/code/kelgroups-audit-3af3d06/lib/KelGroups/Fold.hs','utf8');
const needle = 'Map.adjust (\\m -> m{memberRoles = roles}) key (members gs)';
if (fold.split(needle).length !== 2) throw Error('shadow anchor cardinality');
fs.mkdirSync(path.join(h,'row4-shadow/KelGroups'),{recursive:true});
write(path.join(h,'row4-shadow/KelGroups/Fold.hs'),fold.replace(needle,'Map.insert key (Member key "mutant@example" roles) (members gs)'));
const prefix=['nix','develop','.#ci','--quiet','-c','cabal','exec','-O0','--'];
const flags=['ghc','--make','-O0','-threaded','-XGHC2021','-XOverloadedStrings','-XDerivingStrategies','-XLambdaCase','-XStrictData','-package','kelgroups'];
const compile=(source,out,extra=[]) => [...prefix,...flags,...extra,'-itest',source,'-odir',path.join(h,'build',out),'-hidir',path.join(h,'build',out),'-o',path.join(h,'build',out,'probe')];
const run=(out,...args) => [path.join(h,'build',out,'probe'),...args];
const commands=[
 ['P1',compile(path.join(h,'StoreProbe.hs'),'store')],
 ['P2',run('store','+RTS','-N2','-RTS')],
 ['P3',compile(path.join(h,'Row4Probe.hs'),'row4')],
 ['P4',compile(path.join(h,'Row4Probe.hs'),'row4-mutant',['-i'+path.join(h,'row4-shadow')+':lib'])],
 ['P5',run('row4-mutant','witness')],
 ['P6',run('row4','witness')],
 ['P7',run('row4-mutant','--match','S28-1 direct-only admission')],
 ...['R1','R3','R5','MAJ'].flatMap(n => [[n+'-compile',compile(path.join(h,'TraceProbe.hs'),n)],[n+'-run',run(n,n)]])
].map(([id,argv])=>({id,argv,charge:{builds:0,targeted:1},expectedExit:['P5','P7'].includes(id)?1:0}));
for(const cmd of commands.filter(c=>c.id.endsWith('compile')||['P1','P3','P4'].includes(c.id))) {
 fs.mkdirSync(cmd.argv[cmd.argv.indexOf('-odir')+1],{recursive:true});
}
write(path.join(h,'probe-commands.json'),JSON.stringify(commands,null,2)+'\n');
const sourceHashes=['TraceProbe.hs','StoreProbe.hs','Row4Probe.hs','row4-shadow/KelGroups/Fold.hs','probe-commands.json'].map(p=>({path:p,sha256:crypto.createHash('sha256').update(fs.readFileSync(path.join(h,p))).digest('hex')}));
write(path.join(h,'evidence/probe-inputs.json'),JSON.stringify(sourceHashes,null,2)+'\n');
for(const [name,source] of [['brief-A01.md',path.join(root,'brief.md')],['NOTE-001-A01.md',path.join(root,'inbox/NOTE-001-mandate-amendment-A01.md')]]) write(path.join(h,'evidence',name),fs.readFileSync(source));
console.log(JSON.stringify({commands:commands.length,sourceHashes}));
