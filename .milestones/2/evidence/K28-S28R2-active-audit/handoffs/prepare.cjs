const fs=require('fs'),cp=require('child_process'),crypto=require('crypto');
const h=__dirname,old=h.replace('s28r2','s28r1b');
const hash=b=>crypto.createHash('sha256').update(b).digest('hex');
fs.mkdirSync(h+'/evidence/tmp',{recursive:true});
// Retained instruments are source inputs only; all execution will be fresh.
let trace=fs.readFileSync(old+'/TraceProbe.hs','utf8').replace('import Data.Either (isLeft)\n','');
// New distinguishable fixtures rederived from the demo's +n/log and hook behavior.
trace=trace.replaceAll('DemoState 7','DemoState 17').replaceAll('DemoState 12','DemoState 22').replaceAll('+ 13','+ 23');
fs.writeFileSync(h+'/TraceProbe.hs',trace);
fs.writeFileSync(h+'/Row4Probe.hs',fs.readFileSync(old+'/Row4Probe.hs'));
const fold=fs.readFileSync('lib/KelGroups/Fold.hs','utf8');
const anchor='Map.adjust (\\m -> m{memberRoles = roles}) key (members gs)';
if(fold.split(anchor).length!==2)throw Error('shadow anchor');
fs.mkdirSync(h+'/row4-shadow/KelGroups',{recursive:true});
fs.writeFileSync(h+'/row4-shadow/KelGroups/Fold.hs',fold.replace(anchor,'Map.insert key (Member key "mutant@example" roles) (members gs)'));
const header='{-# LANGUAGE GHC2021, OverloadedStrings #-}\nmodule TypeNegative where\nimport KelGroups.Event qualified as E\nimport KelGroups.Fold qualified as F\nimport S28DemoApp\n';
fs.writeFileSync(h+'/TypeEvent.hs',header+'eventOnlyMismatch = F.applyIntegratedEvent demoIntegration foundingDemo "admin-key-1" (E.IEApp (DemoState 0 []))\n');
fs.writeFileSync(h+'/TypeHistorical.hs',header+'historicalEventMismatch = F.applyEvent (\\st _ -> st) foundingDemo ("admin-key-1", E.App (DemoAdd 1))\n');
const store=fs.readFileSync('lib/KelGroups/Store.hs','utf8');
let body=store.slice(store.indexOf('appendIntegratedEvent\n    ::'));
// Faithful snapshot/decision-before-lock skew, same SQL and TVar effect as candidate.
body=body.replace('appendIntegratedEvent store integration signer event =\n    withMVar (storeAppendLock store) $ \\() -> do\n        gs <- readState store\n        case applyIntegratedEvent integration gs signer event of',
'appendIntegratedEvent store integration signer event = do\n    gs <- readState store\n    n <- kelLength store\n    case applyIntegratedEvent integration gs signer event of');
body=body.replace('            Left err -> pure (Left err)\n            Right result -> do','        Left err -> pure (Left err)\n        Right result -> withMVar (storeAppendLock store) $ \\() -> do').replace('                n <- kelLength store\n','');
body=body.replaceAll('appendIntegratedEvent','skewAppend');
const imports=`{-# LANGUAGE GHC2021, OverloadedStrings #-}
module SkewStore (skewAppend) where
import KelGroups.Store
import KelGroups.Fold
import KelGroups.Event (IntegratedEvent)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (evaluate)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Database.SQLite.Simple (execute)
`;
fs.writeFileSync(h+'/SkewStore.hs',imports+body);
fs.writeFileSync(h+'/evidence/candidate-append.hs',store.slice(store.indexOf('appendIntegratedEvent\n    ::')));
for(const [name,file] of [['input-gate.sh','gate.sh'],['input-mandate.md',h+'/../../handoffs/S28-R2-COMMAND-PLAN.md']]) fs.copyFileSync(file,h+'/evidence/'+name);
fs.writeFileSync(h+'/evidence/base-candidate.diff',cp.execFileSync('git',['diff','368b596fef0b6d393c2ac7afc631d236c55d86d1','HEAD']));
