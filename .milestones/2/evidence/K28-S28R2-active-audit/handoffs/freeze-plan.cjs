const fs=require('fs'),cp=require('child_process'),crypto=require('crypto');
const h=__dirname, old=h.replace('s28r2','s28r1b');
let commands=JSON.parse(fs.readFileSync(old+'/probe-commands.json','utf8').replaceAll('commit-auditor-s28r1b','commit-auditor-s28r2'));
commands=commands.filter(c=>!['P1','P2'].includes(c.id));
const common=['nix','develop','.#ci','--quiet','-c','cabal','exec','-O0','--','ghc','--make','-O0','-threaded','-XGHC2021','-XOverloadedStrings','-XDerivingStrategies','-XLambdaCase','-XStrictData','-package','kelgroups','-itest'];
const build=h+'/build/store';
commands.unshift({id:'P2-compile',argv:[...common,'-i'+h,h+'/StoreProbe.hs','-odir',build,'-hidir',build,'-o',build+'/probe'],charge:{builds:0,targeted:1},expectedExit:0});
for(const mode of ['conservation','codec','lock'])commands.push({id:'P2-'+mode,argv:[build+'/probe',mode,'+RTS','-N2','-RTS'],charge:{builds:0,targeted:1},expectedExit:0});
for(const mode of ['Event','Historical'])commands.push({id:'TYP-'+mode,argv:[...common,'-fno-code',h+'/Type'+mode+'.hs','-odir',h+'/build/type'+mode,'-hidir',h+'/build/type'+mode],charge:{builds:0,targeted:1},expectedExit:1});
for(const c of commands){const i=c.argv.indexOf('-odir');if(i>=0)fs.mkdirSync(c.argv[i+1],{recursive:true});}
if(commands.length!==19)throw Error('floor count');
fs.writeFileSync(h+'/probe-commands.json',JSON.stringify(commands,null,2)+'\n');
let runner=fs.readFileSync(old+'/run-audit.cjs','utf8').replaceAll('commit-auditor-s28r1b','commit-auditor-s28r2').replaceAll('/code/kelgroups-audit-3af3d06','/code/kelgroups-audit-ab25cd1').replaceAll('3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4','ab25cd11b554bcd5ba64ca56a050c2eb21432d3c').replaceAll('dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815','c00b88a29989b11d09696d7afa164f7d9f93b59aee661a1b88a120c7a4934b75').replaceAll('AUDIT-S28R1','AUDIT-S28R2').replaceAll('[1-7]','[1-8]').replaceAll('count>10','count>11').replaceAll('envelope=10B','envelope=11B').replaceAll('gate-v9','gate-v10.2').replaceAll('t>16','t>24').replaceAll('A-01-consumed','S28-R2-command-plan-consumed').replace("'/tmp/g28m5.hs'","'/tmp/g28m5.hs','/tmp/g28m8a.hs'");
// Persist exact mutant bytes as well as diffs; live source re-read before the build begins.
runner=runner.replace("if(sha(diff)!==m[2])throw Error('mutant snapshot mismatch');", "if(sha(diff)!==m[2])throw Error('mutant snapshot mismatch'); const snap=path.join(e,m[1]+'-source');fs.mkdirSync(snap);for(const f of git('diff','--name-only','--','lib','test').trim().split('\\n').filter(Boolean))fs.copyFileSync(path.join(cwd,f),path.join(snap,f.replaceAll('/','_')));");
fs.writeFileSync(h+'/run-audit.cjs',runner);
const files=['TraceProbe.hs','Row4Probe.hs','StoreProbe.hs','SkewStore.hs','TypeEvent.hs','TypeHistorical.hs','row4-shadow/KelGroups/Fold.hs','probe-commands.json','run-audit.cjs'];
fs.writeFileSync(h+'/evidence/probe-inputs.json',JSON.stringify(files.map(p=>({path:h+'/'+p,sha256:crypto.createHash('sha256').update(fs.readFileSync(h+'/'+p)).digest('hex')})),null,2)+'\n');
