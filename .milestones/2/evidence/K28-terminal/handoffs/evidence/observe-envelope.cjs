const fs=require('fs'), cp=require('child_process'), crypto=require('crypto'), path=require('path');
const root=path.resolve(__dirname,'../..'), tree=path.join(root,'exec-84a2dae'), ev=__dirname;
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const event=(tag,s)=>fs.appendFileSync(path.join(root,'STATUS.md'),`${new Date().toISOString()}  ${tag}  ${s}\n`);
const reservations=fs.readFileSync(path.join(ev,'reservations.txt'),'utf8').trim().split('\n').map(l=>l.split(' '));
for(const [p,u,i,d] of reservations){const s=fs.lstatSync(p);if(!s.isFile()||s.isSymbolicLink()||s.uid!=u||s.ino!=i||s.dev!=d)throw Error('reservation collision '+p);}
if(fs.readdirSync(path.join(root,'inbox')).length)throw Error('unread inbox before gate');
let active=null,spent=0,buf='', paused=false;
const disk=()=>{const s=fs.statfsSync(tree);return s.bavail*s.bsize};
const child=cp.spawn('bash',['./gate.sh'],{cwd:tree,detached:true,env:{...process.env,G28_EVIDENCE_DIR:ev,TMPDIR:path.join(ev,'tmp-envelope')},stdio:['ignore','pipe','pipe']});
fs.writeFileSync(path.join(ev,'gate-pgid.txt'),String(child.pid)+'\n');
event('NOTE',`GATE-BEGIN pid=${child.pid} pgid=${child.pid} command=bash./gate.sh reserved_budget=9/12 cache=cold observer=observe-envelope.cjs`);
function begin(id){if(active)throw Error('overlapping receipt');spent++;active={id,start:process.hrtime.bigint(),free:disk()};event('NOTE',`INVOCATION-BEGIN id=${id} build=${spent}/12 targeted=0/24 free_bytes=${active.free}`);if(fs.readdirSync(path.join(root,'inbox')).length){process.kill(-child.pid,'SIGSTOP');paused=true;event('BLOCKED','inbox-arrived gate-group-stopped for instruction review');}}
function line(s){fs.appendFileSync(path.join(ev,'timeline.jsonl'),JSON.stringify({time:new Date().toISOString(),line:s})+'\n');
 if(/^===== LEG 3:/.test(s)){fs.writeFileSync(path.join(ev,'tmp-after-leg2b.txt'),cp.execFileSync('ls',['-lai',path.join(ev,'tmp-envelope')]));begin('leg3');}
 if(/^===== LEG 4:/.test(s))begin('leg4');
 if(/^===== LEG 6:/.test(s))begin('leg6');
 let m=s.match(/^--- (M[1-6]) /);if(m)begin(m[1]);
 m=s.match(/^(M[1-6]) diff sha256=([a-f0-9]+)/);if(m){const diff=cp.execFileSync('git',['diff','--','lib','test'],{cwd:tree});if(sha(diff)!==m[2])throw Error('snapshot hash mismatch '+m[1]);fs.writeFileSync(path.join(ev,m[1]+'.diff'),diff);event('NOTE',`MUTANT-PRESERVED id=${m[1]} sha256=${m[2]} bytes=${diff.length}`);}
 m=s.match(/^(?:(M[1-6]) )?(build|test|ci) exit=(\d+) sha256=([a-f0-9]+)/);if(m){if(!active)throw Error('receipt without invocation');const id=active.id;const files=fs.readdirSync(ev).filter(f=>f.endsWith('.log')&&sha(fs.readFileSync(path.join(ev,f)))===m[4]);const receipt={id,exit:Number(m[3]),duration_observed_seconds:Number(process.hrtime.bigint()-active.start)/1e9,free_before:active.free,free_after:disk(),sha256:m[4],logs:files,bytes:files.length?fs.statSync(path.join(ev,files[0])).size:null};fs.appendFileSync(path.join(ev,'command-receipts.jsonl'),JSON.stringify(receipt)+'\n');event('NOTE',`INVOCATION-END ${JSON.stringify(receipt)}`);active=null;}
 if(/^===== LEG|^LEG-(PASS|FAIL)|^MUTANT-|^KILL-QUOTE|^ABORT|^tree state:|^IDENTITY-FAIL/.test(s))process.stdout.write(s+'\n');
}
const consume=d=>{buf+=d.toString();let i;while((i=buf.indexOf('\n'))>=0){const s=buf.slice(0,i);buf=buf.slice(i+1);line(s);}};
child.stdout.on('data',consume);child.stderr.on('data',consume);
child.on('close',(code,signal)=>{if(buf)line(buf);event('NOTE',`GATE-END exit=${code} signal=${signal} substantive_started=${spent}/12 targeted=0/24 paused=${paused}`);fs.writeFileSync(path.join(ev,'gate-exit.json'),JSON.stringify({code,signal,spent,paused})+'\n');process.exitCode=code??1;});
