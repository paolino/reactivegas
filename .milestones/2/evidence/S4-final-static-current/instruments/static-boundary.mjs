import fs from 'node:fs';
import {execFileSync} from 'node:child_process';
import crypto from 'node:crypto';
import assert from 'node:assert/strict';
const root='/code/reactivegas-66-s4b-final-audit';
const old='94bb7bb64324a48f7361252556b4d15e45b3923f';
const head='04eb6c7d9aeb2a3602fca5ece14cbc033221cb43';
const base='3590c0015b84fd58004bf6fb44dd18b107304c48';
const hash=s=>crypto.createHash('sha256').update(s).digest('hex');
const git=(...a)=>execFileSync('git',['-C',root,...a],{maxBuffer:16*1024*1024});
const blob=(rev,p)=>git('show',`${rev}:${p}`).toString();
const save=(p,s)=>fs.writeFileSync(`evidence/${p}`,typeof s==='string'?s:JSON.stringify(s,null,2)+'\n');
// Bounded lexical scanner: nested Lean block comments, line comments, strings.
// It does not elaborate Lean, load an olean, or execute the inspected program.
function comments(src) {
  const out=[]; let i=0;
  while(i<src.length) {
    if(src.startsWith('--',i)) { const start=i; i=src.indexOf('\n',i); if(i<0)i=src.length; out.push({start,end:i,kind:'line'}); }
    else if(src.startsWith('/-',i)) { const start=i,kind=src.startsWith('/-!',i)?'module-doc':src.startsWith('/--',i)?'declaration-doc':'block'; let d=1;i+=2;
      while(i<src.length&&d) {if(src.startsWith('/-',i)){d++;i+=2;}else if(src.startsWith('-/',i)){d--;i+=2;}else i++;}
      assert.equal(d,0,'unclosed block comment');out.push({start,end:i,kind});
    } else if(src[i]==='"') {i++;let closed=false;while(i<src.length){if(src[i]==='\\'){i+=2;}else if(src[i++]==='"'){closed=true;break;}}assert(closed,'unclosed string');}
    else i++;
  } return out;
}
function erased(src) {let prev=0,r='';for(const c of comments(src)){r+=src.slice(prev,c.start)+' /*COMMENT*/ ';prev=c.end;}return r+src.slice(prev);}
// Controls use synthetic strings only, never changed project files or execution.
assert.equal(erased('def x := "-- string" /- old /- nested -/ -/\n'),erased('def x := "-- string" /- changed -/\n'));
assert.notEqual(erased('def x := 0 /- text -/'),erased('def x := 1 /- text -/'));
assert.notEqual(erased('def x := "old"'),erased('def x := "new"'));
assert.throws(()=>erased('/- unclosed'));
assert.notEqual(erased('def x := 0 /- text -/'),erased('def x := 0 /- text -/\ndef y := 1'));
save('parser-controls.json',{nestedCommentEqual:true,executableTokenDifferenceDetected:true,stringDifferenceDetected:true,unclosedRejected:true,outsideCommentAdditionDetected:true,scope:'synthetic lexical inputs only; no project mutants'});
const tree=rev=>git('ls-tree','-r','-z',rev).toString().split('\0').filter(Boolean).map(x=>{const [meta,path]=x.split('\t');const [mode,type,id]=meta.split(' ');return {mode,type,id,path};});
const before=tree(old),after=tree(head),accepted=tree(base);
const oldMap=new Map(before.map(x=>[x.path,x]));
assert.equal(before.length,after.length);
const changed=after.filter(x=>JSON.stringify(x)!==JSON.stringify(oldMap.get(x.path)));
assert.deepEqual(changed.map(x=>x.path),['lean/Reactivegas/Mirrors.lean','scripts/check-lean-mirrors']);
for(const x of changed)assert.equal(x.mode,oldMap.get(x.path).mode);
assert.equal(git('status','--porcelain=v1','--untracked-files=all').toString(),'');
assert.equal(git('rev-parse','HEAD').toString().trim(),head);
const records=[];
for(const x of changed){
  const a=blob(old,x.path),b=blob(head,x.path);
  save(x.path.replaceAll('/','_')+'.before.txt',a);save(x.path.replaceAll('/','_')+'.after.txt',b);
  let aa=a,bb=b,heredoc=null;
  if(x.path.startsWith('scripts/')){
    const start="cat > \"$DRIVER\" <<'LEAN_EOF'\n",end='\nLEAN_EOF\n';
    function extract(s){assert.equal(s.split(start).length,2);const j=s.indexOf(start)+start.length,k=s.indexOf(end,j);assert(k>j);return {prefix:s.slice(0,j),body:s.slice(j,k),suffix:s.slice(k)};}
    const l=extract(a),r=extract(b);assert.equal(l.prefix,r.prefix);assert.equal(l.suffix,r.suffix);aa=l.body;bb=r.body;
    assert.equal(aa.split('__TRACKED_MODULES__').length,2);assert.equal(bb.split('__TRACKED_MODULES__').length,2);
    const mods=after.filter(x=>x.path.startsWith('lean/')&&x.path.endsWith('.lean')&&x.path!=='lean/lakefile.lean').map(x=>x.path.slice(5,-5).replaceAll('/','.')).sort();
    const literal=mods.map(s=>'"'+s+'"').join(',');
    const ga=aa.replaceAll('__TRACKED_MODULES__',literal),gb=bb.replaceAll('__TRACKED_MODULES__',literal);
    save('driver.before.txt',ga);save('driver.after.txt',gb);
    assert.equal(erased(ga),erased(gb));
    heredoc={quotedDelimiter:true,outerShellBytesIdentical:true,trackedModules:mods.length,placeholderCount:1,rawGeneratedTextEqual:ga===gb,beforeSha256:hash(ga),afterSha256:hash(gb),commentErasedSha256:hash(erased(ga)),generation:'static extraction and literal substitution; wrapper NOT invoked'};
  }
  assert.equal(erased(aa),erased(bb),'noncomment bytes changed');
  const ca=comments(aa),cb=comments(bb);assert.equal(ca.length,cb.length);
  const deltas=ca.flatMap((c,i)=>aa.slice(c.start,c.end)===bb.slice(cb[i].start,cb[i].end)?[]:[{kind:c.kind,oldLine:aa.slice(0,c.start).split('\n').length,newLine:bb.slice(0,cb[i].start).split('\n').length,before:aa.slice(c.start,c.end),after:bb.slice(cb[i].start,cb[i].end)}]);
  assert.equal(deltas.length,1);assert.equal(deltas[0].kind,x.path.startsWith('scripts/')?'declaration-doc':'module-doc');
  records.push({path:x.path,beforeSha256:hash(a),afterSha256:hash(b),noncommentBytesEqual:true,commentErasedSha256:hash(erased(aa)),deltas,heredoc});
}
const inputHashes=[];
for(const x of after){assert.equal(x.type,'blob');const b=git('show',`${head}:${x.path}`);assert.deepEqual(fs.readFileSync(`${root}/${x.path}`),b);inputHashes.push({...x,sha256:hash(b)});}
save('candidate-inputs.json',inputHashes);
save('tree-comparison.json',{base,old,head,baseFiles:accepted.length,oldFiles:before.length,headFiles:after.length,changed,unchangedFiles:after.length-changed.length,fullRange:git('diff','--no-ext-diff','--no-textconv','--numstat',base,head).toString(),commits:git('log','--format=%H %P %aI %cI %s',`${base}..${head}`).toString()});
save('whole-range.patch',git('diff','--no-ext-diff','--no-textconv',base,head).toString());
save('repair.patch',git('diff','--no-ext-diff','--no-textconv',old,head).toString());
save('comment-boundary.json',records);
console.log(JSON.stringify({trackedFiles:after.length,changed:changed.map(x=>x.path),noncommentBytesEqual:true,rawGeneratedDriverTextEqual:false,documentationKinds:records.map(x=>x.deltas[0].kind),projectExecutions:0},null,2));
