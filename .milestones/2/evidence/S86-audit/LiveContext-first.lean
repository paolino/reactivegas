import Reactivegas.CorpusExport
open Lean
-- Re-derive live wrapper context and extents from this seat's compiled imports.
-- No new olean, no altered model, no serializer-independence claim.
#eval (toJson seedView).compress
#eval (toJson Reactivegas.corpusInitial).compress
#eval econAuthIdentity
#eval intAuthIdentity
#eval seedAuth State.empty 0
#eval Reactivegas.probeAuth State.empty 0
#eval seedCorpus.length
#eval (seedCorpus.map (fun t => t.steps.length)).foldl (· + ·) 0
#eval Reactivegas.emitIntegratedCorpus.length
#print axioms step_close_inv
#print axioms step_withdraw_inv
