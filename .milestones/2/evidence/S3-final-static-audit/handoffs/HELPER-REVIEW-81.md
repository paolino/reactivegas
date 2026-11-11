# All 81 helper identities: independent static antecedent review

This audit supplies explicit instantiations to evaluate the repaired grouped recipes; it does not modify the candidate. Parametric key/value types are instantiated to String/Int when present; Pledge and Collection tuples below abbreviate their named source constructors. Universal empty-list premises are valid when they are the actual hypothesis. Explicit membership premises are inhabited instead. All statements/source sites were compared to the reference source. These static instantiations are not elaborated proofs, mutation receipts, or per-identity coverage exemptions. The original three phantom helper names were already withdrawn by R4/C-QUAL and are not revived as findings.

| Identity / site | Antecedent instance and check |
|---|---|
| KelGroups.assocAdjust_keys; KelGroups/Invariants.lean:100 | κ=String, ν=Int, entries=[], keys="a", value=0, f=id where required; no antecedents. |
| KelGroups.assocAdjust_property; KelGroups/Invariants.lean:109 | κ=String, ν=Int, key="a", value=0 where required, entries=[], f=id where required, property=fun _ _=>True; hnew/hold/hchange all satisfied. |
| KelGroups.setInsert_nodup; KelGroups/Invariants.lean:141 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.setInsert_mem; KelGroups/Invariants.lean:149 | value=existing="a", values=["a"]; existing membership true. |
| KelGroups.assocLookup_some_mem; KelGroups/Invariants.lean:166 | key="a", value=0, entries=[("a",0)]; lookup some0 and singleton membership/Nodup. |
| KelGroups.assocErase_sublist; KelGroups/Invariants.lean:34 | κ=String, ν=Int, entries=[], keys="a", value=0, f=id where required; no antecedents. |
| KelGroups.assocErase_keys_nodup; KelGroups/Invariants.lean:45 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.assocErase_key_absent; KelGroups/Invariants.lean:50 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.assocInsert_keys_nodup; KelGroups/Invariants.lean:72 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.assocErase_property; KelGroups/Invariants.lean:79 | κ=String, ν=Int, key="a", value=0 where required, entries=[], f=id where required, property=fun _ _=>True; hnew/hold/hchange all satisfied. |
| KelGroups.assocInsert_property; KelGroups/Invariants.lean:87 | κ=String, ν=Int, key="a", value=0 where required, entries=[], f=id where required, property=fun _ _=>True; hnew/hold/hchange all satisfied. |
| KelGroups.assocLookup_insert_of_none; KelGroups/Types.lean:112 | key="a", other="b", value=0, entries=[]; other!=key and lookup key=none. |
| KelGroups.assocLookup_insert_self; KelGroups/Types.lean:121 | κ=String, ν=Int, key=other="a", entries=[], value=0/f=id where required; any lookup-none premise is true. |
| KelGroups.assocLookup_erase_of_none; KelGroups/Types.lean:71 | κ=String, ν=Int, key=other="a", entries=[], value=0/f=id where required; any lookup-none premise is true. |
| KelGroups.assocLookup_adjust_of_none; KelGroups/Types.lean:91 | κ=String, ν=Int, key=other="a", entries=[], value=0/f=id where required; any lookup-none premise is true. |
| KelGroups.Vote.assocInsert_keys_nodup'; KelGroups/Vote/Invariants.lean:114 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.Vote.assocAdjust_keys'; KelGroups/Vote/Invariants.lean:120 | κ=String, ν=Int, entries=[], keys="a", value=0, f=id where required; no antecedents. |
| KelGroups.Vote.assocAdjust_keys_nodup'; KelGroups/Vote/Invariants.lean:129 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.Vote.assocLookup_some_mem'; KelGroups/Vote/Invariants.lean:135 | key="a", value=0, entries=[("a",0)]; lookup some0 and singleton membership/Nodup. |
| KelGroups.Vote.mem_assocLookup_some'; KelGroups/Vote/Invariants.lean:151 | key="a", value=0, entries=[("a",0)]; lookup some0 and singleton membership/Nodup. |
| KelGroups.Vote.assocErase_other_lookup; KelGroups/Vote/Invariants.lean:175 | k="a", j="b", v=0 where required, entries=[]; distinct-key premise true. |
| KelGroups.Vote.assocInsert_other_lookup; KelGroups/Vote/Invariants.lean:192 | k="a", j="b", v=0 where required, entries=[]; distinct-key premise true. |
| KelGroups.Vote.assocInsert_mem_cases; KelGroups/Vote/Invariants.lean:203 | k=j="a", v=w=0, entries=[]; assocInsert contains ("a",0). |
| KelGroups.Vote.mem_map_fst_erase_of_ne; KelGroups/Vote/Invariants.lean:213 | k="a", j="b", v=0 where required, entries=[]; distinct-key premise true. |
| KelGroups.Vote.mem_map_fst_insert; KelGroups/Vote/Invariants.lean:235 | κ=String, ν=Int, entries=[], keys="a", value=0, f=id where required; no antecedents. |
| KelGroups.Vote.setInsert_mem_cases; KelGroups/Vote/Invariants.lean:246 | v=k="a", l=[]; inserted key is present. |
| KelGroups.Vote.nodup_append_mem; KelGroups/Vote/Invariants.lean:255 | α=String, l1=l2=[]; both Nodup and disjointness premise true. |
| KelGroups.Vote.setInsert_nodup'; KelGroups/Vote/Invariants.lean:268 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.Vote.mem_erase_inv; KelGroups/Vote/Invariants.lean:276 | a="a", b="b", l=["b"]; Nodup and b in erase a l both hold. |
| KelGroups.Vote.nodup_erase; KelGroups/Vote/Invariants.lean:303 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.Vote.closed_guard_absent; KelGroups/Vote/Invariants.lean:317 | qid="q", records=[]; any=false. |
| KelGroups.Vote.assoc_entries_key_unique; KelGroups/Vote/Invariants.lean:326 | entries=[]; explicit hypothesis is keys Nodup. Later membership condition is conclusion, not invented input. |
| KelGroups.Vote.filterMap_keys_nodup; KelGroups/Vote/Invariants.lean:346 | f=fun _=>none, entries=[]; hid is true because none=some c impossible, keys Nodup. |
| KelGroups.Vote.assocErase_sublist'; KelGroups/Vote/Invariants.lean:77 | κ=String, ν=Int, entries=[], keys="a", value=0, f=id where required; no antecedents. |
| KelGroups.Vote.assocErase_keys_nodup'; KelGroups/Vote/Invariants.lean:88 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| KelGroups.Vote.assocErase_key_absent'; KelGroups/Vote/Invariants.lean:93 | keys/values/entries/l=[] as parameter named; all Nodup premises true. Inserted key="a", value=0, f=id where required. |
| pullCollection_mem; Reactivegas/Invariants.lean:110 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_det; Reactivegas/Invariants.lean:116 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| unique_mem_cons_inv; Reactivegas/Invariants.lean:1194 | acc=pend=[], u="a", v=1, p=q=("a",1); hp/hq self-membership, users equal, hu/hun universal premises true on empty old lists. |
| uniquePledges_pend_cons; Reactivegas/Invariants.lean:1228 | col=(7,"ref",false,[],[]), u="a", v=1; hu and uniquePledges hold on empty old pledge lists. |
| user_absent_of_any_false; Reactivegas/Invariants.lean:124 | u="a", l=[]; any=false. |
| sumBal_foldl_bump; Reactivegas/Invariants.lean:135 | l=[], m=[], w=0; no antecedents. |
| option_bind_inv; Reactivegas/Invariants.lean:22 | α=Unit, β=Nat, o=some(), f=fun _=>some 5, b=5; bind is some5. |
| demand_eq_true_of_some; Reactivegas/Invariants.lean:29 | b=true; demand true=some(). |
| demand_none_of_ne_true; Reactivegas/Invariants.lean:36 | b=false; false!=true. |
| bool_not_true; Reactivegas/Invariants.lean:43 | b=false; !b=true. |
| bool_and_left; Reactivegas/Invariants.lean:49 | b1=b2=true; conjunction=true. |
| bool_and_right; Reactivegas/Invariants.lean:55 | b1=b2=true; conjunction=true. |
| eq_nil_of_isEmpty; Reactivegas/Invariants.lean:61 | α=Unit, l=[]; isEmpty=true. |
| stripCollections_referente_ne; Reactivegas/Invariants.lean:68 | r="ref", cols=[]; no antecedent membership before conclusion; strip nil=([],[]). |
| bal_bump_ne_lemma; Reactivegas/Invariants.lean:719 | u="a", k="b", m=[], d=0; k!=u. |
| bal_bump_ne; Reactivegas/Invariants.lean:745 | u="a", k="b", m=[], d=0; k!=u. |
| bal_foldl_bump_ge; Reactivegas/Invariants.lean:750 | l=[], m=[], w=0, k="a"; 0<=w is 0<=0 (explicitly supplied); fold returns m. |
| splitUser_amount_lemma; Reactivegas/Invariants.lean:771 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| splitUser_amount; Reactivegas/Invariants.lean:796 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| refundAll_bal_ge_lemma; Reactivegas/Invariants.lean:801 | l=[], m=[], w="a"; universal nonnegative-pledge premise true on empty l. |
| refundAll_bal_ge; Reactivegas/Invariants.lean:823 | l=[], m=[], w="a"; universal nonnegative-pledge premise true on empty l. |
| stripCollections_sublist_lemma; Reactivegas/Invariants.lean:828 | r="other", c=(7,"ref",false,[],[]), cols=[c], y=c; strip keeps c, so membership premise inhabited. Same antecedent at both sites. |
| pullCollection_mem_lemma; Reactivegas/Invariants.lean:83 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| stripCollections_sublist; Reactivegas/Invariants.lean:847 | r="other", c=(7,"ref",false,[],[]), cols=[c], y=c; strip keeps c, so membership premise inhabited. Same antecedent at both sites. |
| stripCollections_amount_lemma; Reactivegas/Invariants.lean:852 | r="ref", p=("a",1), c=(7,"ref",false,[p],[]), cols=[c]; strip removes c and returns [p] as refund pledges. |
| sumBal_cons; Reactivegas/State.lean:125 | m/t=[], k/u="a", v/d=0 where parameters occur; no antecedents. |
| bal_cons; Reactivegas/State.lean:128 | m/t=[], k/u="a", v/d=0 where parameters occur; no antecedents. |
| bump_sum; Reactivegas/State.lean:131 | m/t=[], k/u="a", v/d=0 where parameters occur; no antecedents. |
| bal_bump; Reactivegas/State.lean:142 | m/t=[], k/u="a", v/d=0 where parameters occur; no antecedents. |
| sumPledges_append; Reactivegas/State.lean:153 | l1=l2=[]; no antecedents. |
| refundAll_sum; Reactivegas/State.lean:159 | m=[], l=[]; no antecedents. |
| not_mem_users_of_splitUser_none; Reactivegas/State.lean:172 | u="a", l=[]; splitUser returns none. |
| splitUser_sum_lemma; Reactivegas/State.lean:194 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| splitUser_sum; Reactivegas/State.lean:221 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| splitUser_sublist_lemma; Reactivegas/State.lean:225 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| splitUser_sublist; Reactivegas/State.lean:253 | u="a", v=1, l=[("a",1)], r=[]; splitUser returns some(1,[]). |
| escrowSum_cons; Reactivegas/State.lean:257 | c=(7,"ref",false,[],[]), t=[]; no antecedents. |
| pullCollection_id_lemma; Reactivegas/State.lean:260 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_id; Reactivegas/State.lean:283 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_sum_lemma; Reactivegas/State.lean:287 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_sum; Reactivegas/State.lean:313 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_sublist_lemma; Reactivegas/State.lean:318 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| pullCollection_sublist; Reactivegas/State.lean:346 | c=7, x=(7,"ref",false,[],[]), cols=[x], rest=[]; pullCollection returns some(x,[]); for det set p1=p2=(x,[]). |
| stripCollections_sum; Reactivegas/State.lean:351 | r="ref", cols=[]; no antecedent membership before conclusion; strip nil=([],[]). |
| stripCollections_referente; Reactivegas/State.lean:374 | r="ref", cols=[]; no antecedent membership before conclusion; strip nil=([],[]). |
