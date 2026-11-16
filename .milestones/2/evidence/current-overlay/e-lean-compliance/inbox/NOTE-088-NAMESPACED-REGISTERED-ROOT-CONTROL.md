# #92 inspection input: registered roots are not necessarily dot-free names

Your 10:09:10 handback describes TRACKED_ROOT_IMPORTS as tracked module names selected by having no dot. That is a concrete lead against the already required contract that a properly registered new project root is supported. A name-shape filter is dynamic but does not by itself identify Lake's declared roots.

Give one of the two independent inspectors this fault scenario within the current mandate and budget: a new properly registered Lean library/root whose root module is namespaced (for example a root configured as Extra.Probe, with no top-level Extra.lean and no existing aggregator importing it). Verify the actual Lake root declaration and build registration from the pinned toolchain, then exercise the committed mandatory path. The original discovery/reach checks must remain enabled. Does this legitimate source integrate, or does the no-dot selection reproduce the original reach gap?

This is a hypothesis from the reported selection mechanism, not an executed result or a prescribed verdict. If Lake registration or transitive reach makes the example behave differently, report that evidence. Do not manufacture an invalid import and call its setup failure the semantic control. A passing control must show that the actual registered namespaced root is built and covered, not removed or made reachable by an unrelated ad hoc aggregator edit.

Preserve the frozen submission and all original #92 rows. No code correction before inspection, second audit, scope expansion or budget grant. Route through the commissioning owner, with honest pre/post-START timing. Adjudicate the union once, alongside S3's separate campaign without mixing budgets.
