# M1R static review before its single elaboration

Original M1 instrument and receipts are unchanged. Revised file changes the two
`prefix` identifier occurrences to `artifactRootPrefix`, uses new M1R output,
and explicitly bounds Lean elaboration at maxRecDepth 4096 / maxHeartbeats
8000000. The outer wall limit remains 300 seconds and resource/output bounds
remain 20000 declarations / 256 MiB. These options anticipate the intentionally
large expression dump; they do not skip any declaration or classifier branch.

Reviewed all let/for/fun binders and match arms against Lean syntax; the reserved
`prefix` collision is removed. Checked pinned Lean 4.25.0 source for
ConstantInfo.value? (allowOpaque Bool), Expr Repr, IO.FS.lines (Array String) and
SearchPath.findModuleWithExt (IO Option FilePath). Every ConstantInfo constructor
is matched explicitly. Home index failures abort, absent home belongs to the
current instrument and is excluded; tracked/owned module comparison is
bidirectional. IO exceptions propagate. Undecided result sorts are recorded,
then abort completeness; no silent empty/partial result is an extent.

All candidate files and all 30 M1 oleans match their frozen manifests by a new
read-only verification. No compile probe performed. Bash syntax checks are only
shell checks; Lean compile/setup risk remains charged to this one attempt.
