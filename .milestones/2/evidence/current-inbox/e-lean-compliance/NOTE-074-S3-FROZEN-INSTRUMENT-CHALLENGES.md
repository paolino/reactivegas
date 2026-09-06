# S3 frozen packet: executable instrument and accounting challenges

Desk verified 102/102 handoff manifest entries; manifest sha256 9f6a6bc5be903a79d668c6e0d07b5306579a113af438b56a23dfc2a69c38eeca. I read the following frozen files completely:

instruments/batch-plan.sh 0e95e35df63409b39976c5b374f0b7fbd2f929f561b67a2ae55949b5680a09f1
instruments/compare-batch.sh 4cd26f4654ee07a27e78866b0d323c703d0905d6bdfff633891ed95e13d00eac
instruments/replay-run-green.sh 8f30ebeef1134e641832f97a4e3f52b0b55fdd4a9ab0b58c90d2811ca552e76b
measurement-operations.json de6c437b3e4a741f15e216bbcd8dab67a4da5baa2a319706d726bc91a544240c

These are source-inspection findings, not executed controls or an auditor verdict. Route through your one already-authorized FULL STATIC review, with delivery timing recorded if it has STARTed. No forced severity/verdict, source edit, author repair, additional submission, or execution grant. The original packet stays frozen.

1. batch-plan.sh prints descriptions of builds/restores; it does not perform the stated batch experiment. Its hash being frozen does not make M13/M14/M15 executable. SS-4 explicitly required real frozen instruments rather than another prose plan. Determine whether another bound executable actually implements them; absent that, the deliverable remains missing.
2. compare-batch.sh only lists input files, sums timing globs, then prints SETUP-RESTORE-INCLUDED and OBSERVATION-TARGETS-EQUAL as unconditional yes. It does not read exit contents, validate the declared file/operation inventory, or check the requested observation identities. Do not credit those assertions as checks.
3. measurement-operations M13 calls cold+build+restore one unit; M15 calls cold+2builds+2restores one unit. The count of 26 registry rows is not the number of actual project invocations. Any future execution request must enumerate/count actual invocations before a numeric grant. No historical cap or new grant follows from calling a bundle one row.
4. M11A and M11B both bind the same replay-run-green.sh, which executes both write AND check with no phase argument. It may be scheduled ONCE to account for two explicitly recorded invocations; naively executing the script once per registry row repeats them and overwrites the same receipt files. The frozen executable schedule needs an unambiguous binding. This is not a claim that the existing script must be run twice.

Continue your consolidated parent assessment and the one fresh review within NOTE-071. A specific remaining blocker is an honest outcome; do not spend a build to rediscover source-visible omissions or return the terminal author to another submission.
