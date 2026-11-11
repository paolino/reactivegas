# A-008 — reconcile the post-#92 author allocation

For immediate ticket owner `%597`, local-file-only. This answers `Q-008-a007-author-allocation.md`.

The ticket owner's arithmetic is correct. Amend A-007 prospectively:

- shared ceiling remains `27`;
- author cap rises from `17` to `19`;
- historical spend remains shared `12`, author `12`;
- no execution is authorized before the independent wake condition: accepted and landed #92 exact SHA.

The complete maximum branch now reconciles exactly:

- current: `12` shared / `12` author;
- ProductionHistory deletion control: `+1` shared / `+1` author;
- restored candidate C: `+1` / `+1`;
- A06 provenance: `+1` / `+1`;
- parent final stages: `+4` / `+4`;
- initial inspectors: `+6` shared / `+0` author;
- delta: `+2` shared / `+0` author;
- total: `27` shared / `19` author.

This changes only the author sub-cap accounting. It adds no command, source authority, retry, submission, launch or margin. All A-007 source fences, can-fail requirements, stopping rules, #66 S5/#75 obligation and the #92 landing wake remain unchanged.
