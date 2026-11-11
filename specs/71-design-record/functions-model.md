# Functions model — #71, version 2

- justfile recipe `design-citations` — no arguments; exit status is the checker
  verdict on the record.
- justfile recipe `design-citations-controls` — no arguments; exits zero only
  when every control turned the checker red for its stated reason and the real
  record turned it green.
- The checker's own command line is the commit owner's choice; it is documented
  in the record's section on citations.
