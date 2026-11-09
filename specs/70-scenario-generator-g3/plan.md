# Plan — #70 G3

One bisect-safe slice on `sim/70-generator-g3`, one draft PR with preview. Files:
simulator gates (a new page-check gate, or the generator gate extended) and this
folder. Generated histories from G1's seeds are loaded into the built page through
its own loading path; the page's display is compared with the core state the
generator produced. Display mutants prove the check can fail. CI gate:
`just simulator`. The page, core and claim gate are read, never edited.
