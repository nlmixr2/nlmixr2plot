## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked the 3 reverse dependencies (babelmixr2, nlmixr2, shinyMixR)
against this version and found no new problems.

## Changes in this release

* Fixes the random check failure in `test-model-diagram.R` ("long products
  keep their input and elimination parts"), which asserted that
  `modelGraph()` took under 30 s and failed on a loaded CRAN machine
  (48.4 s).  The timing assertion is removed and the test is skipped on
  CRAN; no test asserts on elapsed time any more.
* `plot()` of a fit with more than one variance level (between-occasion
  variability) no longer errors, and the saved figures are much smaller.
* On our reference check (two cores, `_R_CHECK_LIMIT_CORES_=TRUE`) tests
  take 122s, donttest examples 22s and the vignettes 16s.
