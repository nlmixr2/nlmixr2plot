## Resubmission

This is a resubmission.  The previous submission was rejected for

    Check: Overall checktime, Result: NOTE
      Overall checktime 12 min > 10 min

with the follow-up note that this came mainly from

    * checking tests ... [594s] OK

and the suggestion to use small toy data, few iterations, or to run less
important tests conditionally.  Thank you -- all three apply here, and
the test time is down by a factor of three.

On our reference check (Ubuntu 24.04, R 4.6.1, restricted to two cores
with `OMP_THREAD_LIMIT=2` and `_R_CHECK_LIMIT_CORES_=TRUE`):

                            before   after
    tests               258s     ->   87s
    donttest examples   108s     ->   75s
    total               6m56s    ->  3m30s

What changed:

* Most of it was a performance bug in the package itself, so the fix
  helps users as well as the check.  `plot()` expanded its paginated
  individual plots one-per-page with `ggtibble::as_gglist()`, which
  deep-copies the plot through `serialize()`/`unserialize()`.  Those
  plots are built inside functions that hold the whole nlmixr2 fit, so
  their aes environments capture it and every page copy walked the entire
  fit object -- 42% of `plot()`'s runtime.  The pages are now built by
  re-adding the paginated facet for each page, taking `plot()` from 5-7s
  to about 2s.

* Fewer iterations: the examples fit with `nBurn = 10, nEm = 20` rather
  than the default 200/300 SAEM iterations.

* Smaller simulations: the tests simulate 10 replicates for the VPC and
  NPDE calculations instead of the default 300.  They assert on plot
  structure rather than on simulated quantiles, so this exercises the
  same code.

* Conditional tests: the two most expensive test files (multiple-endpoint
  PK/PD and censored VPC plots) already run under `skip_on_cran()` and so
  are not part of the check above; they run in our own CI.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this release

* `plot()` on a fit now adds a nested `"bsv"` section with QQ plots,
  BSV-BSV correlation plots and BSV-by-covariate plots (new `covariate`
  argument).
* Plot collections returned by `plot()` are now `gglist` objects from the
  `ggtibble` package instead of the internal `nlmixr2PlotList` class.
* Individual/`augPred` plots are now paginated with
  `ggforce::facet_wrap_paginate()`.  The minimum R version is now 4.3.
* Fixed several censored (LLOQ/ULOQ) VPC errors, including an "object of
  type 'closure' is not subsettable" error with a non-time `idv` and a
  `quantile()` error for prediction-corrected censored VPCs.
