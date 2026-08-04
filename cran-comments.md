## R CMD check results

0 errors | 0 warnings | 0 notes

Checked with `R CMD check --as-cran` (which runs the `\donttest{}`
examples) on Ubuntu 24.04, R 4.6.1, with the check restricted to two
cores (`OMP_THREAD_LIMIT=2`, `_R_CHECK_LIMIT_CORES_=TRUE`).

## Resubmission

This is a resubmission.  The previous submission was rejected because the
overall check time was over 10 minutes.  The check time has been cut by
half; on our two-core reference check it went from 6m56s to 3m30s:

                            before   after
    donttest examples   108s     ->   75s
    tests               258s     ->   87s
    total               6m56s    ->  3m30s

The reduction comes from three changes, none of which drops test or
example coverage:

* A performance fix in the package itself.  `plot()` expanded its
  paginated individual plots one-per-page with `ggtibble::as_gglist()`,
  which deep-copies the plot through `serialize()`/`unserialize()`.
  Those plots are built inside functions that hold the whole nlmixr2 fit,
  so every copy walked the entire fit object.  The pages are now built by
  re-adding the paginated facet for each page, which makes `plot()` about
  three times faster for users as well as in the check.

* The examples fit their models with a small number of SAEM iterations
  (`nBurn = 10, nEm = 20`) rather than the default 200/300.  The fits are
  still real fits and still produce the figures being documented.

* The tests simulate 10 replicates for VPC and NPDE calculations instead
  of the default 300.  The tests assert on plot structure, not on the
  simulated quantiles, so the smaller simulations exercise the same code.

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
