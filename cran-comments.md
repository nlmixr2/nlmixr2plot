## R CMD check results

0 errors | 0 warnings | 1 note

Checked with `R CMD check --as-cran --run-donttest` on Ubuntu 24.04,
R 4.6.1.

### Note: examples with elapsed time > 5s

    plot.nlmixr2AugPred  9.3s
    plot.nlmixr2FitData 11.0s
    vpcPlot              8.9s
    traceplot            6.1s

These examples estimate a nonlinear mixed-effects model before plotting
it, which is the behavior being documented; the runtime is dominated by
the model fit rather than by the plotting code.  They are all wrapped in
`\donttest{}` and so are not run in CRAN's default check.  We have kept
the fits as small as we can while still producing a meaningful figure.

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
