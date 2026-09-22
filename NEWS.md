# nlmixr2plot 5.1.1

* Added automatic model diagrams (#10).  `modelGraph()` parses a model's
  differential equations (from a model function, `rxode2` model/UI or an
  `nlmixr2` fit) into a graph of compartments and flows (mass transfer,
  eliminations, inputs and non-mass-transfer interactions like effect
  compartments or PD stimulation/inhibition), detecting dosing compartments
  from the dosing records.  `modelDiagram()` lays it out with dosing and
  absorption compartments above the central compartment, peripheral
  compartments to the left, eliminations/metabolites below and PD models to
  the right, and draws it with the `"DiagrammeR"` (Graphviz), `"ggplot2"` or
  `"dot"` (DOT source) engine.  Dosing properties (`lag()`, `f()`, `rate()`,
  `dur()`) are shown as annotations on their compartment, and `delay()` terms
  are understood.  `plot()` of an `rxode2` user interface
  (`rxUi`) object or compiled `rxode2` model draws its diagram, and the new
  "Automatic model diagrams" vignette describes the feature.

* `plot()` of an `augPred()` object now accepts a base-R style `log` argument
  (`log = "y"`, `"x"` or `"xy"`) to draw the individual plots on log-scaled
  axes; non-positive values, which cannot be shown on a log axis, are dropped
  (#32).
* `vpcPlot()`, `vpcPlotTad()`, `vpcCens()` and `vpcCensTad()` now use a
  supplied `nlmixr2est::vpcSim()` simulation instead of discarding it and
  re-simulating with the default `n = 300`.  Supplying `n` alongside a
  simulation warns that it is ignored, and `pred_corr = TRUE` errors unless the
  simulation was created with `pred = TRUE` (#57).  The observed-data
  prediction correction for a supplied simulation is computed from that
  simulation's own fit rather than from whichever `vpcSim(pred = TRUE)` ran
  last.
* Fixed the prediction-corrected VPC (`pred_corr = TRUE`) ignoring the `data`
  argument; the observed data was rebuilt from the fit's dataset instead of the
  supplied `data` (#62).
* Fixed stratified censored VPCs (`vpcCens()`/`vpcCensTad()`, or `vpcPlot()`
  with `cens = TRUE`) failing with "The following specified stratification
  columns were NOT found in observation data"; stratification columns missing
  from the fit table are now carried over from the original data, matched by
  row (#56).
* Fixed the confidence-band width (and its legend label) for `vpcPlot()`/
  `vpcPlotTad()` with the `tidyvpc` backend; `ci = c(lower, upper)` was passed
  to `tidyvpc::vpcstats()` as `conf.level = ci[2]` instead of the actual
  interval width `ci[2] - ci[1]`, so the default `ci = c(0.05, 0.95)` (a 90% CI)
  was drawn and labeled as a 95% CI.

# nlmixr2plot 5.1.0

* Fixed an "object of type 'closure' is not subsettable" error in `vpcCensTad()`
  (and `vpcCens()`/`vpcPlot()`/`vpcPlotTad()` with `cens = TRUE` and a non-time
  `idv`); the censored VPC now passes the `sim`/`obs` column mappings to
  `vpc::vpc_cens()` explicitly instead of relying on column guessing
  (nlmixr2#390).
* Fixed the censored VPC confidence band, which was computed as if every
  simulated row were its own replicate.  The simulated `dv` is now mapped to the
  `sim` column instead of being copied into a new `dv` column, so `vpc` groups
  the simulated data by replicate as it does for the uncensored VPC.
* Fixed a `quantile()` "missing values and NaNs not allowed" error in
  prediction-corrected `vpcPlot()`/`vpcPlotTad()` on censored (LLOQ/ULOQ) fits
  with the `vpc` backend; censored records are now dropped before the
  pred-corrected VPC (which is shown for non-censored data only), matching the
  `vpc` package's stated behavior (nlmixr2#390).
* Fixed prediction-corrected `vpcPlot()`/`vpcPlotTad()` with
  `method = "tidyvpc", cens = TRUE`, which previously errored that the observed
  data had no `cens` column; the censoring column is now retained when the
  observed dataset is rebuilt for pred-correction.
* `plot()` on a fit with between-subject variability (BSV) now adds a nested
  `"bsv"` section (inside each data/compartment group) with QQ plots for each
  BSV parameter, BSV-BSV correlation plots (when more than one BSV parameter is
  present) and, via the new `covariate` argument, BSV-by-covariate plots
  (box-and-whisker for categorical or low-cardinality covariates, scatter plus a
  linear trend with confidence interval for continuous ones).  Covariate values
  are taken from each subject's first record, so they are assumed time-invariant
  (#51).
* Plot collections returned by `plot()` (for fits and `augPred` objects) are now
  `gglist` objects from the `ggtibble` package instead of the internal
  `nlmixr2PlotList` class.  They still print/plot all figures at once and support
  broadcasting `ggplot2` additions (e.g. `plot(fit) + ggplot2::theme_bw()`), and
  the top-level object remains a named, nested collection
  (`plot(fit)[["Endpoint: ..."]]`).  The collection can also be converted to a
  `ggtibble` for reporting with `ggtibble::as_ggtibble()`.
* Individual/`augPred` plots are now paginated with
  `ggforce::facet_wrap_paginate()`, removing the hand-written 16-IDs-per-page
  chunking.  Requires `ggtibble (>= 1.0.3.9000)` and `ggforce`; the minimum R
  version is now 4.3.
* `plot()` on a fit (and on an `augPred` object) is now about 3x faster.  The
  paginated individual plots were expanded one-per-page with
  `ggtibble::as_gglist()`, which deep-copies the plot through
  `serialize()`/`unserialize()`; because those plots are built inside functions
  that hold the whole fit, each copy walked the entire fit object.  The pages
  are now produced by re-adding the paginated facet for each page instead.
* Removed obsolete `rxode2::.linCmtSensB()` test guards. That internal
  was removed from `rxode2` in 2025, so the `try()`/`skip_if_not()`
  checks always fell through and never skipped. `linCmt()` gradients are
  now always available, so the guards are unnecessary and the tests run
  unconditionally.

# nlmixr2plot 5.0.2

* Fixed an error when plotting models without compartments (#33)

# nlmixr2plot 5.0.1

* Updated nlmixr2 to optionally use the `tidyvpc` package

# nlmixr2plot 5.0.0

* Updated to use nlmixr2 5.0 file format

# nlmixr2plot 3.0.3

* Updated tests and added `Ops` method (removing `+` method) to work
  with ggplot2 version 4 (#39)

# nlmixr2plot 3.0.1

* Added the ability to add ggplot items to a nlmixr2 plot list with `+`

# nlmixr2plot 3.0.0

* Update to support new version of rxode2

# nlmixr2plot 2.0.9

* Bug fix for `vpcPlot()` where input data frame for models did not
  stratify all the time when the columns were upper case.

# nlmixr2plot 2.0.8

* `plot()` now returns a named list of lists so that users can more easily
  choose which plots to include, if all plots are not desired.  Or the user
  could use those names as the basis of figure captions (fix #8).

* Models without eta values (between subject variability) now have more
  consistent plotting to models with eta values (fix #18).

* The package was updated to align with changes in ggplot2.

* The package was updated to skip tests with `"focei"` and `linCmt()`

# nlmixr2plot 2.0.7

* Added fixes for upcoming 'ggplot2' 3.4 release

* Added changes for the generalized log-likelihood supported by 'nlmixr2est'

* Added a `NEWS.md` file to track changes to the package.
