# nlmixr2plot 5.0.2.9000

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
