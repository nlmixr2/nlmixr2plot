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
