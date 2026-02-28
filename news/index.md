# Changelog

## nlmixr2plot 5.0.1

- Updated nlmixr2 to optionally use the `tidyvpc` package

## nlmixr2plot 5.0.0

CRAN release: 2025-11-29

- Updated to use nlmixr2 5.0 file format

## nlmixr2plot 3.0.3

CRAN release: 2025-08-23

- Updated tests and added `Ops` method (removing `+` method) to work
  with ggplot2 version 4
  ([\#39](https://github.com/nlmixr2/nlmixr2plot/issues/39))

## nlmixr2plot 3.0.1

CRAN release: 2025-02-14

- Added the ability to add ggplot items to a nlmixr2 plot list with `+`

## nlmixr2plot 3.0.0

CRAN release: 2024-09-18

- Update to support new version of rxode2

## nlmixr2plot 2.0.9

CRAN release: 2024-05-29

- Bug fix for
  [`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
  where input data frame for models did not stratify all the time when
  the columns were upper case.

## nlmixr2plot 2.0.8

CRAN release: 2024-01-31

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now returns a
  named list of lists so that users can more easily choose which plots
  to include, if all plots are not desired. Or the user could use those
  names as the basis of figure captions (fix
  [\#8](https://github.com/nlmixr2/nlmixr2plot/issues/8)).

- Models without eta values (between subject variability) now have more
  consistent plotting to models with eta values (fix
  [\#18](https://github.com/nlmixr2/nlmixr2plot/issues/18)).

- The package was updated to align with changes in ggplot2.

- The package was updated to skip tests with `"focei"` and `linCmt()`

## nlmixr2plot 2.0.7

CRAN release: 2022-10-20

- Added fixes for upcoming ‘ggplot2’ 3.4 release

- Added changes for the generalized log-likelihood supported by
  ‘nlmixr2est’

- Added a `NEWS.md` file to track changes to the package.
