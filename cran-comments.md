## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked the 3 reverse dependencies (babelmixr2, nlmixr2, shinyMixR)
against this version and found no new problems.

## Changes in this release

* New automatic model diagrams: `modelGraph()` and `modelDiagram()` draw a
  model's compartments and flows, and `plot()` of an 'rxode2' model draws
  its diagram.  'DiagrammeR' is a new suggested package and is used
  conditionally.
* Many VPC fixes: `vpcPlot()` and friends now honour the `data` argument
  and a supplied `vpcSim()` simulation, and several multiple-endpoint,
  censored and prediction-corrected VPC errors are fixed.
* The expensive tests stay under `skip_on_cran()`.  On our reference
  check (two cores, `_R_CHECK_LIMIT_CORES_=TRUE`) tests take 110s,
  donttest examples 39s and the new vignette 5s.
