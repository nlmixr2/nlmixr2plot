
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixr2plot: The plotting routines for nlmixr2

<!-- badges: start -->

[![R build
status](https://github.com/nlmixr2/nlmixr2plot/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixr2/nlmixr2plot/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/nlmixr2plot/badge)](https://www.codefactor.io/repository/github/nlmixr2/nlmixr2plot)
[![CRAN
checks](https://cranchecks.info/badges/summary/nlmixr2plot)](https://cran.r-project.org/web/checks/check_results_nlmixr2plot.html)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/nlmixr2plot)](https://cran.r-project.org/package=nlmixr2plot)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/nlmixr2plot)](https://cran.r-project.org/package=nlmixr2plot)
[![codecov](https://codecov.io/gh/nlmixr2/nlmixr2plot/branch/main/graph/badge.svg)](https://codecov.io/gh/nlmixr2/nlmixr2plot)
<!-- badges: end -->

The goal of nlmixr2plot is to provide the nlmixr2 core estimation
routines.

## Installation

You can install the development version of nlmixr2plot from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("nlmixr2/nlmixr2data")
remotes::install_github("nlmixr2/lotri")
remotes::install_github("nlmixr2/rxode2")
remotes::install_github("nlmixr2/nlmixr2est")
remotes::install_github("nlmixr2/nlmixr2extra")
remotes::install_github("nlmixr2/nlmixr2plot")
```

For most people, using nlmixr2 directly would be likely easier.

``` r
library(nlmixr2est)
#> Loading required package: nlmixr2data
library(nlmixr2plot)

## The basic model consiss of an ini block that has initial estimates
one.compartment <- function() {
  ini({
    tka <- 0.45 # Log Ka
    tcl <- 1 # Log Cl
    tv <- 3.45    # Log V
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  # and a model block with the error sppecification and model specification
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v
    cp ~ add(add.sd)
  })
}

## The fit is performed by the function nlmixr/nlmix2 specifying the model, data and estimate
fit <- nlmixr2(one.compartment, theo_sd,  est="saem", saemControl(print=0))
#> ℹ parameter labels from comments will be replaced by 'label()'
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem model...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → optimizing duplicate expressions in saem model...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> ✔ done
#> rxode2 1.0.0 using 4 threads (see ?getRxThreads)
#> Calculating covariance matrix
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of saem model...
#> ✔ done
#> → finding duplicate expressions in saem predOnly model 0...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → finding duplicate expressions in saem predOnly model 1...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → optimizing duplicate expressions in saem predOnly model 1...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → finding duplicate expressions in saem predOnly model 2...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> ✔ done
#> → Calculating residuals/tables
#> ✔ done
#> → compress origData in nlmixr2 object, save 5952
#> → compress phiM in nlmixr2 object, save 62360
#> → compress parHist in nlmixr2 object, save 9408
#> → compress saem0 in nlmixr2 object, save 22720

print(fit)
#> ── nlmixr SAEM OBJF by FOCEi approximation ─────────────────────────────────────
#> 
#>  Gaussian/Laplacian Likelihoods: AIC() or $objf etc. 
#>  FOCEi CWRES & Likelihoods: addCwres() 
#> 
#> ── Time (sec $time): ───────────────────────────────────────────────────────────
#> 
#>            setup covariance  saem table compress    other
#> elapsed 0.001535   0.009003 2.737 0.019    0.017 1.748462
#> 
#> ── Population Parameters ($parFixed or $parFixedDf): ───────────────────────────
#> 
#>        Parameter  Est.     SE %RSE Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
#> tka       Log Ka 0.454  0.196 43.1       1.57 (1.07, 2.31)     71.5   -0.0203% 
#> tcl       Log Cl  1.02 0.0853  8.4       2.76 (2.34, 3.26)     27.6      3.46% 
#> tv         Log V  3.45 0.0454 1.32       31.5 (28.8, 34.4)     13.4      9.89% 
#> add.sd           0.693                               0.693                     
#>  
#>   Covariance Type ($covMethod): linFim
#>   No correlations in between subject variability (BSV) matrix
#>   Full BSV covariance ($omega) or correlation ($omegaR; diagonals=SDs) 
#>   Distribution stats (mean/skewness/kurtosis/p-value) available in $shrink 
#> 
#> ── Fit Data (object is a modified tibble): ─────────────────────────────────────
#> # A tibble: 132 × 19
#>   ID     TIME    DV  PRED    RES IPRED   IRES  IWRES eta.ka eta.cl   eta.v    cp
#>   <fct> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 1      0     0.74  0     0.74   0     0.74   1.07   0.103 -0.491 -0.0820  0   
#> 2 1      0.25  2.84  3.27 -0.426  3.87 -1.03  -1.48   0.103 -0.491 -0.0820  3.87
#> 3 1      0.57  6.57  5.85  0.723  6.82 -0.246 -0.356  0.103 -0.491 -0.0820  6.82
#> # … with 129 more rows, and 7 more variables: depot <dbl>, center <dbl>,
#> #   ka <dbl>, cl <dbl>, v <dbl>, tad <dbl>, dosenum <dbl>

# this now gives the goodness of fit plots
plot(fit)
```

<img src="man/figures/README-example-1.png" width="100%" /><img src="man/figures/README-example-2.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis
    #> Warning: Transformation introduced infinite values in continuous y-axis

<img src="man/figures/README-example-3.png" width="100%" /><img src="man/figures/README-example-4.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-5.png" width="100%" /><img src="man/figures/README-example-6.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-7.png" width="100%" /><img src="man/figures/README-example-8.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-9.png" width="100%" /><img src="man/figures/README-example-10.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-11.png" width="100%" /><img src="man/figures/README-example-12.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-13.png" width="100%" /><img src="man/figures/README-example-14.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-15.png" width="100%" /><img src="man/figures/README-example-16.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-17.png" width="100%" /><img src="man/figures/README-example-18.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-19.png" width="100%" /><img src="man/figures/README-example-20.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-21.png" width="100%" /><img src="man/figures/README-example-22.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-23.png" width="100%" /><img src="man/figures/README-example-24.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-25.png" width="100%" /><img src="man/figures/README-example-26.png" width="100%" />

    #> Warning: Transformation introduced infinite values in continuous x-axis

<img src="man/figures/README-example-27.png" width="100%" /><img src="man/figures/README-example-28.png" width="100%" />
