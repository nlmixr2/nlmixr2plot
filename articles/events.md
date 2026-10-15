# nlmixr2plot events for loggers

`rxode2` provides a small event bus: packages that finish a top-level
piece of work announce it with
[`rxode2::rxEventEmit()`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html),
and any package can listen with
[`rxode2::rxEventListen()`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html).
The main consumer is `nlmixr2log` (in development), which records fits,
simulations and their results, but you can write your own listener. See
[`?rxode2::rxEventListen`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html)
for the full API.

Two rules of the bus matter here:

- Events are delivered only from the *outermost* operation. Every
  emitting function opens an operation scope, and an event is delivered
  only when no scope is active, so work done inside another operation is
  silent.
- Emitting is a no-op when no listener is registered, and `nlmixr2plot`
  checks that the bus exists before using it, so with an older `rxode2`
  nothing changes.

This vignette describes what `nlmixr2plot` itself emits.

## What nlmixr2plot emits

`nlmixr2plot` emits a single event, `fitResult`, when one of these
functions returns successfully:

| Function | `kind` | `fit` | `data` |
|----|----|----|----|
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of a fit ([`plot.nlmixr2FitData()`](https://nlmixr2.github.io/nlmixr2plot/reference/plot.nlmixr2FitData.md)) | `"plot"` | the fit | `NULL` |
| [`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md) | `"vpcPlot"` | the fit (see below) | `NULL` |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of an [`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html) object ([`plot.nlmixr2AugPred()`](https://nlmixr2.github.io/nlmixr2plot/reference/plot.nlmixr2AugPred.md)) | `"augPredPlot"` | `NULL` | the [`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html) data |

The `fitResult` payload fields are:

| Field | Contents |
|----|----|
| `fit` | The fit the plots belong to, or `NULL` for `plot(augPred(...))`. |
| `result` | The value the function returns: the [`ggtibble::gglist`](https://humanpred.github.io/ggtibble/reference/gglist.html) of plots from `plot(fit)` or `plot(augPred(...))`, or the `ggplot` from [`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md) (the vpc database with `vpcdb = TRUE`). |
| `kind` | `"plot"`, `"vpcPlot"` or `"augPredPlot"`. |
| `call` | The call, normalized by `rxode2` so its head is the `kind` (e.g. `plot(fit)`, `vpcPlot(fit, n = 5)` or `augPredPlot(ap)`) and large inlined values are replaced by markers. |
| `data` | For `"augPredPlot"` only, the [`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html) data that was plotted; otherwise `NULL`. |

Details:

- **One event per call.** The event is emitted when the function exits,
  after all the plots are built.
- **[`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
  and supplied simulations.** When
  [`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
  is given a
  [`vpcSim()`](https://nlmixr2.github.io/nlmixr2est/reference/vpcSim.html)
  result instead of a fit, the `fit` field is the fit that simulation
  came from, not the simulation.
- **`plot(augPred(...))` has no fit.** An
  [`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html) object does
  not carry its fit, so `fit` is `NULL` and the plotted data is sent as
  `data`; a logger such as `nlmixr2log` uses it to find the run it
  belongs to.
- **Nothing on error or empty results.** If the function fails, or
  returns `NULL`, no event is emitted.

## What is not emitted

Everything that happens *inside* these functions is silent:

- The simulation that
  [`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
  runs (through
  [`nlmixr2est::vpcSim()`](https://nlmixr2.github.io/nlmixr2est/reference/vpcSim.html)
  and
  [`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html))
  emits no `solveComplete`.
- Plots made internally, such as the
  [`traceplot()`](https://nlmixr2.github.io/nlmixr2plot/reference/traceplot.md)
  inside `plot(fit)`, emit no events of their own.
- Calling these functions from inside another operation scope (for
  example, from a function wrapped in
  [`rxode2::rxEventScope()`](https://nlmixr2.github.io/rxode2/reference/rxEventListen.html))
  emits nothing; only the outermost operation reports.

Other events you may see around plotting come from other packages:
`nlmixr2est` emits `fitComplete` when
[`nlmixr2()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
finishes a fit and `solveComplete` from
[`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html),
[`simulate()`](https://rdrr.io/r/stats/simulate.html),
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`vpcSim()`](https://nlmixr2.github.io/nlmixr2est/reference/vpcSim.html)
called at the top level. Those require an `nlmixr2est` with event hooks;
`nlmixr2plot` does not emit them.

## Listening to the events

A listener is a function `function(event, ...)`; the payload fields
arrive as named arguments. Here is a small listener that records what it
sees. First fit a small model:

``` r

library(nlmixr2plot)
library(nlmixr2est)
#> Loading required package: nlmixr2data

one.cmt <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.5)
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

fit <- suppressMessages(
  nlmixr2(one.cmt, nlmixr2data::theo_sd, est = "posthoc",
          control = list(print = 0))
)
```

Then register a listener and make some plots:

``` r

seen <- list()
rxode2::rxEventListen("demo", function(event, ...) {
  p <- list(...)
  seen[[length(seen) + 1L]] <<- data.frame(
    event = event,
    kind = if (is.null(p$kind)) NA_character_ else p$kind,
    hasFit = !is.null(p$fit),
    resultClass = class(p$result)[1],
    call = paste(deparse(p$call), collapse = " ")
  )
})

p <- plot(fit)
v <- suppressMessages(suppressWarnings(vpcPlot(fit, n = 50)))
ap <- suppressMessages(augPred(fit))
a <- plot(ap)

rxode2::rxEventUnlisten("demo")
do.call(rbind, seen)
#>           event        kind hasFit     resultClass                 call
#> 1     fitResult        plot   TRUE          gglist            plot(fit)
#> 2     fitResult     vpcPlot   TRUE ggplot2::ggplot vpcPlot(fit, n = 50)
#> 3 solveComplete     augPred  FALSE  nlmixr2AugPred         augPred(fit)
#> 4     fitResult augPredPlot  FALSE          gglist      augPredPlot(ap)
```

`plot(fit)` and
[`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
each report one `fitResult`, and the simulation inside
[`vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.md)
stays silent. `plot(ap)` reports a `fitResult` with no fit. With an
`nlmixr2est` that has event hooks, the top-level
[`augPred()`](https://rdrr.io/pkg/nlme/man/augPred.html) call also
reports a `solveComplete` (emitted by `nlmixr2est`).

After `rxEventUnlisten()`, plotting reports nothing:

``` r

n <- length(seen)
p <- plot(fit)
length(seen) == n
#> [1] TRUE
```

## Full logging

For a complete record of fits, simulations, updates and the plots made
from them, use `nlmixr2log` (in development), which listens to all of
these events (including `fitResult` from `nlmixr2plot`) and stores the
plots with the run they belong to.
