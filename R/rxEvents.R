## Emitting events on the rxode2 event bus (see rxode2::rxEventListen()), so
## loggers such as nlmixr2log can store the plots made from a fit.  Every
## call goes through these wrappers, which do nothing when rxode2 has no
## event bus.  Work done inside a plot (e.g. vpcPlot's simulation) is silent.

#' @noRd
.nlmixr2plotEventBus <- function() {
  exists("rxEventEmit", envir = asNamespace("rxode2"), inherits = FALSE)
}

#' @noRd
.nlmixr2plotEventEnter <- function() {
  if (.nlmixr2plotEventBus()) getExportedValue("rxode2", ".rxEventEnter")()
  invisible()
}

#' Leave the plot's scope and emit fitResult with the plots
#' @noRd
.nlmixr2plotEventExit <- function(result, fit, call, kind, data = NULL) {
  if (!.nlmixr2plotEventBus()) {
    return(invisible())
  }
  .exit <- getExportedValue("rxode2", ".rxEventExit")
  if (is.null(result)) {
    return(.exit())
  }
  .exit("fitResult", fit = fit, result = result, kind = kind, call = call, data = data,
        fun = kind)
}
