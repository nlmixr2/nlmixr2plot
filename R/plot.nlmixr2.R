#' Prepare data for plotting by converting numeric to human-readable data
#'
#' @param data The data.frame to convert
#' @return The data.frame with compartment names updated to character versions
#'   and censoring indicating what type of censoring was used, if applicable
#' @noRd
.setupPlotData <- function(data) {
  .dat <- as.data.frame(data)
  .w <- which(!is.na(.dat$IRES))
  .dat <- .dat[.w, ]
  .doCmt <- FALSE
  if (any(names(.dat) == "CMT")) {
    # Only keep compartments that actually have observations; state
    # compartments (like depot/central) are still factor levels but have no
    # data and should not become endpoints (#44)
    .dat$CMT <- droplevels(as.factor(.dat$CMT))
    if (length(levels(.dat$CMT)) > 1) {
      .doCmt <- TRUE
    }
  }
  if (!.doCmt) {
    .dat$CMT <- factor(rep("All Data", nrow(.dat)), levels = "All Data")
  } else {
    levels(.dat$CMT) <- paste("Endpoint: ", levels(.dat$CMT))
  }
  if (any(names(.dat) == "CENS")) {
    .censLeft <- any(.dat$CENS == 1)
    .censRight <- any(.dat$CENS == -1)
    if (.censLeft && .censRight) {
      .dat$CENS <- factor(.dat$CENS, c(-1, 0, 1), c("Right censored data", "Observed data", "Left censored data"))
    } else if (.censLeft) {
      .dat$CENS <- factor(.dat$CENS, c(0, 1), c("Observed data", "Censored data"))
    } else if (.censRight) {
      .dat$CENS <- factor(.dat$CENS, c(0, -1), c("Observed data", "Censored data"))
    } else {
      .dat <- .dat[, names(.dat) != "CENS"]
    }
  }
  return(.dat)
}

#' Attach the data to a figure built without it
#'
#' The figures in this package are built by helpers that receive only small
#' values (column names, titles, flags) and get their data here.  A ggplot
#' keeps the frames that built it: `plot_env`, the environments of `aes()`
#' quosures and of facet and smoother formulas, the frame that called each
#' `geom_*()`/`stat_*()` (through the layer's ggproto object) and the frame of
#' `ggplot()` itself, which holds its `data` argument (through the plot's
#' `Layout` ggproto object).  `serialize()` writes every one of those frames
#' in full.  Building from data-free frames means a saved figure holds its
#' data once, in `$data`, instead of the fit, the full plotting data or the
#' other figures that are in scope where it is built.  (An S3 method's frame
#' also references its caller's frame, as `.GenericCallEnv`, so figures are
#' not built directly in methods either.)
#'
#' The builders add their scales in their last `+`.  Each `+` clones the
#' plot's scales and the clone keeps the scales it was cloned from, so a
#' scale added earlier is stored again for every later `+` (about 50 KB each
#' time for an xgxr log scale).  Adding `guides()` keeps a copy of the plot as
#' it was at that point (even when added straight after `ggplot()`), so the
#' builders do not use it.
#'
#' @param p ggplot built without data (`ggplot2::ggplot(mapping = ...)`)
#' @param data data frame for the figure
#' @return `p` with `data` as its data (fortified, as `ggplot2::ggplot()`
#'   does)
#' @noRd
.plotData <- function(p, data) {
  p$data <- ggplot2::fortify(data)
  p
}

#' Number of censoring levels in plotting data
#'
#' @param data data frame from `.setupPlotData()`
#' @return `NULL` when `data` has no `CENS` column, otherwise the number of
#'   levels of `CENS`
#' @noRd
.censLevels <- function(data) {
  if (any(names(data) == "CENS")) {
    length(levels(data$CENS))
  } else {
    NULL
  }
}

#' Colour scale and legend position for censored data
#'
#' @param nCens number of censoring levels (from `.censLevels()`), or `NULL`
#'   for data without censoring
#' @return list of the colour scale and legend theme, or `NULL` when `nCens`
#'   is `NULL`
#' @noRd
.censColor <- function(nCens) {
  if (is.null(nCens)) {
    return(NULL)
  }
  if (nCens == 3) {
    .color <- ggplot2::scale_color_manual(values = c("blue", "black", "red"))
  } else {
    .color <- ggplot2::scale_color_manual(values = c("black", "red"))
  }
  list(
    .color,
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = ggplot2::element_blank()
    )
  )
}

#' Log-scale x and y scales, using xgxr when available
#'
#' @param x,y add a log-scaled x or y axis
#' @return list of scales (empty when neither axis is log-scaled)
#' @noRd
.logScales <- function(x, y) {
  .xgxr <- getOption("rxode2.xgxr", TRUE) &&
    requireNamespace("xgxr", quietly = TRUE)
  .scales <- list()
  if (x) {
    .scales <- c(.scales, list(if (.xgxr) xgxr::xgx_scale_x_log10() else ggplot2::scale_x_log10()))
  }
  if (y) {
    .scales <- c(.scales, list(if (.xgxr) xgxr::xgx_scale_y_log10() else ggplot2::scale_y_log10()))
  }
  .scales
}

.dvPlot <- function(.dat0, vars, cmt, subtitle, log = FALSE) {
  if (any(names(.dat0) == "CENS")) {
    dataPlot <- data.frame(DV = .dat0$DV, CENS = .dat0$CENS, utils::stack(.dat0[, vars, drop = FALSE]))
  } else {
    dataPlot <- data.frame(DV = .dat0$DV, utils::stack(.dat0[, vars, drop = FALSE]))
  }
  .plotData(.dvFigure(.censLevels(.dat0), log, cmt, subtitle), dataPlot)
}

#' DV vs prediction figure, without its data
#'
#' @param nCens number of censoring levels, or `NULL` without censoring
#' @param log use log-scaled axes
#' @param cmt compartment (endpoint) name for the title
#' @param subtitle plot subtitle
#' @return ggplot without data; see `.plotData()`
#' @noRd
.dvFigure <- function(nCens, log, cmt, subtitle) {
  if (is.null(nCens)) {
    .aes <- ggplot2::aes(.data$values, .data$DV)
  } else {
    .aes <- ggplot2::aes(.data$values, .data$DV, color = .data$CENS)
  }
  ggplot2::ggplot(mapping = .aes) +
    ggplot2::facet_wrap(~ind) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "red", linewidth = 1.2) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::xlab("Predictions") +
    ggplot2::ggtitle(cmt, subtitle) +
    rxode2::rxTheme() +
    c(.logScales(x = log, y = log), .censColor(nCens))
}

.scatterPlot <- function(.dat0, vars, .cmt, log = FALSE) {
  dataPlot <- .dat0
  dataPlot$x <- dataPlot[[vars[1]]]
  dataPlot$y <- dataPlot[[vars[2]]]
  .plotData(.scatterFigure(vars, .cmt, .censLevels(.dat0), log), dataPlot)
}

#' Residual (or prediction) scatter figure, without its data
#'
#' @param vars names of the x and y columns (for the titles and axis labels)
#' @param cmt compartment (endpoint) name for the title
#' @param nCens number of censoring levels, or `NULL` without censoring
#' @param log use a log-scaled x axis
#' @return ggplot without data; see `.plotData()`
#' @noRd
.scatterFigure <- function(vars, cmt, nCens, log) {
  if (is.null(nCens)) {
    .aes <- ggplot2::aes(.data$x, .data$y)
  } else {
    .aes <- ggplot2::aes(.data$x, .data$y, color = .data$CENS)
  }
  ggplot2::ggplot(mapping = .aes) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 0, intercept = 0, col = "red") +
    ggplot2::ggtitle(cmt, paste0(vars[1], " vs ", vars[2])) +
    ggplot2::xlab(vars[1]) +
    ggplot2::ylab(vars[2]) +
    rxode2::rxTheme() +
    c(.censColor(nCens), .logScales(x = log, y = FALSE))
}

#' Individual (by subject) figure, without its data
#'
#' @param pred add the population prediction (`PRED`) line
#' @param cens add the censoring intervals (`lowerLim`/`upperLim`)
#' @return ggplot without data, faceted for page 1; see `.plotData()` and
#'   `.individualFacet()`
#' @noRd
.individualFigure <- function(pred, cens) {
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$TIME, y = .data$DV)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(x = .data$TIME, y = .data$IPRED), col = "red", linewidth = 1.2) +
    (if (pred) {
      ggplot2::geom_line(ggplot2::aes(x = .data$TIME, y = .data$PRED), col = "blue", linewidth = 1.2)
    }) +
    (if (cens) {
      geom_cens(ggplot2::aes(lower = .data$lowerLim, upper = .data$upperLim), fill = "purple")
    }) +
    .individualFacet(1L) +
    rxode2::rxTheme()
}

#' Facet for one page of individual plots
#'
#' @param page page number
#' @return a `ggforce::facet_wrap_paginate()` facet
#' @noRd
.individualFacet <- function(page) {
  ggforce::facet_wrap_paginate(~ID, nrow = 4, ncol = 4, page = page)
}

#' Plot a nlmixr2 data object
#'
#' Plot some standard goodness of fit plots for the focei fitted object.  When
#' the model has between-subject variability (BSV), the returned collection also
#' includes a nested `"bsv"` element (inside each data/compartment group) with
#' QQ plots for each BSV parameter, BSV-BSV correlation plots (when more than one
#' BSV parameter is present) and, when `covariate` is supplied, BSV-by-covariate
#' plots.
#'
#' @param x a focei fit object
#' @param covariate Optional character vector of covariate column names (from the
#'   model input data) to plot against each between-subject variability (BSV)
#'   parameter. Default `NULL` (no covariate plots). The first row per individual
#'   is used, so covariates are assumed time-invariant (a time-varying covariate
#'   is represented by its baseline value).
#' @param ... additional arguments (currently ignored)
#' @return A named, nested `ggtibble::gglist` object (a list of ggplot2 objects
#'   with easier plotting of all of them at the same time)
#' @author Wenping Wang & Matthew Fidler
#' @examples
#' \donttest{
#' library(nlmixr2est)
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- 1
#'     tv <- 3.45
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     d/dt(depot) = -ka * depot
#'     d/dt(center) = ka * depot - cl / v * center
#'     cp = center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' ## The fit is performed by the function nlmixr/nlmix2 specifying the model, data and estimate
#' fit <- nlmixr2(one.compartment, theo_sd,  est="saem", saemControl(print=0, nBurn = 10, nEm = 20))
#'
#' # This shows many goodness of fit plots
#' plot(fit)
#' }
#' @export
plot.nlmixr2FitData <- function(x, covariate = NULL, ...) {
  .lst <- list()
  object <- x
  .tp <- traceplot(x)
  if (!is.null(.tp)) {
    .lst[["traceplot"]] <- .tp
  }
  if (exists(".bootPlotData", object$env)) {
    .bp <- nlmixr2extra::bootplot(x)
    .lst[["bootplot"]] <- .bp
  }
  # Between-subject variability plots are model-level (etas are shared across
  # endpoints), so build them once and attach them to the first data group only.
  .bsv <- .bsvPlots(x, covariate = covariate)
  .dat <- .setupPlotData(x)
  .cmts <- levels(.dat$CMT)
  for (.i in seq_along(.cmts)) {
    .cmt <- .cmts[.i]
    .lst[[.cmt]] <- plotCmt(.dat, cmt = .cmt, bsv = if (.i == 1L) .bsv else NULL)
  }

  ggtibble::new_gglist(.lst)
}

#' Plot data from one compartment
#'
#' @inheritParams plot.nlmixr2FitData
#' @param cmt The value of the current compartment
#' @param bsv Optional nested `ggtibble::gglist` of between-subject variability
#'   plots to append to this compartment's plots (or `NULL` for none)
#' @return A list of ggplot2 objects
#' @noRd
plotCmt <- function(x, cmt, bsv = NULL) {
  .lst <- list()
  .hasCwres <- any(names(x) == "CWRES")
  .hasNpde <- any(names(x) == "NPD")
  .hasPred <- any(names(x) == "PRED")
  .hasIpred <- any(names(x) == "IPRED")
  .datCmt <- x[which(x$CMT == cmt), , drop = FALSE]
  if (nrow(.datCmt) > 0) {
    if (.hasPred && .hasIpred) {
      .lst[["dv_pred_ipred_linear"]] <-
        .dvPlot(.datCmt, c("PRED", "IPRED"), cmt, "DV vs PRED/IPRED")

      .lst[["dv_pred_ipred_log"]] <-
        .dvPlot(.datCmt, c("PRED", "IPRED"), cmt, "log-scale DV vs PRED/IPRED", log = TRUE)
    } else if (.hasIpred) {
      .lst[["dv_ipred_linear"]] <-
        .dvPlot(.datCmt, "IPRED", cmt, "DV vs IPRED")

      .lst[["dv_ipred_log"]] <-
        .dvPlot(.datCmt, "IPRED", cmt, "log-scale DV vs IPRED", log = TRUE)
    } else if (.hasPred) {
      .lst[["dv_pred_linear"]] <-
        .dvPlot(.datCmt, "PRED", cmt, "DV vs PRED")

      .lst[["dv_pred_log"]] <-
        .dvPlot(.datCmt, "PRED", cmt, "log-scale DV vs PRED", log = TRUE)
    }

    if (.hasCwres) {
      .lst[["dv_cpred_linear"]] <-
        .dvPlot(.datCmt, c("CPRED", "IPRED"), cmt, "DV vs CPRED/IPRED")

      .lst[["dv_cpred_log"]] <-
        .dvPlot(.datCmt, c("CPRED", "IPRED"), cmt, "log-scale DV vs CPRED/IPRED", log = TRUE)
    }

    if (.hasNpde) {
      .lst[["dv_epred_linear"]] <-
        .dvPlot(.datCmt, c("EPRED", "IPRED"), cmt, "DV vs EPRED/IPRED")

      .lst[["dv_epred_log"]] <-
        .dvPlot(.datCmt, c("EPRED", "IPRED"), cmt, "log-scale DV vs EPRED/IPRED", log = TRUE)
    }

    for (x in intersect(names(.datCmt), c("IPRED", "PRED", "CPRED", "EPRED", "TIME", "tad"))) {
      for (y in intersect(names(.datCmt), c("IWRES", "IRES", "RES", "CWRES", "NPD"))) {
        if (y == "CWRES" && x %in% c("TIME", "CPRED")) {
          .doIt <- TRUE
        } else if (y == "NPD" && x %in% c("TIME", "EPRED")) {
          .doIt <- TRUE
        } else if (!(y %in% c("CWRES", "NPD"))) {
          .doIt <- TRUE
        }
        if (.doIt) {
          .lst[[paste(y, x, "linear", sep = "_")]] <-
            .scatterPlot(.datCmt, c(x, y), cmt, log = FALSE)
          .lst[[paste(y, x, "log", sep = "_")]] <-
            .scatterPlot(.datCmt, c(x, y), cmt, log = TRUE)
        }
      }
    }
    # With multiple endpoints, an endpoint without censoring has only missing
    # limits, which geom_cens() cannot draw (#44)
    .cens <- any(names(.datCmt) == "lowerLim") &&
      any(!is.na(.datCmt$lowerLim) | !is.na(.datCmt$upperLim))
    .pIndividual <- .plotData(
      .individualFigure(pred = any(names(.datCmt) == "PRED"), cens = .cens),
      .datCmt
    )
    .pages <- .paginate(.pIndividual, .individualFacet)
    .nPages <- length(.pages)
    for (.j in seq_len(.nPages)) {
      .lst[[paste("individual", .j, sep = "_")]] <-
        .pages[[.j]] +
        ggplot2::ggtitle(cmt, sprintf("Individual Plots (%s of %s)", .j, .nPages))
    }
  }
  if (!is.null(bsv)) {
    .lst[["bsv"]] <- bsv
  }
  ggtibble::new_gglist(.lst)
}

#' @export
plot.nlmixr2FitCore <- function(x, ...) {
  stop("This is not a nlmixr2 data frame and cannot be plotted")
}

#' @export
plot.nlmixr2FitCoreSilent <- plot.nlmixr2FitCore

#' @title Produce trace-plot for fit if applicable
#'
#' @param x fit object
#' @param ... other parameters
#' @return Fit traceplot or nothing.
#' @author Rik Schoemaker, Wenping Wang & Matthew L. Fidler
#' @export
#' @examples
#' \donttest{
#'
#' library(nlmixr2est)
#' ## The basic model consiss of an ini block that has initial estimates
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45 # Log Ka
#'     tcl <- 1 # Log Cl
#'     tv <- 3.45    # Log V
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   # and a model block with the error sppecification and model specification
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     d/dt(depot) = -ka * depot
#'     d/dt(center) = ka * depot - cl / v * center
#'     cp = center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' ## The fit is performed by the function nlmixr/nlmix2 specifying the model, data and estimate
#' fit <- nlmixr2(one.compartment, theo_sd,  est="saem",
#'                saemControl(print=0, nBurn = 10, nEm = 20))
#'
#' # This shows the traceplot of the fit (useful for saem)
#' traceplot(fit)
#'
#'}
traceplot <- function(x, ...) {
  UseMethod("traceplot")
}

#' @rdname traceplot
#' @export
#' @importFrom ggplot2 .data
traceplot.nlmixr2FitCore <- function(x, ...) {
  .m <- x$parHistStacked
  if (!is.null(.m)) {
    return(.plotData(.traceplotFigure(attr(class(x$parHist), "niter")), .m))
  } else {
    return(invisible(NULL))
  }
}

#' Trace plot figure, without its data
#'
#' @param niter iteration(s) to mark with a vertical line, or `NULL`
#' @return ggplot without data; see `.plotData()`
#' @noRd
.traceplotFigure <- function(niter) {
  ggplot2::ggplot(mapping = ggplot2::aes(.data$iter, .data$val)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~par, scales = "free_y") +
    (if (!is.null(niter)) {
      ggplot2::geom_vline(xintercept = niter, col = "blue", linewidth = 1.2)
    }) +
    rxode2::rxTheme()
}

#' @export
traceplot.nlmixr2FitCoreSilent <- traceplot.nlmixr2FitCore
