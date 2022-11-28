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
    if (length(levels(.dat$CMT)) > 1) {
      .doCmt <- TRUE
    }
  }
  if (!.doCmt) {
    .dat$CMT <- factor("All Data")
  } else {
    levels(.dat$CMT) <- paste("Endpoint: ", levels(.dat$CMT))
  }
  if (any(names(.dat) == "CENS")) {
    .censLeft <- any(.dat$CENS == 1)
    .censRight <- any(.dat$CENS == -1)
    if (.censLeft & .censRight) {
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

.dvPlot <- function(.dat0, vars, log = FALSE) {
  .xgxr <- getOption("rxode2.xgxr", TRUE) &&
    requireNamespace("xgxr", quietly = TRUE)
  if (any(names(.dat0) == "CENS")) {
    dataPlot <- data.frame(DV = .dat0$DV, CENS = .dat0$CENS, utils::stack(.dat0[, vars, drop = FALSE]))
    .aes <- ggplot2::aes(.data$values, .data$DV, color = .data$CENS)
    if (length(levels(.dat0$CENS)) == 3) {
      .color <- ggplot2::scale_color_manual(values = c("blue", "black", "red"))
    } else {
      .color <- ggplot2::scale_color_manual(values = c("black", "red"))
    }
    .legendPos <- ggplot2::theme(
      legend.position = "bottom", legend.box = "horizontal",
      legend.title = ggplot2::element_blank()
    )
  } else {
    dataPlot <- data.frame(DV = .dat0$DV, utils::stack(.dat0[, vars, drop = FALSE]))
    .aes <- ggplot2::aes(.data$values, .data$DV)
    .color <- NULL
    .legendPos <- NULL
  }
  .logx <- NULL
  .logy <- NULL
  if (log) {
    if (.xgxr) {
      .logx <- xgxr::xgx_scale_x_log10()
      .logy <- xgxr::xgx_scale_y_log10()
    } else {
      .logx <- ggplot2::scale_x_log10()
      .logy <- ggplot2::scale_y_log10()
    }
  }
  ggplot2::ggplot(dataPlot, .aes) +
    ggplot2::facet_wrap(~ind) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "red", linewidth = 1.2) +
    .logx +
    .logy +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::xlab("Predictions") +
    rxode2::rxTheme() +
    .color +
    .legendPos
}

.scatterPlot <- function(.dat0, vars, .cmt, log = FALSE) {
  dataPlot <- .dat0
  dataPlot$x <- dataPlot[[vars[1]]]
  dataPlot$y <- dataPlot[[vars[2]]]
  if (any(names(.dat0) == "CENS")) {
    .aes <- ggplot2::aes(.data$x, .data$y, color = .data$CENS)
    if (length(levels(dataPlot$CENS)) == 3) {
      .color <- ggplot2::scale_color_manual(values = c("blue", "black", "red"))
    } else {
      .color <- ggplot2::scale_color_manual(values = c("black", "red"))
    }
    .legendPos <- ggplot2::theme(
      legend.position = "bottom", legend.box = "horizontal",
      legend.title = ggplot2::element_blank()
    )
  } else {
    .aes <- ggplot2::aes(.data$x, .data$y)
    .color <- NULL
    .legendPos <- NULL
  }
  .xgxr <- getOption("rxode2.xgxr", TRUE) &&
    requireNamespace("xgxr", quietly = TRUE)
  .logx <- NULL
  if (log) {
    if (.xgxr) {
      .logx <- xgxr::xgx_scale_x_log10()
    } else {
      .logx <- ggplot2::scale_x_log10()
    }
  }
  ggplot2::ggplot(dataPlot, .aes) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 0, intercept = 0, col = "red") +
    ggplot2::ggtitle(.cmt, paste0(vars[1], " vs ", vars[2])) +
    ggplot2::xlab(vars[1]) +
    ggplot2::ylab(vars[2]) +
    rxode2::rxTheme() +
    .color +
    .legendPos +
    .logx
}

#' Plot a nlmixr2 data object
#'
#' Plot some standard goodness of fit plots for the focei fitted object
#'
#' @param x a focei fit object
#' @param ... additional arguments (currently ignored)
#' @return An `nlmixr2PlotList` object (a list of ggplot2 objects with easier
#'   plotting for all of them at the same time)
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
plot.nlmixr2FitData <- function(x, ...) {
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
  .dat <- .setupPlotData(x)
  for (.cmt in levels(.dat$CMT)) {
    .lst[[.cmt]] <- plotCmt(.dat, cmt = .cmt)
  }

  class(.lst) <- "nlmixr2PlotList"
  .lst
}

#' Plot data from one compartment
#'
#' @inheritParams plot.nlmixr2FitData
#' @param cmt The value of the current compartment
#' @return A list of ggplot2 objects
#' @noRd
plotCmt <- function(x, cmt) {
  .lst <- list()
  .hasCwres <- any(names(x) == "CWRES")
  .hasNpde <- any(names(x) == "NPD")
  .hasPred <- any(names(x) == "PRED")
  .hasIpred <- any(names(x) == "IPRED")
  .datCmt <- x[x$CMT == cmt,, drop = FALSE]
  if (nrow(.datCmt) > 0) {
    if (.hasPred & .hasIpred) {
      .lst[["dv_pred_ipred_linear"]] <-
        .dvPlot(.datCmt, c("PRED", "IPRED")) +
        ggplot2::ggtitle(cmt, "DV vs PRED/IPRED")

      .lst[["dv_pred_ipred_log"]] <-
        .dvPlot(.datCmt, c("PRED", "IPRED"), TRUE) +
        ggplot2::ggtitle(cmt, "log-scale DV vs PRED/IPRED")
    } else if (.hasIpred) {
      .lst[["dv_ipred_linear"]] <-
        .dvPlot(.datCmt, "IPRED") +
        ggplot2::ggtitle(cmt, "DV vs IPRED")

      .lst[["dv_ipred_log"]] <-
        .dvPlot(.datCmt, "IPRED", TRUE) +
        ggplot2::ggtitle(cmt, "log-scale DV vs IPRED")
    } else if (.hasPred) {
      .lst[["dv_pred_linear"]] <-
        .dvPlot(.datCmt, "PRED") +
        ggplot2::ggtitle(cmt, "DV vs PRED")

      .lst[["dv_pred_log"]] <-
        .dvPlot(.datCmt, "PRED", TRUE) +
        ggplot2::ggtitle(cmt, "log-scale DV vs PRED")
    }

    if (.hasCwres) {
      .lst[["dv_cpred_linear"]] <-
        .dvPlot(.datCmt, c("CPRED", "IPRED")) +
        ggplot2::ggtitle(cmt, "DV vs CPRED/IPRED")

      .lst[["dv_cpred_log"]] <-
        .dvPlot(.datCmt, c("CPRED", "IPRED"), TRUE) +
        ggplot2::ggtitle(cmt, "log-scale DV vs CPRED/IPRED")
    }

    if (.hasNpde) {
      .lst[["dv_epred_linear"]] <-
        .dvPlot(.datCmt, c("EPRED", "IPRED")) +
        ggplot2::ggtitle(cmt, "DV vs EPRED/IPRED")

      .lst[["dv_epred_log"]] <-
        .dvPlot(.datCmt, c("EPRED", "IPRED"), TRUE) +
        ggplot2::ggtitle(cmt, "log-scale DV vs EPRED/IPRED")
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
    # .idPlot <- try(plot.nlmixr2AugPred(nlmixr2AugPred(object)));
    # if (inherits(.idPlot, "try-error")){
    .ids <- unique(.datCmt$ID)
    .s <- seq(1, length(.ids), by = 16)
    .j <- 0
    for (i in .s) {
      .j <- .j + 1
      .tmp <- .ids[seq(i, i + 15)]
      .tmp <- .tmp[!is.na(.tmp)]
      .d1 <- .datCmt[.datCmt$ID %in% .tmp, ]

      .pIndividual <- ggplot2::ggplot(.d1, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes(x = .data$TIME, y = .data$IPRED), col = "red", linewidth = 1.2)
      if (any(names(.d1) == "PRED")) {
        .pIndividual <- .pIndividual + ggplot2::geom_line(ggplot2::aes(x = .data$TIME, y = .data$PRED), col = "blue", linewidth = 1.2)
      }
      .pIndividual <- .pIndividual + ggplot2::facet_wrap(~ID) +
        ggplot2::ggtitle(cmt, sprintf("Individual Plots (%s of %s)", .j, length(.s))) +
        rxode2::rxTheme()
      if (any(names(.d1) == "lowerLim")) {
        .pIndividual <-
          .pIndividual +
          geom_cens(ggplot2::aes(lower = .data$lowerLim, upper = .data$upperLim), fill = "purple")
      }
      .lst[[paste("individual", i, sep = "_")]] <- .pIndividual
    }
    .datCmt$id2 <- factor(paste0("id: ", .datCmt$ID, "; dose#: ", .datCmt$dosenum))
    .ids <- unique(.datCmt$id2)
    .s <- seq(1, length(.ids), by = 16)
    .j <- 0
  }
  class(.lst) <- "nlmixr2PlotList"
  .lst
}

#' @export
plot.nlmixr2PlotList <- function(x, y, ...) {
  .x <- x
  class(.x) <- NULL
  for (.i in seq_along(.x)) {
    try(plot(.x[[.i]]))
  }
}

#' @export
print.nlmixr2PlotList <- function(x, ...) {
  plot.nlmixr2PlotList(x, ...)
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
#' fit <- nlmixr2(one.compartment, theo_sd,  est="saem", saemControl(print=0))
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
    .p0 <- ggplot2::ggplot(.m, ggplot2::aes(.data$iter, .data$val)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~par, scales = "free_y")
    .niter <- attr(class(x$parHist), "niter")
    if (!is.null(.niter)) {
      .p0 <-
        .p0 +
        ggplot2::geom_vline(xintercept = .niter, col = "blue", linewidth = 1.2)
    }
    .p0 <- .p0 + rxode2::rxTheme()
    return(.p0)
  } else {
    return(invisible(NULL))
  }
}

#' @export
traceplot.nlmixr2FitCoreSilent <- traceplot.nlmixr2FitCore
