.setupPlotData <- function(data) {
  .dat <- as.data.frame(data)
  .w <- which(!is.na(.dat$RES))
  .dat <- .dat[.w, ]
  .doCmt <- FALSE
  if (any(names(.dat) == "CMT")) {
    if (length(levels(.dat$CMT)) > 1) {
      .doCmt <- TRUE
    }
  }
  if (!.doCmt) {
    .dat$CMT <- factor(rep(1, length(.dat[, 1])), 1, "All Data")
  } else {
    levels(.dat$CMT) <- paste("Endpoint: ", levels(.dat$CMT))
  }
  if (any(names(.dat) == "CENS")) {
    .censLeft <- any(.dat$CENS == 1)
    .censRight <- any(.dat$CENS == -1)
    if (.censLeft & .censRight) {
      .dat$CENS <- factor(.dat$CENS, c(-1, 0, 1), c("Right censored data", "Observed data", "left censored data"))
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

#' @importFrom utils stack
#' @importFrom ggplot2 aes element_blank facet_wrap geom_abline geom_point ggplot scale_color_manual theme scale_x_log10 scale_y_log10 xlab
#' @importFrom rxode2 rxTheme
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
    dataPlot <- data.frame(DV = .dat0$DV, stack(.dat0[, vars, drop = FALSE]))
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
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "red", size = 1.2) +
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
#' @param ... additional arguments
#' @return Nothing, called for its side effects
#' @author Wenping Wang & Matthew Fidler
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
#' # This shows many goodness of fit plots
#' plot(fit)
#'
#' }
#' @export
plot.nlmixr2FitData <- function(x, ...) {
  .lst <- list()
  object <- x
  .tp <- traceplot(x)
  if (!is.null(.tp)) .lst[[length(.lst) + 1]] <- .tp
  if (exists(".bootPlotData", object$env)) {
    .bp <- nlmixr2extra::bootplot(x)
    .lst[[length(.lst) + 1]] <- .bp
  }
  .dat <- .setupPlotData(x)
  .hasCwres <- any(names(.dat) == "CWRES")
  .hasNpde <- any(names(.dat) == "NPD")
  .hasPred <- any(names(.dat) == "PRED")
  .hasIpred <- any(names(.dat) == "IPRED")
  for (.cmt in levels(.dat$CMT)) {
    .dat0 <- .dat[.dat$CMT == .cmt,, drop = FALSE]
    if (dim(.dat0)[1] > 0) {
      if (.hasPred) {
        .p1 <- .dvPlot(.dat0, c("PRED", "IPRED")) +
          ggplot2::ggtitle(.cmt, "DV vs PRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1

        .p1 <- .dvPlot(.dat0, c("PRED", "IPRED"), TRUE) +
          ggplot2::ggtitle(.cmt, "log-scale DV vs PRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1
      } else if (.hasIpred) {
        .p1 <- .dvPlot(.dat0, "IPRED") +
          ggplot2::ggtitle(.cmt, "DV vs IPRED")
        .lst[[length(.lst) + 1]] <- .p1

        .p1 <- .dvPlot(.dat0, "IPRED", TRUE) +
          ggplot2::ggtitle(.cmt, "log-scale DV vs IPRED")
        .lst[[length(.lst) + 1]] <- .p1
      }


      if (.hasCwres) {
        .p1 <- .dvPlot(.dat0, c("CPRED", "IPRED")) +
          ggplot2::ggtitle(.cmt, "DV vs CPRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1

        .p1 <- .dvPlot(.dat0, c("CPRED", "IPRED"), TRUE) +
          ggplot2::ggtitle(.cmt, "log-scale DV vs CPRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1
      }

      if (.hasNpde) {
        .p1 <- .dvPlot(.dat0, c("EPRED", "IPRED")) +
          ggplot2::ggtitle(.cmt, "DV vs EPRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1

        .p1 <- .dvPlot(.dat0, c("EPRED", "IPRED"), TRUE) +
          ggplot2::ggtitle(.cmt, "log-scale DV vs EPRED/IPRED")
        .lst[[length(.lst) + 1]] <- .p1
      }

      for (x in c("IPRED", "PRED", "CPRED", "EPRED", "TIME", "tad")) {
        if (any(names(.dat0) == x)) {
          for (y in c("IWRES", "IRES", "RES", "CWRES", "NPD")) {
            if (any(names(.dat0) == y)) {
              if (y == "CWRES" && x %in% c("TIME", "CPRED")) {
                .doIt <- TRUE
              } else if (y == "NPD" && x %in% c("TIME", "EPRED")) {
                .doIt <- TRUE
              } else if (!(y %in% c("CWRES", "NPD"))) {
                .doIt <- TRUE
              }
              if (.doIt) {
                .p2 <- .scatterPlot(.dat0, c(x, y), .cmt, log = FALSE)
                .lst[[length(.lst) + 1]] <- .p2
                .p2 <- .scatterPlot(.dat0, c(x, y), .cmt, log = TRUE)
                .lst[[length(.lst) + 1]] <- .p2
              }
            }
          }
        }
      }
      # .idPlot <- try(plot.nlmixr2AugPred(nlmixr2AugPred(object)));
      # if (inherits(.idPlot, "try-error")){
      .ids <- unique(.dat0$ID)
      .s <- seq(1, length(.ids), by = 16)
      .j <- 0
      for (i in .s) {
        .j <- .j + 1
        .tmp <- .ids[seq(i, i + 15)]
        .tmp <- .tmp[!is.na(.tmp)]
        .d1 <- .dat0[.dat0$ID %in% .tmp, ]

        .p3 <- ggplot2::ggplot(.d1, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
          ggplot2::geom_point() +
          ggplot2::geom_line(aes(x = .data$TIME, y = .data$IPRED), col = "red", size = 1.2)
        if (any(names(.d1) == "PRED")) {
          .p3 <- .p3 + ggplot2::geom_line(aes(x = .data$TIME, y = .data$PRED), col = "blue", size = 1.2)
        }
        .p3 <- .p3 + ggplot2::facet_wrap(~ID) +
          ggplot2::ggtitle(.cmt, sprintf("Individual Plots (%s of %s)", .j, length(.s))) +
          rxode2::rxTheme()
        if (any(names(.d1) == "lowerLim")) {
          .p3 <-
            .p3 +
            geom_cens(aes(lower = .data$lowerLim, upper = .data$upperLim), fill = "purple")
        }
        .lst[[length(.lst) + 1]] <- .p3
      }
      .dat0$id2 <- factor(paste0("id: ", .dat0$ID, "; dose#: ", .dat0$dosenum))
      .ids <- unique(.dat0$id2)
      .s <- seq(1, length(.ids), by = 16)
      .j <- 0
      # for (i in .s) {
      #   .j <- .j + 1
      #   .tmp <- .ids[seq(i, i + 15)]
      #   .tmp <- .tmp[!is.na(.tmp)]
      #   .d1 <- .dat0[.dat0$id2 %in% .tmp, ]

      #   .p3 <- ggplot2::ggplot(.d1, ggplot2::aes(x = tad, y = DV)) +
      #     ggplot2::geom_point() +
      #     ggplot2::geom_line(aes(x = tad, y = IPRED), col = "red", size = 1.2) +
      #     ggplot2::geom_line(aes(x = tad, y = PRED), col = "blue", size = 1.2) +
      #     ggplot2::facet_wrap(~id2) +
      #     ggplot2::ggtitle(.cmt, sprintf("Individual TAD Plots (%s of %s)", .j, length(.s))) +
      #     rxode2::rxTheme()
      #   if (any(names(.d1) == "lowerLim")) {
      #     .p3 <- .p3 + geom_cens(aes(lower=lowerLim, upper=upperLim), fill="purple")
      #   }
      #   .lst[[length(.lst) + 1]] <- .p3
      # }
    }
  }

  # .id <- unique(.dat0$id2)
  # if (grDevices::dev.cur() != 1){
  #     .x  <- .lst
  #     for (.i in seq_along(.x)){
  #         plot(.x[[.i]])
  #     }
  # }
  class(.lst) <- "nlmixr2PlotList"
  return(.lst)
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

##' @export
plot.nlmixr2FitCoreSilent <- plot.nlmixr2FitCore

#'
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
        ggplot2::geom_vline(xintercept = .niter, col = "blue", size = 1.2)
    }
    .p0 <- .p0 + rxode2::rxTheme()
    return(.p0)
  } else {
    return(invisible(NULL))
  }
}

##' @export
traceplot.nlmixr2FitCoreSilent <- traceplot.nlmixr2FitCore
