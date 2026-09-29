.augPredEndpoint <- NULL

#' Expand a paginated ggplot into one ggplot per page
#'
#' `ggtibble::as_gglist()` copies the plot for each page with
#' `unserialize(serialize(plot))`, which writes out the data and every
#' environment the plot references.  Re-adding the paginated facet with a
#' different `page` produces the same plot without that copy.
#'
#' @param p ggplot whose facet is a `ggforce::facet_wrap_paginate()` for page 1
#' @param facet function of a page number returning the facet for that page
#' @return list of ggplot objects, one per page (always at least one)
#' @noRd
.paginate <- function(p, facet) {
  .n <- ggforce::n_pages(p)
  if (is.null(.n) || is.na(.n) || .n < 1L) {
    .n <- 1L
  }
  lapply(seq_len(.n), function(page) p + facet(page))
}

#' Parse the base-R style `log` argument for augPred plots
#'
#' @param log character string containing any of `"x"` and `"y"`
#' @return list with logical `x` and `y` elements (log-scaled axes)
#' @noRd
.augPredLog <- function(log) {
  if (is.null(log) || (is.logical(log) && length(log) == 1L && !is.na(log) && !log)) {
    log <- ""
  }
  if (!is.character(log) || length(log) != 1L || is.na(log) || !grepl("^[xy]*$", log)) {
    stop(
      "'log' must be a single string containing only \"x\" and/or \"y\" (like \"\", \"x\", \"y\" or \"xy\")",
      call. = FALSE
    )
  }
  .x <- grepl("x", log, fixed = TRUE)
  .y <- grepl("y", log, fixed = TRUE)
  list(x = .x, y = .y)
}

#' Plot a nlmixr2 augPred object
#'
#' @param x augPred object
#'
#' @param y ignored, used to mach plot generic
#'
#' @param ... Other arguments (ignored)
#'
#' @param log a character string which contains `"x"` if the x axis
#'   is to be logarithmic, `"y"` if the y axis is to be logarithmic
#'   and `"xy"` or `"yx"` if both axes are to be logarithmic (as in
#'   [graphics::plot.default()]).  The default `""` uses linear axes.
#'   Non-positive values cannot be shown on a log axis and are dropped.
#'
#' @return A `ggtibble::gglist` object (a list of ggplot2 objects, one per page
#'   of individual plots)
#'
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
#' fit <- nlmixr2est::nlmixr2(one.compartment, theo_sd,  est="saem",
#'                            saemControl(print=0, nBurn = 10, nEm = 20))
#'
#' # augPred shows more points for the fit:
#'
#' a <- nlmixr2est::augPred(fit)
#'
#' # you can plot it with plot(augPred object)
#' plot(a)
#'
#' # or with a log-scaled y axis
#' plot(a, log = "y")
#'
#' }
#' @export
#' @importFrom ggplot2 .data
plot.nlmixr2AugPred <- function(x, y, ..., log = "") {
  .log <- .augPredLog(log)
  if (any(names(x) == "Endpoint")) {
    .ret <- list()
    # Skip endpoint levels without any rows (#44)
    for (.tmp in levels(droplevels(as.factor(x$Endpoint)))) {
      utils::assignInMyNamespace(".augPredEndpoint", .tmp)
      .x <- x[which(x$Endpoint == .tmp), names(x) != "Endpoint"]
      .r <- plot.nlmixr2AugPred(.x, log = log)
      for (.k in seq_along(.r)) {
        .ret[[length(.ret) + 1L]] <- .r[[.k]]
      }
    }
    return(ggtibble::new_gglist(.ret))
  } else {
    if (.log$x || .log$y) {
      dobs <- x[.augPredIsObserved(x), ]
      dpred <- x[!.augPredIsObserved(x), ]
      # Non-positive (and missing) values cannot be drawn on a log axis.  Drop them, but
      # start a new line group after each dropped prediction so the line
      # breaks at the gap instead of bridging it.
      .ok <- function(d) {
        .r <- !is.na(d$time) & !is.na(d$values)
        if (.log$x) {
          .r <- .r & d$time > 0
        }
        if (.log$y) {
          .r <- .r & d$values > 0
        }
        .r
      }
      dobs <- dobs[.ok(dobs), ]
      dpred <- dpred[order(dpred$id, dpred$ind, dpred$time), ]
      .okPred <- .ok(dpred)
      .seg <- stats::ave(as.integer(!.okPred), dpred$id, dpred$ind, FUN = cumsum)
      dpred$.group <- interaction(dpred$id, dpred$ind, .seg, drop = TRUE)
      dpred <- dpred[.okPred, ]
      # Observations are points, not lines, so they have no line group
      dobs$.group <- factor(rep(NA, nrow(dobs)), levels = levels(dpred$.group))
      x <- rbind(dpred, dobs)
    }
    .p <- .plotData(.augPredFigure(.log$x, .log$y, .augPredEndpoint), x)
    return(ggtibble::new_gglist(.paginate(.p, .augPredFacet)))
  }
}

#' Which augPred rows are observations
#'
#' @param data augPred data
#' @return logical vector, `TRUE` for observed (not predicted) rows
#' @noRd
.augPredIsObserved <- function(data) {
  data$ind == "Observed"
}

#' Layer data for the augPred prediction lines
#'
#' The augPred figure holds one data frame (observations and predictions) and
#' each layer selects its rows at build time, so the figure does not store
#' separate copies for its layers.
#'
#' @param data plot data
#' @return the prediction rows of `data`
#' @noRd
.augPredPredicted <- function(data) {
  data[!.augPredIsObserved(data), ]
}

#' Layer data for the augPred observation points
#'
#' @inheritParams .augPredPredicted
#' @return the observation rows of `data`
#' @noRd
.augPredObserved <- function(data) {
  data[.augPredIsObserved(data), ]
}

#' augPred figure, without its data
#'
#' @param logX,logY log-scale the x or y axis.  On a log axis the prediction
#'   lines are grouped by the `.group` column, which splits them where
#'   non-positive values were dropped.
#' @param title plot title (the endpoint, or `NULL`)
#' @return ggplot without data, faceted for page 1; see `.plotData()`
#' @noRd
.augPredFigure <- function(logX, logY, title) {
  ggplot2::ggplot(mapping = ggplot2::aes(.data$time, .data$values, col = .data$ind)) +
    ggplot2::geom_line(
      if (logX || logY) ggplot2::aes(group = .data$.group),
      data = .augPredPredicted,
      linewidth = 1.2
    ) +
    ggplot2::geom_point(data = .augPredObserved) +
    .augPredFacet(1L) +
    rxode2::rxTheme() +
    ggplot2::ggtitle(label = title) +
    .logScales(x = logX, y = logY)
}

#' Facet for one page of augPred plots
#'
#' @param page page number
#' @return a `ggforce::facet_wrap_paginate()` facet
#' @noRd
.augPredFacet <- function(page) {
  ggforce::facet_wrap_paginate(~id, nrow = 4, ncol = 4, page = page)
}
