.augPredEndpoint <- NULL

#' Expand a paginated ggplot into one ggplot per page
#'
#' `ggtibble::as_gglist()` copies the plot for each page with
#' `unserialize(serialize(plot))`.  The plots built here are created inside
#' functions that hold the whole nlmixr2 fit, so their aes environments drag
#' that object along and each copy costs seconds.  Re-adding the paginated
#' facet with a different `page` produces the same plot without walking the
#' environment.
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
#' @return list with logical `x` and `y` elements and `scales`, a list of
#'   ggplot2 scales to add to the plot
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
  .xgxr <- getOption("rxode2.xgxr", TRUE) &&
    requireNamespace("xgxr", quietly = TRUE)
  .scales <- list()
  if (.x) {
    .scales <- c(.scales, list(if (.xgxr) xgxr::xgx_scale_x_log10() else ggplot2::scale_x_log10()))
  }
  if (.y) {
    .scales <- c(.scales, list(if (.xgxr) xgxr::xgx_scale_y_log10() else ggplot2::scale_y_log10()))
  }
  list(x = .x, y = .y, scales = .scales)
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
    dobs <- x[x$ind == "Observed", ]
    dpred <- x[x$ind != "Observed", ]
    .lineAes <- NULL
    if (.log$x || .log$y) {
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
      x <- rbind(dpred[, names(dpred) != ".group"], dobs)
      .lineAes <- ggplot2::aes(group = .data$.group)
    }
    .facet <- function(page) {
      ggforce::facet_wrap_paginate(~id, nrow = 4, ncol = 4, page = page)
    }
    .p <-
      ggplot2::ggplot(x, ggplot2::aes(.data$time, .data$values, col = .data$ind)) +
      ggplot2::geom_line(.lineAes, data = dpred, linewidth = 1.2) +
      ggplot2::geom_point(data = dobs) +
      .facet(1L) +
      .log$scales +
      rxode2::rxTheme() +
      ggplot2::ggtitle(label = .augPredEndpoint)
    return(ggtibble::new_gglist(.paginate(.p, .facet)))
  }
}
