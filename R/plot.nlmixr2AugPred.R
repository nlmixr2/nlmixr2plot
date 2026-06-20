.augPredEndpoint <- NULL
#' Plot a nlmixr2 augPred object
#'
#' @param x augPred object
#'
#' @param y ignored, used to mach plot generic
#'
#' @param ... Other arguments (ignored)
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
#' fit <- nlmixr2est::nlmixr2(one.compartment, theo_sd,  est="saem", saemControl(print=0))
#'
#' # augPred shows more points for the fit:
#'
#' a <- nlmixr2est::augPred(fit)
#'
#' # you can plot it with plot(augPred object)
#' plot(a)
#'
#' }
#' @export
#' @importFrom ggplot2 .data
plot.nlmixr2AugPred <- function(x, y, ...) {
  if (any(names(x) == "Endpoint")) {
    .ret <- list()
    for (.tmp in levels(x$Endpoint)) {
      utils::assignInMyNamespace(".augPredEndpoint", .tmp)
      .x <- x[x$Endpoint == .tmp, names(x) != "Endpoint"]
      .r <- plot.nlmixr2AugPred(.x)
      for (.k in seq_along(.r)) {
        .ret[[length(.ret) + 1L]] <- .r[[.k]]
      }
    }
    return(ggtibble::new_gglist(.ret))
  } else {
    dobs <- x[x$ind == "Observed", ]
    dpred <- x[x$ind != "Observed", ]
    .p <-
      ggplot2::ggplot(x, ggplot2::aes(.data$time, .data$values, col = .data$ind)) +
      ggplot2::geom_line(data = dpred, linewidth = 1.2) +
      ggplot2::geom_point(data = dobs) +
      ggforce::facet_wrap_paginate(~id, nrow = 4, ncol = 4, page = 1) +
      rxode2::rxTheme() +
      ggplot2::ggtitle(label = .augPredEndpoint)
    return(ggtibble::as_gglist(.p))
  }
}
