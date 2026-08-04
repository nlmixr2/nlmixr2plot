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
    .facet <- function(page) {
      ggforce::facet_wrap_paginate(~id, nrow = 4, ncol = 4, page = page)
    }
    .p <-
      ggplot2::ggplot(x, ggplot2::aes(.data$time, .data$values, col = .data$ind)) +
      ggplot2::geom_line(data = dpred, linewidth = 1.2) +
      ggplot2::geom_point(data = dobs) +
      .facet(1L) +
      rxode2::rxTheme() +
      ggplot2::ggtitle(label = .augPredEndpoint)
    return(ggtibble::new_gglist(.paginate(.p, .facet)))
  }
}
