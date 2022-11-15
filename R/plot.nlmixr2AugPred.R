.augPredEndpoint <- NULL
#' Plot a nlmixr2 augPred object
#'
#' @param x augPred object
#'
#' @param y ignored, used to mach plot generic
#'
#' @param ... Other arguments (ignored)
#'
#' @return Nothing called for side effects
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
    .ret <- NULL
    for (.tmp in levels(x$Endpoint)) {
      utils::assignInMyNamespace(".augPredEndpoint", .tmp)
      .x <- x[x$Endpoint == .tmp, names(x) != "Endpoint"]
      .r <- plot.nlmixr2AugPred(.x)
      class(.r) <- NULL
      .ret <- c(.ret, .r)
    }
    class(.ret) <- "nlmixr2PlotList"
    return(.ret)
  } else {
    ids <- unique(x$id)
    .ret <- lapply(seq(1, length(ids), by = 16), function(i) {
      tmp <- ids[seq(i, i + 15)]
      tmp <- tmp[!is.na(tmp)]
      d1 <- x[x$id %in% tmp, ]
      dobs <- d1[d1$ind == "Observed", ]
      dpred <- d1[d1$ind != "Observed", ]
      p3 <-
        ggplot2::ggplot(d1, ggplot2::aes(.data$time, .data$values, col = .data$ind)) +
        ggplot2::geom_line(data = dpred, linewidth = 1.2) +
        ggplot2::geom_point(data = dobs) +
        ggplot2::facet_wrap(~id) +
        rxode2::rxTheme() +
        ggplot2::ggtitle(label=.augPredEndpoint)
      p3
    })
    class(.ret) <- "nlmixr2PlotList"
    return(.ret)
  }
}
