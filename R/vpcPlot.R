#' VPC based on ui model
#'
#' @param fit nlmixr2 fit object
#' @param data this is the data to use to augment the VPC fit.  By
#'     default is the fitted data, (can be retrieved by
#'     \code{\link[nlme]{getData}}), but it can be changed by specifying
#'     this argument.
#' @param n Number of VPC simulations.  By default 100
#' @inheritParams vpc::vpc
#' @inheritParams rxode2::rxSolve
#' @param ... Args sent to \code{\link[rxode2]{rxSolve}}
#' @return Simulated dataset (invisibly)
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#' one.cmt <- function() {
#'  ini({
#'    ## You may label each parameter with a comment
#'    tka <- 0.45 # Log Ka
#'    tcl <- log(c(0, 2.7, 100)) # Log Cl
#'    ## This works with interactive models
#'    ## You may also label the preceding line with label("label text")
#'    tv <- 3.45; label("log V")
#'    ## the label("Label name") works with all models
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    linCmt() ~ add(add.sd)
#'  })
#' }
#'
#' fit <- nlmixr(one.cmt, theo_sd, est="focei")
#'
#' vpcPlot(fit)
#'
#' }
#'
#' @export
vpcPlot <- function(fit, data = NULL, n = 300, bins = "jenks",
                    n_bins = "auto", bin_mid = "mean",
                    show = NULL, stratify = NULL, pred_corr = FALSE,
                    pred_corr_lower_bnd = 0, pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                    uloq = NULL, lloq = NULL, log_y = FALSE, log_y_min = 0.001,
                    xlab = NULL, ylab = NULL, title = NULL, smooth = TRUE, vpc_theme = NULL,
                    facet = "wrap", labeller = NULL, vpcdb = FALSE, verbose = FALSE, ...,
                    seed=1009) {
  rxode2::rxReq("vpc")
  .ui <- fit$ui
  .obsLst <- .vpcUiSetupObservationData(fit, data)
  .no <- .obsLst$namesObs
  .nol <- .obsLst$namesObsLower
  .obs <- .obsLst$obs
  .obsCols <- .obsLst$obsCols
  # Setup stratify
  .wo <- which(.nol == "cmt")
  .multi <- length(fit$ui$predDf$cmt) > 1
  .w <- which(tolower(stratify) == "cmt")
  if (length(.w) == 0 && .multi && length(.wo) == 1) {
    stratify <- unique(c(stratify, .no[.wo]))
  } else {
    .wo <- which(.nol == "dvid")
    if (length(.wo) == 1 && .multi) {
      stratify <- unique(c(stratify, .no[.wo]))
    }
  }
  # Simulate with VPC
  .sim <- vpcSim(fit, ..., keep=stratify, n=n, pred=pred_corr, seed=seed)
  .simCols <- list(
    id="id",
    dv="sim",
    idv="time")
  if (pred_corr) {
    .simCols <- c(.simCols, list(pred="pred"))
    .si <- .lastPredSimulationInfo
    .si$keep <- unique(c(stratify, .obsCols$dv))
    .si$addDosing <- FALSE
    .si$subsetNonmem <- TRUE
    .obs1 <- .obs
    .obs <- do.call(rxode2::rxSolve, .si)
    .both <- intersect(names(.obs1), names(.obs))
    for (.n in .both) {
      if (inherits(.obs1[[.n]], "factor") && !inherits(.obs[[.n]], "factor")) {
        .tmp <- as.integer(.obs[[.n]])
        attr(.tmp, "levels") <- attr(.obs1[[.n]], "levels")
        class(.tmp) <- "factor"
        .obs[[.n]] <- .tmp
      }
    }
    .no <- names(.obs)
    .w <- which(.no == "sim")
    names(.obs)[.w] <- "pred"
    .obsCols$pred <- "pred"
    .obsCols$idv <- "time"
    .obsCols$id <- "id"
    if (any(names(.obs) == "dv")) {
      .obsCols$dv <- "dv"
    }
  }
  .both <- intersect(names(.sim), names(.obs))
  for (.n in .both) {
    if (inherits(.obs[[.n]], "factor") && !inherits(.sim[[.n]], "factor")) {
      .tmp <- as.integer(.sim[[.n]])
      attr(.tmp, "levels") <- attr(.obs[[.n]], "levels")
      class(.tmp) <- "factor"
      .sim[[.n]] <- .tmp
    }
  }
  vpc::vpc_vpc(sim=.sim, sim_cols=.simCols,
               obs=.obs, obs_cols=.obsCols,
               bins=bins, n_bins=n_bins, bin_mid=bin_mid,
               show = show, stratify = stratify, pred_corr = pred_corr,
               pred_corr_lower_bnd = pred_corr_lower_bnd, pi = pi, ci = ci,
               uloq = uloq, lloq = lloq, log_y = log_y, log_y_min = log_y_min,
               xlab = xlab, ylab = ylab, title = title, smooth = smooth, vpc_theme = vpc_theme,
               facet = facet, labeller = labeller, vpcdb = vpcdb, verbose = verbose)
}
