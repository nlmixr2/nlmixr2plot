#' VPC based on ui model
#'
#' @param fit nlmixr2 fit object
#' @param data this is the data to use to augment the VPC fit.  By
#'   default is the fitted data, (can be retrieved by
#'   \code{\link[nlme]{getData}}), but it can be changed by specifying
#'   this argument.
#' @param n Number of VPC simulations.  By default 100
#' @param idv Name of independent variable. For `vpcPlot()` and
#'   `vpcCens()` the default is `"time"` for `vpcPlotTad()` and
#'   `vpcCensTad()` this is `"tad"`
#' @param cens is a boolean to show if this is a censoring plot or
#'   not.  When `cens=TRUE` this is actually a censoring vpc plot
#'   (with `vpcCens()` and `vpcCensTad()`).  When `cens=FALSE` this is
#'   traditional VPC plot (`vpcPlot()` and `vpcPlotTad()`).
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
#' fit <- nlmixr2est::nlmixr(one.cmt, nlmixr2data::theo_sd, est="focei")
#'
#' vpcPlot(fit)
#'
#' }
#'
#' @export
#' @importFrom nlmixr2est vpcSim
#' @importFrom vpc vpc_vpc
vpcPlot <- function(fit, data = NULL, n = 300, bins = "jenks",
                    n_bins = "auto", bin_mid = "mean",
                    show = NULL, stratify = NULL, pred_corr = FALSE,
                    pred_corr_lower_bnd = 0, pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                    uloq = fit$dataUloq, lloq = fit$dataLloq, log_y = FALSE, log_y_min = 0.001,
                    xlab = NULL, ylab = NULL, title = NULL, smooth = TRUE, vpc_theme = NULL,
                    facet = "wrap", scales = "fixed", labeller = NULL, vpcdb = FALSE,
                    verbose = FALSE, ..., seed=1009,
                    idv="time", cens=FALSE) {
  force(idv)
  rxode2::rxReq("vpc")
    # Simulate with VPC
  if (inherits(fit, "nlmixr2vpcSim")) {
    .sim <- fit
    .fit <- attr(class(.sim), "fit")
    .cls <- class(.fit)
    .attr <- attr(.cls, ".foceiEnv")
    .cls <- .cls[-1]
    attr(.cls, ".foceiEnv") <- .attr
    class(.fit) <- .cls
    fit <- .fit
  }
  .ui <- rxode2::rxUiDecompress(fit$ui)
  .obsLst <- .vpcUiSetupObservationData(fit, data=data, idv=idv)
  .obs <- .obsLst$obs
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
  if (!inherits(fit, "nlmixr2vpcSim")) {
    .sim <- nlmixr2est::vpcSim(fit, ..., keep=stratify, n=n, pred=pred_corr, seed=seed)
  }
  .sim <- nlmixr2est::vpcSimExpand(fit, .sim, stratify, .obs)
  if (cens) {
    if (is.null(lloq) && is.null(uloq)) {
      stop("this data is not censored")
    }
    .sim$dv <- .sim$sim
    .sim$idv <- .sim[[idv]]
    .obs <- as.data.frame(fit)
    .obs$idv <- .obs[[idv]]
    .w <- which(tolower(names(.obs)) == idv)
    .time <- .obs[, .w]
    .obs <- .obs[, -.w]
    .obs$TIME <- .time
    return(vpc::vpc_cens(sim=.sim,
                         obs=.obs,
                         bins=bins, n_bins=n_bins, bin_mid=bin_mid,
                         show = show, stratify = stratify, ci = ci,
                         uloq = uloq, lloq = lloq,
                         xlab = xlab, ylab = ylab, title = title, smooth = smooth, vpc_theme = vpc_theme,
                         facet = facet, labeller = labeller, vpcdb = vpcdb, verbose = verbose))
  }
  .simCols <- list(
    id="id",
    dv="sim",
    idv=idv)
  if (pred_corr) {
    .simCols <- c(.simCols, list(pred="pred"))
    .si <- nlmixr2est::.nlmixr2estLastPredSimulationInfo()
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
    .obsCols$idv <- idv
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
  .w <- which(tolower(names(.obs)) == "evid")
  if (length(.w) == 1L) {
    .obs <- .obs[.obs[, .w] == 0 | .obs[, .w] == 2, ]
  }
  .w <- which(tolower(names(.obs)) == "dv")
  .obsCols$dv <-"dv"
  names(.obs)[.w] <- "dv"
  .obs <- .obs[!is.na(.obs[[.w]]), ]
  .w <- which(tolower(names(.obs)) == "ipred")
  if (length(.w) > 0) {
    .obs <- .obs[, -.w]
  }
  .w <- which(tolower(names(.obs)) == "pred")
  if (length(.w) > 0) {
    .obs <- .obs[, -.w]
  }
  .obsCols$idv <- idv
  .w <- which(tolower(names(.sim)) == "id")
  names(.sim)[.w] <- "id"
  vpc::vpc_vpc(sim=.sim, sim_cols=.simCols,
               obs=.obs, obs_cols=.obsCols,
               bins=bins, n_bins=n_bins, bin_mid=bin_mid,
               show = show, stratify = stratify, pred_corr = pred_corr,
               pred_corr_lower_bnd = pred_corr_lower_bnd, pi = pi, ci = ci,
               uloq = uloq, lloq = lloq, log_y = log_y, log_y_min = log_y_min,
               xlab = xlab, ylab = ylab, title = title, smooth = smooth, vpc_theme = vpc_theme,
               facet = facet, scales=scales, labeller = labeller, vpcdb = vpcdb, verbose = verbose)
}

#' @rdname vpcPlot
#' @export
vpcPlotTad <- function(..., idv="tad") {
  vpcPlot(..., idv=idv)
}


#' @rdname vpcPlot
#' @export
vpcCensTad <- function(..., cens=TRUE, idv="tad") {
  vpcPlot(..., cens=cens, idv=idv)
}

#' @rdname vpcPlot
#' @export
vpcCens <- function(..., cens=TRUE, idv="time") {
  vpcPlot(..., cens=cens, idv=idv)
}

#' Setup Observation data for VPC
#'
#' @param fit nlmixr2 fit
#' @param data replacement data
#' @return List with `namesObs`, `namesObsLower`, `obs` and `obsCols`
#' @author Matthew L. Fidler
#' @noRd
.vpcUiSetupObservationData <- function(fit, data=NULL, idv="time") {
  if (!is.null(data)) {
    .obs <- data
  } else {
    .obs <- fit$origData
  }
  .obs <- nlmixr2est::vpcNameDataCmts(fit, .obs)
  .no <- names(.obs)
  .nol <- tolower(.no)
  .wo <- which(.nol == "id")
  if (length(.wo) != 1) {
    stop("cannot find 'id' in original dataset",
         call.=FALSE)
  }
  .obsCols <- list(id=.no[.wo])
  .wo <- which(.nol == "dv")
  if (length(.wo) != 1) {
    stop("cannot find 'dv' in original dataset",
         call.=FALSE)
  }
  .obsCols <- c(.obsCols,
                list(dv=.no[.wo]))
  .wo <- which(.nol == idv)
  if (length(.wo) != 1) {
    if (any(names(fit) == idv)) {
      .fit <- as.data.frame(fit)
      .wid <- which(tolower(names(.fit)) == "id")
      names(.fit)[.wid] <- "ID"
      .fit$nlmixrRowNums <-  fit$env$.rownum
      .fit <- .fit[, c("ID", idv, "nlmixrRowNums")]
      .obs$nlmixrRowNums <- seq_along(.obs$ID)
      .obs <- merge(.obs, .fit, by=c("ID", "nlmixrRowNums"), all.x=TRUE)
      .wo <- which(.nol == idv)
    } else {
      stop("cannot find '", idv, "' in original dataset",
           call.=FALSE)
    }
  } else {
    names(.obs)[.wo] <- idv
  }
  .obsCols <- c(.obsCols,
                list(idv=.no[.wo]))
  list(namesObs=.no,
       namesObsLower=.nol,
       obs=.obs,
       obsCols=.obsCols)
}
