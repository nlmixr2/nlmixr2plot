#' VPC based on ui model
#'
#' @param fit nlmixr2 fit object, or a simulation from
#'   \code{\link[nlmixr2est]{vpcSim}()}.  A supplied simulation is used
#'   as-is (`n` is then ignored).  For `pred_corr = TRUE` it must have been
#'   created with `vpcSim(..., pred = TRUE)`, and the observed data are
#'   pred-corrected by re-solving the population predictions of the
#'   simulation's fit with `...`, so pass the same `...` that was given to
#'   `vpcSim()`.
#' @param data this is the data to use to augment the VPC fit.  By
#'   default is the fitted data, (can be retrieved by
#'   \code{\link[nlme]{getData}}), but it can be changed by specifying
#'   this argument.
#' @param n Number of VPC simulations (ignored when `fit` is a
#'   `vpcSim()` simulation)
#' @param idv Name of independent variable. For `vpcPlot()` and
#'   `vpcCens()` the default is `"time"` for `vpcPlotTad()` and
#'   `vpcCensTad()` this is `"tad"`
#' @param cens is a boolean to show if this is a censoring plot or
#'   not.  When `cens=TRUE` this is actually a censoring vpc plot
#'   (with `vpcCens()` and `vpcCensTad()`).  When `cens=FALSE` this is
#'   traditional VPC plot (`vpcPlot()` and `vpcPlotTad()`).
#' @inheritParams vpc::vpc
#' @inheritParams rxode2::rxSolve
#' @param method the method to use for VPC plotting; can be `"vpc"` (uses the
#'   \pkg{vpc} package) or `"tidyvpc"` (uses the \pkg{tidyvpc} package).  By
#'   default, `"vpc"` is used when the \pkg{vpc} package is available, otherwise
#'   `"tidyvpc"` is used.
#' @param ... Args sent to \code{\link[rxode2]{rxSolve}}
#' @return Simulated dataset (invisibly)
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#' one.cmt <- function() {
#'  ini({
#'    tka <- 0.45; label("Ka")
#'    tcl <- log(c(0, 2.7, 100)); label("Cl")
#'    tv <- 3.45; label("V")
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7; label("Additive residual error")
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    linCmt() ~ add(add.sd)
#'  })
#' }
#'
#' fit <-
#'   nlmixr2est::nlmixr(
#'     one.cmt,
#'     data = nlmixr2data::theo_sd,
#'     est = "saem",
#'     control = nlmixr2est::saemControl(print = 0, nBurn = 10, nEm = 20)
#'   )
#'
#' vpcPlot(fit, n = 100)
#' }
#' @export
vpcPlot <- function(fit, data = NULL, n = 300, bins = "jenks",
                    n_bins = "auto", bin_mid = "mean",
                    show = NULL, stratify = NULL, pred_corr = FALSE,
                    pred_corr_lower_bnd = 0, pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                    uloq = fit$dataUloq, lloq = fit$dataLloq, log_y = FALSE, log_y_min = 0.001,
                    xlab = NULL, ylab = NULL, title = NULL, smooth = TRUE, vpc_theme = NULL,
                    facet = "wrap", scales = "fixed", labeller = NULL, vpcdb = FALSE,
                    verbose = FALSE, ..., seed=1009,
                    idv="time", cens=FALSE,
                    method=c("vpc", "tidyvpc")) {
  force(idv)
  if (missing(method)) {
    method <- ifelse(requireNamespace("vpc", quietly = TRUE), "vpc", "tidyvpc")
  } else {
    method <- match.arg(method)
  }
  if (method == "vpc") {
    tidyvpc <- FALSE
  } else {
    tidyvpc <- TRUE
  }
  # Reuse a supplied simulation (#57); `fit` is replaced by the underlying fit
  # below, so remember whether a simulation was given
  .hasSim <- inherits(fit, "nlmixr2vpcSim")
  if (.hasSim) {
    .sim <- fit
    .fit <- attr(class(.sim), "fit")
    .cls <- class(.fit)
    .attr <- attr(.cls, ".foceiEnv")
    .cls <- .cls[-1]
    attr(.cls, ".foceiEnv") <- .attr
    class(.fit) <- .cls
    fit <- .fit
    .simN <- length(unique(.sim$sim.id))
    if (!missing(n) && !identical(as.integer(n), as.integer(.simN))) {
      warning("'n' is ignored when a 'vpcSim()' simulation is supplied; ",
              "using its ", .simN, " simulations", call.=FALSE)
    }
    if (pred_corr && !any(names(.sim) == "pred")) {
      stop("'pred_corr = TRUE' needs a simulation created with ",
           "'vpcSim(..., pred = TRUE)'", call.=FALSE)
    }
  }
  .ui <- rxode2::rxUiDecompress(fit$ui)
  .obsLst <- .vpcUiSetupObservationData(fit, data=data, idv=idv, cens=cens)
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
  if (!.hasSim) {
    .sim <- nlmixr2est::vpcSim(fit, ..., keep=stratify, n=n, pred=pred_corr, seed=seed)
  } else if (pred_corr && (tidyvpc || !cens)) {
    # The observed-data pred-correction below re-solves the setup that
    # vpcSim(pred=TRUE) stores globally, which may belong to a later vpcSim()
    # of another fit.  Refresh it from this simulation's fit with a
    # small simulation (n=1 hits an nlmixr2est vpcSim() bug when the solve has
    # no sim.id); the supplied simulation itself is still what is plotted.
    # vpc's censored VPC does not pred-correct, so it needs no refresh.
    nlmixr2est::vpcSim(fit, ..., n=2, pred=TRUE, seed=seed)
  }
  .sim <- nlmixr2est::vpcSimExpand(fit, .sim, stratify, .obs)
  if (any(names(.sim) == "evid")) {
    .sim <- .sim[.sim$evid == 0,]
  }
  .evid <- which(tolower(names(.obs)) == "evid")
  if (length(.evid) == 1L) {
    .obs <- .obs[.obs[[.evid]] == 0,,drop=FALSE]
  }
  # an EVID=0 record flagged MDV=1 is still not an observation
  .mdv <- which(tolower(names(.obs)) == "mdv")
  if (length(.mdv) == 1L) {
    .obs <- .obs[.obs[[.mdv]] == 0,,drop=FALSE]
  }
  if (cens & !tidyvpc) {
    if (is.null(lloq) && is.null(uloq)) {
      stop("this data is not censored")
    }
    # Use the observed data prepared above (which honours `data`, #55) instead
    # of re-deriving it from the fit.
    .obs <- .vpcCensObs(.obs)
    # Pass the column mappings explicitly (as in the non-censored vpc path)
    # instead of letting vpc_cens guess them.  Guessing maps idv to "TIME"/"time"
    # and, when idv is "tad", left an extra "idv" column that collided with vpc's
    # internal standardized names (nlmixr2#390).
    # Map dv to the simulated "sim" column instead of copying it into a new "dv"
    # column; vpc renames the mapped column to "dv", and a leftover "sim" column
    # makes vpc:::add_sim_index_number() use the simulated values themselves as
    # the replicate index (giving one "replicate" per row).
    .simCens <- list(
      id=.vpcCensCol(.sim, "id", "simulated"),
      dv=.vpcCensCol(.sim, "sim", "simulated"),
      idv=.vpcCensCol(.sim, idv, "simulated"))
    .obsCens <- list(
      id=.vpcCensCol(.obs, "id", "observed"),
      dv=.vpcCensCol(.obs, "dv", "observed"),
      idv=.vpcCensCol(.obs, idv, "observed"))
    .strat <- .vpcMatchStrata(.obs, .sim, stratify)
    .obs <- .strat$obs
    .sim <- .strat$sim
    .sim <- .vpcCensDropStray(.sim, .simCens, stratify)
    .obs <- .vpcCensDropStray(.obs, .obsCens, stratify)
    rxode2::rxReq("vpc")
    return(vpc::vpc_cens(sim=.sim, sim_cols=.simCens,
                         obs=.obs, obs_cols=.obsCens,
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
    .keep <- c(stratify, .obsCols$dv)
    if (cens && tidyvpc) {
      # keep the censoring column through the pred-corrected obs rebuild so the
      # tidyvpc cens=TRUE path can still find it
      .keep <- c(.keep, names(.obs)[tolower(names(.obs)) == "cens"])
    }
    .si$keep <- unique(.keep)
    if (!is.null(data)) {
      # .si carries the fit's dataset; rebuild the observations from the
      # user-supplied data instead (#62)
      .si$events <- data
    }
    .si$addDosing <- FALSE
    .si$subsetNonmem <- TRUE
    .obs1 <- .obs
    .obs <- do.call(rxode2::rxSolve, .si)
    .both <- intersect(names(.obs1), names(.obs))
    for (.n in .both) {
      .obs[[.n]] <- .vpcMatchFactor(.obs[[.n]], .obs1[[.n]])
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
  .strat <- .vpcMatchStrata(.obs, .sim, stratify)
  .obs <- .strat$obs
  .sim <- .strat$sim
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
  if (!tidyvpc && length(.w) > 0) {
    .obs <- .obs[, -.w]
  }
  .obsCols$idv <- idv
  .w <- which(tolower(names(.sim)) == "id")
  names(.sim)[.w] <- "id"
  if (idv == "tad") {
    # drop values before TAD
    .sim <- .sim[!is.na(.sim[[idv]]), ]
  }
  # use tidyvpc
  if (tidyvpc) {
    rxode2::rxReq("tidyvpc")
    # Add arguments as needed
    .tidyObs <- c(".obs", paste0("x=", .obsCols$idv), paste0("y=", .obsCols$dv))
    .tidySim <- c(".tidyObs", ".sim",
                  paste0("x=", .simCols$idv),
                  paste0("y=", .simCols$dv))
    if (pred_corr) {
      .tidyObs <- c(.tidyObs, paste0("pred=", .obsCols$pred))
      .tidySim <- c(.tidySim, paste0("pred=", .simCols$pred))
    }
    if (cens && tidyvpc) {
      .w <- which(tolower(names(.obs)) == "cens")
      if (length(.w) != 1L) {
        stop(
          "For 'cens = TRUE' and method = 'tidyvpc', the observed data must contain exactly one 'cens' column, ",
          "encoded as 0 for non-censored, 1 for blq, and -1 for alq",
          call. = FALSE
        )
      }
      .cens <- names(.obs)[.w]
      .censVals <- unique(.obs[[.w]])
      .censVals[is.na(.censVals)] <- 0
      .obs$blq <- .obs[[.w]] == 1
      .obs$alq <- .obs[[.w]] == -1
      .obs$lloq <- ifelse(.obs[[.w]] == 1, .obs[[.obsCols$dv]], NA_real_)
      .obs$uloq <- ifelse(.obs[[.w]] == -1, .obs[[.obsCols$dv]], NA_real_)
      .obs <- .obs |>
        tidyr::fill(lloq, uloq, .direction="down")
      # if there are only 0 and 1, then the data is blq
      if (length(.censVals) == 2 && all(sort(.censVals) == c(0, 1))) {
        .tidyObs <- c(.tidyObs, "blq=blq", "lloq=lloq")
      } else if (length(.censVals) == 2 && all(sort(.censVals) == c(-1, 0))) {
        .tidyObs <- c(.tidyObs, "alq=alq", "uloq = uloq")
      } else if (length(.censVals) == 3 && all(sort(.censVals) == c(-1, 0, 1))) {
        .tidyObs <- c(.tidyObs, "blq=blq", "alq=alq",
                      "lloq=lloq", "uloq=uloq")
      } else {
        stop("it is unclear the censoring type of the data, please make sure the 'cens' column is coded as 0 for non-censored, 1 for blq, and -1 for alq",
             call.=FALSE)
      }
    }
    .tidyObs <- str2lang(paste0("tidyvpc::observed(",
                                paste(.tidyObs, collapse=", "),
                                ")"))
    .tidySim <- str2lang(paste0("tidyvpc::simulated(",
                                paste(.tidySim, collapse=", "),
                                ")"))

    .tidyObs <- eval(.tidyObs)
    .tidySim <- eval(.tidySim)

    if (!is.null(stratify)) {
      .strat <- str2lang(paste0("tidyvpc::stratify(.tidySim, ~",
                                paste(stratify, collapse="+"),
                                ")"))
      .tidySim <- eval(.strat)
    }
    .tidyBin <- ".tidySim"
    .binless <- FALSE
    if (inherits(bins, "character")) {
      if (is.character(n_bins) && length(n_bins) == 1L &&
            n_bins == "auto") {
        n_bins <- min(max(3, ceiling(nrow(.obs)/40)), 15)
      }
      if (is.numeric(n_bins) && length(n_bins) == 1L) {
        if (n_bins < 1) {
          .binless <- TRUE
        } else {
          .tidyBin <- c(.tidyBin, paste0("bin=", deparse1(bins)))
          .tidyBin <- c(.tidyBin, paste0("nbins=n_bins"))
        }
      }
    } else {
      .tidyBin <- c(.tidyBin, paste0("bin='breaks', breaks=", deparse1(bins)))
    }
    if (!.binless) {
      .tidyBin <- c(.tidyBin, "bin_mid=paste0(\"x\", bin_mid)")
    }
    .tidyBin <- str2lang(paste0(ifelse(.binless, "tidyvpc::binless(",
                                       "tidyvpc::binning("),
                                paste(.tidyBin, collapse=", "),
                                ")"))
    .tidyBin <- eval(.tidyBin)
    if (ci[2] != 1-ci[1]) {
      warning("tidyvpc does not support asymmetric confidence intervals, changing to symmetric",
              immediate.=TRUE, call.=FALSE)
      ci <- c(ci[1], 1-ci[1])
    }
    if (pred_corr) {
      .tidyBin <- eval(str2lang(paste0("tidyvpc::predcorrect(.tidyBin)")))
    }
    .vpcStats <- nlmixr2est::.collectWarn(eval(str2lang(paste0("tidyvpc::vpcstats(.tidyBin, qpred = c(", pi[1],
                                                              ", 0.5, ", pi[2],
                                                              "), conf.level=", ci[2] - ci[1], ")"))),
                                         lst=TRUE)
    .warn <- .vpcStats[[2]]
    .warn <- .warn[.warn != "", drop=FALSE]
    if (length(.warn) > 0L) {
      lapply(.warn, function(w) {
        warning(sub("\n+$", "", w), call.=FALSE)
      })
    }
    .vpcGg <- plot(.vpcStats[[1]])
    if (!is.null(xlab)) {
      .vpcGg <- .vpcGg + ggplot2::xlab(xlab)
    }
    if (!is.null(ylab)) {
      .vpcGg <- .vpcGg + ggplot2::ylab(ylab)
    }
    if (!is.null(title)) {
      .vpcGg <- .vpcGg + ggplot2::ggtitle(title)
    }
    if (!missing(show)) {
      warning("tidyvpc does not support showing specific percentiles, showing all", immediate.=TRUE,
              call.=FALSE)
    }
    if (log_y) {
      .vpcGg <- .vpcGg + xgxr::xgx_scale_y_log10()
    }
    .vpcGg
  } else {
    # use vpc
    rxode2::rxReq("vpc")
    .lloq <- lloq
    .uloq <- uloq
    if (pred_corr && (!is.null(lloq) || !is.null(uloq))) {
      # vpc's pred-correction NAs out censored values (obs and sim) but then
      # computes simulated quantiles without na.rm, so any censored point makes
      # quantile() error.  vpc only shows non-censored data for a pred-corrected
      # censored VPC, so drop the censored rows here and disable vpc's loq
      # handling to avoid the NA-driven crash.  Censored records are encoded at
      # the censoring limit (DV == lloq/uloq), so use strict comparisons to drop
      # those boundary rows as well.
      if (!is.null(lloq)) {
        .obs <- .obs[!is.na(.obs[[.obsCols$dv]]) & .obs[[.obsCols$dv]] > lloq, , drop=FALSE]
        .sim <- .sim[!is.na(.sim[[.simCols$dv]]) & .sim[[.simCols$dv]] > lloq, , drop=FALSE]
      }
      if (!is.null(uloq)) {
        .obs <- .obs[!is.na(.obs[[.obsCols$dv]]) & .obs[[.obsCols$dv]] < uloq, , drop=FALSE]
        .sim <- .sim[!is.na(.sim[[.simCols$dv]]) & .sim[[.simCols$dv]] < uloq, , drop=FALSE]
      }
      .lloq <- NULL
      .uloq <- NULL
    }
    vpc::vpc_vpc(sim=.sim, sim_cols=.simCols,
                 obs=.obs, obs_cols=.obsCols,
                 bins=bins, n_bins=n_bins, bin_mid=bin_mid,
                 show = show, stratify = stratify, pred_corr = pred_corr,
                 pred_corr_lower_bnd = pred_corr_lower_bnd, pi = pi, ci = ci,
                 uloq = .uloq, lloq = .lloq, log_y = log_y, log_y_min = log_y_min,
                 xlab = xlab, ylab = ylab, title = title, smooth = smooth, vpc_theme = vpc_theme,
                 facet = facet, scales=scales, labeller = labeller, vpcdb = vpcdb, verbose = verbose)
  }
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

#' Find the column `col` maps to for a censored VPC
#'
#' Prefers an exact name match and falls back to a case-insensitive one, so an
#' expanded simulation carrying both "TIME" and "time" resolves to the exact
#' match instead of being rejected as ambiguous.  Errors on a missing or
#' ambiguous match (as `.vpcUiSetupObservationData()` does) so `character(0)` or
#' several matches are never handed to `vpc`.
#'
#' @param data data frame to look in
#' @param col column name to find
#' @param what "simulated" or "observed", used in the error message
#' @return the matching name in `data`
#' @noRd
.vpcCensCol <- function(data, col, what) {
  .wo <- which(names(data) == col)
  if (length(.wo) != 1) {
    .wo <- which(tolower(names(data)) == tolower(col))
  }
  if (length(.wo) != 1) {
    stop("cannot find a unique '", col, "' column in the ", what,
         " data for the censored VPC",
         if (length(.wo) == 0) "" else
           paste0(" (matched: ", paste(names(data)[.wo], collapse=", "), ")"),
         call.=FALSE)
  }
  names(data)[.wo]
}

#' Drop columns that collide with vpc's standardized names
#'
#' `vpc` renames the mapped columns to "id"/"dv"/"idv"; an unrelated column
#' already using one of those names sends `vpc::standardize_column()` down the
#' rename branch that is broken in vpc 1.2.4 ("object of type 'closure' is not
#' subsettable").  Drop those strays, keeping the mapped and stratify columns.
#'
#' @param data data frame to clean
#' @param cols list of `id`/`dv`/`idv` mappings for `data`
#' @param stratify stratification columns to keep (may be `NULL`)
#' @return `data` without the colliding columns
#' @noRd
.vpcCensDropStray <- function(data, cols, stratify=NULL) {
  .stray <- setdiff(intersect(names(cols), names(data)),
                    c(unlist(cols), stratify))
  if (length(.stray) == 0L) return(data)
  data[, setdiff(names(data), .stray), drop=FALSE]
}

#' Mark censored observations for a censored VPC
#'
#' nlmixr2 data encodes a censored record at its censoring limit (`DV` equal to
#' the limit) with a non-zero `CENS` column.  `vpc` decides whether an observation
#' is censored by comparing `dv` strictly against the limit (`dv < lloq`,
#' `dv > uloq`), so a record sitting exactly at the limit would be counted as
#' uncensored.  Move the flagged records past their limit instead: `-Inf` for
#' below the limit (`CENS == 1`) and `Inf` for above it (`CENS == -1`), so each
#' is counted only on its own side of the censoring.  Without a `CENS` column
#' `vpc` relies on the `dv`/limit comparison alone, so warn: records encoded at
#' the limit are then counted as uncensored.
#'
#' `vpc` also counts a missing `dv` as censored, so records without an
#' observation (e.g. a missed sample) are dropped first, as the fitted data does.
#'
#' @param obs observed data with a `dv` column (any case)
#' @return `obs` without missing observations, with `dv` moved past the limit
#'   for censored records
#' @noRd
.vpcCensObs <- function(obs) {
  .wd <- .vpcCensCol(obs, "dv", "observed")
  obs <- obs[!is.na(obs[[.wd]]), , drop=FALSE]
  if (!any(tolower(names(obs)) == "cens")) {
    warning("the observed data has no 'cens' column; censoring for the VPC is ",
            "judged by comparing 'dv' to the limit, so records at the limit ",
            "count as uncensored", call.=FALSE)
    return(obs)
  }
  .cens <- obs[[.vpcCensCol(obs, "cens", "observed")]]
  if (is.factor(.cens)) .cens <- as.numeric(as.character(.cens))
  .dv <- obs[[.wd]]
  .dv[!is.na(.cens) & .cens == 1] <- -Inf
  .dv[!is.na(.cens) & .cens == -1] <- Inf
  obs[[.wd]] <- .dv
  obs
}

#' Match simulated stratification columns to the observed ones
#'
#' Recode each column shared by `sim` and `obs` to the observed factor levels
#' (see `.vpcMatchFactor()`), then keep only the levels that are actually
#' observed in the stratification columns, so unobserved compartments (like
#' `depot`) do not become endpoints (#44).
#'
#' @param obs observed data
#' @param sim simulated data
#' @param stratify stratification columns
#' @return list with `obs` and `sim`
#' @noRd
.vpcMatchStrata <- function(obs, sim, stratify) {
  .both <- intersect(names(sim), names(obs))
  for (.n in .both) {
    sim[[.n]] <- .vpcMatchFactor(sim[[.n]], obs[[.n]])
  }
  for (.n in intersect(stratify, .both)) {
    if (inherits(obs[[.n]], "factor")) {
      .lvl <- levels(droplevels(obs[[.n]]))
      obs[[.n]] <- factor(as.character(obs[[.n]]), levels=.lvl)
      sim[[.n]] <- factor(as.character(sim[[.n]]), levels=.lvl)
    }
  }
  list(obs=obs, sim=sim)
}

#' Recode a column to match a reference factor
#'
#' Simulated and pred-corrected data can return a stratification column (like
#' `cmt`) as an integer code or as character labels, while the observed data
#' has it as a factor.  Integer codes are taken as level indices and character
#' values are matched by label (#44).
#'
#' @param x column to recode
#' @param ref reference column
#' @return `x` as a factor with the levels of `ref`, or `x` unchanged when
#'   `ref` is not a factor or `x` already is one
#' @noRd
.vpcMatchFactor <- function(x, ref) {
  if (!inherits(ref, "factor") || inherits(x, "factor")) return(x)
  .lvl <- levels(ref)
  if (is.character(x)) return(factor(x, levels=.lvl))
  .tmp <- as.integer(x)
  attr(.tmp, "levels") <- .lvl
  class(.tmp) <- "factor"
  .tmp
}

#' Setup Observation data for VPC
#'
#' @param fit nlmixr2 fit
#' @param data replacement data
#' @return List with `namesObs`, `namesObsLower`, `obs` and `obsCols`
#' @author Matthew L. Fidler
#' @noRd
.vpcUiSetupObservationData <- function(fit, data=NULL, idv="time", cens=FALSE) {
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
  if (!cens) {
    .no <- .no[which(tolower(.no) != "cens")]
    .nol <- .no[which(tolower(.no) != "cens")]
    .obs <- .obs[which(tolower(names(.obs)) != "cens")]
  }
  list(namesObs=.no,
       namesObsLower=tolower(.nol),
              obs=.obs,
              obsCols=.obsCols)
}
