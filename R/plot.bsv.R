#' Does a fit have estimated between-subject variability (BSV)?
#'
#' @param x A nlmixr2 fit object
#' @return `TRUE` when the fit reports at least one between-subject eta
#' @noRd
.bsvHasBsv <- function(x) {
  # Testing the shape of `omega` (as `nrow(omega) > 0` once did) fails on a
  # model with several variance levels, where `omega` is a list; these plots
  # draw the etas `.bsvEtas()` reports, so ask that question instead.
  length(.bsvEtas(x)) > 0L
}

#' Subject-level omega matrix of a fit
#'
#' `omega` is a matrix for a single-level model, but a list holding one matrix
#' per level once the model has between-occasion variability (named for the
#' levels, e.g. `id` and `OCC`); the subject level is then `omega$id`, as
#' `nlmixr2est`'s own print method reads it.
#'
#' @param x A nlmixr2 fit object
#' @return The subject-level omega matrix, or `NULL` when there is none
#' @noRd
.bsvOmega <- function(x) {
  .omega <- x$omega
  if (is.list(.omega)) {
    # `[[` rather than `$`, so a plain list never partially matches another
    # level whose name merely starts with `id`
    .omega <- .omega[["id"]]
  }
  if (!is.matrix(.omega)) {
    return(NULL)
  }
  .omega
}

#' Names of the BSV eta columns for a fit
#'
#' The subject-level etas with a non-zero variance in `omega` that `ranef()`
#' reports a column for.  Taking the names from `omega` rather than from
#' whatever else `ranef()` holds keeps out a mixture fit's `mixnum` label,
#' occasion etas, and a `fix(0)` eta kept alongside estimated ones.  The
#' subject-level omega is checked first so `ranef()` is not asked for a fit
#' with no subject etas, where it may not be a data frame at all.
#'
#' @param x A nlmixr2 fit object
#' @return Character vector of eta column names (excluding the ID column)
#' @noRd
.bsvEtas <- function(x) {
  .omega <- .bsvOmega(x)
  if (is.null(.omega)) {
    return(character(0))
  }
  # `which()` also drops an NA variance, which is not an estimated eta either
  .etas <- rownames(.omega)[which(diag(.omega) != 0)]
  if (length(.etas) == 0L) {
    return(character(0))
  }
  intersect(.etas, colnames(nlmixr2est::ranef(x)))
}

#' Generalized smoothing layer used throughout the BSV plots
#'
#' The consistent point + reference-line + linear-smoother layers used by
#' both the BSV-BSV correlation plots and the continuous BSV-covariate plots.
#' The caller owns the data, `aes()`, axis labels, title and theme; this helper
#' only supplies the shared geom styling so it stays consistent across the
#' package.  It takes no plot, so the smoother's `y ~ x` formula (and the
#' layers) keep only this helper's empty frame.
#'
#' @return list of the point, `y = 0` reference line and `lm` smoother layers,
#'   to add to a `ggplot2` object whose mapping supplies `x` and `y`
#' @noRd
.bsvSmooth <- function() {
  list(
    ggplot2::geom_point(alpha = 0.5),
    ggplot2::geom_abline(slope = 0, intercept = 0, col = "red"),
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE)
  )
}

#' QQ plots of each BSV parameter
#'
#' @param x A nlmixr2 fit object
#' @return A `ggtibble::gglist` of QQ plots (one per eta) or `NULL`
#' @noRd
.bsvQq <- function(x) {
  .etas <- .bsvEtas(x)
  if (length(.etas) == 0L) {
    return(NULL)
  }
  .re <- nlmixr2est::ranef(x)
  .lst <- list()
  for (.eta in .etas) {
    .nm <- paste("QQ plot for", .eta)
    .lst[[.nm]] <- .plotData(.bsvQqFigure(.eta, .nm), .re)
  }
  ggtibble::new_gglist(.lst)
}

#' QQ plot figure for one BSV parameter, without its data
#'
#' @param eta BSV parameter (column) name
#' @param title plot subtitle
#' @return ggplot without data; see `.plotData()`
#' @noRd
.bsvQqFigure <- function(eta, title) {
  ggplot2::ggplot(mapping = ggplot2::aes(sample = .data[[eta]])) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::xlab("Theoretical quantiles") +
    ggplot2::ylab(eta) +
    ggplot2::ggtitle("Between-subject variability", title) +
    rxode2::rxTheme()
}

#' BSV-BSV correlation plots
#'
#' One scatter plot (with smoother) per pair of BSV parameters.  Only produced
#' when the model has more than one BSV parameter.
#'
#' @param x A nlmixr2 fit object
#' @return A `ggtibble::gglist` of correlation plots or `NULL`
#' @noRd
.bsvCor <- function(x) {
  .etas <- .bsvEtas(x)
  if (length(.etas) <= 1L) {
    return(NULL)
  }
  .re <- nlmixr2est::ranef(x)
  .pairs <- utils::combn(.etas, 2L)
  .lst <- list()
  for (.j in seq_len(ncol(.pairs))) {
    .a <- .pairs[1L, .j]
    .b <- .pairs[2L, .j]
    .nm <- paste(.a, "vs", .b)
    .lst[[.nm]] <- .plotData(.bsvScatterFigure(.a, .b, .nm), .re)
  }
  ggtibble::new_gglist(.lst)
}

#' Scatter (with smoother) figure of two BSV-plot columns, without its data
#'
#' Used for BSV-BSV correlation and continuous BSV-covariate plots.
#'
#' @param xCol,yCol x and y column names (also the axis labels)
#' @param title plot subtitle
#' @return ggplot without data; see `.plotData()`
#' @noRd
.bsvScatterFigure <- function(xCol, yCol, title) {
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[xCol]], y = .data[[yCol]])) +
    .bsvSmooth() +
    ggplot2::xlab(xCol) +
    ggplot2::ylab(yCol) +
    ggplot2::ggtitle("Between-subject variability", title) +
    rxode2::rxTheme()
}

#' Box-and-whisker figure of a BSV parameter by a categorical covariate,
#' without its data
#'
#' @param covCol covariate column name (x axis)
#' @param eta BSV parameter column name (y axis)
#' @param title plot subtitle
#' @return ggplot without data; see `.plotData()`
#' @noRd
.bsvBoxFigure <- function(covCol, eta, title) {
  ggplot2::ggplot(mapping = ggplot2::aes(x = factor(.data[[covCol]]), y = .data[[eta]])) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_abline(slope = 0, intercept = 0, col = "red") +
    ggplot2::xlab(covCol) +
    ggplot2::ylab(eta) +
    ggplot2::ggtitle("Between-subject variability", title) +
    rxode2::rxTheme()
}

#' Find the (case-insensitive) ID column name in a data frame
#'
#' @param data A data frame
#' @return The name of the ID column or `NA_character_` if not present
#' @noRd
.bsvIdCol <- function(data) {
  .w <- which(tolower(colnames(data)) == "id")
  if (length(.w) == 0L) {
    return(NA_character_)
  }
  colnames(data)[.w[1L]]
}

#' Per-subject data frame of etas merged with baseline covariates
#'
#' Covariates are taken from the first row per individual in the original
#' (input) data, so covariate plotting assumes time-invariant covariates (a
#' time-varying covariate is represented by its baseline value).  Covariate
#' names not found in the data are dropped with a warning.
#'
#' @param x A nlmixr2 fit object
#' @param covariate Character vector of covariate column names
#' @return A data frame of etas + covariates (one row per subject) with the
#'   resolved covariate names stored in the `"covariates"` attribute, or `NULL`
#' @noRd
.bsvCovData <- function(x, covariate) {
  covariate <- covariate[!is.na(covariate)]
  if (length(covariate) == 0L) {
    return(NULL)
  }
  .re <- nlmixr2est::ranef(x)
  .gd <- as.data.frame(nlmixr2est::getData(x))
  .reId <- .bsvIdCol(.re)
  .gdId <- .bsvIdCol(.gd)
  if (is.na(.reId) || is.na(.gdId)) {
    return(NULL)
  }
  # Keep only the ID and the BSV etas, so a non-eta `ranef()` column (such as a
  # mixture fit's `mixnum`) cannot collide with a covariate of the same name in
  # the merge below and leave that covariate as `<name>.y`
  .re <- .re[, c(.reId, .bsvEtas(x)), drop = FALSE]
  .present <- intersect(covariate, colnames(.gd))
  .missing <- setdiff(covariate, colnames(.gd))
  if (length(.missing) > 0L) {
    warning(
      "between-subject variability covariate(s) not found in the data and skipped: ",
      paste(.missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(.present) == 0L) {
    return(NULL)
  }
  .base <- .gd[!duplicated(.gd[[.gdId]]), c(.gdId, .present), drop = FALSE]
  .base[[.gdId]] <- as.character(.base[[.gdId]])
  .re[[.reId]] <- as.character(.re[[.reId]])
  names(.base)[names(.base) == .gdId] <- ".bsvID"
  names(.re)[names(.re) == .reId] <- ".bsvID"
  .m <- merge(.re, .base, by = ".bsvID", sort = FALSE)
  attr(.m, "covariates") <- .present
  .m
}

#' BSV-covariate correlation plots
#'
#' One plot per BSV parameter x covariate, sorted by BSV parameter then
#' covariate.  Categorical (factor/character/logical) or low-cardinality
#' (<= 5 unique values) covariates use box-and-whisker plots; continuous
#' covariates use a scatter plot with a smoothing line.
#'
#' @param x A nlmixr2 fit object
#' @param covariate Character vector of covariate column names
#' @return A `ggtibble::gglist` of covariate plots or `NULL`
#' @noRd
.bsvCov <- function(x, covariate) {
  if (is.null(covariate)) {
    return(NULL)
  }
  .etas <- .bsvEtas(x)
  if (length(.etas) == 0L) {
    return(NULL)
  }
  .m <- .bsvCovData(x, covariate)
  if (is.null(.m)) {
    return(NULL)
  }
  .covs <- attr(.m, "covariates")
  .lst <- list()
  for (.eta in .etas) {
    for (.cov in .covs) {
      .col <- .m[[.cov]]
      if (all(is.na(.col))) {
        next
      }
      .u <- unique(.col[!is.na(.col)])
      .isCategorical <-
        is.factor(.col) || is.character(.col) || is.logical(.col) || length(.u) <= 5L
      .nm <- paste(.eta, "vs", .cov)
      if (.isCategorical) {
        .lst[[.nm]] <- .plotData(.bsvBoxFigure(.cov, .eta, .nm), .m)
      } else {
        .lst[[.nm]] <- .plotData(.bsvScatterFigure(.cov, .eta, .nm), .m)
      }
    }
  }
  if (length(.lst) == 0L) {
    return(NULL)
  }
  ggtibble::new_gglist(.lst)
}

#' Build the nested between-subject variability (BSV) plot collection
#'
#' @param x A nlmixr2 fit object
#' @param covariate Optional character vector of covariate column names to plot
#'   against each BSV parameter
#' @return A named, nested `ggtibble::gglist` (`QQ plots`, `BSV correlation`,
#'   `BSV covariate correlation`) or `NULL` when the model has no BSV
#' @noRd
.bsvPlots <- function(x, covariate = NULL) {
  if (!.bsvHasBsv(x)) {
    return(NULL)
  }
  .lst <- list()
  .lst[["QQ plots"]] <- .bsvQq(x)
  .lst[["BSV correlation"]] <- .bsvCor(x)
  .lst[["BSV covariate correlation"]] <- .bsvCov(x, covariate)
  if (length(.lst) == 0L) {
    return(NULL)
  }
  ggtibble::new_gglist(.lst)
}
