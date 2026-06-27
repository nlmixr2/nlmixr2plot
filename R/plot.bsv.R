#' Does a fit have estimated between-subject variability (BSV)?
#'
#' @param x A nlmixr2 fit object
#' @return `TRUE` when the model has a non-empty omega (BSV) matrix
#' @noRd
.bsvHasBsv <- function(x) {
  .omega <- x$omega
  !is.null(.omega) && nrow(.omega) > 0L
}

#' Names of the BSV eta columns for a fit
#'
#' `ranef()` returns a data frame of `ID` plus one column per between-subject
#' eta (inter-occasion etas are already dropped upstream by `nlmixr2est`).
#'
#' @param x A nlmixr2 fit object
#' @return Character vector of eta column names (excluding the ID column)
#' @noRd
.bsvEtas <- function(x) {
  setdiff(colnames(nlmixr2est::ranef(x)), c("ID", "id"))
}

#' Generalized smoothing layer used throughout the BSV plots
#'
#' Adds the consistent point + reference-line + linear-smoother layers used by
#' both the BSV-BSV correlation plots and the continuous BSV-covariate plots.
#' The caller owns the data, `aes()`, axis labels, title and theme; this helper
#' only supplies the shared geom styling so it stays consistent across the
#' package.
#'
#' @param p A `ggplot2` object whose mapping already supplies `x` and `y`
#' @return `p` with the point, `y = 0` reference line and `lm` smoother added
#' @noRd
.bsvSmooth <- function(p) {
  p +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 0, intercept = 0, col = "red") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE)
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
    .lst[[.nm]] <-
      ggplot2::ggplot(.re, ggplot2::aes(sample = .data[[.eta]])) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::xlab("Theoretical quantiles") +
      ggplot2::ylab(.eta) +
      ggplot2::ggtitle("Between-subject variability", .nm) +
      rxode2::rxTheme()
  }
  ggtibble::new_gglist(.lst)
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
    .lst[[.nm]] <-
      .bsvSmooth(
        ggplot2::ggplot(.re, ggplot2::aes(x = .data[[.a]], y = .data[[.b]]))
      ) +
      ggplot2::xlab(.a) +
      ggplot2::ylab(.b) +
      ggplot2::ggtitle("Between-subject variability", .nm) +
      rxode2::rxTheme()
  }
  ggtibble::new_gglist(.lst)
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
  .present <- intersect(covariate, colnames(.gd))
  .missing <- setdiff(covariate, colnames(.gd))
  if (length(.missing) > 0L) {
    warning("between-subject variability covariate(s) not found in the data and skipped: ",
            paste(.missing, collapse = ", "), call. = FALSE)
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
  .m <- .bsvCovData(x, covariate)
  if (is.null(.m)) {
    return(NULL)
  }
  .covs <- attr(.m, "covariates")
  .etas <- .bsvEtas(x)
  .lst <- list()
  for (.eta in .etas) {
    for (.cov in .covs) {
      .col <- .m[[.cov]]
      if (all(is.na(.col))) {
        next
      }
      .u <- unique(.col[!is.na(.col)])
      .isCategorical <-
        is.factor(.col) || is.character(.col) || is.logical(.col) ||
        length(.u) <= 5L
      .nm <- paste(.eta, "vs", .cov)
      if (.isCategorical) {
        .lst[[.nm]] <-
          ggplot2::ggplot(.m, ggplot2::aes(x = factor(.data[[.cov]]), y = .data[[.eta]])) +
          ggplot2::geom_boxplot() +
          ggplot2::geom_abline(slope = 0, intercept = 0, col = "red") +
          ggplot2::xlab(.cov) +
          ggplot2::ylab(.eta) +
          ggplot2::ggtitle("Between-subject variability", .nm) +
          rxode2::rxTheme()
      } else {
        .lst[[.nm]] <-
          .bsvSmooth(
            ggplot2::ggplot(.m, ggplot2::aes(x = .data[[.cov]], y = .data[[.eta]]))
          ) +
          ggplot2::xlab(.cov) +
          ggplot2::ylab(.eta) +
          ggplot2::ggtitle("Between-subject variability", .nm) +
          rxode2::rxTheme()
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
