#' Create an nlmixr2PlotList object
#'
#' @param x The list to cast as an nlmixr2PlotList
#' @return An nlmixr2PlotList object
#' @noRd
newNlmixr2PlotList <- function(x) {
  stopifnot(is.list(x))
  stopifnot(!is.null(names(x)))
  stopifnot(!any(names(x) %in% ""))
  class(x) <- "nlmixr2PlotList"
  x
}

#' Coerce an object to be an nlmixr2PlotList
#'
#' @param ... Something that can be converted into an nlmixr2PlotList (see details)
#' @return An nlmixr2PlotList object
#' @noRd
asNlmixr2PlotList <- function(...) {
  x <- list(...)
  if (length(x) == 1 & is.list(x[[1]]) & is.null(names(x))) {
    ret <- x[[1]]
  } else {
    stopifnot(!is.null(names(x)))
    stopifnot(!any(names(x) %in% ""))
    ret <- list()
    for (idx in seq_along(x)) {
      .gg <- try(ggplot2::is_ggplot(x[[idx]]), silent=TRUE)
      if (inherits(.gg, "try-error")) .gg <- FALSE
      ret[[names(x)[idx]]] <-
        if (.gg || inherits(x[[idx]], "gg")) {
          x[[idx]]
        } else if (inherits(x[[idx]], "nlmixr2PlotList")) {
          x[[idx]]
        } else if (is.list(x[[idx]])) {
          do.call(what=asNlmixr2PlotList, args=x[[idx]])
        } else {
          stop(sprintf(
            "Cannot add class %s to an nlmixr2PlotList", paste(class(x[[idx]]), collapse=", ")
          ))
        }
    }
  }
  newNlmixr2PlotList(ret)
}

#' @export
Ops.nlmixr2PlotList <- function(e1, e2) {
  if(nargs() == 1L) {
    stop("Unary methods are not supported")
  } else if (!(.Generic %in% "+")) {
    stop(".Generic ", .Generic, " is not supported")
  }
  if (!inherits(e2, "nlmixr2PlotList")) {
    .nmE1 <- names(e1)
    .ret <- stats::setNames(lapply(
      seq_along(e1),
      function(i) {
        .gg <- try(ggplot2::is_ggplot(e1[[i]]), silent=TRUE)
        if (inherits(.gg, "try-error")) .gg <- FALSE
        if (.gg ||
            inherits(e1[[i]], "gg") ||
            inherits(e1[[i]], "nlmixr2PlotList")) {
          e1[[i]] + e2
        } else if (is.null(e1[[i]])) {
          NULL
        } else {
          stop(sprintf(
            "cannot add class %s to an nlmixr2PlotList", paste(class(e1[[i]]), collapse=", ")
          ), call.=FALSE)
        }
      }
    ), .nmE1)
    class(.ret) <- "nlmixr2PlotList"
    .ret
  } else {
    .ret <- c(e1, e2)
    class(.ret) <- "nlmixr2PlotList"
    .ret
  }
}

#' @export
chooseOpsMethod.nlmixr2PlotList <- function(x, y, mx, my, cl, reverse) {
  TRUE
}
