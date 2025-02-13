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
      ret[[names(x)[idx]]] <-
        if (inherits(x[[idx]], "gg")) {
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
`+.nlmixr2PlotList` <- function(x, y) {
  if (inherits(x, "nlmixr2PlotList")) {
    if (!inherits(y, "nlmixr2PlotList")) {
      .nx <- names(x)
      .ret <- stats::setNames(lapply(
        seq_along(x),
        function(i) {
          if (inherits(x[[i]], "gg")) {
            ggplot2::`%+%`(x[[i]], y)
          } else if (inherits(x[[i]], "nlmixr2PlotList")) {
            `+.nlmixr2PlotList`(x[[i]], y)
          } else if (is.null(x[[i]])) {
            NULL
          } else {
            stop(sprintf(
              "cannot add class %s to an nlmixr2PlotList", paste(class(x[[i]]), collapse=", ")
            ), call.=FALSE)
          }
        }
      ), .nx)
      class(.ret) <- "nlmixr2PlotList"
      .ret
    } else {
      .x <- x
      .y <- y
      .ret <- c(.x, .y)
      class(.ret) <- "nlmixr2PlotList"
      .ret
    }
  } else {
    stop(sprintf(
      "Cannot add class %s to an nlmixr2PlotList", paste(class(y), collapse=", ")
    ))
  }
}
