#' @export
print.nlmixr2PlotList <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  for (.i in seq_along(.x)) {
    try(print(.x[[.i]]))
  }
}
