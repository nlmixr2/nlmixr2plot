.augPredEndpoint <- NULL

#' @export
#' @importFrom utils assignInMyNamespace
plot.nlmixr2AugPred <- function(x, y, ...) {
  if (any(names(x) == "Endpoint")) {
    for (.tmp in levels(x$Endpoint)) {
      utils::assignInMyNamespace(".augPredEndpoint", .tmp)
      .x <- x[x$Endpoint == .tmp, names(x) != "Endpoint"]
      plot.nlmixr2AugPred(.x)
    }
  } else {
    ids <- unique(x$id)
    time <- values <- ind <- id <- NULL # Rcheck fix
    for (i in seq(1, length(ids), by = 16)) {
      tmp <- ids[seq(i, i + 15)]
      tmp <- tmp[!is.na(tmp)]
      d1 <- x[x$id %in% tmp, ]
      dobs <- d1[d1$ind == "Observed", ]
      dpred <- d1[d1$ind != "Observed", ]
      p3 <-
        ggplot2::ggplot(d1, ggplot2::aes(time, values, col = ind)) +
        ggplot2::geom_line(data = dpred, size = 1.2) +
        ggplot2::geom_point(data = dobs) +
        ggplot2::facet_wrap(~id) +
        rxode2::rxTheme() +
        ggplot2::ggtitle(label=.augPredEndpoint)
      print(p3)
    }
  }
}
