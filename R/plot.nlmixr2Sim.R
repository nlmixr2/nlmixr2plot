#' @importFrom dplyr %>% do group_by mutate ungroup
#' @importFrom tidyr spread
#' @export
plot.nlmixr2Sim <- function(x, y, ...) {
  p1 <- eff <- Percentile <- sim.id <- id <- p2 <- p50 <- p05 <- p95 <- . <- NULL
  .args <- list(...)
  save <- getOption("nlmixr2.save", FALSE)
  rxode2::rxReq("dplyr")
  rxode2::rxReq("tidyr")
  if (is.null(.args$p)) {
    .p <- c(0.05, 0.5, 0.95)
  } else {
    .p <- .args$p
  }
  if (save) {
    .digest <- digest::digest(list(
      .args,
      as.character(utils::packageVersion("nlmixr2")),
      as.character(utils::packageVersion("rxode2"))
    ))
    .saveFile <- file.path(
      getOption("nlmixr2.save.dir", getwd()),
      paste0("nlmixr2SimPlot-", .digest, ".rds")
    )
    if (file.exists(.saveFile)) {
      message(sprintf("Loading nlmixr2SimPlot already summarized (%s)", .saveFile))
      .ret <- readRDS(.saveFile)
      return(.ret)
    }
  }
  if (x$env$nStud <= 1) {
    if (x$env$nSub < 2500) {
      warning("In order to put confidence bands around the intervals, you need at least 2500 simulations.")
      message("Summarizing data for plot")
      .ret <- x %>%
        dplyr::group_by(time) %>%
        dplyr::do(data.frame(p1 = .p, eff = quantile(.$sim, probs = .p))) %>%
        dplyr::mutate(Percentile = factor(sprintf("%02d%%", round(p1 * 100))))
      message("done.")
      .ret <- ggplot2::ggplot(.ret, aes(time, eff, col = Percentile, fill = Percentile)) +
        ggplot2::geom_line(size = 1.2)
      return(.ret)
    } else {
      .n <- round(sqrt(x$env$nSub))
    }
  } else {
    .n <- x$env$nStud
  }
  message("Summarizing data for plot")
  .ret <- x %>%
    dplyr::mutate(id = sim.id %% .n) %>%
    dplyr::group_by(id, time) %>%
    dplyr::do(data.frame(p1 = .p, eff = quantile(.$sim, probs = .p))) %>%
    dplyr::group_by(p1, time) %>%
    dplyr::do(data.frame(p2 = .p, eff = quantile(.$eff, probs = .p))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p2 = sprintf("p%02d", (p2 * 100))) %>%
    tidyr::spread(p2, eff) %>%
    dplyr::mutate(Percentile = factor(sprintf("%02d%%", round(p1 * 100))))
  message("done.")
  .ret <- ggplot2::ggplot(.ret, aes(time, p50, col = Percentile, fill = Percentile)) +
    ggplot2::geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.5) +
    ggplot2::geom_line(size = 1.2)
  if (save) {
    saveRDS(.ret, file = .saveFile)
  }
  return(.ret)
}
