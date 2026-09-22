test_that("plot(augPred) supports log axes (#32)", {
  d <- data.frame(
    id = factor(rep(1:2, each = 6)),
    time = rep(c(0, 1, 2), 4),
    values = c(0, 1, 2, 0.5, 1.5, 2.5, 0, 2, 3, 1, 2, 3),
    ind = factor(rep(rep(c("Pred", "Observed"), each = 3), 2), c("Pred", "Observed"))
  )
  class(d) <- c("nlmixr2AugPred", "data.frame")

  .isLog <- function(scale) {
    .t <- if (is.function(scale$get_transformation)) {
      scale$get_transformation()
    } else {
      scale$trans
    }
    identical(.t$name, "log-10")
  }

  for (.xgxr in c(TRUE, FALSE)) {
    withr::local_options(list(rxode2.xgxr = .xgxr))

    p <- plot(d)
    b <- ggplot2::ggplot_build(p[[1]])
    expect_false(.isLog(b$layout$panel_scales_x[[1]]))
    expect_false(.isLog(b$layout$panel_scales_y[[1]]))

    p <- plot(d, log = "y")
    expect_s3_class(p, "gglist")
    b <- ggplot2::ggplot_build(p[[1]])
    expect_false(.isLog(b$layout$panel_scales_x[[1]]))
    expect_true(.isLog(b$layout$panel_scales_y[[1]]))
    # non-positive values are dropped rather than producing -Inf
    expect_true(all(is.finite(b$data[[1]]$y)))
    expect_true(all(is.finite(b$data[[2]]$y)))

    p <- plot(d, log = "x")
    b <- ggplot2::ggplot_build(p[[1]])
    expect_true(.isLog(b$layout$panel_scales_x[[1]]))
    expect_false(.isLog(b$layout$panel_scales_y[[1]]))
    expect_true(all(is.finite(b$data[[1]]$x)))
    expect_true(all(is.finite(b$data[[2]]$x)))

    for (.l in c("xy", "yx")) {
      p <- plot(d, log = .l)
      b <- ggplot2::ggplot_build(p[[1]])
      expect_true(.isLog(b$layout$panel_scales_x[[1]]))
      expect_true(.isLog(b$layout$panel_scales_y[[1]]))
    }
  }

  # a prediction dipping to a non-positive value breaks the line rather
  # than bridging the gap
  d3 <- data.frame(
    id = factor(rep(1L, 6)),
    time = c(1, 2, 3, 4, 1, 3),
    values = c(1, 2, -1, 1, 1.5, 0.5),
    ind = factor(c(rep("Pred", 4), rep("Observed", 2)), c("Pred", "Observed"))
  )
  class(d3) <- class(d)
  withr::with_options(list(rxode2.xgxr = FALSE), {
    expect_no_warning(b <- ggplot2::ggplot_build(plot(d3, log = "y")[[1]]))
  })
  expect_equal(nrow(b$data[[1]]), 3L)
  expect_equal(length(unique(b$data[[1]]$group)), 2L)
  expect_equal(nrow(b$data[[2]]), 2L)

  # unsorted rows and several subjects: breaks are found in time order and
  # per subject
  d4 <- data.frame(
    id = factor(c(2, 1, 2, 1, 2, 1, 2, 1)),
    time = c(3, 3, 1, 1, 2, 2, 4, 4),
    values = c(1, 1, 2, 2, -1, 3, 1, 4),
    ind = factor(rep("Pred", 8), c("Pred", "Observed"))
  )
  class(d4) <- class(d)
  withr::with_options(list(rxode2.xgxr = FALSE), {
    b <- ggplot2::ggplot_build(plot(d4, log = "y")[[1]])
  })
  .l <- b$data[[1]]
  .ng <- tapply(.l$group, .l$PANEL, function(g) length(unique(g)))
  # panel 1 (id 1) is one unbroken line; panel 2 (id 2) breaks at time 2
  expect_equal(as.vector(.ng), c(1L, 2L))
  expect_equal(.l$x[.l$PANEL == 1], c(1, 2, 3, 4))

  # missing times are dropped and break the line
  d5 <- d3
  d5$time[2] <- NA
  withr::with_options(list(rxode2.xgxr = FALSE), {
    expect_no_warning(b <- ggplot2::ggplot_build(plot(d5, log = "y")[[1]]))
  })
  expect_equal(nrow(b$data[[1]]), 2L)
  expect_equal(length(unique(b$data[[1]]$group)), 2L)

  expect_error(plot(d, log = FALSE), NA)
  expect_error(plot(d, log = "z"), "log")
  expect_error(plot(d, log = TRUE), "log")
  expect_error(plot(d, log = c("x", "y")), "log")

  # multiple endpoints pass `log` through to each endpoint's plot
  d2 <- d
  d2$Endpoint <- factor(rep(c("a", "b"), 6))
  class(d2) <- class(d)
  p <- plot(d2, log = "y")
  expect_length(p, 2)
  for (.p in p) {
    b <- ggplot2::ggplot_build(.p)
    expect_true(.isLog(b$layout$panel_scales_y[[1]]))
  }
})
