test_that(".setupPlotData drops compartments without observations (#44)", {
  .d <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(1, 2, 1, 2),
    DV = c(1, 2, 3, 4),
    IRES = c(0.1, 0.2, 0.3, 0.4),
    CMT = factor(c("Cc", "Cc", "csf", "csf"),
                 levels = c("depot", "central", "peripheral1", "Cc", "csf"))
  )
  .ret <- .setupPlotData(.d)
  expect_equal(levels(.ret$CMT), c("Endpoint:  Cc", "Endpoint:  csf"))

  # A single observed endpoint collapses to "All Data" even when other
  # (unobserved) compartments are factor levels
  .ret1 <- .setupPlotData(.d[.d$CMT == "Cc", ])
  expect_equal(levels(.ret1$CMT), "All Data")

  # Rows filtered out by missing IRES do not keep their endpoint
  .d2 <- .d
  .d2$IRES[.d2$CMT == "csf"] <- NA
  expect_equal(levels(.setupPlotData(.d2)$CMT), "All Data")
})

test_that("plot.nlmixr2AugPred skips endpoints without data (#44)", {
  .x <- data.frame(
    id = factor(rep(1, 4)),
    time = c(1, 2, 1, 2),
    ind = factor(c("Observed", "Individual", "Observed", "Individual")),
    values = c(1, 1.1, 2, 2.1),
    Endpoint = factor(c("Cc", "Cc", "csf", "csf"),
                      levels = c("depot", "central", "Cc", "csf"))
  )
  class(.x) <- c("nlmixr2AugPred", "data.frame")
  .p <- plot(.x)
  .titles <- vapply(.p, function(p) p$labels$title, character(1))
  expect_equal(.titles, c("Cc", "csf"))
})
