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

  # Character compartments are handled like factors
  .d3 <- .d
  .d3$CMT <- as.character(.d3$CMT)
  expect_equal(levels(.setupPlotData(.d3)$CMT), c("Endpoint:  Cc", "Endpoint:  csf"))
})

test_that(".vpcMatchFactor recodes integer and character columns (#44)", {
  .ref <- factor(c("cp", "pca"), levels = c("depot", "center", "cp", "pca"))
  expect_equal(.vpcMatchFactor(c("pca", "cp"), .ref),
               factor(c("pca", "cp"), levels = levels(.ref)))
  expect_equal(.vpcMatchFactor(c(3L, 4L), .ref),
               factor(c("cp", "pca"), levels = levels(.ref)))
  expect_equal(.vpcMatchFactor(c(3, 4), .ref),
               factor(c("cp", "pca"), levels = levels(.ref)))
  # unchanged when the reference is not a factor or x is already a factor
  expect_identical(.vpcMatchFactor(1:2, 1:2), 1:2)
  expect_identical(.vpcMatchFactor(.ref, factor("a")), .ref)
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

  # NA endpoints do not create all-NA rows in any endpoint's plot
  .x$Endpoint[2] <- NA
  .p <- plot(.x)
  expect_length(.p, 2L)
  expect_false(anyNA(.p[[1]]$data$values))
})

test_that(".vpcCensEndpoint stratifies censored VPCs by observed endpoints (#44)", {
  .obs <- data.frame(CMT = factor(c("cp", "pca"),
                                  levels = c("depot", "center", "cp", "pca")))
  # integer dvid codes are decoded with the observed dvid levels
  .r <- .vpcCensEndpoint(.obs, data.frame(dvid = c(2L, 1L)), "dvid",
                         data.frame(dvid = factor(c("cp", "pca"))))
  expect_equal(.r$obs$dvid, factor(c("cp", "pca")))
  expect_equal(.r$sim$dvid, factor(c("pca", "cp"), levels = c("cp", "pca")))
  # integer cmt codes are model compartment numbers
  .r <- .vpcCensEndpoint(.obs, data.frame(cmt = c(4L, 3L)), "cmt")
  expect_equal(.r$sim$cmt, factor(c("pca", "cp"), levels = c("cp", "pca")))
  # an uppercase CMT stratification is recoded in place
  .r <- .vpcCensEndpoint(.obs, data.frame(CMT = c("pca", "cp")), "CMT")
  expect_equal(levels(.r$obs$CMT), c("cp", "pca"))
  expect_equal(.r$sim$CMT, factor(c("pca", "cp"), levels = c("cp", "pca")))
})
