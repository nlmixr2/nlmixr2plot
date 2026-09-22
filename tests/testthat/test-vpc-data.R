test_that("pred_corr VPC uses the supplied data (#62)", {
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("vpc")

  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  fit <- try(suppressMessages(
    nlmixr2est::nlmixr(one.cmt, nlmixr2data::theo_sd, est="focei",
                       control=nlmixr2est::foceiControl(print=0, eval.max=10))
  ), silent=TRUE)
  skip_if(inherits(fit, "try-error"))

  od <- fit$origData
  half <- od[od$ID %in% unique(od$ID)[1:4], ]
  .nobs <- sum(half$EVID == 0 & !is.na(half$DV))

  db <- suppressWarnings(
    vpcPlot(fit, data=half, n=5, vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$obs$id)), 4L)

  db <- suppressWarnings(
    vpcPlot(fit, data=half, n=5, pred_corr=TRUE, vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$obs$id)), 4L)
  expect_equal(nrow(db$obs), .nobs)

  # the default (no data) still uses the full fitted data
  db <- suppressWarnings(
    vpcPlot(fit, n=5, pred_corr=TRUE, vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$obs$id)), length(unique(od$ID)))

  # the simulation uses the supplied data as well (#68)
  db <- suppressWarnings(
    vpcPlot(fit, data=half, n=5, vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$sim$id)), 4L)
  expect_equal(nrow(db$sim), 5L * .nobs)

  db <- suppressWarnings(
    vpcPlot(fit, data=half, n=5, pred_corr=TRUE, vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$sim$id)), 4L)
  expect_equal(nrow(db$sim), 5L * .nobs)

  # the fit's data is restored after simulating from the supplied data
  expect_equal(fit$origData, od)
  expect_equal(nrow(fit$simInfo$events), nrow(od))

  # stratifying by a data covariate keeps the supplied subjects and rows
  db <- suppressWarnings(
    vpcPlot(fit, data=half, n=5, stratify="WT", vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$sim$id)), 4L)
  expect_equal(nrow(db$sim), 5L * .nobs)
  expect_setequal(unique(db$sim$WT), unique(half$WT))

  # ...and without data, stratifying still uses the full fitted data
  db <- suppressWarnings(
    vpcPlot(fit, n=5, stratify="WT", vpcdb=TRUE, method="vpc"))
  expect_equal(length(unique(db$sim$id)), length(unique(od$ID)))
  expect_setequal(unique(db$sim$WT), unique(od$WT))

  # the fit's data is restored even when the simulation errors
  expect_error(.vpcSimData(fit, half, nretry=-1))
  expect_equal(fit$origData, od)
  expect_equal(nrow(fit$simInfo$events), nrow(od))

  # a supplied simulation was made from its own data, so `data` only replaces
  # the observed side (and says so)
  .sim <- nlmixr2est::vpcSim(fit, n=3, pred=TRUE)
  expect_warning(
    db <- vpcPlot(.sim, data=half, vpcdb=TRUE, method="vpc"),
    "does not change a supplied")
  expect_equal(length(unique(db$obs$id)), 4L)
  expect_equal(length(unique(db$sim$id)), length(unique(od$ID)))

  skip_if_not_installed("tidyvpc")
  for (.pc in c(FALSE, TRUE)) {
    .warn <- character(0)
    withCallingHandlers(
      vpcPlot(fit, data=half, n=5, pred_corr=.pc, method="tidyvpc"),
      warning=function(w) {
        .warn <<- c(.warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    expect_false(any(grepl("xsim", .warn)), info=paste("pred_corr =", .pc))
  }
})
