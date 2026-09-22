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
})
