test_that(".vpcSimDropMissingDv() drops simulated records of missing observations (#74)", {
  obs <- data.frame(ID=1, DV=c(NA, 1, NA, 3, 4))
  sim <- data.frame(sim.id=rep(1:2, each=4), id=1,
                    nlmixrRowNums=rep(2:5, 2), sim=1:8)
  ret <- .vpcSimDropMissingDv(sim, obs, "DV")
  expect_equal(ret$nlmixrRowNums, rep(c(2L, 4L, 5L), 2))
  # nothing missing, or no row numbers to match on: unchanged
  expect_identical(.vpcSimDropMissingDv(sim, data.frame(ID=1, DV=1:5), "DV"), sim)
  expect_identical(.vpcSimDropMissingDv(sim[, -3], obs, "DV"), sim[, -3])
  # reordered observed data matches on its own row numbers
  obs2 <- obs[c(3, 1, 2, 4, 5), ]
  obs2$nlmixrRowNums <- c(3L, 1L, 2L, 4L, 5L)
  ret <- .vpcSimDropMissingDv(sim, obs2, "DV")
  expect_equal(ret$nlmixrRowNums, rep(c(2L, 4L, 5L), 2))
})

.vpcBadSimWarn <- function(expr) {
  .w <- character(0)
  .ret <- withCallingHandlers(expr, warning=function(w) {
    .w <<- c(.w, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("not a replicate|recycled", .w)))
  .ret
}

test_that("tidyvpc VPC handles missing DV observations (#74)", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("tidyvpc")

  one <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }
  fit <- try(suppressMessages(
    nlmixr2est::nlmixr(one, nlmixr2data::theo_sd, est="saem",
                       control=nlmixr2est::saemControl(print=0, nBurn=10, nEm=20))
  ), silent=TRUE)
  skip_if(inherits(fit, "try-error"))

  d <- nlmixr2data::theo_sd
  w <- which(d$EVID == 0)
  d$DV[w[c(5, 50, 100)]] <- NA

  p <- .vpcBadSimWarn(vpcPlot(fit, data=d, n=5, method="tidyvpc"))
  expect_s3_class(p, "ggplot")
  p <- .vpcBadSimWarn(vpcPlot(fit, data=d, n=5, method="tidyvpc", pred_corr=TRUE))
  expect_s3_class(p, "ggplot")
  sim <- nlmixr2est::vpcSim(fit, n=5)
  # exactly the records of the missing observations are dropped
  ret <- .vpcSimDropMissingDv(sim, d, "DV")
  expect_equal(nrow(ret), 5 * sum(d$EVID == 0 & !is.na(d$DV)))
  expect_false(anyNA(d$DV[ret$nlmixrRowNums]))
  expect_true(all(d$EVID[ret$nlmixrRowNums] == 0))
  p <- .vpcBadSimWarn(vpcPlot(sim, data=d, method="tidyvpc"))
  expect_s3_class(p, "ggplot")
})

test_that("stratified tidyvpc VPC handles missing DV observations (#74)", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("tidyvpc")

  pk.emax <- function() {
    ini({
      tka <- log(1); tcl <- log(0.1); tv <- log(10)
      eta.ka ~ 1; eta.cl ~ 2; eta.v ~ 1
      prop.err <- 0.1; pkadd.err <- 0.1
      te0 <- log(100); eta.e0 ~ .5
      pdadd.err <- 10
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      e0 <- exp(te0 + eta.e0)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      pca <- e0 * (1 - cp / (1 + cp))
      pca ~ add(pdadd.err)
    })
  }
  fit <- try(suppressMessages(
    nlmixr2est::nlmixr(pk.emax, nlmixr2data::warfarin, est="saem",
                       control=nlmixr2est::saemControl(print=0, nBurn=10, nEm=20))
  ), silent=TRUE)
  skip_if(inherits(fit, "try-error"))

  d <- nlmixr2data::warfarin
  w <- which(d$evid == 0)
  d$dv[w[c(5, 50, 300)]] <- NA

  p <- .vpcBadSimWarn(vpcPlot(fit, data=d, n=5, method="tidyvpc"))
  expect_s3_class(p, "ggplot")
})
