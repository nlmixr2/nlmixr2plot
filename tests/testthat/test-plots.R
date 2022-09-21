test_that("test plots with vdiffr", {

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
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

  censData <- nlmixr2data::theo_md
  # Assign CENS = 1 for bloq values, otherwise CENS = 0.
  censData$CENS[censData$DV < 3 & censData$AMT == 0] <- 1
  censData$CENS[censData$DV >= 3 & censData$AMT == 0] <- 0
  #
  # Set DV to LOQ for all censored items
  censData$DV[censData$CENS == 1] <-  3
#

  fit <- nlmixr2est::nlmixr(one.cmt, censData,
                            est="focei",
                            table=nlmixr2est::tableControl(npde=TRUE))

  fitSim <- nlmixr2est::vpcSim(fit)

  apo <- nlmixr2est::augPred(fit)
  expect_error(plot(apo), NA)
  expect_error(vpcPlot(fit), NA)
  expect_error(vpcPlot(fitSim), NA)
  expect_error(vpcPlotTad(fit), NA)
  expect_error(vpcPlotTad(fitSim), NA)
  expect_error(vpcPlot(fit, pred_corr=TRUE), NA)
  expect_error(vpcPlot(fitSim, pred_corr=TRUE), NA)
  expect_error(vpcPlotTad(fit, pred_corr=TRUE), NA)
  expect_error(vpcPlotTad(fitSim, pred_corr=TRUE), NA)
  expect_error(vpcCens(fit), NA)
  expect_error(vpcCens(fitSim, pred_corr=TRUE), NA)
  expect_error(vpcCensTad(fit), NA)
  expect_error(vpcCensTad(fitSim, pred_corr=TRUE), NA)

  expect_error(plot(fit), NA)

  expect_error(traceplot(fit),NA)

  #vdiffr::expect_doppelganger("vpc plot", vp)
  #vdiffr::expect_doppelganger("vpc pred_corr plot", vp2)
  #vdiffr::expect_doppelganger("traceplot", tp)

  #for (i in seq_along(ap)) {
  #    vdiffr::expect_doppelganger(sprintf("augPred %03d", i), ap[[i]])
  #}

  #for (i in seq_along(gof)) {
  #    vdiffr::expect_doppelganger(sprintf("gof %03d", i), gof[[i]])
  #}

  withr::with_options(list(rxode2.xgxr=FALSE), {
    expect_error(plot(fit), NA)

    #for (i in seq_along(gof)) {
    #  vdiffr::expect_doppelganger(sprintf("gof without xgxr %03d", i), gof[[i]])
    #}
  })

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      linCmt() ~ add(add.sd)
    })
  }

  fit2 <- nlmixr2est::nlmixr(one.cmt, nlmixr2data::theo_sd, est="focei",
                             table=nlmixr2est::tableControl(npde=TRUE))

  ## apo <- nlmixr2est::augPred(fit2)
  ## ap <- plot(apo)

  expect_error(vpcPlot(fit2), NA)

  #vp2 <- vpcPlot(fit2, pred_corr=TRUE)

  expect_error(plot(fit2), NA)

  expect_error(traceplot(fit2), NA)

  #vdiffr::expect_doppelganger("vpc plot np", vp)
  #vdiffr::expect_doppelganger("vpc pred_corr plot np", vp2)
  #vdiffr::expect_doppelganger("traceplot np", tp)

  ## for (i in seq_along(ap)) {
  ##     vdiffr::expect_doppelganger(sprintf("augPred %03d", i), ap[[i]])
  ## }

  #for (i in seq_along(gof)) {
  #    vdiffr::expect_doppelganger(sprintf("gof %03d np", i), gof[[i]])
  #}

  withr::with_options(list(rxode2.xgxr=FALSE), {
    expect_error(plot(fit), NA)

    #for (i in seq_along(gof)) {
    #  vdiffr::expect_doppelganger(sprintf("gof without xgxr np %03d", i), gof[[i]])
    #}
  })
})
