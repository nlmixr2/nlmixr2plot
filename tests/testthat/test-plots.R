test_that("test plots with vdiffr", {

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

  censData <- nlmixr2data::theo_md
  # Assign CENS = 1 for bloq values, otherwise CENS = 0.
  censData$CENS[censData$DV < 3 & censData$AMT == 0] <- 1
  censData$CENS[censData$DV >= 3 & censData$AMT == 0] <- 0

  # Set DV to LOQ for all censored items
  censData$DV[censData$CENS == 1] <-  3

  .linB <- try(rxode2::.linCmtSensB(), silent=TRUE)
  if (inherits(.linB, "try-error")) .linB <- TRUE
  skip_if_not(.linB)

  suppressMessages(
    fit <-
      nlmixr2est::nlmixr(
        one.cmt, censData,
        est="focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10),
        table=nlmixr2est::tableControl(npde=TRUE)
      )
  )

  fitSim <- nlmixr2est::vpcSim(fit, n = 10)

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

  expect_error(plotted <- plot(fit), NA)
  expect_length(plotted, 2)
  expect_named(plotted, c("traceplot", "All Data"))
  expect_named(plotted[["All Data"]])

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

  oneCmtNoIiv <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      linCmt() ~ add(add.sd)
    })
  }

  suppressMessages(
    fitNoIiv <-
      nlmixr2est::nlmixr(
        object = oneCmtNoIiv,
        data = nlmixr2data::theo_sd,
        est = "focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10),
        table = nlmixr2est::tableControl(npde=TRUE)
      )
  )

  ## apo <- nlmixr2est::augPred(fitNoIiv)
  ## ap <- plot(apo)

  suppressWarnings(
    expect_error(vpcPlot(fitNoIiv, n = 10), NA)
  )

  #vp2 <- vpcPlot(fitNoIiv, pred_corr=TRUE)

  expect_error(currentPlot <- plot(fitNoIiv), NA)
  expect_named(currentPlot, c("traceplot", "All Data"))
  expect_true(length(currentPlot[["All Data"]]) > 1)

  expect_error(traceplot(fitNoIiv), NA)

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
