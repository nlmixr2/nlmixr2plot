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

  fit <- nlmixr2est::nlmixr(one.cmt, nlmixr2data::theo_sd, est="focei")
  apo <- nlmixr2est::augPred(fit)
  ap <- plot(apo)
  vp <- vpcPlot(fit)

  gof <- plot(fit)

  tp <- traceplot(fit)

  vdiffr::expect_doppelganger("vpc plot", vp)
  vdiffr::expect_doppelganger("traceplot", tp)

  for (i in seq_along(ap)) {
      vdiffr::expect_doppelganger(sprintf("augPred %03d", i), ap[[i]])
  }

  for (i in seq_along(gof)) {
      vdiffr::expect_doppelganger(sprintf("gof %03d", i), gof[[i]])
  }

})
