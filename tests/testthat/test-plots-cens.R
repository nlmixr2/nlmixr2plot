test_that("plot censoring", {
  skip_on_cran()

  dat <-
    xgxr::case1_pkpd |>
    dplyr::rename(DV=LIDV) |>
    dplyr::filter(CMT %in% 1:2) |>
    dplyr::filter(TRTACT != "Placebo")

  doses <- unique(dat$DOSE)
  nid <- 10 # 7 ids per dose group
  dat2 <-
    do.call(
      "rbind",
      lapply(doses, function(x) {
        ids <- dat |>
          dplyr::filter(DOSE == x) |>
          dplyr::summarize(ids=unique(ID)) |>
          dplyr::pull()
        ids <- ids[seq(1, nid)]
        dat |>
          dplyr::filter(ID %in% ids)
      })
    )

  ## Use 2 compartment model
  cmt2 <- function() {
    ini({
      lka <- log(0.1) # log Ka
      lv <- log(10) # Log Vc
      lcl <- log(4) # Log Cl
      lq <- log(10) # log Q
      lvp <- log(20) # Log Vp

      eta.ka ~ 0.01
      eta.v ~ 0.1
      eta.cl ~ 0.1
      logn.sd = 10
    })
    model({
      ka <- exp(lka + eta.ka)
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      q <- exp(lq)
      vp <- exp(lvp)
      linCmt() ~ lnorm(logn.sd)
    })
  }

  ## Check parsing
  suppressMessages(
    cmt2m <- nlmixr2est::nlmixr(cmt2)
  )

  skip_if_not(rxode2parse::.linCmtSens())

  suppressMessages(
    fit <-
      nlmixr2est::nlmixr(
        cmt2m, dat2, "saem",
        control=nlmixr2est::saemControl(print=0, nBurn = 10, nEm = 20),
        table=nlmixr2est::tableControl(cwres=TRUE, npde=TRUE)
      )
  )

  apo <- nlmixr2est::augPred(fit)
  expect_error(plot(apo), NA)
  expect_error(vpcPlot(fit, stratify="DOSE", n = 10), NA)
  expect_message(
    vpcPlot(fit, pred_corr=TRUE, stratify="DOSE", log_y=TRUE, n = 10),
    regexp = "Prediction-correction cannot be used together with censored data"
  )

  expect_error(plot(fit), NA)

  expect_error(traceplot(fit), NA)

  #vdiffr::expect_doppelganger("vpc plot", vp)
  #vdiffr::expect_doppelganger("vpc pred_corr plot", vp2)
  #vdiffr::expect_doppelganger("traceplot", tp)

  #for (i in seq_along(ap)) {
  #    vdiffr::expect_doppelganger(sprintf("augPred %03d", i), ap[[i]])
  #}

  #for (i in seq_along(gof)) {
  #    vdiffr::expect_doppelganger(sprintf("gof %03d", i), gof[[i]])
  #}

  theo_cens <- nlmixr2data::theo_sd
  theo_cens$cens <- 0
  theo_cens$cens[theo_cens$DV <= 1] <- 1
  theo_cens$DV[theo_cens$DV <= 1 & theo_cens$AMT == 0] <- 1

  m1 <- function() {
    ini({
      tka <- 0.5
      tcl <- -3.2
      tv <- -1
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      add.err <- 0.1
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.err)
    })
  }
  fit1 <- nlmixr2est::nlmixr(m1, theo_cens,
                 est = "focei", control=nlmixr2est::foceiControl(print=0),
                 table = nlmixr2est::tableControl(npde = TRUE, censMethod = "cdf"))
  expect_error(vpcPlot(fit = fit1), NA)
})
