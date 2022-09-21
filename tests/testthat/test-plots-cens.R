test_that("plot censoring", {
  skip_on_cran()

  dat <- xgxr::case1_pkpd |>
    dplyr::rename(DV=LIDV) |>
    dplyr::filter(CMT %in% 1:2) |>
    dplyr::filter(TRTACT != "Placebo")

  doses <- unique(dat$DOSE)
  nid <- 10 # 7 ids per dose group
  dat2 <- do.call("rbind",
                  lapply(doses, function(x) {
                    ids <- dat |>
                      dplyr::filter(DOSE == x) |>
                      dplyr::summarize(ids=unique(ID)) |>
                      dplyr::pull()
                    ids <- ids[seq(1, nid)]
                    dat |>
                      dplyr::filter(ID %in% ids)
                  }))

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
  cmt2m <- nlmixr2est::nlmixr(cmt2)

  fit <- nlmixr2est::nlmixr(cmt2m, dat2, "saem",
                                     control=list(print=0),
                                     table=nlmixr2est::tableControl(cwres=TRUE, npde=TRUE))

  apo <- nlmixr2est::augPred(fit)
  expect_error(plot(apo), NA)
  expect_error(vpcPlot(fit, stratify="DOSE"), NA)
  expect_error(vpcPlot(fit, pred_corr=TRUE, stratify="DOSE", log_y=TRUE), NA)

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
})
