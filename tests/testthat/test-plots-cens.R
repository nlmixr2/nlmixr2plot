test_that("plot censoring", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("vpc")
  skip_if_not_installed("tidyvpc")

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
          dplyr::reframe(ids=unique(ID)) |>
          dplyr::pull()
        ids <- ids[seq_len(nid)]
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

  suppressMessages(
    fit <-
      nlmixr2est::nlmixr(
        cmt2m, dat2, "saem",
        control=nlmixr2est::saemControl(print=0, nBurn = 10, nEm = 20),
        table=nlmixr2est::tableControl(cwres=TRUE, npde=TRUE, nsim = 10)
      )
  )

  apo <- nlmixr2est::augPred(fit)
  expect_error(plot(apo), NA)
  expect_error(vpcPlot(fit, stratify="DOSE", n = 10), NA)

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
                 table = nlmixr2est::tableControl(npde = TRUE, nsim = 10,
                                                  censMethod = "cdf"))
  expect_error(vpcPlot(fit = fit1, n = 10), NA)

  # nlmixr2#390: censored VPC with a non-time idv (tad) must not error with
  # "object of type 'closure' is not subsettable"
  expect_error(vpcCens(fit1, cens = TRUE, n = 10), NA)
  expect_error(vpcCensTad(fit1, cens = TRUE, idv = "tad", n = 10), NA)
  # also works when an nlmixr2vpcSim object is passed instead of a fit
  sim390 <- nlmixr2est::vpcSim(fit1, n = 10, pred = TRUE)
  expect_error(vpcCens(sim390, cens = TRUE, n = 10), NA)
  expect_error(vpcCensTad(sim390, cens = TRUE, idv = "tad", n = 10), NA)

  # #57: a supplied simulation is reused, not re-simulated at the default n
  sim57 <- nlmixr2est::vpcSim(fit1, n = 5, pred = TRUE, seed = 7)
  for (.idv in c("time", "tad")) {
    .db <- vpcCens(sim57, cens = TRUE, idv = .idv, vpcdb = TRUE)
    expect_equal(sort(unique(.db$sim$sim)), 1:5)
    .db <- vpcPlot(sim57, idv = .idv, vpcdb = TRUE)
    expect_equal(nrow(.db$sim), nrow(sim57))
    expect_equal(length(unique(.db$sim$sim)), 5L)
  }
  .db <- vpcPlot(sim57, pred_corr = TRUE, vpcdb = TRUE)
  expect_equal(length(unique(.db$sim$sim)), 5L)
  # the observed pred-correction must come from sim57's own fit, not whatever
  # vpcSim(pred = TRUE) ran last (simulate another model to make it stale)
  fit2 <- nlmixr2est::nlmixr(rxode2::ini(m1, tcl = 1, tv = 3), theo_cens,
                             est = "posthoc",
                             control = nlmixr2est::foceiControl(print = 0))
  invisible(nlmixr2est::vpcSim(fit2, n = 2, pred = TRUE))
  .dbStale <- vpcPlot(sim57, pred_corr = TRUE, vpcdb = TRUE)
  # compare with the fit path, which simulates and pred-corrects in one go
  .dbFit <- vpcPlot(fit1, n = 5, seed = 7, pred_corr = TRUE, vpcdb = TRUE)
  expect_equal(.dbStale$obs, .dbFit$obs)
  expect_equal(.dbStale$sim, .dbFit$sim)
  .dbSimTad <- vpcPlotTad(sim57, pred_corr = TRUE, vpcdb = TRUE)
  .dbFitTad <- vpcPlotTad(fit1, n = 5, seed = 7, pred_corr = TRUE,
                          vpcdb = TRUE)
  expect_equal(.dbSimTad$obs, .dbFitTad$obs)
  expect_equal(.dbSimTad$sim, .dbFitTad$sim)
  expect_warning(vpcPlot(sim57, n = 10, vpcdb = TRUE), "'n' is ignored")
  expect_no_warning(vpcPlot(sim57, n = 5, vpcdb = TRUE),
                    message = "'n' is ignored")
  expect_error(
    vpcPlot(nlmixr2est::vpcSim(fit1, n = 5), pred_corr = TRUE),
    "pred = TRUE")

  # The censored VPC must group the simulated data by replicate.  A leftover
  # "sim" column made vpc use the simulated values themselves as the replicate
  # index, giving one "replicate" per row and a meaningless confidence band.
  for (.idv in c("time", "tad")) {
    .db <- vpcPlot(fit1, cens = TRUE, idv = .idv, n = 10, vpcdb = TRUE)
    expect_true(all(c("id", "dv", "idv") %in% names(.db$sim)))
    # "sim" is vpc's replicate index, so it must be 1..n, not the simulated
    # values that used to be left in that column
    expect_equal(sort(unique(.db$sim$sim)), 1:10)
  }

  # #55: the censored VPC must honour `data` (it used to always use the fit)
  .od <- fit1$origData
  # a subset that does not start at the first subject
  .half <- .od[.od$ID %in% unique(.od$ID)[5:8], ]
  .nObs <- function(d) sum(d$AMT == 0 & !is.na(d$DV))
  .all <- vpcCens(fit1, cens = TRUE, n = 5, vpcdb = TRUE)
  .sub <- vpcCens(fit1, data = .half, cens = TRUE, n = 5, vpcdb = TRUE)
  expect_equal(nrow(.all$obs), .nObs(.od))
  expect_equal(nrow(.sub$obs), .nObs(.half))
  # censored records (CENS == 1, DV at the limit) count as below the limit
  expect_equal(sum(is.na(.all$obs$dv)), sum(.od$cens == 1 & .od$AMT == 0))
  expect_equal(sum(is.na(.sub$obs$dv)), sum(.half$cens == 1 & .half$AMT == 0))
  # records that are not observations are dropped from the supplied data:
  # EVID=0 with MDV=1 (even alongside EVID) and a missing DV
  .skip <- .od
  .skip$MDV <- as.integer(.skip$EVID != 0)
  .wObs <- which(.skip$EVID == 0)
  .skip$MDV[.wObs[3]] <- 1L
  .skip$DV[.wObs[5]] <- NA
  .skipDb <- vpcCens(fit1, data = .skip, cens = TRUE, n = 5, vpcdb = TRUE)
  expect_equal(nrow(.skipDb$obs), .nObs(.od) - 2L)

  # ... and so must the tad variant (leading subjects only: a subset that is
  # not row-aligned with the fit loses tad, #60)
  .lead <- .od[.od$ID %in% unique(.od$ID)[1:4], ]
  .tad <- vpcCensTad(fit1, data = .lead, cens = TRUE, n = 5, vpcdb = TRUE)
  expect_equal(nrow(.tad$obs), .nObs(.lead))
  expect_false(anyNA(.tad$obs$idv))

  # #56: a stratified censored VPC must find the covariate in the observed data
  # (as.data.frame(fit) drops it) and assign it to the right rows
  .db <- vpcCens(fit1, cens = TRUE, n = 5, stratify = "WT", vpcdb = TRUE)
  .wt <- unique(fit1$origData[, c("ID", "WT")])
  # WT is constant within ID here, so each observed row's stratum must be its
  # subject's WT
  expect_equal(as.character(.db$obs$strat),
               as.character(.wt$WT[match(as.character(.db$obs$id),
                                         as.character(.wt$ID))]))

  # nlmixr2#390: prediction-corrected VPC on censored data must not crash
  # with a quantile() NA error
  expect_error(vpcPlot(fit = fit1, pred_corr = TRUE, n = 10), NA)
  expect_error(vpcPlotTad(fit = fit1, pred_corr = TRUE, n = 10), NA)
  if (requireNamespace("tidyvpc", quietly = TRUE)) {
    expect_error(
      vpcPlot(fit = fit1, pred_corr = TRUE, n = 10, method = "tidyvpc"), NA)
    expect_error(
      vpcPlot(fit = fit1, pred_corr = TRUE, n = 10, method = "tidyvpc",
              cens = TRUE), NA)
    # #57: a supplied simulation through the pred-corrected tidyvpc paths
    expect_error(vpcPlot(sim57, pred_corr = TRUE, method = "tidyvpc"), NA)
    expect_error(
      vpcCens(sim57, pred_corr = TRUE, method = "tidyvpc"), NA)
    # #74: missing DV observations are dropped from the simulation too, so
    # tidyvpc still gets a replicate of the observed records
    .na <- theo_cens
    .w <- which(.na$AMT == 0)
    .na$DV[.w[c(5, 50, 100)]] <- NA
    for (.pc in c(FALSE, TRUE)) {
      .warn <- character(0)
      withCallingHandlers(
        .p <- vpcCens(fit1, data = .na, n = 5, pred_corr = .pc,
                      method = "tidyvpc"),
        warning = function(w) {
          .warn <<- c(.warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      expect_s3_class(.p, "ggplot")
      expect_false(any(grepl("not a replicate|recycled", .warn)),
                   info = paste("pred_corr =", .pc))
    }
  }
})
