test_that("multiple endpoint plots", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("vpc")
  skip_if_not_installed("tidyvpc")

  pk.turnover.emax3 <- function() {
    ini({
      tktr <- log(1)
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)

      eta.ktr ~ 1
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      prop.err <- 0.1
      pkadd.err <- 0.1

      temax <- logit(0.8)
      tec50 <- log(0.5)
      tkout <- log(0.05)
      te0 <- log(100)

      eta.emax ~ .5
      eta.ec50  ~ .5
      eta.kout ~ .5
      eta.e0 ~ .5

      pdadd.err <- 10
    })
    model({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      emax = expit(temax+eta.emax)
      ec50 =  exp(tec50 + eta.ec50)
      kout = exp(tkout + eta.kout)
      e0 = exp(te0 + eta.e0)

      DCP = center/v
      PD=1-emax*DCP/(ec50+DCP)

      effect(0) = e0
      kin = e0*kout

      d/dt(depot) = -ktr * depot
      d/dt(gut) =  ktr * depot -ka * gut
      d/dt(center) =  ka * gut - cl / v * center
      d/dt(effect) = kin*PD -kout*effect

      cp = center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      effect ~ add(pdadd.err) | pca
    })
  }

  suppressMessages(
    fit <-
      nlmixr2est::nlmixr(
        pk.turnover.emax3,
        nlmixr2data::warfarin,
        est = "saem",
        control=nlmixr2est::saemControl(print=0, nBurn = 10, nEm = 20),
        table=list(cwres=TRUE, npde=TRUE, nsim = 10)
      )
  )

  # augPred() does not support this multiple-endpoint model in every
  # nlmixr2est version (upstream dispatch on "nlmixr2FitCore"); when it is
  # available, check that plot(augPred) works, otherwise still exercise the
  # rest of the plotting surface below.
  apo <- tryCatch(nlmixr2est::augPred(fit), error = function(e) e)
  if (inherits(apo, "error")) {
    message("skipping augPred plot (augPred unavailable): ",
            conditionMessage(apo))
  } else {
    expect_error(plot(apo), NA)
  }
  expect_error(vpcPlot(fit, n = 10), NA)
  expect_error(vpcPlot(fit, pred_corr=TRUE, n = 10), NA)

  suppressWarnings(
    expect_error(plot(fit), NA)
  )
  suppressWarnings(
    expect_named(plot(fit))
  )
  # Only observed endpoints are plotted; state compartments like depot, gut
  # and center are not endpoints (#44)
  suppressWarnings(
    .names <- names(plot(fit))
  )
  expect_equal(grep("^Endpoint:", .names, value = TRUE),
               c("Endpoint:  cp", "Endpoint:  pca"))
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

test_that("cmt-coded multiple endpoints only plot observed endpoints (#44)", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("vpc")
  skip_if_not_installed("tidyvpc")

  pk.emax <- function() {
    ini({
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      prop.err <- 0.1
      pkadd.err <- 0.1
      te0 <- log(100)
      eta.e0 ~ .5
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

  # Code the endpoints by `cmt` (not `dvid`), so the compartment factor also
  # has levels for the unobserved states depot and center.  Use both labels
  # and the numeric compartment codes (depot = 1, center = 2, cp = 3, pca = 4)
  d <- nlmixr2data::warfarin
  .cmtLabel <- ifelse(d$evid != 0, "depot", as.character(d$dvid))
  d$dvid <- NULL
  .coding <- list(
    character = .cmtLabel,
    numeric = c(depot = 1L, cp = 3L, pca = 4L)[.cmtLabel]
  )
  for (.c in names(.coding)) {
    d$cmt <- unname(.coding[[.c]])

    suppressMessages(suppressWarnings(
      fit <- nlmixr2est::nlmixr(
        pk.emax, d, est = "saem",
        control = nlmixr2est::saemControl(print = 0, nBurn = 10, nEm = 20)
      )
    ))
    expect_true(all(c("depot", "center") %in% levels(fit$CMT)))

    suppressWarnings(.names <- names(plot(fit)))
    expect_equal(grep("^Endpoint:", .names, value = TRUE),
                 c("Endpoint:  cp", "Endpoint:  pca"))

    .panels <- function(p) {
      as.character(ggplot2::ggplot_build(p)$layout$layout$cmt)
    }
    for (.method in c("vpc", "tidyvpc")) {
      for (.pc in c(FALSE, TRUE)) {
        suppressWarnings(
          .p <- vpcPlot(fit, n = 10, pred_corr = .pc, method = .method)
        )
        expect_equal(.panels(.p), c("cp", "pca"),
                     info = paste(.c, .method, "pred_corr =", .pc))
      }
    }
    # the censored vpc path stratifies by the observed endpoints too
    suppressWarnings(
      .p <- vpcPlot(fit, n = 10, cens = TRUE, lloq = 2, method = "vpc")
    )
    expect_equal(.panels(.p), c("cp", "pca"), info = paste(.c, "cens"))
  }
})
