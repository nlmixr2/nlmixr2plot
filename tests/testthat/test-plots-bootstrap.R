test_that("the bootstrap figure of plot(fit) does not keep plot()'s frame", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("nlmixr2extra")
  skip_if_not_installed("withr")

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
  fit <- try(
    suppressMessages(
      nlmixr2est::nlmixr(
        one.cmt,
        nlmixr2data::theo_sd,
        est = "focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10)
      )
    ),
    silent = TRUE
  )
  skip_if(inherits(fit, "try-error"))
  # bootstrapFit() writes its fits to a folder in the working directory
  withr::local_dir(withr::local_tempdir())
  utils::capture.output(suppressMessages(suppressWarnings(
    nlmixr2extra::bootstrapFit(fit, nboot = 3, plotHist = TRUE)
  )))
  skip_if_not(exists(".bootPlotData", fit$env))

  .p <- suppressWarnings(plot(fit))
  expect_named(.p, c("traceplot", "bootplot", "All Data"))
  # bootplot()'s figure keeps the frame that called it.  Within plot() that
  # was plot()'s own frame, with the plotting data and every other figure
  # (151.6 MB for this fit); drawn from plot() it is now no larger than when
  # drawn on its own.
  expect_lte(
    .figureSizeWithoutData(.p[["bootplot"]]) - .figureSizeWithoutData(.bootplotFigure(fit)),
    1024
  )
  expect_false(any(grepl("<gglist>", .figureHeldData(.p[["bootplot"]]), fixed = TRUE)))
})
