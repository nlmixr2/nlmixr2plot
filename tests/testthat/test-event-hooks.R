skip_if_not(exists("rxEventEmit", envir = asNamespace("rxode2"), inherits = FALSE),
            "rxode2 has no event bus")
skip_if_not_installed("nlmixr2est")
skip_if_not_installed("nlmixr2data")

.rec <- new.env()
.listen <- function(env = parent.frame()) {
  .rec$ev <- list()
  rxode2::rxEventListen("nlmixr2plot-test", function(event, ...) {
    .rec$ev[[length(.rec$ev) + 1L]] <- list(event = event, p = list(...))
  })
  withr::defer(rxode2::rxEventUnlisten("nlmixr2plot-test"), envir = env)
}
.events <- function() vapply(.rec$ev, function(e) e$event, character(1))
.one <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.5)
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}
.fit <- function() suppressMessages(suppressWarnings(
  nlmixr2est::nlmixr2(.one, nlmixr2data::theo_sd, est = "posthoc")
))

test_that("plot(fit) emits one fitResult with the plot list", {
  fit <- .fit()
  .listen()
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  p <- plot(fit)
  expect_identical(.events(), "fitResult")
  expect_identical(.rec$ev[[1]]$p$kind, "plot")
  expect_true(inherits(.rec$ev[[1]]$p$fit, "nlmixr2FitCore"))
})

test_that("vpcPlot emits one fitResult and no solveComplete", {
  fit <- .fit()
  .listen()
  suppressMessages(suppressWarnings(vpcPlot(fit, n = 5)))
  expect_identical(.events(), "fitResult")
  expect_identical(.rec$ev[[1]]$p$kind, "vpcPlot")
})

test_that("plot(augPred(fit)) sends the augPred data along", {
  fit <- .fit()
  ap <- suppressMessages(nlmixr2est::augPred(fit))
  .listen()
  suppressMessages(plot(ap))
  expect_identical(.events(), "fitResult")
  expect_null(.rec$ev[[1]]$p$fit)
  expect_s3_class(.rec$ev[[1]]$p$data, "nlmixr2AugPred")
})
