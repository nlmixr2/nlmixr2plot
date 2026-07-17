library(testthat)
library(nlmixr2plot)

# Thread policy on CI / CRAN: keep the test process single-threaded so the
# repeated model fits do not oversubscribe the (small) hosted runners.  On CRAN
# also cap rxode2 within-solve threads to two per CRAN's policy.
.on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
.on_ci <- isTRUE(as.logical(Sys.getenv("CI", "false")))
if (.on_ci || .on_cran) {
  options(Ncpus = 1L)
  Sys.setenv(TESTTHAT_CPUS = "1")
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
}
if (.on_cran && requireNamespace("rxode2", quietly = TRUE)) {
  rxode2::setRxThreads(2L)
}

# -------------------------------------------------------------------------
# CI test partitioning
#
# Every test file fits real nlmixr2 models, so the full suite is expensive on a
# single hosted runner.  push/PR R-CMD-check runs only the "essential" subset --
# every test file EXCEPT the slow ones listed in .slow_batches below -- which
# still exercises the core plotting surface (plot(fit) goodness-of-fit plots,
# vpcPlot()/traceplot(), augPred() plots and the gglist collection structure).
#
# The slow files run separately in the weekly slow-tests workflow, split into
# batches that run one-at-a-time (non-overlapping).  That workflow sets
# NLMIXR2PLOT_TEST_BATCH=<n> to run only batch n's files.
#
# Names are the test file basename with the leading "test-" and trailing ".R"
# removed (what testthat's `filter` matches).  When a test file grows past a few
# minutes, move it into one of the batches here.
# -------------------------------------------------------------------------
.slow_batches <- list(
  # batch 1 -- heaviest fit-based files (multi-endpoint PK/PD + censored VPC)
  c("plots-multiple-endpoints", "plots-cens"),
  # batch 2 -- between-subject-variability plot detail (QQ / correlation /
  # covariate plots across several fits)
  c("plots-bsv")
)
.slow_all <- unlist(.slow_batches)

.batch <- Sys.getenv("NLMIXR2PLOT_TEST_BATCH")

.filter <- NULL
if (nzchar(.batch)) {
  # Slow-batch mode: run ONLY this batch's slow files.
  .b <- suppressWarnings(as.integer(.batch))
  if (is.na(.b) || .b < 1L || .b > length(.slow_batches)) {
    stop(sprintf("NLMIXR2PLOT_TEST_BATCH=%s out of range (1..%d)",
                 .batch, length(.slow_batches)))
  }
  .files <- .slow_batches[[.b]]
  .filter <- paste0("^(", paste(.files, collapse = "|"), ")$")
} else if (.on_ci && !.on_cran && length(.slow_all) > 0L) {
  # Essential subset on push/PR CI: everything EXCEPT the slow files.
  .filter <- paste0("^(?!(", paste(.slow_all, collapse = "|"), ")$)")
}
# Locally (and on CRAN) .filter stays NULL -> run everything.

test_check("nlmixr2plot", filter = .filter, perl = TRUE)
