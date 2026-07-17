library(testthat)
library(nlmixr2plot)

# Thread policy on CI / CRAN: keep the test process single-threaded so the
# repeated model fits do not oversubscribe the (small) hosted runners.  On CRAN
# also cap rxode2 within-solve threads to two per CRAN's policy.
.onCran <- !identical(Sys.getenv("NOT_CRAN"), "true")
.onCI   <- isTRUE(as.logical(Sys.getenv("CI", "false")))
if (.onCI || .onCran) {
  options(Ncpus = 1L)
  Sys.setenv(TESTTHAT_CPUS = "1")
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
}
if (.onCran && requireNamespace("rxode2", quietly = TRUE)) {
  rxode2::setRxThreads(2L)
}

# -------------------------------------------------------------------------
# CI test partitioning
#
# Every test file fits real nlmixr2 models, so the full suite is expensive on a
# single hosted runner.  push/PR R-CMD-check runs only the "essential" subset --
# every test file EXCEPT the slow ones listed in .slowBatches below -- which
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
.slowBatches <- list(
  # batch 1 -- heaviest fit-based files (multi-endpoint PK/PD + censored VPC)
  c("plots-multiple-endpoints", "plots-cens"),
  # batch 2 -- between-subject-variability plot detail (QQ / correlation /
  # covariate plots across several fits)
  c("plots-bsv")
)
.slowAll <- unlist(.slowBatches)

.batch <- Sys.getenv("NLMIXR2PLOT_TEST_BATCH")

.filter <- NULL
if (nzchar(.batch)) {
  # Slow-batch mode: run ONLY this batch's slow files.
  .b <- suppressWarnings(as.integer(.batch))
  if (is.na(.b) || .b < 1L || .b > length(.slowBatches)) {
    stop(sprintf("NLMIXR2PLOT_TEST_BATCH=%s out of range (1..%d)",
                 .batch, length(.slowBatches)))
  }
  .files <- .slowBatches[[.b]]
  .filter <- paste0("^(", paste(.files, collapse = "|"), ")$")
} else if (.onCI && !.onCran && length(.slowAll) > 0L) {
  # Essential subset on push/PR CI: everything EXCEPT the slow files.
  .filter <- paste0("^(?!(", paste(.slowAll, collapse = "|"), ")$)")
}
# Locally (and on CRAN) .filter stays NULL -> run everything.

test_check("nlmixr2plot", filter = .filter, perl = TRUE)
