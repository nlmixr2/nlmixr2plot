test_that(".vpcCensCol prefers an exact match over a case-insensitive one", {
  # an expanded simulation can carry both the observed "TIME" and the simulated
  # "time"; the exact match must win instead of erroring as ambiguous
  .d <- data.frame(id=1, TIME=1, time=2, sim=3)
  expect_equal(nlmixr2plot:::.vpcCensCol(.d, "time", "simulated"), "time")
  expect_equal(nlmixr2plot:::.vpcCensCol(.d, "TIME", "simulated"), "TIME")

  # with no exact match it falls back to the case-insensitive one
  .d2 <- data.frame(ID=1, TIME=1, DV=2)
  expect_equal(nlmixr2plot:::.vpcCensCol(.d2, "time", "observed"), "TIME")
  expect_equal(nlmixr2plot:::.vpcCensCol(.d2, "dv", "observed"), "DV")
  expect_equal(nlmixr2plot:::.vpcCensCol(.d2, "id", "observed"), "ID")
})

test_that(".vpcCensCol errors informatively on missing and ambiguous columns", {
  .d <- data.frame(ID=1, TIME=1, DV=2)
  expect_error(nlmixr2plot:::.vpcCensCol(.d, "tad", "observed"),
               "cannot find a unique 'tad' column in the observed data")

  # ambiguous case-insensitive match: no exact "time", but two candidates
  .amb <- data.frame(ID=1, TIME=1, Time=2, DV=3)
  expect_error(nlmixr2plot:::.vpcCensCol(.amb, "time", "simulated"),
               "matched: TIME, Time")
})

test_that(".vpcCensDropStray drops only unmapped id/dv/idv columns", {
  .cols <- list(id="ID", dv="sim", idv="tad")
  .d <- data.frame(ID=1, sim=2, tad=3, dv=4, idv=5, keepMe=6)
  .res <- nlmixr2plot:::.vpcCensDropStray(.d, .cols)
  # "dv"/"idv" are vpc's standardized names but not the mapped columns
  expect_false(any(c("dv", "idv") %in% names(.res)))
  expect_true(all(c("ID", "sim", "tad", "keepMe") %in% names(.res)))

  # a mapped column that already uses a standardized name is kept
  .cols2 <- list(id="id", dv="sim", idv="time")
  .d2 <- data.frame(id=1, sim=2, time=3)
  expect_equal(names(nlmixr2plot:::.vpcCensDropStray(.d2, .cols2)),
               c("id", "sim", "time"))

  # nothing to drop leaves the data untouched
  expect_equal(nlmixr2plot:::.vpcCensDropStray(.d2, .cols2, NULL), .d2)
})

test_that(".vpcCensDropStray keeps stratify columns", {
  .cols <- list(id="ID", dv="sim", idv="tad")
  .d <- data.frame(ID=1, sim=2, tad=3, dv=4, idv=5)
  # a stratify column that happens to be named "dv" must survive
  .res <- nlmixr2plot:::.vpcCensDropStray(.d, .cols, stratify="dv")
  expect_true("dv" %in% names(.res))
  expect_false("idv" %in% names(.res))
})

test_that(".vpcCensObs moves censored dv past its limit (#55)", {
  .d <- data.frame(ID=1, TIME=1:4, DV=c(1, 2, 1, 5), CENS=c(1, 0, NA, -1))
  .res <- nlmixr2plot:::.vpcCensObs(.d)
  expect_equal(.res$DV, c(-Inf, 2, 1, Inf))
  expect_equal(.res[, c("ID", "TIME", "CENS")], .d[, c("ID", "TIME", "CENS")])

  # without a cens column the data is returned unchanged
  .d2 <- .d[, c("ID", "TIME", "DV")]
  expect_equal(nlmixr2plot:::.vpcCensObs(.d2), .d2)
})

test_that(".vpcCensObs counts each censored record only on its own side (#55)", {
  skip_if_not_installed("vpc")
  .res <- nlmixr2plot:::.vpcCensObs(
    data.frame(DV=c(1, 2, 3, 5), CENS=c(1, 0, 0, -1)))
  .frac <- function(lloq=NULL, uloq=NULL) {
    .dv <- .res$DV
    # mirror vpc:::format_vpc_input_data()
    if (!is.null(uloq)) .dv[.dv > uloq] <- NA
    if (!is.null(lloq)) .dv[.dv < lloq] <- NA
    if (is.null(uloq)) {
      vpc:::loq_frac(.dv, limit=lloq, cens="left")
    } else if (is.null(lloq)) {
      vpc:::loq_frac(.dv, limit=uloq, cens="right")
    } else {
      vpc:::loq_frac(.dv, limit=c(lloq, uloq), cens="both")
    }
  }
  # the record at the upper limit must not count as below the lower limit
  expect_equal(.frac(lloq=1), 1/4)
  expect_equal(.frac(uloq=5), 1/4)
  expect_equal(.frac(lloq=1, uloq=5), 2/4)
})
