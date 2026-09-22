test_that(".vpcCensCol prefers an exact match over a case-insensitive one", {
  # an expanded simulation can carry both the observed "TIME" and the simulated
  # "time"; the exact match must win instead of erroring as ambiguous
  .d <- data.frame(id=1, TIME=1, time=2, sim=3)
  expect_equal(.vpcCensCol(.d, "time", "simulated"), "time")
  expect_equal(.vpcCensCol(.d, "TIME", "simulated"), "TIME")

  # with no exact match it falls back to the case-insensitive one
  .d2 <- data.frame(ID=1, TIME=1, DV=2)
  expect_equal(.vpcCensCol(.d2, "time", "observed"), "TIME")
  expect_equal(.vpcCensCol(.d2, "dv", "observed"), "DV")
  expect_equal(.vpcCensCol(.d2, "id", "observed"), "ID")
})

test_that(".vpcCensCol errors informatively on missing and ambiguous columns", {
  .d <- data.frame(ID=1, TIME=1, DV=2)
  expect_error(.vpcCensCol(.d, "tad", "observed"),
               "cannot find a unique 'tad' column in the observed data")

  # ambiguous case-insensitive match: no exact "time", but two candidates
  .amb <- data.frame(ID=1, TIME=1, Time=2, DV=3)
  expect_error(.vpcCensCol(.amb, "time", "simulated"),
               "matched: TIME, Time")
})

test_that(".vpcCensDropStray drops only unmapped id/dv/idv columns", {
  .cols <- list(id="ID", dv="sim", idv="tad")
  .d <- data.frame(ID=1, sim=2, tad=3, dv=4, idv=5, keepMe=6)
  .res <- .vpcCensDropStray(.d, .cols)
  # "dv"/"idv" are vpc's standardized names but not the mapped columns
  expect_false(any(c("dv", "idv") %in% names(.res)))
  expect_true(all(c("ID", "sim", "tad", "keepMe") %in% names(.res)))

  # a mapped column that already uses a standardized name is kept
  .cols2 <- list(id="id", dv="sim", idv="time")
  .d2 <- data.frame(id=1, sim=2, time=3)
  expect_equal(names(.vpcCensDropStray(.d2, .cols2)),
               c("id", "sim", "time"))

  # nothing to drop leaves the data untouched
  expect_equal(.vpcCensDropStray(.d2, .cols2, NULL), .d2)
})

test_that(".vpcCensDropStray keeps stratify columns", {
  .cols <- list(id="ID", dv="sim", idv="tad")
  .d <- data.frame(ID=1, sim=2, tad=3, dv=4, idv=5)
  # a stratify column that happens to be named "dv" must survive
  .res <- .vpcCensDropStray(.d, .cols, stratify="dv")
  expect_true("dv" %in% names(.res))
  expect_false("idv" %in% names(.res))
})

test_that(".vpcCensObs moves censored dv past its limit (#55)", {
  .d <- data.frame(ID=1, TIME=1:4, DV=c(1, 2, 1, 5), CENS=c(1, 0, NA, -1))
  .res <- .vpcCensObs(.d)
  expect_equal(.res$DV, c(-Inf, 2, 1, Inf))
  expect_equal(.res[, c("ID", "TIME", "CENS")], .d[, c("ID", "TIME", "CENS")])

  # a factor cens column is read by its values, not its codes
  .df <- .d
  .df$CENS <- factor(.df$CENS)
  expect_equal(.vpcCensObs(.df)$DV, c(-Inf, 2, 1, Inf))

  # without a cens column the data is returned unchanged, with a warning
  .d2 <- .d[, c("ID", "TIME", "DV")]
  expect_warning(.res2 <- .vpcCensObs(.d2), "no 'cens' column")
  expect_equal(.res2, .d2)
})

test_that(".vpcCensObs drops missing observations (#55)", {
  .d <- data.frame(ID=1, TIME=1:4, DV=c(NA, 2, NA, 1), CENS=c(0, 0, 1, 1))
  .res <- .vpcCensObs(.d)
  # a missing DV is dropped even when flagged as censored, as in the fit
  expect_equal(.res$TIME, c(2L, 4L))
  expect_equal(.res$DV, c(2, -Inf))

  # also without a cens column
  .res2 <- suppressWarnings(
    .vpcCensObs(.d[, c("ID", "TIME", "DV")]))
  expect_equal(.res2$TIME, c(2L, 4L))
})

test_that(".vpcCensObs errors on an ambiguous cens column (#55)", {
  .d <- data.frame(ID=1, DV=1, CENS=1, Cens=0)
  expect_error(.vpcCensObs(.d),
               "cannot find a unique 'cens' column in the observed data")
})

test_that("vpc_cens counts each censored record only on its own side (#55)", {
  skip_if_not_installed("vpc")
  # one bin, 4 observations: BLQ, two uncensored, ALQ
  .obs <- .vpcCensObs(
    data.frame(id=1:4, time=1, DV=c(1, 2, 3, 5), CENS=c(1, 0, 0, -1)))
  .sim <- data.frame(id=rep(1:4, 2), time=1, sim=3, rep=rep(1:2, each=4))
  .frac <- function(lloq=NULL, uloq=NULL) {
    .db <- suppressWarnings(suppressMessages(vpc::vpc_cens(sim=.sim, sim_cols=list(id="id", dv="sim", idv="time"),
                         obs=.obs, obs_cols=list(id="id", dv="DV", idv="time"),
                         bins=c(0, 2), lloq=lloq, uloq=uloq, vpcdb=TRUE)))
    .db$aggr_obs$obs50
  }
  # the record at the upper limit must not count as below the lower limit,
  # and vice versa (vpc takes only one of lloq/uloq)
  expect_equal(.frac(lloq=1), 1/4)
  expect_equal(.frac(uloq=5), 1/4)
})

test_that(".vpcSimData needs a CMT column for a non-normal endpoint (#68)", {
  # the compartments of a model with a non-normal endpoint are taken from the
  # fit's saved data by row number, which does not match a supplied `data`
  .fit <- list(ui=list(predDf=data.frame(distribution=c("norm", "pois"),
                                         cmt=c(3L, 4L))),
               env=new.env(parent=emptyenv()))
  .d <- data.frame(ID=1, TIME=0, DV=1, cmt=1)
  expect_error(.vpcSimData(.fit, .d), "needs a 'CMT' column")

  # the guard does not fire with an uppercase CMT column, nor for an
  # all-normal model (the fake fit still fails later, inside vpcSim())
  .msg <- function(fit, data) {
    tryCatch(.vpcSimData(fit, data), error=function(e) conditionMessage(e))
  }
  .msg2 <- function(fit, data, ...) {
    tryCatch(.vpcSimData(fit, data, ...), error=function(e) conditionMessage(e))
  }
  .d$CMT <- 3
  expect_false(grepl("needs a 'CMT' column", .msg(.fit, .d)))

  # ...nor when the normal-endpoint filter (which does the row-number lookup)
  # is turned off
  .d$CMT <- NULL
  expect_false(grepl("needs a 'CMT' column",
                     .msg2(.fit, .d, normRelated=FALSE)))

  # a factor or label CMT is read by its level order, not the model's
  # compartment numbers, so it is rejected rather than silently mismatched
  .d$CMT <- factor("cp", levels=c("depot", "count", "cp"))
  expect_error(.vpcSimData(.fit, .d), "must be the model's compartment number")
  .d$CMT <- "cp"
  expect_error(.vpcSimData(.fit, .d), "must be the model's compartment number")

  .fit$ui$predDf <- data.frame(distribution="norm", cmt=3L)
  .d$CMT <- NULL
  expect_false(grepl("needs a 'CMT' column", .msg(.fit, .d)))
})
