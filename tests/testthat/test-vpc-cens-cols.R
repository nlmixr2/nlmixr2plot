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

test_that(".vpcCensAddStratify copies stratify columns by original-data row", {
  local_mocked_bindings(vpcNameDataCmts=function(fit, data) data,
                        .package="nlmixr2est")
  .orig <- data.frame(ID=c(1, 1, 1, 2, 2, 2), EVID=c(1, 0, 0, 1, 0, 0),
                      WT=c(70, 71, 72, 80, 81, 82),
                      SEX=c("m", "m", "m", "f", "f", "f"))
  # the fit table is deliberately out of original-data order, and WT varies
  # within ID so a merge by ID (rather than by row) would give wrong values
  .fit <- list(origData=.orig, env=list(.rownum=c(6, 2, 5, 3)))
  .obs <- data.frame(ID=c(2, 1, 2, 1), DV=1:4)
  .res <- nlmixr2plot:::.vpcCensAddStratify(.obs, .fit, c("WT", "SEX"))
  expect_equal(.res$WT, c(82, 71, 81, 72))
  expect_equal(.res$SEX, c("f", "m", "f", "m"))
  # columns already present and NULL stratify are left alone
  .obs$WT <- 1
  expect_equal(nlmixr2plot:::.vpcCensAddStratify(.obs, .fit, "WT"), .obs)
  expect_equal(nlmixr2plot:::.vpcCensAddStratify(.obs, .fit, NULL), .obs)
})

test_that(".vpcCensAddStratify errors instead of misaligning rows", {
  local_mocked_bindings(vpcNameDataCmts=function(fit, data) data,
                        .package="nlmixr2est")
  .orig <- data.frame(ID=c(1, 1, 2), WT=c(70, 70, 80))
  .obs <- data.frame(ID=c(1, 2), DV=1:2)
  expect_error(
    nlmixr2plot:::.vpcCensAddStratify(.obs, list(origData=.orig,
                                                 env=list(.rownum=2)), "WT"),
    "cannot align")
  expect_error(
    nlmixr2plot:::.vpcCensAddStratify(.obs, list(origData=.orig,
                                                 env=list(.rownum=c(2, 9))), "WT"),
    "cannot align")
  expect_error(
    nlmixr2plot:::.vpcCensAddStratify(.obs, list(origData=.orig,
                                                 env=list(.rownum=c(2, 3))), "AGE"),
    "stratification column\\(s\\) not found in the data: AGE")
  # names must match exactly, as vpcSimExpand() and vpc_cens() require
  expect_error(
    nlmixr2plot:::.vpcCensAddStratify(.obs, list(origData=.orig,
                                                 env=list(.rownum=c(2, 3))), "wt"),
    "not found in the data: wt")
})
