# Which etas the between-subject-variability (BSV) plots draw.  These use
# mocked fits, so unlike the fit-based tests in test-plots-bsv.R (a slow batch)
# they run on every push/PR.

test_that(".bsvHasBsv() is FALSE when omega has only non-subject levels", {
  # An occasion-only model reports a non-NULL `omega` list with no `id` level,
  # and its `ranef()` is the bare ID vector (nlmixr2est drops the occasion
  # columns without `drop = FALSE`).  The guard must not mistake it for BSV, and
  # must not need `ranef()` to be a data frame to say so.
  x <- list(omega = list(OCC = matrix(0.1, dimnames = list("eOcc", "eOcc"))))
  local_mocked_bindings(
    ranef = function(object, ...) factor(1:2),
    .package = "nlmixr2est"
  )
  expect_false(.bsvHasBsv(x))
  expect_identical(.bsvEtas(x), character(0))
  expect_false(.bsvHasBsv(list(omega = NULL)))
})

test_that(".bsvEtas() keeps only estimated subject-level etas", {
  # `ranef()` of a mixture fit carries the per-subject `mixnum` assignment, and
  # a `fix(0)` eta kept beside estimated ones has a zero `omega` diagonal;
  # neither is a BSV term, so neither gets QQ or correlation plots.
  .om <- diag(c(0.1, 0))
  dimnames(.om) <- list(c("etaCl", "etaKa"), c("etaCl", "etaKa"))
  local_mocked_bindings(
    ranef = function(object, ...) {
      data.frame(ID = 1:2, etaCl = c(-0.1, 0.1), etaKa = 0, mixnum = 1:2)
    },
    .package = "nlmixr2est"
  )
  expect_equal(.bsvEtas(list(omega = .om)), "etaCl")
  # The same subject block nested in a multi-level `omega` list
  .lst <- list(omega = list(id = .om, OCC = matrix(0.1, dimnames = list("o", "o"))))
  expect_equal(.bsvEtas(.lst), "etaCl")
})

test_that("BSV plots are skipped when every subject eta is fixed at zero", {
  # No estimated subject variance means nothing to plot, and `ranef()` is not
  # needed to say so.
  .om <- matrix(0, dimnames = list("etaCl", "etaCl"))
  local_mocked_bindings(
    ranef = function(object, ...) stop("ranef() should not be called"),
    .package = "nlmixr2est"
  )
  expect_identical(.bsvEtas(list(omega = .om)), character(0))
  expect_null(.bsvPlots(list(omega = .om), covariate = "WT"))
  # An NA variance (e.g. a failed estimate) is not an estimated eta either
  .om[1, 1] <- NA_real_
  expect_identical(.bsvEtas(list(omega = .om)), character(0))
})

test_that("a level whose name starts with id is not taken as the subject level", {
  .om <- matrix(0.1, dimnames = list("etaOcc", "etaOcc"))
  expect_null(.bsvOmega(list(omega = list(idOcc = .om))))
})

test_that("a mixnum covariate is not shadowed by ranef()'s mixnum column", {
  .om <- matrix(0.1, dimnames = list("etaCl", "etaCl"))
  local_mocked_bindings(
    ranef = function(object, ...) {
      data.frame(ID = 1:2, etaCl = c(-0.1, 0.1), mixnum = 1:2)
    },
    getData = function(object, ...) {
      data.frame(ID = c(1, 1, 2), mixnum = c(2, 2, 1))
    },
    .package = "nlmixr2est"
  )
  .m <- .bsvCovData(list(omega = .om), "mixnum")
  expect_setequal(names(.m), c(".bsvID", "etaCl", "mixnum"))
  expect_equal(.m$mixnum[order(.m$.bsvID)], c(2, 1))
})
