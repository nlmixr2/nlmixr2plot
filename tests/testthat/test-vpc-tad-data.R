test_that("supplied data gets the fitted tad by content, not row number (#60)", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")

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

  suppressMessages(
    fit <-
      nlmixr2est::nlmixr(
        one.cmt,
        nlmixr2data::theo_md,
        est = "focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10)
      )
  )

  .full <- .vpcUiSetupObservationData(fit, idv = "tad")
  expect_equal(.full$obsCols$idv, "tad")
  expect_equal(nrow(.full$obs), nrow(fit$origData))
  expect_false(any(names(.full$obs) == "nlmixrRowNums"))

  od <- fit$origData
  .in <- od$ID %in% unique(od$ID)[5:8]
  .ref <- .full$obs$tad[.in]
  expect_true(any(!is.na(.ref)))

  sub <- od[.in, ]
  .o <- .vpcUiSetupObservationData(fit, data = sub, idv = "tad", cens = TRUE)
  expect_equal(.o$obsCols$idv, "tad")
  expect_equal(.o$obs$tad, .ref)

  # reordered rows without the original row names
  sub2 <- sub[rev(seq_len(nrow(sub))), ]
  rownames(sub2) <- NULL
  .o <- .vpcUiSetupObservationData(fit, data = sub2, idv = "tad")
  expect_equal(.o$obs$tad, rev(.ref))

  # column names differing only in case still match on every column
  sub5 <- sub
  names(sub5)[names(sub5) == "TIME"] <- "time"
  .o <- .vpcUiSetupObservationData(fit, data = sub5, idv = "tad")
  expect_equal(.o$obs$tad, .ref)

  # without a shared time column, rows cannot be matched reliably
  sub6 <- sub
  names(sub6)[names(sub6) == "TIME"] <- "hours"
  expect_warning(.o <- .vpcUiSetupObservationData(fit, data = sub6, idv = "tad"), "do not match the fitted data")
  expect_true(all(is.na(.o$obs$tad)))

  if (requireNamespace("data.table", quietly = TRUE)) {
    .o <- .vpcUiSetupObservationData(fit, data = data.table::as.data.table(sub), idv = "tad")
    expect_equal(.o$obs$tad, .ref)
  }

  # observations not in the fitted data warn instead of silently being NA
  sub3 <- sub
  sub3$TIME[which(sub3$EVID == 0)[1]] <- 999
  expect_warning(
    .vpcUiSetupObservationData(fit, data = sub3, idv = "tad"),
    "1 observation\\(s\\) in 'data' do not match"
  )

  # without evid/mdv (and amt), observations still match the fitted rows
  # rather than an identical-looking dose row, and unmatched rows warn
  sub7 <- sub[sub$EVID == 0, setdiff(names(sub), c("EVID", "AMT"))]
  .o <- .vpcUiSetupObservationData(fit, data = sub7, idv = "tad")
  expect_equal(.o$obs$tad, .ref[sub$EVID == 0])
  sub7$TIME[1] <- 999
  expect_warning(
    .vpcUiSetupObservationData(fit, data = sub7, idv = "tad"),
    "1 observation\\(s\\) in 'data' do not match"
  )

  # numbers supplied as text still match
  sub8 <- sub
  sub8$TIME <- as.character(sub8$TIME)
  .o <- .vpcUiSetupObservationData(fit, data = sub8, idv = "tad")
  expect_equal(.o$obs$tad, .ref)

  # the VPC itself runs with the subset data
  for (.m in c("vpc", "tidyvpc")) {
    if (requireNamespace(.m, quietly = TRUE)) {
      expect_error(
        suppressWarnings(
          vpcPlotTad(fit, data = sub, n = 10, method = .m)
        ),
        NA
      )
    }
  }

  # a supplied tad column is used as-is
  sub4 <- sub
  sub4$tad <- 42
  .o <- .vpcUiSetupObservationData(fit, data = sub4, idv = "tad")
  expect_true(all(.o$obs$tad == 42))
})

test_that(".vpcFitColForData does not guess between identical-looking fitted rows (#60)", {
  local_mocked_bindings(vpcNameDataCmts = function(object, data) data, .package = "nlmixr2est")
  # two observations at the same time in different compartments with
  # different tad; row 1 is a dose
  .orig <- data.frame(ID = 1, TIME = c(0, 5, 5, 6), DV = c(NA, 0, 0, 1), CMT = c(1, 2, 3, 2), EVID = c(1, 0, 0, 0))
  .fit <- list(origData = .orig, tad = c(5, 2, 6), env = list(.rownum = 2:4))

  # with cmt kept each row gets its own tad
  expect_equal(.vpcFitColForData(.fit, .orig[4:2, ], "tad", supplied = TRUE), c(6, 2, 5))

  # without cmt the two time-5 rows cannot be told apart
  .sub <- .orig[2:4, c("ID", "TIME", "DV", "EVID")]
  expect_warning(
    .r <- .vpcFitColForData(.fit, .sub, "tad", supplied = TRUE),
    "2 observation\\(s\\) in 'data' match several fitted rows"
  )
  expect_equal(.r, c(NA, NA, 6))

  # identical-looking rows with the same value are not ambiguous
  .fit$tad <- c(5, 5, 6)
  expect_warning(.r <- .vpcFitColForData(.fit, .sub, "tad", supplied = TRUE), NA)
  expect_equal(.r, c(5, 5, 6))

  # an observation never picks up an identical-looking (unfitted) dose row
  .orig2 <- data.frame(ID = 1, TIME = c(0, 0), DV = c(0, 0), EVID = c(1, 0))
  .fit2 <- list(origData = .orig2, tad = 0, env = list(.rownum = 2L))
  expect_equal(.vpcFitColForData(.fit2, .orig2[2, c("ID", "TIME", "DV")], "tad", supplied = TRUE), 0)

  # a missing value and the literal text "NA" are different values
  .orig4 <- data.frame(ID = 1, TIME = 5, DV = 2, GROUP = NA_character_, EVID = 0)
  .fit4 <- list(origData = .orig4, tad = 5, env = list(.rownum = 1L))
  .sub4 <- .orig4
  .sub4$GROUP <- "NA"
  expect_warning(
    .r <- .vpcFitColForData(.fit4, .sub4, "tad", supplied = TRUE),
    "1 observation\\(s\\) in 'data' do not match"
  )
  expect_true(is.na(.r))
  expect_equal(.vpcFitColForData(.fit4, .orig4, "tad", supplied = TRUE), 5)

  # numbers are compared at full precision (not the 15 digits of
  # as.character()), while numbers given as text still match
  .orig6 <- data.frame(ID = 1e15, TIME = 0.1 + 0.2, DV = 2, EVID = 0)
  .fit6 <- list(origData = .orig6, tad = 5, env = list(.rownum = 1L))
  .sub6 <- .orig6
  .sub6$ID <- 1e15 + 1
  expect_warning(
    .r <- .vpcFitColForData(.fit6, .sub6, "tad", supplied = TRUE),
    "1 observation\\(s\\) in 'data' do not match"
  )
  expect_true(is.na(.r))
  .sub6 <- .orig6
  .sub6$TIME <- 0.3
  expect_warning(.vpcFitColForData(.fit6, .sub6, "tad", supplied = TRUE), "1 observation\\(s\\) in 'data' do not match")
  .sub6 <- .orig6
  .sub6$ID <- factor("1e15")
  .sub6$DV <- "2"
  expect_equal(.vpcFitColForData(.fit6, .sub6, "tad", supplied = TRUE), 5)

  # -0 is 0, and the same text in another encoding is the same text
  .orig7 <- data.frame(ID = 1, TIME = 0, DV = 2, GROUP = "\u00e9", EVID = 0)
  .fit7 <- list(origData = .orig7, tad = 5, env = list(.rownum = 1L))
  .sub7 <- .orig7
  .sub7$TIME <- -0
  .sub7$GROUP <- iconv(.sub7$GROUP, "UTF-8", "latin1")
  expect_equal(.vpcFitColForData(.fit7, .sub7, "tad", supplied = TRUE), 5)

  # records vpcPlot() drops (mdv = 1, even with evid = 0) do not warn
  .sub5 <- data.frame(ID = 1, TIME = 7, DV = 0, EVID = 0, MDV = 1)
  expect_warning(.r <- .vpcFitColForData(.fit4, .sub5, "tad", supplied = TRUE), NA)
  expect_true(is.na(.r))

  # duplicated supplied rows each get the fitted value
  expect_equal(.vpcFitColForData(.fit, .orig[c(4, 4), ], "tad", supplied = TRUE), c(6, 6))

  # a new observation that only looks like a dose row still warns
  .new <- data.frame(ID = 1, TIME = 0, DV = 0)
  .orig3 <- data.frame(ID = 1, TIME = c(0, 1), DV = c(0, 2), EVID = c(1, 0))
  .fit3 <- list(origData = .orig3, tad = 1, env = list(.rownum = 2L))
  expect_warning(
    .r <- .vpcFitColForData(.fit3, .new, "tad", supplied = TRUE),
    "1 observation\\(s\\) in 'data' do not match"
  )
  expect_true(is.na(.r))
})
