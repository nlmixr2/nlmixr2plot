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
        one.cmt, nlmixr2data::theo_md,
        est="focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10)
      )
  )

  .full <- .vpcUiSetupObservationData(fit, idv="tad")
  expect_equal(.full$obsCols$idv, "tad")
  expect_equal(nrow(.full$obs), nrow(fit$origData))
  expect_false(any(names(.full$obs) == "nlmixrRowNums"))

  od <- fit$origData
  .in <- od$ID %in% unique(od$ID)[5:8]
  .ref <- .full$obs$tad[.in]
  expect_true(any(!is.na(.ref)))

  sub <- od[.in, ]
  .o <- .vpcUiSetupObservationData(fit, data=sub, idv="tad", cens=TRUE)
  expect_equal(.o$obsCols$idv, "tad")
  expect_equal(.o$obs$tad, .ref)

  # reordered rows without the original row names
  sub2 <- sub[rev(seq_len(nrow(sub))), ]
  rownames(sub2) <- NULL
  .o <- .vpcUiSetupObservationData(fit, data=sub2, idv="tad")
  expect_equal(.o$obs$tad, rev(.ref))

  # column names differing only in case still match on every column
  sub5 <- sub
  names(sub5)[names(sub5) == "TIME"] <- "time"
  .o <- .vpcUiSetupObservationData(fit, data=sub5, idv="tad")
  expect_equal(.o$obs$tad, .ref)

  # without a shared time column, rows cannot be matched reliably
  sub6 <- sub
  names(sub6)[names(sub6) == "TIME"] <- "hours"
  expect_warning(.o <- .vpcUiSetupObservationData(fit, data=sub6, idv="tad"),
                 "do not match the fitted data")
  expect_true(all(is.na(.o$obs$tad)))

  if (requireNamespace("data.table", quietly=TRUE)) {
    .o <- .vpcUiSetupObservationData(fit, data=data.table::as.data.table(sub),
                                     idv="tad")
    expect_equal(.o$obs$tad, .ref)
  }

  # observations not in the fitted data warn instead of silently being NA
  sub3 <- sub
  sub3$TIME[which(sub3$EVID == 0)[1]] <- 999
  expect_warning(.vpcUiSetupObservationData(fit, data=sub3, idv="tad"),
                 "1 observation\\(s\\) in 'data' do not match")

  # without evid/mdv (and amt), observations still match the fitted rows
  # rather than an identical-looking dose row, and unmatched rows warn
  sub7 <- sub[sub$EVID == 0, setdiff(names(sub), c("EVID", "AMT"))]
  .o <- .vpcUiSetupObservationData(fit, data=sub7, idv="tad")
  expect_equal(.o$obs$tad, .ref[sub$EVID == 0])
  sub7$TIME[1] <- 999
  expect_warning(.vpcUiSetupObservationData(fit, data=sub7, idv="tad"),
                 "1 observation\\(s\\) in 'data' do not match")

  # numbers supplied as text still match
  sub8 <- sub
  sub8$TIME <- as.character(sub8$TIME)
  .o <- .vpcUiSetupObservationData(fit, data=sub8, idv="tad")
  expect_equal(.o$obs$tad, .ref)

  # a supplied tad column is used as-is
  sub4 <- sub
  sub4$tad <- 42
  .o <- .vpcUiSetupObservationData(fit, data=sub4, idv="tad")
  expect_true(all(.o$obs$tad == 42))
})

test_that(".vpcFitColForData does not guess between identical-looking fitted rows (#60)", {
  local_mocked_bindings(vpcNameDataCmts=function(object, data) data,
                        .package="nlmixr2est")
  # two observations at the same time in different compartments with
  # different tad; row 1 is a dose
  .orig <- data.frame(ID=1, TIME=c(0, 5, 5, 6), DV=c(NA, 0, 0, 1),
                      CMT=c(1, 2, 3, 2), EVID=c(1, 0, 0, 0))
  .fit <- list(origData=.orig, tad=c(5, 2, 6), env=list(.rownum=2:4))

  # with cmt kept each row gets its own tad
  expect_equal(.vpcFitColForData(.fit, .orig[4:2, ], "tad", supplied=TRUE),
               c(6, 2, 5))

  # without cmt the two time-5 rows cannot be told apart
  .sub <- .orig[2:4, c("ID", "TIME", "DV", "EVID")]
  expect_warning(.r <- .vpcFitColForData(.fit, .sub, "tad", supplied=TRUE),
                 "2 observation\\(s\\) in 'data' match several fitted rows")
  expect_equal(.r, c(NA, NA, 6))

  # identical-looking rows with the same value are not ambiguous
  .fit$tad <- c(5, 5, 6)
  expect_warning(.r <- .vpcFitColForData(.fit, .sub, "tad", supplied=TRUE), NA)
  expect_equal(.r, c(5, 5, 6))

  # an observation never picks up an identical-looking (unfitted) dose row
  .orig2 <- data.frame(ID=1, TIME=c(0, 0), DV=c(0, 0), EVID=c(1, 0))
  .fit2 <- list(origData=.orig2, tad=0, env=list(.rownum=2L))
  expect_equal(.vpcFitColForData(.fit2, .orig2[2, c("ID", "TIME", "DV")],
                                 "tad", supplied=TRUE), 0)
})
