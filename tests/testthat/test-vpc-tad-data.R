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

  # observations not in the fitted data warn instead of silently being NA
  sub3 <- sub
  sub3$TIME[which(sub3$EVID == 0)[1]] <- 999
  expect_warning(.vpcUiSetupObservationData(fit, data=sub3, idv="tad"),
                 "1 observation\\(s\\) in 'data' do not match")

  # a supplied tad column is used as-is
  sub4 <- sub
  sub4$tad <- 42
  .o <- .vpcUiSetupObservationData(fit, data=sub4, idv="tad")
  expect_true(all(.o$obs$tad == 42))
})
