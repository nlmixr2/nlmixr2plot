# Between-subject variability (BSV) plots (issue #51)

# Helper: the geom classes used by a ggplot's layers
.bsvGeoms <- function(p) {
  vapply(p$layers, function(.l) class(.l$geom)[1], character(1))
}

test_that("BSV plots: multi-eta fit gives QQ, correlation, and covariate plots", {
  skip_if_not_installed("nlmixr2data")

  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
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

  fit <-
    suppressMessages(nlmixr2est::nlmixr(
      one.cmt, nlmixr2data::theo_sd, est = "focei",
      control = nlmixr2est::foceiControl(print = 0, eval.max = 10)
    ))

  # --- no covariate: QQ + correlation only, nested under "All Data" ---
  p <- plot(fit)
  expect_s3_class(p, "gglist")
  expect_true("bsv" %in% names(p[["All Data"]]))

  bsv <- p[["All Data"]][["bsv"]]
  expect_s3_class(bsv, "gglist")
  expect_named(bsv, c("QQ plots", "BSV correlation"))

  expect_named(
    bsv[["QQ plots"]],
    c("QQ plot for eta.ka", "QQ plot for eta.cl", "QQ plot for eta.v")
  )
  expect_true(ggplot2::is_ggplot(bsv[["QQ plots"]][[1]]))

  expect_named(
    bsv[["BSV correlation"]],
    c("eta.ka vs eta.cl", "eta.ka vs eta.v", "eta.cl vs eta.v")
  )
  # the shared smoother adds a linear smooth
  expect_true("GeomSmooth" %in% .bsvGeoms(bsv[["BSV correlation"]][["eta.ka vs eta.cl"]]))

  # --- continuous covariate adds the covariate sub-list (smoother) ---
  pc <- plot(fit, covariate = "WT")
  bsvc <- pc[["All Data"]][["bsv"]]
  expect_named(bsvc, c("QQ plots", "BSV correlation", "BSV covariate correlation"))
  expect_named(
    bsvc[["BSV covariate correlation"]],
    c("eta.ka vs WT", "eta.cl vs WT", "eta.v vs WT")
  )
  # WT has > 5 unique values -> continuous -> smoother (not boxplot)
  expect_true("GeomSmooth" %in% .bsvGeoms(bsvc[["BSV covariate correlation"]][["eta.ka vs WT"]]))
  expect_false("GeomBoxplot" %in% .bsvGeoms(bsvc[["BSV covariate correlation"]][["eta.ka vs WT"]]))

  # --- unknown covariate name warns and produces no covariate sub-list ---
  expect_warning(p2 <- plot(fit, covariate = "NOPE"))
  expect_false("BSV covariate correlation" %in% names(p2[["All Data"]][["bsv"]]))
})

test_that("BSV plots: categorical / low-cardinality covariate uses box-and-whisker", {
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("dplyr")

  # warfarin uses a lowercase `id` column, exercising case-insensitive ID matching
  PKdata <- nlmixr2data::warfarin %>%
    dplyr::filter(dvid == "cp") %>%
    dplyr::select(-dvid) %>%
    dplyr::mutate(sex = ifelse(sex == "male", 1, 0))

  One.comp.KA.solved <- function() {
    ini({
      lka  <- log(1.15)
      lcl  <- log(0.135)
      lv   <- log(8)
      prop.err <- 0.15
      add.err  <- 0.6
      eta.ka ~ 0.5
      eta.cl ~ 0.1
      eta.v  ~ 0.1
    })
    model({
      cl <- exp(lcl + eta.cl)
      v  <- exp(lv + eta.v)
      ka <- exp(lka + eta.ka)
      linCmt() ~ prop(prop.err) + add(add.err)
    })
  }

  fit <-
    suppressMessages(nlmixr2est::nlmixr(
      One.comp.KA.solved, PKdata, est = "saem",
      nlmixr2est::saemControl(nBurn = 2, nEm = 3, print = 0)
    ))

  pc <- plot(fit, covariate = c("sex", "wt"))
  bsvc <- pc[["All Data"]][["bsv"]][["BSV covariate correlation"]]
  expect_s3_class(bsvc, "gglist")
  # eta-outer, covariate-inner ordering
  expect_named(
    bsvc,
    c("eta.ka vs sex", "eta.ka vs wt",
      "eta.cl vs sex", "eta.cl vs wt",
      "eta.v vs sex", "eta.v vs wt")
  )
  # sex has 2 unique values -> categorical -> boxplot; wt continuous -> smoother
  expect_true("GeomBoxplot" %in% .bsvGeoms(bsvc[["eta.ka vs sex"]]))
  expect_true("GeomSmooth" %in% .bsvGeoms(bsvc[["eta.ka vs wt"]]))
})

test_that("BSV plots: single-eta fit has QQ but no correlation plots", {
  poisModel <- function() {
    ini({
      tlambda <- log(2)
      eta.lambda ~ 0.1
    })
    model({
      lambda <- exp(tlambda + eta.lambda)
      DV ~ dpois(lambda)
    })
  }

  d <- data.frame(
    ID = rep(1:5, each = 4),
    TIME = rep(0:3, 5),
    DV = c(1, 2, 3, 2, 0, 1, 2, 3, 2, 1, 3, 2, 1, 2, 1, 0, 3, 2, 4, 1)
  )

  fit <-
    suppressMessages(try(
      nlmixr2est::nlmixr(
        poisModel, d, est = "focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 1, maxOuterIterations = 0)
      ),
      silent = TRUE
    ))
  skip_if(inherits(fit, "try-error"))

  bsv <- plot(fit)[["All Data"]][["bsv"]]
  expect_s3_class(bsv, "gglist")
  expect_named(bsv, "QQ plots")
  expect_named(bsv[["QQ plots"]], "QQ plot for eta.lambda")
})

test_that("BSV plots: a fit without between-subject variability has no bsv element", {
  skip_if_not_installed("nlmixr2data")

  oneCmtNoIiv <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      linCmt() ~ add(add.sd)
    })
  }

  fit <-
    suppressMessages(nlmixr2est::nlmixr(
      oneCmtNoIiv, nlmixr2data::theo_sd, est = "focei",
      control = nlmixr2est::foceiControl(print = 0, eval.max = 10)
    ))

  p <- plot(fit)
  expect_false("bsv" %in% names(p[["All Data"]]))
})
