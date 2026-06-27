test_that("plot(augPred) returns a gglist that broadcasts ggplot2 additions", {
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("dplyr")

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

  fitOne.comp.KA.solved_S <-
    suppressMessages(nlmixr2est::nlmixr(
      One.comp.KA.solved,
      PKdata,
      est = "saem",
      nlmixr2est::saemControl(nBurn = 2,
                              nEm   = 3,
                              print = 0)
    ))

  ap <- nlmixr2est::augPred(fitOne.comp.KA.solved_S)

  pl <- plot(ap)

  # plot lists are now `gglist` objects (from ggtibble)
  expect_s3_class(pl, "gglist")
  expect_true(ggplot2::is_ggplot(pl[[1]]))

  # adding a ggplot2 component broadcasts to every plot in the list
  p2 <- pl + ggplot2::xlab("cool")
  expect_s3_class(p2, "gglist")
  expect_true(ggplot2::is_ggplot(p2[[1]]))
  expect_false(identical(p2[[1]], pl[[1]]))

  # unsupported arithmetic still errors
  expect_error(pl - 1)
})

test_that("plot(fit) returns a named, nested gglist", {
  skip_if_not_installed("nlmixr2data")

  one.compartment <- function() {
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
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }

  fit <-
    suppressMessages(nlmixr2est::nlmixr2(
      one.compartment, nlmixr2data::theo_sd, est = "saem",
      nlmixr2est::saemControl(print = 0, nBurn = 10, nEm = 20)
    ))

  p <- plot(fit)
  expect_s3_class(p, "gglist")
  # top level is a named, nested gglist
  expect_true("All Data" %in% names(p))
  expect_s3_class(p[["All Data"]], "gglist")
  expect_true(length(p[["All Data"]]) > 1)

  # as_ggtibble flattens the nesting and composes captions from the names
  gt <- ggtibble::as_ggtibble(p)
  expect_s3_class(gt, "ggtibble")
  expect_true(any(grepl("^All Data ", as.character(gt$caption))))
})
