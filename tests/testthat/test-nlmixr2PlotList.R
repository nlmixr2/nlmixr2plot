test_that("newNlmixr2PlotList returns the correct class", {
  expect_type(newNlmixr2PlotList(list(A="A")), type="list")
  expect_s3_class(newNlmixr2PlotList(list(A="A")), class="nlmixr2PlotList", exact=TRUE)
})

test_that("newNlmixr2PlotList detects errors correctly", {
  expect_error(newNlmixr2PlotList("A"))
  expect_error(newNlmixr2PlotList(list("A")))
  expect_error(newNlmixr2PlotList(list(A="A", "B")))
})

test_that("test addition operator", {

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

  p2 <- pl + ggplot2::xlab("cool")
  expect_true(inherits(p2, "nlmixr2PlotList"))
  expect_true(inherits(pl, "nlmixr2PlotList"))

  expect_true(ggplot2::is_ggplot(p2[[1]]))
  expect_true(ggplot2::is_ggplot(pl[[1]]))

  expect_false(identical(p2[[1]], pl[[1]]))

  expect_error(
    +pl,
    regexp = "Unary methods are not supported"
  )
  expect_error(
    pl - 1,
    regexp = ".Generic - is not supported"
  )
})
