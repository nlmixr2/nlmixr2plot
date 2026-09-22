.pkTurnover <- function() {
  ini({
    tktr <- log(1)
    tka <- log(1)
    tcl <- log(0.1)
    tv <- log(10)
    poplogit <- 2
    tec50 <- log(0.5)
    tkout <- log(0.05)
    te0 <- log(100)
    prop.err <- 0.1
    pkadd.err <- 0.1
    pdadd.err <- 10
  })
  model({
    ktr <- exp(tktr)
    ka <- exp(tka)
    cl <- exp(tcl)
    v <- exp(tv)
    emax <- expit(poplogit)
    ec50 <- exp(tec50)
    kout <- exp(tkout)
    e0 <- exp(te0)
    DCP <- center / v
    PD <- 1 - emax * DCP / (ec50 + DCP)
    effect(0) <- e0
    kin <- e0 * kout
    d/dt(depot) <- -ktr * depot
    d/dt(gut) <- ktr * depot - ka * gut
    d/dt(center) <- ka * gut - cl / v * center
    d/dt(effect) <- kin * PD - kout * effect
    cp <- center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ add(pdadd.err)
  })
}

.edge <- function(g, from, to, type) {
  .e <- g$edges
  .f <- if (all(is.na(from))) is.na(.e$from) else .e$from %in% from
  .t <- if (all(is.na(to))) is.na(.e$to) else .e$to %in% to
  .e[.f & .t & .e$type == type, , drop = FALSE]
}

test_that("modelGraph classifies transfer, elimination and PD interaction", {
  g <- suppressMessages(modelGraph(.pkTurnover))
  expect_s3_class(g, "nlmixr2ModelGraph")
  expect_equal(g$nodes$name, c("depot", "gut", "center", "effect"))
  expect_equal(nrow(.edge(g, "depot", "gut", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "gut", "center", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "center", NA, "elimination")), 1L)
  expect_equal(nrow(.edge(g, "effect", NA, "elimination")), 1L)
  expect_equal(nrow(.edge(g, "center", "effect", "interaction")), 1L)
  expect_false(any(g$edges$type == "transfer" & g$edges$to == "effect",
                   na.rm = TRUE))
  n <- g$nodes
  rownames(n) <- n$name
  expect_equal(n["depot", "role"], "dosing")
  expect_true(n["depot", "dosing"])
  expect_equal(n["center", "role"], "central")
  expect_equal(n["effect", "role"], "effect")
  # dosing/absorption above central, PD to the right
  expect_gt(n["depot", "y"], n["gut", "y"])
  expect_gt(n["gut", "y"], n["center", "y"])
  expect_gt(n["effect", "x"], n["center", "x"])
  expect_output(print(g), "interaction")
})

test_that("modelGraph handles peripherals, metabolites, effect compartments and inputs", {
  m <- rxode2::rxode2({
    C2 = centr/V2
    C3 = peri/V3
    C4 = peri2/V4
    d/dt(depot) = -KA*depot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3 - (Q2*C2 - Q2*C4) - centr*kmet
    d/dt(peri) = Q*C2 - Q*C3
    d/dt(peri2) = Q2*C2 - Q2*C4
    d/dt(met) = kmet*centr - kelm*met
    d/dt(eff) = Kin - Kout*(1-C2/(EC50+C2))*eff
    d/dt(ce) = ke0*(C2 - ce)
  })
  g <- modelGraph(m)
  n <- g$nodes
  rownames(n) <- n$name
  expect_equal(n["centr", "role"], "central")
  expect_equal(n[c("peri", "peri2"), "role"], c("peripheral", "peripheral"))
  expect_equal(n["met", "role"], "metabolite")
  expect_equal(n[c("eff", "ce"), "role"], c("effect", "effect"))
  # peripherals left, metabolite below, PD right
  expect_true(all(n[c("peri", "peri2"), "x"] < n["centr", "x"]))
  expect_lt(n["met", "y"], n["centr", "y"])
  expect_true(all(n[c("eff", "ce"), "x"] > n["centr", "x"]))
  # factor order in products does not matter for matching transfers
  expect_equal(nrow(.edge(g, "centr", "met", "transfer")), 1L)
  tr <- g$edges[g$edges$type == "transfer", ]
  expect_true(all(tr$bidirectional[tr$from %in% c("peri", "peri2") |
                                     tr$to %in% c("peri", "peri2")]))
  expect_false(any(tr$bidirectional[tr$to == "met"]))
  expect_equal(nrow(.edge(g, NA, "eff", "input")), 1L)
  expect_equal(nrow(.edge(g, "centr", "ce", "interaction")), 1L)
  expect_equal(nrow(.edge(g, "centr", NA, "elimination")), 1L)
  # no node sits between centr and ce on the same row
  expect_false(n["ce", "y"] == n["eff", "y"] && n["ce", "x"] > n["eff", "x"])
  # compartments do not overlap
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("inhibition is an interaction with a negative sign", {
  m <- rxode2::rxode2({
    d/dt(central) = -kel*central
    d/dt(resp) = kin*(1 - imax*central/(ic50 + central)) - kout*resp
  })
  g <- modelGraph(m, dosing = "central")
  e <- .edge(g, "central", "resp", "interaction")
  expect_equal(nrow(e), 1L)
  expect_equal(e$sign, -1)
  expect_equal(nrow(.edge(g, NA, "resp", "input")), 1L)
})

test_that("dosing compartments come from data or the dosing argument", {
  d <- data.frame(ID = 1, TIME = 0:3, AMT = c(100, 0, 50, 0),
                  EVID = c(1, 0, 1, 0), CMT = c("center", "center", "gut", "center"),
                  DV = 0)
  g <- suppressMessages(modelGraph(.pkTurnover, data = d))
  expect_equal(g$nodes$name[g$nodes$dosing], c("gut", "center"))
  expect_equal(g$nodes$role[g$nodes$name == "center"], "central")
  expect_equal(g$nodes$role[g$nodes$name == "gut"], "dosing")
  d$CMT <- c(1, 1, 3, -3)
  d$EVID <- c(1, 0, 1, 1)
  g <- suppressMessages(modelGraph(.pkTurnover, data = d))
  expect_equal(g$nodes$name[g$nodes$dosing], c("depot", "center"))
  d$CMT <- NULL
  g <- suppressMessages(modelGraph(.pkTurnover, data = d))
  expect_equal(g$nodes$name[g$nodes$dosing], "depot")
  g <- suppressMessages(modelGraph(.pkTurnover, dosing = "center"))
  expect_equal(g$nodes$name[g$nodes$dosing], "center")
  expect_error(suppressMessages(modelGraph(.pkTurnover, dosing = "nope")),
               "not in the model")
  expect_error(suppressMessages(modelGraph(.pkTurnover, dosing = 1)),
               "character")
})

test_that("modelDiagram engines", {
  g <- suppressMessages(modelGraph(.pkTurnover))
  p <- modelDiagram(g, engine = "ggplot2")
  expect_s3_class(p, "ggplot")
  expect_error(print(p), NA)
  p <- modelDiagram(g, engine = "ggplot2", labels = TRUE)
  expect_error(print(p), NA)
  dot <- modelDiagram(g, engine = "dot", labels = TRUE)
  expect_type(dot, "character")
  expect_match(dot, "\"depot\" -> \"gut\"", fixed = TRUE)
  expect_match(dot, "ktr * depot", fixed = TRUE)
  expect_match(dot, "style = dashed", fixed = TRUE)
  expect_error(modelDiagram(g, engine = "ggplot2", labels = NA), "TRUE or FALSE")
  expect_error(modelDiagram(g, engine = "nope"))
  skip_if_not_installed("DiagrammeR")
  expect_s3_class(modelDiagram(g, engine = "DiagrammeR"), "htmlwidget")
  expect_s3_class(plot(g, engine = "DiagrammeR"), "htmlwidget")
  withr::local_options(nlmixr2plot.diagram.engine = "ggplot2")
  expect_s3_class(plot(g), "ggplot")
})

test_that("bidirectional transfer is drawn once in DOT", {
  m <- rxode2::rxode2({
    d/dt(central) = -k12*central + k21*periph - kel*central
    d/dt(periph) = k12*central - k21*periph
  })
  dot <- modelDiagram(m, engine = "dot")
  expect_match(dot, "dir = both", fixed = TRUE)
  expect_equal(lengths(regmatches(dot, gregexpr("\"periph\" ->|-> \"periph\"", dot))), 1L)
  expect_s3_class(modelDiagram(m, engine = "ggplot2"), "ggplot")
})

test_that("linCmt models are diagrammed through linToOde()", {
  skip_if_not("linToOde" %in% getNamespaceExports("rxode2"))
  f <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      tq <- 1
      tvp <- 1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      q <- exp(tq)
      vp <- exp(tvp)
      linCmt() ~ add(add.sd)
    })
  }
  g <- suppressMessages(modelGraph(f))
  expect_equal(g$nodes$role, c("dosing", "central", "peripheral"))
})

test_that("modelGraph errors for unsupported objects and models without ODEs", {
  expect_error(modelGraph(1), "cannot create a model diagram")
  m <- rxode2::rxode2({
    a = b + 1
  })
  expect_error(modelGraph(m), "no differential equations")
})

test_that("dosing compartments are detected from a fit's data", {
  skip_if_not_installed("nlmixr2data")
  f <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      eta.cl ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }
  d <- nlmixr2data::theo_sd
  d$CMT[d$EVID != 0] <- 2
  fit <- suppressMessages(
    nlmixr2est::nlmixr(f, d, est = "posthoc",
                       control = nlmixr2est::foceiControl(print = 0))
  )
  g <- modelGraph(fit)
  expect_equal(g$nodes$name[g$nodes$dosing], "center")
  g <- modelGraph(fit, data = nlmixr2data::theo_sd)
  expect_equal(g$nodes$name[g$nodes$dosing], "depot")
  expect_s3_class(modelDiagram(fit, engine = "ggplot2"), "ggplot")
})

test_that("binding transfers mass from both binding partners (TMDD)", {
  m <- rxode2::rxode2({
    d/dt(central) = -kel*central - kon*central*target + koff*complex
    d/dt(target) = ksyn - kdeg*target - kon*central*target + koff*complex
    d/dt(complex) = kon*target*central - koff*complex - kint*complex
  })
  g <- modelGraph(m, dosing = "central")
  expect_equal(nrow(.edge(g, "central", "complex", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "target", "complex", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "complex", c("central", "target"), "transfer")), 2L)
  expect_equal(nrow(.edge(g, NA, "target", "input")), 1L)
  expect_equal(sum(g$edges$type == "interaction"), 0L)
  expect_equal(nrow(unique(g$nodes[, c("x", "y")])), nrow(g$nodes))
  expect_s3_class(modelDiagram(g, engine = "ggplot2"), "ggplot")
})
