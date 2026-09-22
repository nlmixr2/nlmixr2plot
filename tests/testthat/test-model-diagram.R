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

test_that("zero-order transfer is mass transfer", {
  m <- rxode2::rxode2({
    d/dt(depot) = -rate
    d/dt(center) = rate - cl*center
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "center", "transfer")), 1L)
  expect_equal(sum(g$edges$type %in% c("input", "interaction")), 0L)
  expect_equal(nrow(.edge(g, "depot", NA, "elimination")), 0L)
})

test_that("interaction arrows do not cross other compartments", {
  m <- rxode2::rxode2({
    d/dt(center) = -cl*center
    d/dt(eff1) = kin - kout*center*eff1
    d/dt(eff2) = kin - kout*center*eff2
    d/dt(eff3) = kin - kout*center*eff3
    d/dt(eff4) = kin - kout*center*eff4
  })
  g <- modelGraph(m, dosing = "center")
  n <- g$nodes
  rownames(n) <- n$name
  for (.e in paste0("eff", 1:4)) {
    .others <- setdiff(n$name, c("center", .e))
    expect_false(nlmixr2plot:::.mdSegmentCrosses(
      n["center", "x"], n["center", "y"], n[.e, "x"], n[.e, "y"],
      n[.others, "x"], n[.others, "y"]), label = .e)
  }
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("terms inside if/else blocks are used", {
  m <- rxode2::rxode2({
    if (t > tlag) {
      ktr = ka
    } else {
      ktr = 0
    }
    d/dt(depot) = -ktr*depot
    if (sex == 1) {
      d/dt(center) = ktr*depot - cl*center/v
    } else {
      d/dt(center) = ktr*depot - cl*center/v - q*center/v + q*periph/vp
    }
    d/dt(periph) = q*center/v - q*periph/vp
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "center", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "center", "periph", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "periph", "center", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "center", NA, "elimination")), 1L)
  expect_equal(g$nodes$role[g$nodes$name == "periph"], "peripheral")
})

test_that("interaction direction accounts for denominators and intermediate variables", {
  m <- rxode2::rxode2({
    d/dt(center) = -cl*center
    d/dt(e1) = kin/(1 + imax*center) - kout*e1
    d/dt(e2) = kin*(1 + emax*center/(ec50 + center)) - kout*e2
  })
  g <- modelGraph(m, dosing = "center")
  expect_equal(.edge(g, "center", "e1", "interaction")$sign, -1)
  expect_equal(.edge(g, "center", "e2", "interaction")$sign, 1)
  # inhibition written through an intermediate variable
  g <- suppressMessages(modelGraph(.pkTurnover))
  e <- .edge(g, "center", "effect", "interaction")
  expect_equal(nrow(e), 1L)
  expect_equal(e$sign, -1)
  expect_equal(nrow(.edge(g, NA, "effect", "input")), 1L)
})

test_that("parallel and externally driven transfers are mass transfer", {
  m <- rxode2::rxode2({
    d/dt(A) = -k1*A - k2*A
    d/dt(B) = k1*A + k2*A - kel*B
  })
  g <- modelGraph(m)
  e <- .edge(g, "A", "B", "transfer")
  expect_equal(nrow(e), 1L)
  expect_match(e$label, "k1 * A", fixed = TRUE)
  expect_match(e$label, "k2 * A", fixed = TRUE)
  expect_equal(nrow(.edge(g, "A", NA, "elimination")), 0L)
  expect_equal(sum(g$edges$type == "interaction"), 0L)
  m <- rxode2::rxode2({
    d/dt(A) = -Vmax*E
    d/dt(B) = Vmax*E
    d/dt(E) = kin - kout*E
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  # the driving compartment is connected to the transfer's destination
  e <- .edge(g, "E", "B", "interaction")
  expect_equal(nrow(e), 1L)
  expect_equal(e$sign, 1)
  expect_equal(nrow(.edge(g, "E", "A", "interaction")), 0L)
  m <- rxode2::rxode2({
    d/dt(A) = -Vmax*E*A/(Km + A)
    d/dt(B) = Vmax*E*A/(Km + A) - kel*B
    d/dt(E) = kin - kout*E
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "E", "B", "interaction")), 1L)
  expect_equal(nrow(.edge(g, "A", "B", "interaction")), 0L)
})

test_that("no interaction arrow crosses a compartment in the final layout", {
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    d/dt(center) = ka*depot - cl*center - kmet1*center - kmet2*center
    d/dt(met1) = kmet1*center - kel1*met1
    d/dt(met2) = kmet2*center - kel2*met2
    d/dt(eff1) = kin - kout*eff1*center - k12*eff1 + k21*eff2
    d/dt(eff2) = k12*eff1 - k21*eff2
    d/dt(ce) = ke0*(center - ce)
  })
  g <- modelGraph(m)
  n <- g$nodes
  rownames(n) <- n$name
  e <- g$edges[g$edges$type == "interaction", ]
  expect_gt(nrow(e), 0L)
  for (.i in seq_len(nrow(e))) {
    .others <- setdiff(n$name, c(e$from[.i], e$to[.i]))
    expect_false(nlmixr2plot:::.mdSegmentCrosses(
      n[e$from[.i], "x"], n[e$from[.i], "y"], n[e$to[.i], "x"], n[e$to[.i], "y"],
      n[.others, "x"], n[.others, "y"]), label = paste(e$from[.i], e$to[.i]))
  }
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("numeric cmt follows rxode2's compartment order", {
  m <- rxode2::rxode2({
    cmt(center)
    d/dt(depot) = -ka*depot
    d/dt(center) = ka*depot - cl*center
    d/dt(blood) = 0
  })
  expect_equal(rxode2::rxModelVars(m)$state[1:2], c("center", "depot"))
  d <- data.frame(ID = 1, TIME = 0:1, AMT = c(100, 0), EVID = c(1, 0),
                  CMT = c(2, 2), DV = 0)
  g <- modelGraph(m, data = d)
  expect_equal(g$nodes$name[g$nodes$dosing], "depot")
  d$CMT <- 1
  g <- modelGraph(m, data = d)
  expect_equal(g$nodes$name[g$nodes$dosing], "center")
  # `d/dt(blood) = 0` is a compartment without flows
  expect_false(any(g$edges$to %in% "blood" | g$edges$from %in% "blood"))
  expect_true("blood" %in% g$nodes$name)
})

test_that("identical terms in if/else branches are one flow", {
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    if (sex == 1) {
      d/dt(center) = ka*depot - cl1*center
    } else {
      d/dt(center) = ka*depot - cl2*center
    }
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "center", "transfer")), 1L)
  expect_equal(sum(g$edges$type == "interaction"), 0L)
  e <- .edge(g, "center", NA, "elimination")
  expect_equal(nrow(e), 1L)
  expect_equal(e$label, "cl1 * center + cl2 * center")
})

test_that("transit chains stack above central; Michaelis-Menten is elimination", {
  m <- rxode2::rxode2({
    d/dt(depot) = -ktr*depot
    d/dt(transit1) = ktr*depot - ktr*transit1
    d/dt(transit2) = ktr*transit1 - ktr*transit2
    d/dt(center) = ktr*transit2 - vmax*center/(km + center)
  })
  g <- modelGraph(m)
  n <- g$nodes
  rownames(n) <- n$name
  expect_equal(n[c("transit1", "transit2"), "role"], c("transit", "transit"))
  expect_equal(n[c("depot", "transit1", "transit2", "center"), "y"], c(3, 2, 1, 0))
  expect_equal(n[c("depot", "transit1", "transit2"), "x"], c(0, 0, 0))
  expect_equal(nrow(.edge(g, "center", NA, "elimination")), 1L)
  expect_equal(sum(g$edges$type == "interaction"), 0L)
})

test_that("DOT labels of bidirectional transfers use escaped line breaks", {
  m <- rxode2::rxode2({
    d/dt(c1) = -k1*c1 + k2*p1
    d/dt(p1) = k1*c1 - k2*p1
  })
  dot <- modelDiagram(m, engine = "dot", labels = TRUE)
  expect_match(dot, "k1 * c1\\nk2 * p1", fixed = TRUE)
  expect_false(grepl("\r", dot, fixed = TRUE))
  expect_false(any(grepl("^[^\"]*\"[^\"]*$", strsplit(dot, "\n")[[1]])))
})

test_that("variables used in residual error lines are still substituted", {
  f <- function() {
    ini({
      emax <- 0.5
      ec50 <- 1
      kin <- 1
      kout <- 0.1
      k <- 0.1
      sd <- 0.1
    })
    model({
      EFF <- 1 - emax * center / (ec50 + center)
      d/dt(center) <- -k * center
      d/dt(resp) <- kin * EFF - kout * resp
      EFF ~ add(sd)
    })
  }
  g <- suppressMessages(modelGraph(f, dosing = "center"))
  e <- .edge(g, "center", "resp", "interaction")
  expect_equal(nrow(e), 1L)
  expect_equal(e$sign, -1)
})

test_that("negative numeric constants keep their sign", {
  expect_equal(nlmixr2plot:::.mdTerms(-0.5)[[1]]$sign, -1)
  t <- nlmixr2plot:::.mdTerms(as.call(list(quote(`*`), -0.5, quote(center))))
  expect_equal(t[[1]]$sign, -1)
})
