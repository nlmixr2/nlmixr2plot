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
  # a negative cmt turns a compartment off; it is not a dose
  d$CMT <- c(1, 1, 3, -2)
  d$EVID <- c(1, 0, 1, 1)
  d$AMT <- c(100, 0, 50, 50)
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
    expect_false(.mdSegmentCrosses(
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
      d/dt(center) = ktr*depot - cl*center/v - q*center/v + q*periph/vp
    } else {
      d/dt(center) = ktr*depot - cl*center/v - q*center/v + q*periph/vp - cl2*center
    }
    d/dt(periph) = q*center/v - q*periph/vp
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "center", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "center", "periph", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "periph", "center", "transfer")), 1L)
  e <- .edge(g, "center", NA, "elimination")
  expect_equal(nrow(e), 1L)
  expect_equal(e$label, "cl * center/v + ifelse(sex == 1, 0, cl2 * center)")
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
    expect_false(.mdSegmentCrosses(
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
  expect_equal(e$label,
               "ifelse(sex == 1, cl1 * center, 0) + ifelse(sex == 1, 0, cl2 * center)")
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
  expect_equal(.mdTerms(-0.5)[[1]]$sign, -1)
  t <- .mdTerms(as.call(list(quote(`*`), -0.5, quote(center))))
  expect_equal(t[[1]]$sign, -1)
})

test_that("conditions on compartment amounts are dependencies", {
  m <- rxode2::rxode2({
    d/dt(center) = -cl*center
    if (center > 100) {
      tox_rate = k1
    } else {
      tox_rate = 0
    }
    d/dt(tox) = tox_rate - kout*tox
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "center", "tox", "interaction")), 1L)
})

test_that("a reused variable name is not mistaken for a transfer", {
  m <- rxode2::rxode2({
    flux = cl/v*central
    d/dt(central) = -flux
    flux = q/v2*periph
    d/dt(periph) = flux - k21*periph
  })
  g <- modelGraph(m)
  expect_equal(sum(g$edges$type == "transfer"), 0L)
  expect_equal(nrow(.edge(g, "central", NA, "elimination")), 1L)
})

test_that("factor evid/amt columns use their labels", {
  d <- data.frame(ID = 1, TIME = 0:2, AMT = factor(c("100", "0", "0")),
                  EVID = factor(c("1", "0", "0")),
                  CMT = factor(c("gut", "center", "center")), DV = 0)
  g <- suppressMessages(modelGraph(.pkTurnover, data = d))
  expect_equal(g$nodes$name[g$nodes$dosing], "gut")
})

test_that("an effect compartment with several drivers is placed clear of all arrows", {
  m <- rxode2::rxode2({
    d/dt(central) = -cl*central - q*central + q*peri
    d/dt(peri) = q*central - q*peri
    d/dt(eff) = kin*central - kout*eff*peri
  })
  g <- modelGraph(m)
  n <- g$nodes
  rownames(n) <- n$name
  e <- g$edges[g$edges$type == "interaction", ]
  expect_equal(sort(e$from), c("central", "peri"))
  for (.i in seq_len(nrow(e))) {
    .others <- setdiff(n$name, c(e$from[.i], e$to[.i]))
    expect_false(.mdSegmentCrosses(
      n[e$from[.i], "x"], n[e$from[.i], "y"], n[e$to[.i], "x"], n[e$to[.i], "y"],
      n[.others, "x"], n[.others, "y"]), label = e$from[.i])
  }
})

test_that("ggplot2 arrows between the same compartments do not overlap", {
  m <- rxode2::rxode2({
    d/dt(C) = -cl*C
    d/dt(eff) = kin*C - kout*eff*C
  })
  g <- modelGraph(m)
  e <- .edge(g, "C", "eff", "interaction")
  expect_equal(sort(e$sign), c(-1, 1))
  p <- modelDiagram(g, engine = "ggplot2")
  seg <- p$layers[[which(vapply(p$layers, function(l) {
    inherits(l$geom, "GeomSegment")
  }, logical(1)))]]$data
  seg <- seg[seg$flow != "mass transfer", ]
  expect_equal(nrow(seg), 2L)
  expect_false(isTRUE(all.equal(seg$y[1], seg$y[2])))
})

test_that("direction is seen through monotone functions, ifelse and Hill terms", {
  m <- rxode2::rxode2({
    d/dt(center) = -cl*center
    d/dt(pd1) = kin*exp(-k*center) - kout*pd1
    d/dt(pd2) = kin*(1 + emax*center^g/(ec50^g + center^g)) - kout*pd2
    d/dt(pd3) = kin*(1 + sin(center)) - kout*pd3
  })
  g <- modelGraph(m, dosing = "center")
  expect_equal(.edge(g, "center", "pd1", "interaction")$sign, -1)
  expect_equal(.edge(g, "center", "pd2", "interaction")$sign, 1)
  # undetermined direction
  expect_equal(.edge(g, "center", "pd3", "interaction")$sign, 0)
  dot <- modelDiagram(g, engine = "dot")
  expect_match(dot, "arrowhead = dot", fixed = TRUE)
  expect_match(dot, "arrowhead = tee", fixed = TRUE)
  expect_s3_class(modelDiagram(g, engine = "ggplot2"), "ggplot")
  m <- rxode2::rxode2({
    d/dt(center) = ifelse(t < 12, -k1, -k2)*center
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "center", NA, "elimination")), 1L)
  expect_equal(sum(g$edges$type == "input"), 0L)
})

test_that("PD inputs go above, outputs below and exchange compartments right", {
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    d/dt(center) = ka*depot - cl*center - q*center + q*periph
    d/dt(periph) = q*center - q*periph
    d/dt(resp) = kin - kout*(1 - center/(ec50 + center))*resp - k12*resp + k21*resp2
    d/dt(resp2) = k12*resp - k21*resp2
  })
  g <- modelGraph(m)
  n <- g$nodes
  rownames(n) <- n$name
  expect_lt(n["periph", "x"], n["center", "x"])
  expect_gt(n["resp", "x"], n["center", "x"])
  expect_gt(n["resp2", "x"], n["resp", "x"])
  ec <- .mdEdgeCoords(g)
  inp <- ec[ec$type == "input" & ec$to == "resp", ]
  expect_equal(nrow(inp), 1L)
  expect_gt(inp$y0, inp$y1)
  out <- ec[ec$type == "elimination" & ec$from %in% c("resp", "center"), ]
  expect_true(all(out$y1 < out$y0))
})

test_that("ifelse branches with several negative terms are losses", {
  m <- rxode2::rxode2({
    d/dt(central) = ifelse(time > 12, -CL1*central - Q*central, -CL2*central - Q*central)
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "central", NA, "elimination")), 1L)
  expect_equal(sum(g$edges$type == "input"), 0L)
})

test_that("transfer matching ignores the order of sums and products", {
  m <- rxode2::rxode2({
    d/dt(A) = -Vmax*A/(Km + A)
    d/dt(B) = A*Vmax/(A + Km) - kel*B
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  expect_equal(sum(g$edges$type == "interaction"), 0L)
})

test_that("ifelse flows switched off with 0 are still transfers", {
  m <- rxode2::rxode2({
    d/dt(depot) = ifelse(time < 12, -ka*depot, 0)
    d/dt(central) = ifelse(time < 12, ka*depot, 0) - cl*central
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "central", "transfer")), 1L)
  expect_equal(sum(g$edges$type %in% c("input", "interaction")), 0L)
})

test_that("compartments acting on central are placed clear of other arrows", {
  m <- rxode2::rxode2({
    d/dt(central) = -cl*central - k1*central*eff1 - k2*central*eff2
    d/dt(eff1) = kin1 - kout1*eff1
    d/dt(eff2) = kin2 - kout2*eff2
  })
  g <- modelGraph(m, dosing = "central")
  n <- g$nodes
  rownames(n) <- n$name
  e <- g$edges[g$edges$type == "interaction", ]
  expect_equal(sort(e$from), c("eff1", "eff2"))
  expect_equal(e$sign, c(-1, -1))
  for (.i in seq_len(nrow(e))) {
    .others <- setdiff(n$name, c(e$from[.i], e$to[.i]))
    expect_false(.mdSegmentCrosses(
      n[e$from[.i], "x"], n[e$from[.i], "y"], n[e$to[.i], "x"], n[e$to[.i], "y"],
      n[.others, "x"], n[.others, "y"]), label = e$from[.i])
  }
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("ifelse with a transfer and an elimination is split into flows", {
  m <- rxode2::rxode2({
    d/dt(depot) = ifelse(t < 12, -ka*depot, 0)
    d/dt(center) = ifelse(t < 12, ka*depot - cl*center, -cl*center)
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "center", "transfer")), 1L)
  e <- .edge(g, "center", NA, "elimination")
  expect_equal(nrow(e), 1L)
  expect_equal(e$label, "cl * center")
  expect_equal(sum(g$edges$type == "interaction"), 0L)
})

test_that("negated symbolic exponents are inhibition", {
  m <- rxode2::rxode2({
    d/dt(C) = -cl*C
    d/dt(effect) = kin*C^(-gamma) - kout*effect
  })
  g <- modelGraph(m, dosing = "C")
  expect_equal(.edge(g, "C", "effect", "interaction")$sign, -1)
})

test_that("exchange partners of effect compartments stay off interaction arrows", {
  m <- rxode2::rxode2({
    d/dt(C) = -cl*C
    d/dt(eff1) = kin - kout*eff1*C - k12*eff1 + k21*eff2
    d/dt(eff2) = k12*eff1 - k21*eff2*C
  })
  g <- modelGraph(m, dosing = "C")
  n <- g$nodes
  rownames(n) <- n$name
  e <- g$edges[g$edges$type == "interaction", ]
  expect_true(any(e$to == "eff2"))
  for (.i in seq_len(nrow(e))) {
    .others <- setdiff(n$name, c(e$from[.i], e$to[.i]))
    expect_false(.mdSegmentCrosses(
      n[e$from[.i], "x"], n[e$from[.i], "y"], n[e$to[.i], "x"], n[e$to[.i], "y"],
      n[.others, "x"], n[.others, "y"]), label = paste(e$from[.i], e$to[.i]))
  }
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("equations inside if blocks keep their condition", {
  m <- rxode2::rxode2({
    d/dt(center) = -cl*center
    if (center > 100) {
      d/dt(tox) = k1 - kout*tox
    } else {
      d/dt(tox) = -kout*tox
    }
  })
  g <- modelGraph(m, dosing = "center")
  expect_equal(nrow(.edge(g, "center", "tox", "interaction")), 1L)
  expect_equal(nrow(.edge(g, NA, "tox", "input")), 0L)
  expect_equal(nrow(.edge(g, "tox", NA, "elimination")), 1L)
  # an unconditional loss is not matched with a conditional gain
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    if (t < 12) {
      d/dt(central) = ka*depot - cl*central
    } else {
      d/dt(central) = -cl*central
    }
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "depot", "central", "transfer")), 0L)
  expect_equal(nrow(.edge(g, "depot", NA, "elimination")), 1L)
  expect_equal(nrow(.edge(g, "depot", "central", "interaction")), 1L)
})

test_that("mass transfer arrows do not cross compartments", {
  m <- rxode2::rxode2({
    d/dt(central) = -k1*central - k2*central - k3*central
    d/dt(m1) = k1*central + k*m3 - kel1*m1
    d/dt(m2) = k2*central - kel2*m2
    d/dt(m3) = k3*central - k*m3
  })
  g <- modelGraph(m, dosing = "central")
  n <- g$nodes
  rownames(n) <- n$name
  e <- g$edges[!is.na(g$edges$from) & !is.na(g$edges$to), ]
  for (.i in seq_len(nrow(e))) {
    .others <- setdiff(n$name, c(e$from[.i], e$to[.i]))
    expect_false(.mdSegmentCrosses(
      n[e$from[.i], "x"], n[e$from[.i], "y"], n[e$to[.i], "x"], n[e$to[.i], "y"],
      n[.others, "x"], n[.others, "y"]), label = paste(e$from[.i], e$to[.i]))
  }
  expect_equal(nrow(unique(n[, c("x", "y")])), nrow(n))
})

test_that("identical externally driven rates pair one-to-one", {
  m <- rxode2::rxode2({
    d/dt(A) = -k*E
    d/dt(B) = k*E
    d/dt(C) = -k*E
    d/dt(D) = k*E
    d/dt(E) = kin - kout*E
  })
  g <- modelGraph(m)
  tr <- g$edges[g$edges$type == "transfer", ]
  expect_equal(nrow(tr), 2L)
  expect_equal(sort(paste(tr$from, tr$to)), c("A B", "C D"))
})

test_that("scaled transfer is an elimination plus an interaction", {
  m <- rxode2::rxode2({
    d/dt(A) = -k*A
    d/dt(B) = k*A*V1/V2 - kel*B
  })
  g <- modelGraph(m)
  expect_equal(sum(g$edges$type == "transfer"), 0L)
  expect_equal(nrow(.edge(g, "A", NA, "elimination")), 1L)
  e <- .edge(g, "A", "B", "interaction")
  expect_equal(nrow(e), 1L)
  expect_equal(e$sign, 1)
})

test_that("missing cmt on a dose record doses the default compartment", {
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    d/dt(central) = ka*depot - cl*central
  })
  d <- data.frame(time = 0:1, amt = c(100, 0), evid = c(1, 0),
                  cmt = c(NA_character_, "central"))
  expect_equal(modelGraph(m, data = d)$nodes$name[modelGraph(m, data = d)$nodes$dosing],
               "depot")
  d$cmt <- c(NA, 2)
  expect_equal(modelGraph(m, data = d)$nodes$name[modelGraph(m, data = d)$nodes$dosing],
               "depot")
  d <- data.frame(time = 0:2, amt = c(100, 50, 0), evid = c(1, 1, 0),
                  cmt = factor(c(NA, "central", "central")))
  g <- modelGraph(m, data = d)
  expect_equal(g$nodes$name[g$nodes$dosing], c("depot", "central"))
})

test_that("repeated identical terms keep their mass", {
  m <- rxode2::rxode2({
    d/dt(A) = -k*A - k*A
    d/dt(B) = k*A
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "A", NA, "elimination")), 1L)
})

test_that("only saturating quotients are taken as increasing", {
  m <- rxode2::rxode2({
    d/dt(C) = -k*C
    d/dt(r1) = kin*C/(1 + C)^2 - kout*r1
    d/dt(r2) = kin*emax*C/(ec50 + C) - kout*r2
    d/dt(r3) = kin*C^g/(ec50^g + C^g) - kout*r3
  })
  g <- modelGraph(m, dosing = "C")
  expect_equal(.edge(g, "C", "r1", "interaction")$sign, 0)
  expect_equal(.edge(g, "C", "r2", "interaction")$sign, 1)
  expect_equal(.edge(g, "C", "r3", "interaction")$sign, 1)
})

test_that("a dataset without dose records has no dosing compartment", {
  m <- rxode2::rxode2({
    d/dt(A) = -k*A
  })
  g <- modelGraph(m, data = data.frame(time = 0, evid = 1, amt = 0, cmt = 1))
  expect_false(any(g$nodes$dosing))
  g <- modelGraph(m, data = data.frame(time = 0:1, evid = 0, dv = 1))
  expect_false(any(g$nodes$dosing))
  # no dosing columns: rxode2's default dosing compartment
  g <- modelGraph(m, data = data.frame(time = 0:1, dv = 1))
  expect_true(g$nodes$dosing[g$nodes$name == "A"])
  expect_s3_class(modelDiagram(g, engine = "ggplot2"), "ggplot")
})

test_that("DOT column spacing grows with long compartment names", {
  m <- rxode2::rxode2({
    d/dt(central) = -q*central + q*very_long_peripheral_compartment_name - k*central
    d/dt(very_long_peripheral_compartment_name) = q*central -
      q*very_long_peripheral_compartment_name
  })
  dot <- modelDiagram(m, engine = "dot")
  pos <- regmatches(dot, regexpr("\"very_long[^\"]*\" \\[pos = \"[-0-9.]+", dot))
  x <- as.numeric(sub(".*pos = \"", "", pos))
  # the peripheral is one column left of central (at 0)
  expect_lt(x, -0.11 * nchar("very_long_peripheral_compartment_name") / 2 - 0.5)
})

test_that("branch merging pairs repeated terms one-to-one", {
  m <- rxode2::rxode2({
    d/dt(A) = ifelse(time < 12, -k*A, -k*A - k*A)
    d/dt(B) = k*A
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  e <- .edge(g, "A", NA, "elimination")
  expect_equal(nrow(e), 1L)
  expect_match(e$label, "^ifelse\\((t|time) < 12, 0, k \\* A\\)$")
  m <- rxode2::rxode2({
    if (time < 12) {
      d/dt(A) = -k*A
    } else {
      d/dt(A) = -k*A - k*A
    }
    d/dt(B) = k*A
  })
  g <- modelGraph(m)
  expect_equal(nrow(.edge(g, "A", "B", "transfer")), 1L)
  expect_equal(nrow(.edge(g, "A", NA, "elimination")), 1L)
})

test_that("a saturating quotient needs a positive constant", {
  m <- rxode2::rxode2({
    d/dt(C) = -k*C
    d/dt(resp) = C/((-1) + C) - kout*resp
  })
  g <- modelGraph(m, dosing = "C")
  expect_equal(.edge(g, "C", "resp", "interaction")$sign, 0)
})

test_that("plot() of an rxode2 ui or compiled model draws its diagram", {
  ui <- suppressMessages(rxode2::rxode2(.pkTurnover))
  expect_s3_class(ui, "rxUi")
  p <- plot(ui, engine = "ggplot2")
  expect_s3_class(p, "ggplot")
  expect_identical(plot(ui, engine = "dot"),
                   suppressMessages(modelDiagram(.pkTurnover, engine = "dot")))
  expect_match(plot(ui, engine = "dot", dosing = "center", labels = TRUE),
               "ktr * depot", fixed = TRUE)
  d <- data.frame(ID = 1, TIME = 0:1, AMT = c(100, 0), EVID = c(1, 0),
                  CMT = c("gut", "gut"), DV = 0)
  lines <- strsplit(plot(ui, engine = "dot", data = d), "\n")[[1]]
  expect_match(grep("^  \"gut\" \\[", lines, value = TRUE), "penwidth = 2", fixed = TRUE)
  m <- rxode2::rxode2({
    d/dt(depot) = -ka*depot
    d/dt(central) = ka*depot - cl*central
  })
  expect_s3_class(plot(m, engine = "ggplot2"), "ggplot")
  skip_if_not_installed("DiagrammeR")
  withr::local_options(nlmixr2plot.diagram.engine = NULL)
  expect_s3_class(plot(ui), "htmlwidget")
})

test_that("the saturating constant must be known positive", {
  m <- rxode2::rxode2({
    d/dt(C) = -k*C
    d/dt(r1) = C/((1 - 2) + C) - kout*r1
    d/dt(r2) = C/((1 + 2) + C) - kout*r2
    d/dt(r3) = C/(exp(a) + C) - kout*r3
  })
  g <- modelGraph(m, dosing = "C")
  expect_equal(.edge(g, "C", "r1", "interaction")$sign, 0)
  expect_equal(.edge(g, "C", "r2", "interaction")$sign, 1)
  expect_equal(.edge(g, "C", "r3", "interaction")$sign, 1)
})

test_that("combined labels keep repeated contributions", {
  m <- rxode2::rxode2({
    d/dt(A) = -k*A - k*A
    d/dt(B) = k*A + k*A
  })
  g <- modelGraph(m)
  e <- .edge(g, "A", "B", "transfer")
  expect_equal(nrow(e), 1L)
  expect_equal(e$label, "k * A + k * A")
  expect_equal(nrow(.edge(g, "A", NA, "elimination")), 0L)
})
