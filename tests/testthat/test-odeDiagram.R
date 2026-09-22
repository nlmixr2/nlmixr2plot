.odeEdgeFrom <- function(d, to) d$edges$from[d$edges$to == to]

.odeLabel <- function(d, id) d$nodes$label[match(id, d$nodes$id)]

test_that("odeDiagram decomposes a one-compartment oral model", {
  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      f(depot) <- 0.8
      alag(depot) <- 0.5
      center(0) <- 1
      dur(center) <- 2
      cp <- center / v
      cp ~ add(add.sd)
    })
  }
  d <- odeDiagram(one.cmt)
  expect_s3_class(d, "odeBlockDiagram")
  expect_equal(d$states, c("depot", "center"))
  n <- d$nodes
  e <- d$edges

  # one summing junction and one integrator per state
  expect_equal(sum(n$type == "sum"), 2L)
  expect_equal(sum(n$type == "integrator"), 2L)

  gains <- n[n$type == "gain", ]
  expect_equal(gains$label, c("ka", "ka", "cl/v"))
  expect_equal(gains$state, c("depot", "center", "center"))
  # signs at the summing junctions
  expect_equal(e$label[match(gains$id, e$from)], c("-", "+", "-"))
  # taps: depot feeds both ka gains, center feeds cl/v (feedback)
  expect_equal(e$from[match(gains$id, e$to)],
               c("state_1", "state_1", "state_2"))
  expect_equal(e$feedback[match(gains$id, e$to)], c(TRUE, FALSE, TRUE))

  # initial condition on the center integrator
  expect_equal(n$extra[n$type == "integrator"], c(NA, "center(0) = 1"))

  # dose chain for depot: dose -> lag -> F -> sum
  fid <- n$id[n$type == "bioavailability"]
  expect_equal(.odeLabel(d, fid), "F = 0.8")
  lid <- .odeEdgeFrom(d, fid)
  expect_equal(n$type[n$id == lid], "delay")
  expect_match(.odeLabel(d, lid), "tlag = 0.5", fixed = TRUE)
  expect_equal(n$type[n$id == .odeEdgeFrom(d, lid)], "dose")
  expect_true("sum_1" %in% e$to[e$from == fid])

  # dose chain for center: dose -> dur -> sum
  iid <- n$id[n$type == "infusion"]
  expect_match(.odeLabel(d, iid), "dur = 2", fixed = TRUE)
  expect_equal(n$type[n$id == .odeEdgeFrom(d, iid)], "dose")

  # output traced back to center
  oid <- n$id[n$type == "output"]
  expect_equal(.odeLabel(d, oid), "cp = center/v")
  expect_equal(.odeEdgeFrom(d, oid), "state_2")

  expect_output(print(d), "-ka")
  expect_output(print(d), "center(0) = 1", fixed = TRUE)

  dot <- as.character(d)
  expect_match(dot, "rankdir=LR", fixed = TRUE)
  expect_match(dot, "shape=triangle", fixed = TRUE)
  expect_match(dot, "shape=circle", fixed = TRUE)

  expect_equal(sum(odeDiagram(one.cmt, doses = character(0))$nodes$type == "dose"), 0L)
  expect_equal(sum(odeDiagram(one.cmt, showZeroIni = TRUE)$nodes$extra == "depot(0) = 0",
                   na.rm = TRUE), 1L)
})

test_that("odeDiagram handles nonlinear terms, inputs and intermediates", {
  pkpd <- function() {
    ini({
      kin <- 1
      kout <- 0.1
      vmax <- 1
      km <- 1
      v <- 10
      add.sd <- 0.1
    })
    model({
      cp <- center / v
      d/dt(center) <- -vmax * cp / (km + cp)
      d/dt(eff) <- kin - kout * (1 + cp) * eff
      eff(0) <- kin / kout
      eff ~ add(add.sd)
    })
  }
  d <- odeDiagram(pkpd)
  n <- d$nodes
  blocks <- n[n$type == "block", ]
  expect_equal(nrow(blocks), 2L)
  expect_equal(blocks$extra, c("cp = center/v", "cp = center/v"))
  # the effect block is fed by both eff (feedback) and center
  effBlock <- blocks$id[blocks$state == "eff"]
  expect_setequal(.odeEdgeFrom(d, effBlock), c("state_1", "state_2"))
  expect_equal(n$label[n$type == "input"], "kin")
  expect_equal(n$extra[n$type == "integrator"], c(NA, "eff(0) = kin/kout"))
})

test_that("odeDiagram converts linCmt() models", {
  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      lag(depot) <- 2
      linCmt() ~ add(add.sd)
    })
  }
  d <- odeDiagram(one.cmt)
  expect_equal(d$states, c("depot", "central"))
  expect_equal(d$nodes$label[d$nodes$type == "gain"], c("ka", "ka", "cl/v"))
  expect_equal(sum(d$nodes$type == "delay"), 1L)
})

test_that("odeDiagram errors without ODEs", {
  alg <- function() {
    ini({
      a <- 1
      add.sd <- 0.1
    })
    model({
      y <- a * t
      y ~ add(add.sd)
    })
  }
  expect_error(odeDiagram(alg), "no ODEs")
})

test_that("plot(ui) returns a DiagrammeR widget", {
  skip_if_not_installed("DiagrammeR")
  mod <- function() {
    ini({
      k <- 0.1
      add.sd <- 0.1
    })
    model({
      d/dt(x) <- -k * x
      x ~ add(add.sd)
    })
  }
  w <- plot(rxode2::rxode2(mod))
  expect_s3_class(w, "grViz")
})

test_that("odeDiagram keeps conditional derivatives and condition dependencies", {
  alt <- function() {
    ini({
      k1 <- 1
      k2 <- 2
      add.sd <- 1
    })
    model({
      if (t > 5) {
        d/dt(p) <- -k1 * p
      } else {
        d/dt(p) <- -k2 * p
      }
      p ~ add(add.sd)
    })
  }
  d <- odeDiagram(alt)
  expect_equal(d$nodes$label[d$nodes$type == "block"],
               "ifelse(t > 5, -k1 * p, -k2 * p)")
  expect_equal(sum(d$nodes$type == "gain"), 0L)

  stateCond <- function() {
    ini({
      ka <- 1
      add.sd <- 1
    })
    model({
      if (center > 5) {
        k <- 1
      } else {
        k <- 2
      }
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      center ~ add(add.sd)
    })
  }
  d <- odeDiagram(stateCond)
  n <- d$nodes
  expect_equal(n$label[n$type == "gain"], c("ka", "ka"))
  expect_equal(n$label[n$type == "block"], "k * center")
})

test_that("terms shared by both branches of an if stay outside ifelse()", {
  added <- function() {
    ini({
      k <- 1
      kin <- 1
      add.sd <- 1
    })
    model({
      d/dt(x) <- -k * x
      if (t > 5) {
        d/dt(x) <- d/dt(x) + kin
      }
      x ~ add(add.sd)
    })
  }
  d <- odeDiagram(added, doses = character(0))
  n <- d$nodes
  expect_equal(n$label[n$type == "gain"], "k")
  expect_equal(n$label[n$type == "input"], "ifelse(t > 5, kin, 0)")
  expect_equal(sum(n$type == "block"), 0L)

  stateTest <- function() {
    ini({
      k <- 1
      add.sd <- 1
    })
    model({
      d/dt(x) <- -k * x
      if (x > 2) {
        d/dt(x) <- d/dt(x) - 1
      } else {
        d/dt(x) <- d/dt(x) + 1
      }
      x ~ add(add.sd)
    })
  }
  d <- odeDiagram(stateTest, doses = character(0))
  n <- d$nodes
  # the shared -k*x stays a gain; the state-dependent test makes a block
  expect_equal(n$label[n$type == "gain"], "k")
  bid <- n$id[n$type == "block"]
  expect_equal(n$label[n$id == bid], "ifelse(x > 2, -1, 1)")
  expect_equal(.odeEdgeFrom(d, bid), "state_1")

  # shared terms are matched as a multiset
  rhs <- .odeIfelse(quote(t > 5), quote(-k * x - k * x + a), quote(-k * x + b))
  expect_equal(.odeDeparse(rhs), "-(k * x) + ifelse(t > 5, -(k * x) + a, b)")
  vals <- list(k = 2, x = 3, a = 5, b = 7)
  expect_equal(eval(rhs, c(vals, t = 6)), -2 * 2 * 3 + 5)
  expect_equal(eval(rhs, c(vals, t = 0)), -2 * 3 + 7)
})

test_that("identical branches and accumulation without a prior", {
  same <- function() {
    ini({
      k <- 1
      add.sd <- 1
    })
    model({
      d/dt(x) <- -x
      if (x > 2) {
        d/dt(y) <- -k * y
      } else {
        d/dt(y) <- -k * y
      }
      y ~ add(add.sd)
    })
  }
  d <- odeDiagram(same, doses = character(0))
  n <- d$nodes
  expect_equal(n$label[n$type == "gain" & n$state == "y"], "k")
  expect_equal(sum(n$type == "block"), 0L)

  rhs <- .odeCollect(list(quote(d/dt(x) <- d/dt(x) + p)), "x")$ddt$x[[1]]$rhs
  expect_false(.odeHasDdt(rhs, "x"))
  expect_equal(eval(rhs, list(p = 3)), 3)
})

test_that("repeated derivative terms retain their multiplicity", {
  mod <- function() {
    ini({ k <- 1; add.sd <- 1 })
    model({
      d/dt(x) <- -k * x - k * x + 1 + 1
      x ~ add(add.sd)
    })
  }
  d <- odeDiagram(mod, doses = character(0))
  expect_equal(d$nodes$label[d$nodes$type == "gain"], c("k", "k"))
  expect_equal(d$nodes$label[d$nodes$type == "input"], c("1", "1"))
  expect_equal(d$edges$label[d$edges$to == "sum_1"], c("-", "-", "+", "+"))
})

test_that("suppressed assignments retain state dependencies", {
  mod <- function() {
    ini({ k <- 1; add.sd <- 1 })
    model({
      cp ~ x / 2
      d/dt(x) <- -k * cp
      y <- cp
      y ~ add(add.sd)
    })
  }
  d <- odeDiagram(mod, doses = character(0))
  block <- d$nodes[d$nodes$type == "block", ]
  expect_equal(block$label, "k * cp")
  expect_equal(block$extra, "cp = x/2")
  expect_equal(.odeEdgeFrom(d, block$id), "state_1")
  expect_equal(sum(d$nodes$type == "input"), 0L)
  expect_equal(.odeEdgeFrom(d, d$nodes$id[d$nodes$type == "output"]), "state_1")
})

test_that("conditional derivative updates preserve branch and fallback values", {
  collect <- function(expr) .odeCollect(as.list(expr)[-1], "x")$ddt$x[[1]]$rhs
  rhs <- collect(quote({
    d/dt(x) <- -k * x
    if (t > 5) {
      d/dt(x) <- d/dt(x) + 2
    } else {
      d/dt(x) <- d/dt(x) + 3
    }
    d/dt(x) <- d/dt(x) + 4
  }))
  expect_equal(eval(rhs, list(k = 2, x = 3, t = 6)), 0)
  expect_equal(eval(rhs, list(k = 2, x = 3, t = 0)), 1)
  rhs <- collect(quote({
    d/dt(x) <- -x
    if (t > 5) {
      if (x > 2) d/dt(x) <- 10
    }
  }))
  expect_equal(eval(rhs, list(x = 3, t = 6)), 10)
  expect_equal(eval(rhs, list(x = 1, t = 6)), -1)
  expect_equal(eval(rhs, list(x = 3, t = 0)), -3)
})

test_that("a leading unary minus becomes the summing-junction sign", {
  mm <- function() {
    ini({
      vmax <- 1
      km <- 1
      add.sd <- 1
    })
    model({
      d/dt(center) <- -vmax * center / (km + center)
      center ~ add(add.sd)
    })
  }
  d <- odeDiagram(mm)
  bid <- d$nodes$id[d$nodes$type == "block"]
  expect_equal(d$nodes$label[d$nodes$id == bid], "vmax * center/(km + center)")
  expect_equal(d$edges$label[d$edges$from == bid], "-")
})
