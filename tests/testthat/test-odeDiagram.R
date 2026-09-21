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
  expect_equal(d$nodes$label[d$nodes$type == "gain"], c("k1", "k2"))

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
