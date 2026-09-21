#' Is this expression `d/dt(state)`?
#'
#' @param e expression
#' @return state name, or `NULL` when `e` is not a derivative
#' @noRd
.odeDdtState <- function(e) {
  if (is.call(e) && identical(e[[1]], quote(`/`)) &&
        identical(e[[2]], quote(d)) && is.call(e[[3]]) &&
        identical(e[[3]][[1]], quote(dt)) && is.name(e[[3]][[2]])) {
    return(as.character(e[[3]][[2]]))
  }
  NULL
}

#' Collect the assignments of an rxode2 model
#'
#' Walks `ui$lstExpr` (including the bodies of `if`/`else` blocks) and sorts
#' the assignments into derivatives, compartment properties and ordinary
#' left-hand-side variables.
#'
#' @param lst list of model expressions (`ui$lstExpr`)
#' @param states state names
#' @return list with `ddt` (named list per state of alternative definitions,
#'   each `list(rhs=, cond=)` where `cond` holds the variables of the enclosing
#'   `if` conditions), `prop` (named list of per-state property expressions:
#'   `f`, `alag`, `rate`, `dur`, `ini`), `lhs` (named list of lists of rhs
#'   expressions for every other variable) and `cond` (named list of the `if`
#'   condition variables each lhs variable was assigned under)
#' @noRd
.odeCollect <- function(lst, states) {
  .env <- new.env(parent = emptyenv())
  .env$ddt <- list()
  .env$prop <- list()
  .env$lhs <- list()
  .env$cond <- list()
  .propNames <- c(f = "f", F = "f", alag = "alag", lag = "alag",
                  rate = "rate", dur = "dur")
  .add <- function(e, cond = character(0)) {
    if (!is.call(e)) return(invisible())
    .op <- e[[1]]
    if (identical(.op, quote(`{`))) {
      lapply(as.list(e)[-1], .add, cond = cond)
      return(invisible())
    }
    if (identical(.op, quote(`if`))) {
      lapply(as.list(e)[-(1:2)], .add, cond = unique(c(cond, all.vars(e[[2]]))))
      return(invisible())
    }
    if (!(identical(.op, quote(`<-`)) || identical(.op, quote(`=`)))) {
      return(invisible())
    }
    .l <- e[[2]]
    .r <- e[[3]]
    .s <- .odeDdtState(.l)
    if (!is.null(.s)) {
      .alt <- .env$ddt[[.s]]
      .n <- length(.alt)
      if (.n > 0L && .odeHasDdt(.r, .s)) {
        # d/dt(x) <- d/dt(x) + ... accumulates onto the prior definition
        .alt[[.n]] <- list(rhs = .odeSubDdt(.r, .s, .alt[[.n]]$rhs),
                           cond = unique(c(.alt[[.n]]$cond, cond)))
      } else if (length(cond) > 0L) {
        # conditional definitions are alternatives; keep them all
        .alt[[.n + 1L]] <- list(rhs = .r, cond = cond)
      } else {
        .alt <- list(list(rhs = .r, cond = cond))
      }
      .env$ddt[[.s]] <- .alt
      return(invisible())
    }
    if (is.call(.l) && is.name(.l[[1]]) && length(.l) == 2L) {
      .fn <- as.character(.l[[1]])
      .arg <- .l[[2]]
      if (.fn %in% states && is.numeric(.arg) && .arg == 0) {
        .env$prop[[.fn]]$ini <- .r
        return(invisible())
      }
      if (.fn %in% names(.propNames) && is.name(.arg) &&
            as.character(.arg) %in% states) {
        .env$prop[[as.character(.arg)]][[.propNames[[.fn]]]] <- .r
        return(invisible())
      }
    }
    if (is.name(.l)) {
      .n <- as.character(.l)
      .env$lhs[[.n]] <- c(.env$lhs[[.n]], list(.r))
      .env$cond[[.n]] <- unique(c(.env$cond[[.n]], cond))
    }
    invisible()
  }
  lapply(lst, .add)
  list(ddt = .env$ddt, prop = .env$prop, lhs = .env$lhs, cond = .env$cond)
}

#' Does an expression reference `d/dt(state)`?
#' @noRd
.odeHasDdt <- function(e, state) {
  if (identical(.odeDdtState(e), state)) return(TRUE)
  is.call(e) && any(vapply(as.list(e)[-1], .odeHasDdt, logical(1), state = state))
}

#' Replace `d/dt(state)` inside an expression with its prior definition
#'
#' @param e expression
#' @param state state name
#' @param prior prior rhs for `d/dt(state)`
#' @return expression
#' @noRd
.odeSubDdt <- function(e, state, prior) {
  if (identical(.odeDdtState(e), state)) {
    return(call("(", prior))
  }
  if (is.call(e)) {
    for (i in seq_along(e)[-1]) {
      e[[i]] <- .odeSubDdt(e[[i]], state, prior)
    }
  }
  e
}

#' States a variable depends on (directly or through intermediates)
#'
#' @param vars variable names
#' @param states state names
#' @param lhs named list of lists of rhs expressions
#' @param cond named list of `if` condition variables per lhs variable
#' @return character vector of states
#' @noRd
.odeStateDeps <- function(vars, states, lhs, cond = list()) {
  .seen <- character(0)
  .out <- character(0)
  .todo <- vars
  while (length(.todo) > 0L) {
    .v <- .todo[1]
    .todo <- .todo[-1]
    if (.v %in% .seen) next
    .seen <- c(.seen, .v)
    if (.v %in% states) {
      .out <- c(.out, .v)
    } else if (!is.null(lhs[[.v]])) {
      .todo <- c(.todo, unlist(lapply(lhs[[.v]], all.vars)), cond[[.v]])
    }
  }
  states[states %in% .out]
}

#' Split a sum into signed terms
#'
#' @param e expression
#' @param sign current sign (1 or -1)
#' @return list of `list(sign=, term=)`
#' @noRd
.odeTerms <- function(e, sign = 1) {
  if (is.call(e)) {
    .op <- e[[1]]
    if (identical(.op, quote(`(`))) {
      return(.odeTerms(e[[2]], sign))
    }
    if (identical(.op, quote(`+`))) {
      if (length(e) == 2L) return(.odeTerms(e[[2]], sign))
      return(c(.odeTerms(e[[2]], sign), .odeTerms(e[[3]], sign)))
    }
    if (identical(.op, quote(`-`))) {
      if (length(e) == 2L) return(.odeTerms(e[[2]], -sign))
      return(c(.odeTerms(e[[2]], sign), .odeTerms(e[[3]], -sign)))
    }
  }
  if (is.numeric(e) && length(e) == 1L && e == 0) {
    return(list())
  }
  list(list(sign = sign, term = e))
}

#' Flatten a product/quotient into numerator and denominator factors
#'
#' @param e expression
#' @param num is `e` in the numerator?
#' @return list with `num` and `den` lists of factors plus `sign`
#' @noRd
.odeFactors <- function(e, num = TRUE) {
  if (is.call(e)) {
    .op <- e[[1]]
    if (identical(.op, quote(`(`))) {
      return(.odeFactors(e[[2]], num))
    }
    if (identical(.op, quote(`-`)) && length(e) == 2L) {
      .r <- .odeFactors(e[[2]], num)
      .r$sign <- -.r$sign
      return(.r)
    }
    if (identical(.op, quote(`*`)) || identical(.op, quote(`/`))) {
      .a <- .odeFactors(e[[2]], num)
      .b <- .odeFactors(e[[3]], if (identical(.op, quote(`*`))) num else !num)
      return(list(num = c(.a$num, .b$num), den = c(.a$den, .b$den),
                  sign = .a$sign * .b$sign))
    }
  }
  if (num) {
    list(num = list(e), den = list(), sign = 1)
  } else {
    list(num = list(), den = list(e), sign = 1)
  }
}

#' Rebuild a product from factor lists
#'
#' @param num numerator factors
#' @param den denominator factors
#' @return expression (or `NULL` when it is exactly 1)
#' @noRd
.odeProd <- function(num, den) {
  .wrap <- function(f) {
    if (is.call(f) && as.character(f[[1]]) %in% c("+", "-")) call("(", f) else f
  }
  .mul <- function(fs) {
    Reduce(function(a, b) call("*", a, .wrap(b)), lapply(fs[-1], identity),
           .wrap(fs[[1]]))
  }
  if (length(num) == 0L && length(den) == 0L) return(NULL)
  .n <- if (length(num) == 0L) 1 else .mul(num)
  if (length(den) == 0L) return(.n)
  .d <- .mul(den)
  if (length(den) > 1L) .d <- call("(", .d)
  call("/", .n, .d)
}

#' Deparse an expression to a single line
#' @noRd
.odeDeparse <- function(e) {
  if (is.null(e)) return("1")
  paste(deparse(e, width.cutoff = 500L), collapse = " ")
}

#' Classify one signed term of a derivative
#'
#' @param term expression (unsigned)
#' @param states state names
#' @param lhs named list of lists of rhs expressions
#' @param cond named list of `if` condition variables per lhs variable
#' @param extra extra variables the term depends on (the conditions the
#'   derivative was defined under)
#' @return list with `type` ("input", "gain" or "block"), `from` (states),
#'   `gain` (expression, for gains) and `sign` adjustment
#' @noRd
.odeClassify <- function(term, states, lhs, cond = list(), extra = character(0)) {
  .vars <- all.vars(term)
  .from <- .odeStateDeps(c(.vars, extra), states, lhs, cond)
  if (length(.from) == 0L) {
    return(list(type = "input", from = character(0), sign = 1))
  }
  .f <- .odeFactors(term)
  .isState <- vapply(.f$num, function(x) is.name(x) &&
                       as.character(x) %in% states, logical(1))
  if (sum(.isState) == 1L &&
        length(.odeStateDeps(extra, states, lhs, cond)) == 0L) {
    .s <- as.character(.f$num[[which(.isState)]])
    .rest <- c(.f$num[!.isState], .f$den)
    .restDeps <- .odeStateDeps(unlist(lapply(.rest, all.vars)), states,
                               lhs, cond)
    if (length(.restDeps) == 0L) {
      return(list(type = "gain", from = .s, sign = .f$sign,
                  gain = .odeProd(.f$num[!.isState], .f$den)))
    }
  }
  list(type = "block", from = .from, sign = 1)
}

#' Build a block diagram of an rxode2 ODE model
#'
#' Converts the ordinary differential equations of an rxode2/nlmixr2 model
#' into a control-systems style block diagram.  Every state gets an integrator
#' (\eqn{1/s}) fed by a summing junction; the additive terms of `d/dt(state)`
#' become the inputs of that junction:
#'
#' - terms linear in a single state become gain blocks (triangles) tapped from
#'   that state's integrator output (feedback when the state is the one being
#'   integrated),
#' - terms that do not depend on any state become external inputs,
#' - any other term becomes a nonlinear function block fed by every state it
#'   depends on (including dependencies through intermediate variables).
#'
#' Dosing is drawn as an input into the dosed compartment's summing junction
#' and annotated with the model's lag time (`alag()`/`lag()`), bioavailability
#' (`f()`/`F()`) and modeled infusion rate (`rate()`) or duration (`dur()`).
#' Initial conditions (`state(0)`) are shown on the integrator and model
#' outputs (endpoints of the residual error model) are drawn on the right.
#'
#' @param x an rxode2 model function, `rxUi` object or nlmixr2 fit
#' @param doses character vector of states that receive doses.  By default
#'   the first state plus every state with a bioavailability, lag time, rate
#'   or duration.  Use `character(0)` to hide dosing.
#' @param showZeroIni show `state(0) = 0` on integrators without an explicit
#'   initial condition
#' @param ... ignored
#' @return an `odeBlockDiagram` object (a list of `nodes` and `edges`
#'   data frames); use `plot()` to render it with 'DiagrammeR' or
#'   `as.character()` to get the Graphviz DOT source
#' @export
#' @author Matthew L. Fidler
#' @examples
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- 1
#'     tv <- 3.45
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     d/dt(depot) <- -ka * depot
#'     d/dt(center) <- ka * depot - cl / v * center
#'     f(depot) <- 0.8
#'     alag(depot) <- 0.5
#'     cp <- center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' d <- odeDiagram(one.compartment)
#' d
#'
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   plot(d)
#'   # or directly from the model
#'   plot(rxode2::rxode2(one.compartment))
#' }
odeDiagram <- function(x, doses = NULL, showZeroIni = FALSE, ...) {
  if (inherits(x, "nlmixr2FitCore")) x <- x$ui
  if (is.function(x)) x <- rxode2::rxode2(x)
  .ui <- rxode2::assertRxUi(x)
  if (isTRUE(.ui$props$linCmt)) .ui <- rxode2::linToOde(.ui)
  .states <- .ui$mv0$state
  .c <- .odeCollect(.ui$lstExpr, .states)
  .states <- .states[.states %in% names(.c$ddt)]
  if (length(.states) == 0L) {
    stop("the model has no ODEs to draw as a block diagram", call. = FALSE)
  }
  .nodes <- list()
  .edges <- list()
  .node <- function(id, label, type, state = NA_character_, extra = NA_character_) {
    .nodes[[length(.nodes) + 1L]] <<- data.frame(
      id = id, label = label, type = type, state = state, extra = extra,
      stringsAsFactors = FALSE)
    id
  }
  .edge <- function(from, to, label = "", feedback = FALSE) {
    .edges[[length(.edges) + 1L]] <<- data.frame(
      from = from, to = to, label = label, feedback = feedback,
      stringsAsFactors = FALSE)
  }
  .sign <- function(s) if (s < 0) "-" else "+"
  .k <- 0L
  .id <- function(prefix) {
    .k <<- .k + 1L
    paste0(prefix, .k)
  }
  .sumId <- structure(paste0("sum_", seq_along(.states)), names = .states)
  .stateId <- structure(paste0("state_", seq_along(.states)), names = .states)
  for (.i in seq_along(.states)) {
    .s <- .states[.i]
    .node(.sumId[[.s]], "\u03a3", "sum", .s)
    .ini <- .c$prop[[.s]]$ini
    .iniLab <- if (!is.null(.ini)) {
      paste0(.s, "(0) = ", .odeDeparse(.ini))
    } else if (showZeroIni) {
      paste0(.s, "(0) = 0")
    } else {
      NA_character_
    }
    .intId <- .node(paste0("int_", .i), "1/s", "integrator", .s, .iniLab)
    .node(.stateId[[.s]], .s, "state", .s)
    .edge(.sumId[[.s]], .intId, paste0("d/dt(", .s, ")"))
    .edge(.intId, .stateId[[.s]])
  }
  for (.i in seq_along(.states)) {
    .s <- .states[.i]
    .terms <- unlist(lapply(.c$ddt[[.s]], function(a) {
      lapply(.odeTerms(a$rhs), function(t) c(t, list(cond = a$cond)))
    }), recursive = FALSE)
    .terms <- .terms[!duplicated(vapply(.terms, function(t) {
      paste(t$sign, .odeDeparse(t$term))
    }, character(1)))]
    for (.t in .terms) {
      .cl <- .odeClassify(.t$term, .states, .c$lhs, .c$cond, .t$cond)
      .sg <- .sign(.t$sign * .cl$sign)
      if (.cl$type == "input") {
        .n <- .node(.id("input_"), .odeDeparse(.t$term), "input", .s)
        .edge(.n, .sumId[[.s]], .sg)
      } else if (.cl$type == "gain") {
        .n <- .node(.id("gain_"), .odeDeparse(.cl$gain), "gain", .s)
        .fb <- match(.cl$from, .states) >= .i
        .edge(.stateId[[.cl$from]], .n, feedback = .fb)
        .edge(.n, .sumId[[.s]], .sg)
      } else {
        .vars <- all.vars(.t$term)
        .inter <- .vars[!(.vars %in% .states) & .vars %in% names(.c$lhs)]
        .inter <- .inter[vapply(.inter, function(v) {
          length(.odeStateDeps(v, .states, .c$lhs, .c$cond)) > 0L
        }, logical(1))]
        .def <- vapply(.inter, function(v) {
          paste0(v, " = ", paste(vapply(.c$lhs[[v]], .odeDeparse, character(1)),
                                 collapse = " | "))
        }, character(1))
        .n <- .node(.id("block_"), .odeDeparse(.t$term), "block", .s,
                    if (length(.def) > 0L) paste(.def, collapse = "\n") else NA_character_)
        for (.f in .cl$from) {
          .edge(.stateId[[.f]], .n, feedback = match(.f, .states) >= .i)
        }
        .edge(.n, .sumId[[.s]], .sg)
      }
    }
  }
  # dosing
  if (is.null(doses)) {
    doses <- unique(c(.states[1], names(.c$prop)[vapply(names(.c$prop), function(s) {
      any(c("f", "alag", "rate", "dur") %in% names(.c$prop[[s]]))
    }, logical(1))]))
  }
  doses <- .states[.states %in% doses]
  for (.s in doses) {
    .p <- .c$prop[[.s]]
    .prev <- .node(.id("dose_"), paste0("Dose\n", .s), "dose", .s)
    .chain <- list(
      alag = c("delay", "e^(-s\u00b7tlag)\ntlag = "),
      f = c("bioavailability", "F = "),
      rate = c("infusion", "zero-order\nrate = "),
      dur = c("infusion", "zero-order\ndur = "))
    for (.nm in names(.chain)) {
      if (is.null(.p[[.nm]])) next
      .n <- .node(.id(paste0(.nm, "_")),
                  paste0(.chain[[.nm]][2], .odeDeparse(.p[[.nm]])),
                  .chain[[.nm]][1], .s)
      .edge(.prev, .n)
      .prev <- .n
    }
    .edge(.prev, .sumId[[.s]], "+")
  }
  # outputs
  .pd <- .ui$predDf
  if (!is.null(.pd)) {
    for (.j in seq_len(nrow(.pd))) {
      .v <- as.character(.pd$var[.j])
      .from <- .odeStateDeps(.v, .states, .c$lhs, .c$cond)
      if (length(.from) == 0L) next
      .lab <- .v
      if (!(.v %in% .states) && !is.null(.c$lhs[[.v]])) {
        .lab <- paste0(.v, " = ",
                       .odeDeparse(.c$lhs[[.v]][[length(.c$lhs[[.v]])]]))
      }
      .err <- as.character(.pd$errType[.j])
      .n <- .node(.id("output_"), .lab, "output", NA_character_,
                  if (!is.na(.err)) paste0("error: ", .err) else NA_character_)
      for (.f in .from) .edge(.stateId[[.f]], .n)
    }
  }
  structure(list(nodes = do.call(rbind, .nodes),
                 edges = do.call(rbind, .edges),
                 states = .states),
            class = "odeBlockDiagram")
}

#' Escape text for a Graphviz HTML-like label
#' @noRd
.odeHtml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("\n", "<br/>", x, fixed = TRUE)
}

#' Graphviz node attributes for each node type
#' @noRd
.odeNodeStyle <- list(
  sum = 'shape=circle, width=0.4, fixedsize=true, style=filled, fillcolor="#FFFFFF", fontsize=16',
  integrator = 'shape=box, style="filled", fillcolor="#DCE8F5", color="#2F5D8A", penwidth=1.5',
  state = 'shape=plaintext, fontcolor="#2F5D8A", fontsize=14',
  gain = 'shape=triangle, orientation=270, style=filled, fillcolor="#FFF2CC", color="#B8860B", margin=0.02',
  block = 'shape=box, style="rounded,filled", fillcolor="#F3E5F5", color="#7B1FA2"',
  input = 'shape=box, style="rounded,dashed", color="#666666"',
  dose = 'shape=cds, style=filled, fillcolor="#D5F5E3", color="#1E8449"',
  delay = 'shape=box, style=filled, fillcolor="#FDEBD0", color="#CA6F1E"',
  bioavailability = 'shape=triangle, orientation=270, style=filled, fillcolor="#D5F5E3", color="#1E8449", margin=0.02',
  infusion = 'shape=box, style=filled, fillcolor="#D6EAF8", color="#1F618D"',
  output = 'shape=box, style="rounded,filled", fillcolor="#EEEEEE", color="#333333"'
)

#' @export
as.character.odeBlockDiagram <- function(x, ...) {
  .n <- x$nodes
  .lines <- vapply(seq_len(nrow(.n)), function(i) {
    .lab <- .odeHtml(.n$label[i])
    .extra <- .n$extra[i]
    if (!is.na(.extra)) {
      .lab <- paste0(.lab, '<br/><font point-size="9">',
                     .odeHtml(.extra), "</font>")
    }
    sprintf('  %s [label=<%s>, %s];', .n$id[i], .lab,
            .odeNodeStyle[[.n$type[i]]])
  }, character(1))
  .e <- x$edges
  .elines <- vapply(seq_len(nrow(.e)), function(i) {
    .attr <- character(0)
    if (nzchar(.e$label[i])) {
      .l <- .e$label[i]
      if (.l %in% c("+", "-")) {
        .attr <- c(.attr, sprintf('headlabel="%s", labelfontsize=14, labeldistance=1.5',
                                  if (.l == "-") "\u2212" else "+"))
      } else {
        .attr <- c(.attr, sprintf('label="%s", fontsize=9', .l))
      }
    }
    if (isTRUE(.e$feedback[i])) {
      .attr <- c(.attr, 'constraint=false, style=dashed, color="#555555"')
    }
    sprintf("  %s -> %s%s;", .e$from[i], .e$to[i],
            if (length(.attr) > 0L) paste0(" [", paste(.attr, collapse = ", "), "]") else "")
  }, character(1))
  # keep each compartment's sum -> integrator -> state on one row
  .ranks <- vapply(seq_along(x$states), function(i) {
    sprintf("  subgraph cluster_%d { style=invis; sum_%d; int_%d; state_%d; }",
            i, i, i, i)
  }, character(1))
  paste(c("digraph odeBlockDiagram {",
          '  graph [rankdir=LR, nodesep=0.35, ranksep=0.45, fontname="Helvetica"];',
          '  node [fontname="Helvetica", fontsize=11];',
          '  edge [fontname="Helvetica", arrowsize=0.7];',
          .lines, .ranks, .elines, "}"), collapse = "\n")
}

#' @export
print.odeBlockDiagram <- function(x, ...) {
  cat("ODE block diagram with ", length(x$states), " state(s): ",
      paste(x$states, collapse = ", "), "\n", sep = "")
  .n <- x$nodes
  for (.s in x$states) {
    .w <- .n[!is.na(.n$state) & .n$state == .s, , drop = FALSE]
    .ini <- .w$extra[.w$type == "integrator"]
    cat("  ", .s, if (!is.na(.ini)) paste0(" [", .ini, "]"), "\n", sep = "")
    for (.t in c("gain", "block", "input")) {
      .ids <- .w$id[.w$type == .t]
      .sg <- x$edges$label[match(.ids, x$edges$from)]
      .l <- paste0(ifelse(.sg == "-", "-", ""), .w$label[.w$type == .t])
      if (length(.ids) == 0L) .l <- character(0)
      if (length(.l) > 0L) cat("    ", .t, ": ", paste(.l, collapse = "; "), "\n", sep = "")
    }
    .d <- .w$label[.w$type %in% c("delay", "bioavailability", "infusion")]
    if (any(.w$type == "dose")) {
      cat("    dose", if (length(.d) > 0L) paste0(": ", paste(gsub("\n", " ", .d), collapse = "; ")),
          "\n", sep = "")
    }
  }
  .o <- .n$label[.n$type == "output"]
  if (length(.o) > 0L) cat("  outputs: ", paste(.o, collapse = "; "), "\n", sep = "")
  cat("use plot() to draw it (requires 'DiagrammeR')\n")
  invisible(x)
}

#' @rdname odeDiagram
#' @param y ignored, for the `plot()` generic
#' @export
plot.odeBlockDiagram <- function(x, y, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("'DiagrammeR' is required to draw the ODE block diagram; ",
         "install it with install.packages(\"DiagrammeR\")", call. = FALSE)
  }
  DiagrammeR::grViz(as.character(x))
}

#' Plot an rxode2 model as an ODE block diagram
#'
#' `plot()` on an rxode2 user interface object (`rxUi`) draws the model's
#' ordinary differential equations as a block diagram; see [odeDiagram()] for
#' details.
#'
#' @inheritParams odeDiagram
#' @param y ignored, for the `plot()` generic
#' @return a 'DiagrammeR' `grViz` htmlwidget
#' @export
#' @examples
#' mod <- function() {
#'   ini({
#'     kin <- 1
#'     kout <- 0.1
#'     cl <- 1
#'     v <- 10
#'     add.sd <- 0.1
#'   })
#'   model({
#'     d/dt(center) <- -cl / v * center
#'     cp <- center / v
#'     d/dt(eff) <- kin - kout * (1 + cp) * eff
#'     eff(0) <- kin / kout
#'     eff ~ add(add.sd)
#'   })
#' }
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   plot(rxode2::rxode2(mod))
#' }
plot.rxUi <- function(x, y, ...) {
  plot(odeDiagram(x, ...))
}
