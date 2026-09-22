# Automatic model diagrams (#10)
#
# The model's differential equations are parsed (not regex matched) into a
# graph of compartments (nodes) and flows (edges); the graph is then laid out
# following pharmacometric conventions and drawn with one of several engines.

#' Build a compartment graph from a model's differential equations
#'
#' The differential equations are parsed into additive terms.  A term that
#' is subtracted from one compartment and added (identically) to another is
#' mass transfer between the two compartments.  A remaining subtracted term
#' that contains the compartment's own amount is an elimination (output); a
#' remaining term that depends on other compartments is an interaction that
#' does not transport mass (for example an effect compartment or a
#' pharmacodynamic stimulation/inhibition); a remaining added term that does
#' not depend on any other compartment is a (zero-order) input.
#' Dependencies through intermediate variables (like `cp <- central/v`) are
#' followed.  A compartment that drives a transfer between two other
#' compartments (like an enzyme) is drawn as an interaction with the
#' destination.  Production or loss driven only by another compartment
#' (like `ke0*cp` in an effect compartment) is represented by the interaction
#' arrow alone, without a separate input/output arrow.
#'
#' @details
#'
#' Some limitations on how equations must be written:
#'
#' - Mass transfer is only detected when the same term (up to reordering of
#'   the factors of a product) is subtracted from the source and added to the
#'   destination, e.g. `d/dt(depot) <- -ka*depot` and
#'   `d/dt(central) <- ka*depot - ...`.  Scaled transfer (like a
#'   stoichiometric or volume conversion in only one of the equations) is
#'   shown as an elimination plus an interaction.
#'
#' - `linCmt()` models are converted to ODEs with `rxode2::linToOde()`,
#'   which requires a version of 'rxode2' that provides it.
#'
#' @param object model to diagram: a model function, an `rxode2` user
#'   interface (`rxUi`) object, a compiled `rxode2` model or a fitted
#'   `nlmixr2` object.
#' @param dosing optional character vector naming the dosing compartments.
#'   When `NULL` the dosing compartments are detected from the dosing records
#'   in `data`; when there is no data the first compartment (the default
#'   `rxode2` dosing compartment) is used.
#' @param data optional dataset used to detect the dosing compartments (from
#'   the dosing records' `cmt`).  For fitted models this defaults to the data
#'   the model was fit with.
#' @return a `nlmixr2ModelGraph` object; a list with:
#'
#' - `nodes`: data frame with the compartment `name`, its `role`
#'   (`"dosing"`, `"central"`, `"peripheral"`, `"transit"`, `"metabolite"`,
#'   `"effect"` or `"other"`), whether it is `dosing` and the layout
#'   coordinates `x` and `y`.
#'
#' - `edges`: data frame with `from`, `to` (`NA` for inputs/eliminations),
#'   `type` (`"transfer"`, `"elimination"`, `"input"` or `"interaction"`),
#'   `sign` (`1` when the term is added, `-1` when it is subtracted; for
#'   interactions `1` is stimulation, `-1` inhibition and `0` an effect whose
#'   direction cannot be determined from the equations, e.g. through
#'   `ifelse()` or a conditionally assigned variable),
#'   `bidirectional` (for transfers) and `label` (the model term(s)).
#' @export
#' @author Matthew L. Fidler
#' @family model diagrams
#' @examples
#' \donttest{
#' one.cmt <- function() {
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
#'     d/dt(central) <- ka * depot - cl / v * central
#'     cp <- central / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' modelGraph(one.cmt)
#' }
modelGraph <- function(object, dosing = NULL, data = NULL) {
  .info <- .mdModelInfo(object)
  if (is.null(data)) data <- .info$data
  .states <- .info$states
  if (length(.states) == 0L) {
    stop("the model has no differential equations to diagram", call. = FALSE)
  }
  if (is.null(dosing)) {
    dosing <- .mdDosingFromData(data, .states, .info$order)
    if (length(dosing) == 0L) dosing <- .states[1]
  } else {
    if (!is.character(dosing)) {
      stop("'dosing' must be a character vector of compartment names",
           call. = FALSE)
    }
    .bad <- setdiff(dosing, .states)
    if (length(.bad) > 0L) {
      stop("'dosing' compartment(s) not in the model: ",
           paste(.bad, collapse = ", "), call. = FALSE)
    }
  }
  .edges <- .mdClassifyTerms(.info$terms, .states)
  .nodes <- .mdLayout(.states, .edges, dosing)
  structure(list(nodes = .nodes, edges = .edges),
            class = "nlmixr2ModelGraph")
}

#' Automatic model diagram
#'
#' Draws a compartment diagram of a model from its differential equations
#' (see [modelGraph()] for how the equations are interpreted).
#'
#' The layout follows common pharmacometric conventions: dosing and
#' absorption/transit compartments are above the compartment they feed; the
#' central compartment is in the middle with the compartments it exchanges
#' mass with (peripheral compartments) to its left; unidirectional transfer
#' (e.g. to a metabolite) and eliminations go below; compartments that
#' interact with the model without mass transfer (e.g. effect compartments or
#' pharmacodynamic models) go to the right, with their own inputs above,
#' outputs below and exchange compartments further right.
#'
#' Mass transfer is drawn with solid arrows; interactions without mass
#' transfer are dashed (with a "tee" arrow head for inhibition and a "dot"
#' arrow head when the direction is undetermined with `DiagrammeR`; dotted
#' for inhibition and dot-dashed when undetermined with `ggplot2`).
#'
#' @inheritParams modelGraph
#' @param object model to diagram (see [modelGraph()]) or a
#'   `nlmixr2ModelGraph` object.
#' @param engine drawing engine: `"DiagrammeR"` (a Graphviz htmlwidget from
#'   the 'DiagrammeR' package), `"ggplot2"` (a `ggplot` object) or `"dot"`
#'   (the Graphviz DOT source as a character string, to customize or render
#'   elsewhere).  The default is `"DiagrammeR"` when that package is
#'   installed and `"ggplot2"` otherwise; it may be changed with
#'   `options(nlmixr2plot.diagram.engine = ...)`.
#' @param labels logical; when `TRUE` label the arrows with the model terms.
#' @param ... ignored.
#' @return the diagram drawn by the requested `engine`.
#' @export
#' @author Matthew L. Fidler
#' @family model diagrams
#' @examples
#' \donttest{
#' pk.turnover.emax <- function() {
#'   ini({
#'     tktr <- log(1)
#'     tka <- log(1)
#'     tcl <- log(0.1)
#'     tv <- log(10)
#'     poplogit <- 2
#'     tec50 <- log(0.5)
#'     tkout <- log(0.05)
#'     te0 <- log(100)
#'     prop.err <- 0.1
#'     pkadd.err <- 0.1
#'     pdadd.err <- 10
#'   })
#'   model({
#'     ktr <- exp(tktr)
#'     ka <- exp(tka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     emax <- expit(poplogit)
#'     ec50 <- exp(tec50)
#'     kout <- exp(tkout)
#'     e0 <- exp(te0)
#'     DCP <- center / v
#'     PD <- 1 - emax * DCP / (ec50 + DCP)
#'     effect(0) <- e0
#'     kin <- e0 * kout
#'     d/dt(depot) <- -ktr * depot
#'     d/dt(gut) <- ktr * depot - ka * gut
#'     d/dt(center) <- ka * gut - cl / v * center
#'     d/dt(effect) <- kin * PD - kout * effect
#'     cp <- center / v
#'     cp ~ prop(prop.err) + add(pkadd.err)
#'     effect ~ add(pdadd.err)
#'   })
#' }
#' modelDiagram(pk.turnover.emax, engine = "ggplot2")
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   modelDiagram(pk.turnover.emax, engine = "DiagrammeR")
#' }
#' }
modelDiagram <- function(object, dosing = NULL, data = NULL,
                         engine = getOption("nlmixr2plot.diagram.engine"),
                         labels = FALSE, ...) {
  if (inherits(object, "nlmixr2ModelGraph")) {
    .graph <- object
  } else {
    .graph <- modelGraph(object, dosing = dosing, data = data)
  }
  if (is.null(engine)) {
    engine <- if (requireNamespace("DiagrammeR", quietly = TRUE)) {
      "DiagrammeR"
    } else {
      "ggplot2"
    }
  }
  engine <- match.arg(engine, c("DiagrammeR", "ggplot2", "dot"))
  if (!(is.logical(labels) && length(labels) == 1L && !is.na(labels))) {
    stop("'labels' must be TRUE or FALSE", call. = FALSE)
  }
  switch(engine,
         DiagrammeR = .mdDiagrammeR(.graph, labels),
         ggplot2 = .mdGgplot(.graph, labels),
         dot = .mdDot(.graph, labels))
}

#' @rdname modelDiagram
#' @param x a `nlmixr2ModelGraph` object
#' @export
plot.nlmixr2ModelGraph <- function(x, ...,
                                   engine = getOption("nlmixr2plot.diagram.engine"),
                                   labels = FALSE) {
  modelDiagram(x, engine = engine, labels = labels)
}

#' @export
print.nlmixr2ModelGraph <- function(x, ...) {
  cat("nlmixr2 model graph\n\ncompartments:\n")
  print(x$nodes[, c("name", "role", "dosing")], row.names = FALSE)
  cat("\nflows:\n")
  .e <- x$edges
  if (nrow(.e) == 0L) {
    cat("  (none)\n")
  } else {
    .e$from[is.na(.e$from)] <- "(input)"
    .e$to[is.na(.e$to)] <- "(output)"
    print(.e[, c("from", "to", "type", "sign", "label")], row.names = FALSE)
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Model extraction
# ---------------------------------------------------------------------------

#' Get the model lines, states and data from a supported object
#'
#' @param object model object
#' @return list(states, terms, data)
#' @noRd
.mdModelInfo <- function(object) {
  .data <- NULL
  if (inherits(object, "nlmixr2FitCore") || inherits(object, "nlmixr2FitData")) {
    .data <- tryCatch(object$origData, error = function(e) NULL)
    object <- object$ui
  }
  if (inherits(object, "rxode2")) {
    .mv <- rxode2::rxModelVars(object)
    # braces let a normalized `}\nelse` parse
    .lines <- as.list(str2lang(paste0("{\n", .mv$model["normModel"], "\n}")))[-1]
    .order <- .mv$state
  } else {
    if (is.function(object)) object <- rxode2::rxode2(object)
    if (!inherits(object, "rxUi")) {
      stop("cannot create a model diagram from an object of class '",
           paste(class(object), collapse = "', '"), "'", call. = FALSE)
    }
    if (.mdIsLinCmt(object)) object <- .mdLinToOde(object)
    .lines <- object$lstExpr
    # residual error lines (`cp ~ add(sd)`) are not assignments
    .err <- object$predDf$line
    if (length(.err) > 0L) .lines <- .lines[-.err]
    .order <- object$mv0$state
  }
  .parsed <- .mdParseLines(.lines)
  .states <- .parsed$states
  # keep rxode2's compartment order (used to map numeric `cmt` values)
  .states <- c(intersect(.order, .states), setdiff(.states, .order))
  .deps <- .parsed$deps
  .terms <- do.call(rbind, lapply(.states, function(.s) {
    .t <- .parsed$ode[[.s]]
    if (length(.t) == 0L) return(NULL)
    data.frame(
      state = .s,
      sign = vapply(.t, function(x) x$sign, numeric(1)),
      key = vapply(.t, function(x) .mdTermKey(x$expr), character(1)),
      label = vapply(.t, function(x) {
        .mdDeparse(.mdFold(x$expr, .parsed$defs))
      }, character(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(.terms)) {
    .terms$states <- unlist(lapply(.states, function(.s) {
      lapply(.parsed$ode[[.s]], function(x) {
        .mdExprStates(x$expr, .states, .deps)
      })
    }), recursive = FALSE)
    # direction (1 increasing, -1 decreasing, NA unknown) of each term in
    # each compartment it depends on
    .terms$dir <- unlist(lapply(.states, function(.s) {
      lapply(.parsed$ode[[.s]], function(x) {
        .st <- .mdExprStates(x$expr, .states, .deps)
        stats::setNames(vapply(.st, function(o) {
          as.numeric(.mdDirection(x$expr, o, .states, .deps))
        }, numeric(1)), .st)
      })
    }), recursive = FALSE)
  }
  # rxode2's full compartment order, including compartments without ODEs
  # (e.g. from `cmt()`), maps numeric `cmt` values
  list(states = .states, terms = .terms, data = .data,
       order = c(.order, setdiff(.states, .order)))
}

#' Dosing compartments from the dosing records of a dataset
#'
#' @param data dataset (or NULL)
#' @param states compartment names that are diagrammed
#' @param order rxode2's compartment order (numeric `cmt` values index it)
#' @return character vector of dosed compartments (possibly empty)
#' @noRd
.mdDosingFromData <- function(data, states, order = states) {
  if (!is.data.frame(data) || nrow(data) == 0L) return(character(0))
  .nm <- tolower(names(data))
  .col <- function(n) {
    .w <- which(.nm == n)
    if (length(.w) == 0L) return(NULL)
    data[[.w[1]]]
  }
  .evid <- .col("evid")
  .amt <- .col("amt")
  if (is.null(.evid) && is.null(.amt)) return(character(0))
  .dose <- rep(TRUE, nrow(data))
  # factors are converted through their labels, not their level codes
  .num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  if (!is.null(.evid)) {
    .evid <- .num(.evid)
    # 0 = observation, 2 = other event, 3 = reset
    .dose <- .dose & !is.na(.evid) & !(.evid %in% c(0, 2, 3))
  }
  if (!is.null(.amt)) {
    .amt <- .num(.amt)
    .dose <- .dose & !is.na(.amt) & .amt != 0
  }
  if (!any(.dose)) return(character(0))
  .cmt <- .col("cmt")
  if (is.null(.cmt)) return(intersect(states, order[1]))
  .cmt <- .cmt[.dose]
  if (is.factor(.cmt)) .cmt <- as.character(.cmt)
  if (is.character(.cmt)) {
    .num <- suppressWarnings(as.numeric(.cmt))
    .ret <- .cmt[is.na(.num)]
    .ret[.ret == "(default)"] <- order[1]
    .cmt <- .num[!is.na(.num)]
  } else {
    .ret <- character(0)
    .cmt <- as.numeric(.cmt)
  }
  # negative compartment numbers turn compartments off; they are not doses
  .cmt <- .cmt[!is.na(.cmt) & .cmt > 0 & .cmt <= length(order)]
  .ret <- c(.ret, order[.cmt])
  intersect(states, .ret)
}

#' @noRd
.mdIsLinCmt <- function(ui) {
  any(vapply(ui$lstExpr, function(x) {
    "linCmt" %in% all.names(x)
  }, logical(1)))
}

#' @noRd
.mdLinToOde <- function(ui) {
  if (!("linToOde" %in% getNamespaceExports("rxode2"))) {
    stop("diagramming 'linCmt()' models requires a version of 'rxode2' with 'linToOde()'",
         call. = FALSE)
  }
  .fun <- getExportedValue("rxode2", "linToOde")
  .fun(ui)
}

#' Parse model lines into ODE terms and variable -> state dependencies
#'
#' Variables that are assigned once (outside of `if` blocks) and depend on a
#' compartment amount are substituted into the differential equations before
#' they are split into terms, so that the direction of an effect expressed
#' through an intermediate variable (e.g. `PD <- 1 - emax*cp/(ec50 + cp)`) is
#' seen.
#'
#' @param lines list of model expressions
#' @return list(ode = named list of term lists, deps = named list of
#'   variables to the (possibly empty) set of variables they depend on)
#' @noRd
.mdParseLines <- function(lines) {
  .env <- new.env(parent = emptyenv())
  .env$ode <- list()
  .env$deps <- list()
  .env$defs <- list()
  .env$count <- list()
  .env$inIf <- character(0)
  .env$states <- character(0)
  .isAssign <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)) ||
         identical(x[[1]], quote(`~`)))
  }
  # first pass: compartment names and how often (and where) variables are
  # assigned
  .count <- function(x, inIf) {
    if (!is.call(x)) return(invisible())
    .f <- x[[1]]
    if (identical(.f, quote(`{`))) {
      for (.i in seq_along(x)[-1]) .count(x[[.i]], inIf)
    } else if (identical(.f, quote(`if`))) {
      .count(x[[3]], TRUE)
      if (length(x) == 4L) .count(x[[4]], TRUE)
    } else if (.isAssign(x)) {
      .state <- .mdDdtState(x[[2]])
      if (!is.null(.state)) {
        .env$states <- union(.env$states, .state)
      } else if (is.name(x[[2]])) {
        .n <- as.character(x[[2]])
        .env$count[[.n]] <- (if (is.null(.env$count[[.n]])) 0 else .env$count[[.n]]) + 1
        if (inIf) .env$inIf <- union(.env$inIf, .n)
      }
    }
    invisible()
  }
  for (.l in lines) .count(.l, FALSE)
  # second pass; `cond` holds the variables of the enclosing `if` conditions
  .walk <- function(x, cond) {
    if (!is.call(x)) return(invisible())
    .f <- x[[1]]
    if (identical(.f, quote(`{`))) {
      for (.i in seq_along(x)[-1]) .walk(x[[.i]], cond)
    } else if (identical(.f, quote(`if`))) {
      .cond <- union(cond, all.vars(x[[2]]))
      .walk(x[[3]], .cond)
      if (length(x) == 4L) .walk(x[[4]], .cond)
    } else if (.isAssign(x)) {
      .lhs <- x[[2]]
      .rhs <- .mdSubstitute(x[[3]], .env$defs)
      .state <- .mdDdtState(.lhs)
      if (!is.null(.state)) {
        # the same term in several `if`/`else` branches is one flow
        .old <- .env$ode[[.state]]
        .id <- function(t) paste(t$sign, .mdTermKey(t$expr))
        .oldId <- vapply(.old, .id, character(1))
        for (.t in .mdTerms(.rhs)) {
          # a literal zero (e.g. `d/dt(x) <- 0`) is no flow
          if (is.numeric(.t$expr) && all(.t$expr == 0)) next
          if (!(.id(.t) %in% .oldId)) {
            .old <- c(.old, list(.t))
            .oldId <- c(.oldId, .id(.t))
          }
        }
        .env$ode[[.state]] <- .old
      } else if (is.name(.lhs)) {
        .n <- as.character(.lhs)
        # a conditional assignment also depends on its condition
        .env$deps[[.n]] <- union(.env$deps[[.n]], union(all.vars(x[[3]]), cond))
        if (.n %in% .env$states || .n %in% .env$inIf) {
          # values from `if` branches cannot be substituted
          .env$defs[[.n]] <- NULL
        } else if (!identical(.env$count[[.n]], 1)) {
          # a reassigned variable is substituted with its current value so
          # that a reused name (like `flux`) is not mistaken for one flow
          .env$defs[[.n]] <- .rhs
        } else if (any(all.vars(.rhs) %in% .env$states)) {
          .env$defs[[.n]] <- .rhs
        }
      }
    }
    invisible()
  }
  for (.l in lines) .walk(.l, character(0))
  # only single-assignment definitions can be folded back into labels
  .fold <- .env$defs[vapply(names(.env$defs), function(.n) {
    identical(.env$count[[.n]], 1)
  }, logical(1))]
  list(ode = .env$ode, deps = .env$deps, defs = .fold,
       states = .env$states)
}

#' Replace substituted definitions by their variable names (for labels)
#' @noRd
.mdFold <- function(expr, defs) {
  if (length(defs) == 0L) return(expr)
  .keys <- vapply(defs, .mdDeparse, character(1))
  .fold <- function(e) {
    if (is.call(e)) {
      .w <- which(.keys == .mdDeparse(e))
      if (length(.w) > 0L) return(as.name(names(defs)[.w[length(.w)]]))
      for (.i in seq_along(e)[-1]) {
        .v <- .fold(e[[.i]])
        if (!is.null(.v)) e[[.i]] <- .v
      }
    }
    e
  }
  .fold(expr)
}

#' Substitute variable definitions into an expression
#' @noRd
.mdSubstitute <- function(expr, defs) {
  if (length(defs) == 0L) return(expr)
  do.call(substitute, list(expr, defs))
}

#' Return the state name for a `d/dt(state)` expression or NULL
#' @noRd
.mdDdtState <- function(lhs) {
  if (is.call(lhs) && identical(lhs[[1]], quote(`/`)) &&
        identical(lhs[[2]], quote(d)) && is.call(lhs[[3]]) &&
        identical(lhs[[3]][[1]], quote(dt)) && length(lhs[[3]]) == 2L) {
    return(as.character(lhs[[3]][[2]]))
  }
  NULL
}

#' States an expression depends on (directly or through variables)
#' @noRd
.mdExprStates <- function(expr, states, deps) {
  .seen <- character(0)
  .todo <- all.vars(expr)
  while (length(.todo) > 0L) {
    .v <- .todo[1]
    .todo <- .todo[-1]
    if (.v %in% .seen) next
    .seen <- c(.seen, .v)
    # a state's own amount is not expanded through assignments
    if (!(.v %in% states) && !is.null(deps[[.v]])) {
      .todo <- c(.todo, setdiff(deps[[.v]], .seen))
    }
  }
  intersect(states, .seen)
}

#' Split an expression into signed additive terms
#'
#' Sums and differences are split, products distribute over sums and a sum
#' in a numerator is split over its denominator.
#' @param x expression
#' @return list of list(sign, expr)
#' @noRd
.mdTerms <- function(x) {
  if (is.call(x)) {
    .f <- x[[1]]
    if (identical(.f, quote(`(`))) return(.mdTerms(x[[2]]))
    if (identical(.f, quote(`+`))) {
      if (length(x) == 2L) return(.mdTerms(x[[2]]))
      return(c(.mdTerms(x[[2]]), .mdTerms(x[[3]])))
    }
    if (identical(.f, quote(`-`))) {
      if (length(x) == 2L) return(.mdNeg(.mdTerms(x[[2]])))
      return(c(.mdTerms(x[[2]]), .mdNeg(.mdTerms(x[[3]]))))
    }
    if (identical(.f, quote(`*`)) && length(x) == 3L) {
      .a <- .mdTerms(x[[2]])
      .b <- .mdTerms(x[[3]])
      .ret <- list()
      for (.i in .a) {
        for (.j in .b) {
          .ret[[length(.ret) + 1L]] <-
            list(sign = .i$sign * .j$sign, expr = .mdMult(.i$expr, .j$expr))
        }
      }
      return(.ret)
    }
    if (identical(.f, quote(`/`)) && length(x) == 3L) {
      .den <- .mdTerms(x[[3]])
      .denSign <- 1
      if (length(.den) == 1L) {
        # keep a single signed denominator's sign on the term
        .denSign <- .den[[1]]$sign
        .denExpr <- .den[[1]]$expr
      } else {
        .denExpr <- x[[3]]
      }
      return(lapply(.mdTerms(x[[2]]), function(.t) {
        list(sign = .t$sign * .denSign,
             expr = as.call(list(quote(`/`), .t$expr, .denExpr)))
      }))
    }
  }
  if (is.call(x) && identical(x[[1]], quote(ifelse)) && length(x) == 4L) {
    # `ifelse(cond, -a, -b)`: a common sign of both branches is kept
    .t <- c(.mdTerms(x[[3]]), .mdTerms(x[[4]]))
    # a zero branch (switching the flow off) has no sign
    .t <- .t[!vapply(.t, function(t) is.numeric(t$expr) && all(t$expr == 0),
                     logical(1))]
    .signs <- vapply(.t, function(t) t$sign, numeric(1))
    if (length(.signs) > 0L && all(.signs == -1)) {
      return(list(list(sign = -1,
                       expr = as.call(list(quote(ifelse), x[[2]],
                                           .mdNegExpr(x[[3]]),
                                           .mdNegExpr(x[[4]]))))))
    }
  }
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x < 0) {
    return(list(list(sign = -1, expr = -x)))
  }
  list(list(sign = 1, expr = x))
}

#' Negate an expression, removing a leading unary minus when possible
#' @noRd
.mdNegExpr <- function(x) {
  if (is.numeric(x) && all(x == 0)) return(x)
  .t <- .mdTerms(x)
  if (all(vapply(.t, function(t) t$sign, numeric(1)) == -1)) {
    # every term is subtracted (`-ka*depot` parses as `(-ka)*depot`)
    return(Reduce(function(a, b) as.call(list(quote(`+`), a, b)),
                  lapply(.t, function(t) t$expr)))
  }
  as.call(list(quote(`-`), x))
}

#' Canonical form of an expression: operands of sums and products sorted
#' @noRd
.mdCanon <- function(x) {
  if (!is.call(x)) return(x)
  if (identical(x[[1]], quote(`(`))) return(.mdCanon(x[[2]]))
  for (.op in list(quote(`+`), quote(`*`))) {
    if (identical(x[[1]], .op) && length(x) == 3L) {
      .ops <- list()
      .flat <- function(e) {
        if (is.call(e) && identical(e[[1]], quote(`(`))) return(.flat(e[[2]]))
        if (is.call(e) && identical(e[[1]], .op) && length(e) == 3L) {
          .flat(e[[2]])
          .flat(e[[3]])
        } else {
          .ops[[length(.ops) + 1L]] <<- .mdCanon(e)
        }
      }
      .flat(x)
      .ops <- .ops[order(vapply(.ops, .mdDeparse, character(1)))]
      return(Reduce(function(a, b) as.call(list(.op, a, b)), .ops))
    }
  }
  for (.i in seq_along(x)[-1]) {
    .v <- .mdCanon(x[[.i]])
    if (!is.null(.v)) x[[.i]] <- .v
  }
  x
}

#' @noRd
.mdNeg <- function(terms) {
  lapply(terms, function(.t) {
    .t$sign <- -.t$sign
    .t
  })
}

#' Multiply two expressions, dropping multiplications by one
#' @noRd
.mdMult <- function(a, b) {
  if (is.numeric(a) && length(a) == 1L && a == 1) return(b)
  if (is.numeric(b) && length(b) == 1L && b == 1) return(a)
  as.call(list(quote(`*`), a, b))
}

#' Remove all parentheses; deparse re-adds the ones that are needed
#' @noRd
.mdStripParen <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], quote(`(`))) return(.mdStripParen(x[[2]]))
    for (.i in seq_along(x)[-1]) {
      .v <- .mdStripParen(x[[.i]])
      if (!is.null(.v)) x[[.i]] <- .v
    }
  }
  x
}

#' @noRd
.mdDeparse <- function(x) {
  paste(deparse(.mdStripParen(x), width.cutoff = 500L), collapse = " ")
}

#' Direction of the dependence of an expression on a compartment
#'
#' Rates and parameters (including symbolic exponents) are assumed positive.
#' Monotone functions (`exp`, `log`, `sqrt`, `expit`, positive powers) keep
#' the direction of their argument, and a quotient whose numerator and
#' denominator both increase is taken as a saturating (Emax/Hill) increase.
#' @param expr expression
#' @param o compartment name
#' @return 1 (increasing), -1 (decreasing), 0 (independent) or NA (unknown)
#' @noRd
.mdDirection <- function(expr, o, states, deps) {
  .dep <- function(e) o %in% .mdExprStates(e, states, deps)
  .comb <- function(d) {
    d <- d[is.na(d) | d != 0]
    if (length(d) == 0L) return(0)
    if (anyNA(d) || length(unique(d)) > 1L) return(NA_real_)
    d[1]
  }
  # sign of an expression that does not depend on `o`
  .sgn <- function(e) {
    if (is.numeric(e) && length(e) == 1L && !is.na(e)) return(sign(e))
    if (!is.call(e)) return(1)
    if (identical(e[[1]], quote(`(`))) return(.sgn(e[[2]]))
    if (identical(e[[1]], quote(`-`)) && length(e) == 2L) return(-.sgn(e[[2]]))
    if ((identical(e[[1]], quote(`*`)) || identical(e[[1]], quote(`/`))) &&
          length(e) == 3L) {
      return(.sgn(e[[2]]) * .sgn(e[[3]]))
    }
    1
  }
  .d <- function(e) {
    if (is.name(e)) {
      if (identical(as.character(e), o)) return(1)
      # a variable that was not substituted (conditional/reassigned)
      return(if (.dep(e)) NA_real_ else 0)
    }
    if (!is.call(e)) return(0)
    .f <- e[[1]]
    .fn <- if (is.name(.f)) as.character(.f) else ""
    if (.fn == "(") return(.d(e[[2]]))
    if (.fn == "+") return(.comb(vapply(as.list(e)[-1], .d, numeric(1))))
    if (.fn == "-") {
      if (length(e) == 2L) return(-.d(e[[2]]))
      return(.comb(c(.d(e[[2]]), -.d(e[[3]]))))
    }
    if (.fn == "*") {
      .a <- .d(e[[2]])
      .b <- .d(e[[3]])
      # a factor that does not depend on `o` may still carry a sign (R
      # parses `-k*C` as `(-k)*C`)
      if (identical(.a, 0)) return(.b * .sgn(e[[2]]))
      if (identical(.b, 0)) return(.a * .sgn(e[[3]]))
      return(.comb(c(.a, .b)))
    }
    if (.fn == "/") {
      .n <- .d(e[[2]])
      .m <- .d(e[[3]])
      if (identical(.m, 0)) return(.n * .sgn(e[[3]]))
      if (identical(.n, 0)) return(-.m * .sgn(e[[2]]))
      # saturating forms (Emax/Hill: `C^g/(ec50^g + C^g)`) increase with C
      if (identical(.n, 1) && identical(.m, 1)) return(1)
      return(.comb(c(.n, -.m)))
    }
    if (.fn %in% c("exp", "log", "sqrt", "expit", "log1p", "log10", "log2") &&
          length(e) == 2L) {
      return(.d(e[[2]]))
    }
    if (.fn %in% c("^", "**") && length(e) == 3L) {
      .b <- .d(e[[2]])
      if (.dep(e[[3]])) return(NA_real_)
      if (is.numeric(e[[3]])) return(sign(e[[3]]) * .b)
      if (is.call(e[[3]]) && identical(e[[3]][[1]], quote(`-`)) &&
            length(e[[3]]) == 2L && is.numeric(e[[3]][[2]])) {
        return(-sign(e[[3]][[2]]) * .b)
      }
      # a symbolic exponent (e.g. a Hill coefficient) is assumed positive
      return(.b)
    }
    if (.dep(e)) NA_real_ else 0
  }
  .d(expr)
}

#' Interaction sign from a direction (unknown directions are 0)
#' @noRd
.mdSign <- function(d) {
  d <- unname(d)
  if (length(d) == 0L || is.na(d)) 0 else d
}

#' Numerator and denominator factors of a product/quotient term
#' @return list(num = list of expressions, den = list of expressions)
#' @noRd
.mdTermFactors <- function(x) {
  .env <- new.env(parent = emptyenv())
  .env$num <- list()
  .env$den <- list()
  .flat <- function(e, num) {
    if (is.call(e) && identical(e[[1]], quote(`(`))) return(.flat(e[[2]], num))
    if (is.call(e) && length(e) == 3L && identical(e[[1]], quote(`*`))) {
      .flat(e[[2]], num)
      .flat(e[[3]], num)
    } else if (is.call(e) && length(e) == 3L && identical(e[[1]], quote(`/`))) {
      .flat(e[[2]], num)
      .flat(e[[3]], !num)
    } else if (num) {
      .env$num[[length(.env$num) + 1L]] <- e
    } else {
      .env$den[[length(.env$den) + 1L]] <- e
    }
  }
  .flat(x, TRUE)
  list(num = .env$num, den = .env$den)
}

#' Canonical key of a product/quotient term (factor order ignored)
#' @noRd
.mdTermKey <- function(x) {
  .f <- .mdTermFactors(x)
  .num <- vapply(.f$num, function(e) .mdDeparse(.mdCanon(e)), character(1))
  .den <- vapply(.f$den, function(e) .mdDeparse(.mdCanon(e)), character(1))
  paste0(paste(sort(.num), collapse = "*"), "/",
         paste(sort(.den), collapse = "*"))
}

# ---------------------------------------------------------------------------
# Classification
# ---------------------------------------------------------------------------

#' Classify ODE terms into graph edges
#' @param terms data frame of terms from `.mdModelInfo()`
#' @param states compartment names
#' @return edge data frame
#' @noRd
.mdClassifyTerms <- function(terms, states) {
  .empty <- data.frame(from = character(0), to = character(0),
                       type = character(0), sign = numeric(0),
                       label = character(0), stringsAsFactors = FALSE)
  if (is.null(terms) || nrow(terms) == 0L) {
    .empty$bidirectional <- logical(0)
    return(.empty)
  }
  .n <- nrow(terms)
  .used <- rep(FALSE, .n)
  .matchedFrom <- vector("list", .n)
  .rows <- list()
  .add <- function(from, to, type, sign, label) {
    .rows[[length(.rows) + 1L]] <<-
      data.frame(from = from, to = to, type = type, sign = sign,
                 label = label, stringsAsFactors = FALSE)
  }
  # mass transfer: -term in the source (containing the source amount) and
  # +term in another compartment.  One +term may receive mass from several
  # sources (e.g. binding `kon*C*R` into a complex from both C and R) ...
  for (.i in seq_len(.n)) {
    if (.used[.i] || terms$sign[.i] > 0) next
    .src <- terms$state[.i]
    # a term subtracted from one compartment and added to another is
    # conserved flow, whatever drives it (first-order `k*A`, zero-order
    # `rate`, or another compartment like an enzyme `Vmax*E`)
    .j <- which(terms$sign > 0 & terms$state != .src &
                  terms$key == terms$key[.i] &
                  !vapply(.matchedFrom, function(m) .src %in% m, logical(1)))
    if (length(.j) == 0L) next
    # ... and one -term may go to several destinations (e.g. dissociation
    # `koff*RC` back to both C and R); keep one destination per compartment
    .j <- .j[!duplicated(terms$state[.j])]
    .used[c(.i, .j)] <- TRUE
    for (.k in .j) {
      .matchedFrom[[.k]] <- c(.matchedFrom[[.k]], .src)
      .add(.src, terms$state[.k], "transfer", 1, terms$label[.k])
    }
  }
  # compartments that drive a transfer without being its source (e.g. an
  # enzyme `E` in `Vmax*E*A`) stimulate/inhibit the destination
  for (.j in which(!vapply(.matchedFrom, is.null, logical(1)))) {
    .drivers <- setdiff(terms$states[[.j]],
                        c(.matchedFrom[[.j]], terms$state[.j]))
    for (.o in .drivers) {
      .add(.o, terms$state[.j], "interaction",
           .mdSign(terms$dir[[.j]][.o]), terms$label[.j])
    }
  }
  for (.i in which(!.used)) {
    .s <- terms$state[.i]
    .st <- terms$states[[.i]]
    .others <- setdiff(.st, .s)
    # direction of the effect of each other compartment on d/dt(.s)
    .dir <- function(o) {
      terms$sign[.i] * .mdSign(terms$dir[[.i]][o])
    }
    if (terms$sign[.i] < 0) {
      if (.s %in% .st || length(.others) == 0L) {
        .add(.s, NA_character_, "elimination", -1, terms$label[.i])
      }
    } else if (length(.others) == 0L) {
      .add(NA_character_, .s, "input", 1, terms$label[.i])
    }
    for (.o in .others) .add(.o, .s, "interaction", .dir(.o), terms$label[.i])
  }
  .e <- do.call(rbind, .rows)
  if (is.null(.e)) .e <- .empty
  # combine duplicated flows
  if (nrow(.e) > 0L) {
    .id <- paste(.e$from, .e$to, .e$type, .e$sign, sep = "\r")
    .e <- do.call(rbind, lapply(unique(.id), function(.k) {
      .w <- .e[.id == .k, , drop = FALSE]
      .w$label[1] <- paste(unique(.w$label), collapse = " + ")
      .w[1, , drop = FALSE]
    }))
  }
  .tr <- .e$type == "transfer"
  .pairs <- paste(.e$from, .e$to, sep = "\r")
  .rev <- paste(.e$to, .e$from, sep = "\r")
  .e$bidirectional <- .tr & .rev %in% .pairs[.tr]
  rownames(.e) <- NULL
  .e
}

# ---------------------------------------------------------------------------
# Layout
# ---------------------------------------------------------------------------

#' Choose the central compartment
#' @noRd
.mdCentral <- function(states, edges) {
  .cn <- states[tolower(states) %in% c("central", "center", "centr", "cent")]
  if (length(.cn) > 0L) return(.cn[1])
  .tr <- edges[edges$type == "transfer", , drop = FALSE]
  .deg <- vapply(states, function(.s) sum(.tr$from == .s | .tr$to == .s),
                 numeric(1))
  .elim <- states %in% edges$from[edges$type == "elimination"]
  .score <- .deg + 0.5 * .elim
  if (all(.deg == 0)) return(states[1])
  states[which.max(.score)]
}

#' Lay out the compartments on a grid
#'
#' @return node data frame with name, role, dosing, x, y
#' @noRd
.mdLayout <- function(states, edges, dosing) {
  .central <- .mdCentral(states, edges)
  .tr <- edges[edges$type == "transfer", , drop = FALSE]
  .bi <- .tr[.tr$bidirectional, , drop = FALSE]
  .uni <- .tr[!.tr$bidirectional, , drop = FALSE]
  .int <- edges[edges$type == "interaction", , drop = FALSE]
  .x <- stats::setNames(rep(NA_real_, length(states)), states)
  .y <- .x
  .role <- stats::setNames(rep("other", length(states)), states)
  .free <- function(x, y) {
    !any(!is.na(.x) & abs(.x - x) < 0.9 & abs(.y - y) < 0.9)
  }
  .place <- function(s, x, y, step) {
    while (!.free(x, y)) y <- y + step
    .x[s] <<- x
    .y[s] <<- y
  }
  # breadth-first placement of the (mass transfer) neighbors of placed nodes
  # `side` is where exchange (bidirectional) partners go: left of the PK
  # model, right of PD models so they stay off the arrows coming in from the
  # left
  .spread <- function(start, side = -1) {
    .queue <- start
    while (length(.queue) > 0L) {
      .s <- .queue[1]
      .queue <- .queue[-1]
      .isCentral <- .s == .central
      # upstream (unidirectional into .s): above
      .up <- setdiff(unique(.uni$from[.uni$to == .s]), names(.x)[!is.na(.x)])
      for (.k in seq_along(.up)) {
        .place(.up[.k], .x[.s] + (.k - 1), .y[.s] + 1, 1)
        if (.role[.up[.k]] == "other") .role[.up[.k]] <<- "transit"
      }
      # bidirectional exchange: to the left, fanned up and down
      .lr <- unique(c(.bi$to[.bi$from == .s], .bi$from[.bi$to == .s]))
      .lr <- setdiff(.lr, names(.x)[!is.na(.x)])
      .nb <- length(.lr)
      .off <- (seq_len(.nb) - 1) - (.nb - 1) / 2
      for (.k in seq_along(.lr)) {
        .place(.lr[.k], .x[.s] + side, .y[.s] + .off[.k], -1)
        if (.role[.lr[.k]] == "other") .role[.lr[.k]] <<- "peripheral"
      }
      # downstream (unidirectional out of .s): below
      .dn <- setdiff(unique(.uni$to[.uni$from == .s]), names(.x)[!is.na(.x)])
      for (.k in seq_along(.dn)) {
        .place(.dn[.k], .x[.s] + (.k - 1), .y[.s] - 1, -1)
        if (.role[.dn[.k]] == "other") {
          .role[.dn[.k]] <<- if (.isCentral) "metabolite" else "other"
        }
      }
      .queue <- c(.queue, .up, .lr, .dn)
    }
  }
  .role[.central] <- "central"
  .place(.central, 0, 0, -1)
  .spread(.central)
  # choose a row in column `nx` for `s` where the straight interaction
  # arrows between `s` and the already placed compartments (either
  # direction) are clear of the other compartments
  .pickRow <- function(s, nx, y0) {
    .placed <- names(.x)[!is.na(.x)]
    .partners <- unique(c(.int$from[.int$to == s], .int$to[.int$from == s]))
    .partners <- intersect(.partners, .placed)
    for (.d in c(0, rbind(-seq_along(states), seq_along(states)))) {
      .cy <- y0 + .d
      .clear <- all(vapply(.partners, function(.f) {
        .w <- !is.na(.x) & names(.x) != .f
        !.mdSegmentCrosses(.x[.f], .y[.f], nx, .cy, .x[.w], .y[.w])
      }, logical(1)))
      if (.free(nx, .cy) && .clear) return(.cy)
    }
    y0
  }
  # compartments interacting without mass transfer: to the right
  repeat {
    .placed <- names(.x)[!is.na(.x)]
    .cand <- .int[.int$from %in% .placed & !(.int$to %in% .placed), ,
                  drop = FALSE]
    if (nrow(.cand) > 0L) {
      .s <- .cand$to[1]
      .nx <- max(.x, na.rm = TRUE) + 1
      .place(.s, .nx, .pickRow(.s, .nx, .y[.cand$from[1]]), -1)
      .role[.s] <- "effect"
      .spread(.s, side = 1)
      next
    }
    .left <- states[is.na(.x)]
    if (length(.left) == 0L) break
    # compartments acting on the placed ones first, then dosing compartments
    .acting <- intersect(.left, .int$from[.int$to %in% .placed])
    .s <- c(.acting, intersect(dosing, .left), .left)[1]
    .nx <- max(.x, na.rm = TRUE) + 1
    .place(.s, .nx, .pickRow(.s, .nx, 0), -1)
    .spread(.s, side = if (length(.acting) > 0L) 1 else -1)
  }
  .role[states %in% dosing & .role != "central"] <- "dosing"
  data.frame(name = states, role = unname(.role[states]),
             dosing = states %in% dosing,
             x = unname(.x[states]), y = unname(.y[states]),
             stringsAsFactors = FALSE)
}

#' Does the segment (x0, y0)-(x1, y1) pass through any of the node boxes?
#'
#' Boxes are centered on (x, y) (grid units) with half width `hw` and half
#' height `hh`; the segment is sampled finely enough for unit-grid layouts.
#' @noRd
.mdSegmentCrosses <- function(x0, y0, x1, y1, x, y, hw = 0.4, hh = 0.3) {
  if (length(x) == 0L) return(FALSE)
  .t <- seq(0, 1, length.out = 101L)
  .px <- x0 + .t * (x1 - x0)
  .py <- y0 + .t * (y1 - y0)
  any(vapply(seq_along(x), function(.i) {
    any(abs(.px - x[.i]) < hw & abs(.py - y[.i]) < hh)
  }, logical(1)))
}

#' Positions of the invisible input/output end points
#'
#' @return edge data frame with added columns x0, y0, x1, y1 (in grid units)
#' @noRd
.mdEdgeCoords <- function(graph) {
  .n <- graph$nodes
  .e <- graph$edges
  .px <- stats::setNames(.n$x, .n$name)
  .py <- stats::setNames(.n$y, .n$name)
  .occupied <- function(x, y) any(abs(.n$x - x) < 0.5 & abs(.n$y - y) < 0.6)
  .e$x0 <- .px[.e$from]
  .e$y0 <- .py[.e$from]
  .e$x1 <- .px[.e$to]
  .e$y1 <- .py[.e$to]
  for (.i in seq_len(nrow(.e))) {
    if (.e$type[.i] == "elimination") {
      .x <- .e$x0[.i]
      .y <- .e$y0[.i]
      .e$x1[.i] <- .x
      .e$y1[.i] <- .y - 0.7
      if (.occupied(.x, .y - 1)) {
        .e$x1[.i] <- .x + 0.45
        .e$y1[.i] <- .y - 0.6
      }
    } else if (.e$type[.i] == "input") {
      .x <- .e$x1[.i]
      .y <- .e$y1[.i]
      .e$x0[.i] <- .x
      .e$y0[.i] <- .y + 0.7
      if (.occupied(.x, .y + 1)) {
        .e$x0[.i] <- .x + 0.45
        .e$y0[.i] <- .y + 0.6
      }
    }
  }
  .e
}

.mdRoleColors <- c(dosing = "#F2C57C", central = "#7FB3D5",
                   peripheral = "#A9CCE3", transit = "#FAD7A0",
                   metabolite = "#D2B4DE", effect = "#A9DFBF",
                   other = "#E5E7E9")

# ---------------------------------------------------------------------------
# Engines
# ---------------------------------------------------------------------------

#' Graphviz DOT source for a model graph
#' @noRd
.mdDot <- function(graph, labels = FALSE) {
  .xs <- 1.6
  .ys <- 1.1
  # quote a DOT string; "\r" (from combined labels) becomes a DOT line break
  .q <- function(x) {
    paste0("\"", gsub("\r", "\\n", gsub("\"", "\\\\\"", x), fixed = TRUE), "\"")
  }
  .n <- graph$nodes
  .e <- .mdEdgeCoords(graph)
  .lines <- c("digraph model {",
              "  graph [layout = neato, splines = true, outputorder = edgesfirst];",
              "  node [shape = box, style = \"rounded,filled\", fontname = Helvetica];",
              "  edge [fontname = Helvetica, fontsize = 10];")
  for (.i in seq_len(nrow(.n))) {
    .lines <- c(.lines, sprintf(
      "  %s [pos = \"%g,%g!\", fillcolor = %s%s];",
      .q(.n$name[.i]), .n$x[.i] * .xs, .n$y[.i] * .ys,
      .q(.mdRoleColors[[.n$role[.i]]]),
      if (.n$dosing[.i]) ", penwidth = 2" else ""))
  }
  .done <- rep(FALSE, nrow(.e))
  for (.i in seq_len(nrow(.e))) {
    if (.done[.i]) next
    .t <- .e$type[.i]
    .from <- .e$from[.i]
    .to <- .e$to[.i]
    .attr <- character(0)
    .lab <- .e$label[.i]
    if (.t %in% c("elimination", "input")) {
      .pt <- paste0(".", .t, .i)
      .lines <- c(.lines, sprintf(
        "  %s [shape = point, style = invis, width = 0.01, pos = \"%g,%g!\"];",
        .q(.pt),
        (if (.t == "elimination") .e$x1[.i] else .e$x0[.i]) * .xs,
        (if (.t == "elimination") .e$y1[.i] else .e$y0[.i]) * .ys))
      if (.t == "elimination") .to <- .pt else .from <- .pt
    } else if (.t == "transfer" && .e$bidirectional[.i]) {
      .j <- which(.e$type == "transfer" & .e$from == .to & .e$to == .from)
      .done[.j] <- TRUE
      .attr <- c(.attr, "dir = both")
      .lab <- paste(c(.lab, .e$label[.j]), collapse = "\r")
    } else if (.t == "interaction") {
      .attr <- c(.attr, "style = dashed", "color = gray40",
                 if (.e$sign[.i] < 0) "arrowhead = tee",
                 if (.e$sign[.i] == 0) "arrowhead = dot")
    }
    if (labels) .attr <- c(.attr, paste0("label = ", .q(.lab)))
    .lines <- c(.lines, sprintf(
      "  %s -> %s%s;", .q(.from), .q(.to),
      if (length(.attr) > 0L) paste0(" [", paste(.attr, collapse = ", "), "]") else ""))
    .done[.i] <- TRUE
  }
  paste(c(.lines, "}"), collapse = "\n")
}

#' @noRd
.mdDiagrammeR <- function(graph, labels = FALSE) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("the 'DiagrammeR' engine requires the 'DiagrammeR' package; install it or use engine = \"ggplot2\"",
         call. = FALSE)
  }
  DiagrammeR::grViz(.mdDot(graph, labels))
}

#' Move a segment's end points to the boundaries of the node boxes
#' @noRd
.mdClip <- function(x0, y0, x1, y1, hw, hh, clip0, clip1) {
  .dx <- x1 - x0
  .dy <- y1 - y0
  .t <- function(dx, dy) {
    .tx <- if (abs(dx) > 1e-8) hw / abs(dx) else Inf
    .ty <- if (abs(dy) > 1e-8) hh / abs(dy) else Inf
    min(.tx, .ty, 0.45)
  }
  .t0 <- if (clip0) .t(.dx, .dy) else 0
  .t1 <- if (clip1) .t(.dx, .dy) else 0
  c(x0 + .t0 * .dx, y0 + .t0 * .dy, x1 - .t1 * .dx, y1 - .t1 * .dy)
}

#' @noRd
.mdGgplot <- function(graph, labels = FALSE) {
  .xs <- 1.6
  .hw <- 0.5
  .hh <- 0.2
  .n <- graph$nodes
  .n$x <- .n$x * .xs
  .e <- .mdEdgeCoords(graph)
  .e$x0 <- .e$x0 * .xs
  .e$x1 <- .e$x1 * .xs
  .px <- stats::setNames(.n$x, .n$name)
  .py <- stats::setNames(.n$y, .n$name)
  # arrows between the same two compartments (in either direction)
  .pair <- ifelse(is.na(.e$from) | is.na(.e$to), paste0(".", seq_len(nrow(.e))),
                  paste(pmin(.e$from, .e$to), pmax(.e$from, .e$to), sep = "\r"))
  .pairN <- as.integer(stats::ave(seq_along(.pair), .pair, FUN = length))
  .pairK <- as.integer(stats::ave(seq_along(.pair), .pair, FUN = seq_along))
  .seg <- do.call(rbind, lapply(seq_len(nrow(.e)), function(.i) {
    .x0 <- .e$x0[.i]
    .y0 <- .e$y0[.i]
    .x1 <- .e$x1[.i]
    .y1 <- .e$y1[.i]
    if (.e$type[.i] %in% c("transfer", "interaction") && .e$from[.i] == .e$to[.i]) {
      return(NULL)
    }
    if (.pairN[.i] > 1L) {
      # spread several arrows between the same two compartments (both
      # directions of an exchange, or a stimulation and an inhibition) to
      # either side of the center line
      .a <- c(pmin(.e$from[.i], .e$to[.i]), pmax(.e$from[.i], .e$to[.i]))
      .dx <- .px[.a[2]] - .px[.a[1]]
      .dy <- .py[.a[2]] - .py[.a[1]]
      .len <- sqrt(.dx^2 + .dy^2)
      .off <- (.pairK[.i] - (.pairN[.i] + 1) / 2) * 0.12
      .ox <- -.dy / .len * .off
      .oy <- .dx / .len * .off
      .x0 <- .x0 + .ox
      .x1 <- .x1 + .ox
      .y0 <- .y0 + .oy
      .y1 <- .y1 + .oy
    }
    .c <- .mdClip(.x0, .y0, .x1, .y1, .hw, .hh,
                  clip0 = .e$type[.i] != "input",
                  clip1 = .e$type[.i] != "elimination")
    data.frame(x = .c[1], y = .c[2], xend = .c[3], yend = .c[4],
               flow = ifelse(.e$type[.i] == "interaction",
                             c("inhibition", "modulation", "stimulation")[sign(.e$sign[.i]) + 2],
                             "mass transfer"),
               label = .e$label[.i], stringsAsFactors = FALSE)
  }))
  .n$role <- factor(.n$role, levels = names(.mdRoleColors))
  .p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = .n,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$role),
      width = 2 * .hw, height = 2 * .hh, color = "gray30",
      linewidth = ifelse(.n$dosing, 1, 0.4)
    ) +
    ggplot2::geom_text(data = .n,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$name)) +
    ggplot2::scale_fill_manual(values = .mdRoleColors, drop = TRUE,
                               name = "compartment") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(.seg) && nrow(.seg) > 0L) {
    .p <- .p +
      ggplot2::geom_segment(
        data = .seg,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                     yend = .data$yend, linetype = .data$flow),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "inches"),
                               type = "closed")
      ) +
      ggplot2::scale_linetype_manual(
        values = c("mass transfer" = "solid", stimulation = "dashed",
                   inhibition = "dotted", modulation = "dotdash"),
        name = "flow")
    if (labels) {
      .p <- .p +
        ggplot2::geom_label(
          data = .seg,
          ggplot2::aes(x = (.data$x + .data$xend) / 2,
                       y = (.data$y + .data$yend) / 2,
                       label = .data$label),
          size = 2.5
        )
    }
  }
  .p
}
