# Helpers for checking what a saved figure carries with it.  serialize()
# writes every environment a ggplot references in full, except namespaces,
# package environments and the global, base and empty environments (written
# as references), so those are the environments that matter here.

# Value bound to `nm` in `env`, or NULL when it is a missing argument (the
# empty symbol, which cannot be assigned to a variable and then used).  This
# forces a promise bound to `nm`.
.figureEnvValue <- function(env, nm) {
  .l <- tryCatch(mget(nm, envir = env), error = function(e) list(NULL))
  if (is.symbol(.l[[1]]) && !nzchar(as.character(.l[[1]]))) {
    return(NULL)
  }
  .l[[1]]
}

# Add to `acc$envs` (and `acc$todo`) the environments that serialize() meets
# while writing `x`, apart from `x` itself, and to `acc$from` the position in
# `acc$envs` of `x` (`from`; 0 for the figure itself).  serialize() asks
# `refhook` about each environment it would write in full (and each external
# pointer and weak reference, which it writes as usual when the hook returns
# NULL); answering with a string writes only that string, so each environment
# is visited on its own and nothing is evaluated.  This is how the
# environments of unevaluated promises (which serialize() writes with their
# expressions) are found without forcing them.  Source files are skipped as
# in .figureSize().
.figureFindEnvs <- function(x, acc, from) {
  serialize(x, NULL, refhook = function(e) {
    if (!is.environment(e) || identical(e, x)) {
      return(NULL)
    }
    if (inherits(e, "srcfile")) {
      return("srcfile")
    }
    for (.e in acc$envs) {
      if (identical(.e, e)) {
        return("seen")
      }
    }
    acc$envs <- c(acc$envs, list(e))
    acc$from <- c(acc$from, from)
    acc$todo <- c(acc$todo, length(acc$envs))
    "new"
  })
  invisible()
}

# Every environment that serialize() writes out in full for `x`, with the
# attribute `from`: for each, the position of the environment it was found
# in (0 for `x` itself)
.figureEnvs <- function(x) {
  .acc <- new.env(parent = emptyenv())
  .acc$envs <- list()
  .acc$from <- integer(0)
  .acc$todo <- integer(0)
  .figureFindEnvs(x, .acc, 0L)
  while (length(.acc$todo) > 0L) {
    .i <- .acc$todo[[1L]]
    .acc$todo <- .acc$todo[-1L]
    .figureFindEnvs(.acc$envs[[.i]], .acc, .i)
  }
  structure(.acc$envs, from = .acc$from)
}

# How the figure reaches environment `i` of `envs` (from .figureEnvs()), as
# "figure > a > b": each environment is named by its ggproto class, or by its
# first bindings
.figureEnvPath <- function(envs, i) {
  .from <- attr(envs, "from")
  .path <- character(0)
  while (i > 0L) {
    .e <- envs[[i]]
    .path <- c(
      if (inherits(.e, "ggproto")) {
        class(.e)[1]
      } else {
        paste0("{", paste(utils::head(ls(.e, all.names = TRUE), 4L), collapse = ","), "}")
      },
      .path
    )
    i <- .from[[i]]
  }
  paste(c("figure", .path), collapse = " > ")
}

# Data frames (more than one row), fits and plots bound in the environments a
# figure references, as "name <class>" strings.  The first one found in each
# environment also says how the figure reaches that environment (see
# .figureEnvPath()), so a failing test shows what keeps it.  A figure should
# hold its data only in `$data`; one-row data frames are ggplot2's own (like
# the intercept and slope of geom_abline()).  Reading a binding forces a
# promise bound there, so the bindings are read from a copy of the figure
# after all of its environments have been found.
.figureHeldData <- function(fig) {
  fig <- unserialize(serialize(fig, NULL))
  .envs <- .figureEnvs(fig)
  .ret <- character(0)
  for (.i in seq_along(.envs)) {
    .e <- .envs[[.i]]
    .first <- TRUE
    for (.nm in setdiff(ls(.e, all.names = TRUE), "...")) {
      .v <- .figureEnvValue(.e, .nm)
      if ((is.data.frame(.v) && nrow(.v) > 1L) || inherits(.v, c("ggplot", "gglist"))) {
        .ret <- c(
          .ret,
          paste0(.nm, " <", class(.v)[1], ">", if (.first) paste0(" in ", .figureEnvPath(.envs, .i)))
        )
        .first <- FALSE
      }
    }
  }
  .ret
}

# Serialized size (bytes) of `x`, apart from the source files that srcrefs
# point to.  devtools::load_all() keeps them on this package's functions and
# calls, and a package installed with its source kept (as GitHub Actions
# installs rxode2, whose StatCens ggproto is in a figure with geom_cens()) on
# its own; a figure reaches them through those functions.  A source file's
# environment leads on to every environment of its package (the lazy-load
# table), with whatever those hold at the time, so it would count the whole
# session.  Packages are installed without their source by default.
.figureSize <- function(x) {
  length(serialize(x, NULL, refhook = function(e) {
    if (inherits(e, "srcfile")) "srcfile" else NULL
  }))
}

# Serialized size (bytes) of a figure apart from its `$data`
.figureSizeWithoutData <- function(fig) {
  .figureSize(fig) - .figureSize(fig$data)
}
