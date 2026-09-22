# Automatic model diagram

Draws a compartment diagram of a model from its differential equations
(see
[`modelGraph()`](https://nlmixr2.github.io/nlmixr2plot/reference/modelGraph.md)
for how the equations are interpreted).

## Usage

``` r
modelDiagram(
  object,
  dosing = NULL,
  data = NULL,
  engine = getOption("nlmixr2plot.diagram.engine"),
  labels = FALSE,
  ...
)

# S3 method for class 'nlmixr2ModelGraph'
plot(x, ..., engine = getOption("nlmixr2plot.diagram.engine"), labels = FALSE)

# S3 method for class 'rxUi'
plot(
  x,
  ...,
  dosing = NULL,
  data = NULL,
  engine = getOption("nlmixr2plot.diagram.engine"),
  labels = FALSE
)

# S3 method for class 'rxode2'
plot(
  x,
  ...,
  dosing = NULL,
  data = NULL,
  engine = getOption("nlmixr2plot.diagram.engine"),
  labels = FALSE
)
```

## Arguments

- object:

  model to diagram (see
  [`modelGraph()`](https://nlmixr2.github.io/nlmixr2plot/reference/modelGraph.md))
  or a `nlmixr2ModelGraph` object.

- dosing:

  optional character vector naming the dosing compartments. When `NULL`
  the dosing compartments are detected from the dosing records in `data`
  (a dataset without dose records has no dosing compartment); when there
  is no data, or it has no `evid`/`amt` columns, the first compartment
  (the default `rxode2` dosing compartment) is used.

- data:

  optional dataset used to detect the dosing compartments (from the
  dosing records' `cmt`). For fitted models this defaults to the data
  the model was fit with.

- engine:

  drawing engine: `"DiagrammeR"` (a Graphviz htmlwidget from the
  'DiagrammeR' package), `"ggplot2"` (a `ggplot` object) or `"dot"` (the
  Graphviz DOT source as a character string, to customize or render
  elsewhere). The default is `"DiagrammeR"` when that package is
  installed and `"ggplot2"` otherwise; it may be changed with
  `options(nlmixr2plot.diagram.engine = ...)`.

- labels:

  logical; when `TRUE` label the arrows with the model terms.

- ...:

  ignored.

- x:

  a `nlmixr2ModelGraph` object, an `rxode2` user interface (`rxUi`)
  object or a compiled `rxode2` model

## Value

the diagram drawn by the requested `engine`.

## Details

The layout follows common pharmacometric conventions: dosing and
absorption/transit compartments are above the compartment they feed; the
central compartment is in the middle with the compartments it exchanges
mass with (peripheral compartments) to its left; unidirectional transfer
(e.g. to a metabolite) and eliminations go below; compartments that
interact with the model without mass transfer (e.g. effect compartments
or pharmacodynamic models) go to the right, with their own inputs above,
outputs below and exchange compartments further right.

Mass transfer is drawn with solid arrows; interactions without mass
transfer are dashed (with a "tee" arrow head for inhibition and a "dot"
arrow head when the direction is undetermined with `DiagrammeR`; dotted
for inhibition and dot-dashed when undetermined with `ggplot2`).

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) of an `rxode2`
user interface (`rxUi`) object, like `rxode2::rxode2(modelFunction)`, or
of a compiled `rxode2` model draws its model diagram, so
`plot(rxode2(model))` is the same as `modelDiagram(model)`. (A fitted
`nlmixr2` model keeps its goodness-of-fit
[`plot()`](https://rdrr.io/r/graphics/plot.default.html); use
`modelDiagram(fit)` for its diagram.)

## See also

Other model diagrams:
[`modelGraph()`](https://nlmixr2.github.io/nlmixr2plot/reference/modelGraph.md)

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
pk.turnover.emax <- function() {
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
modelDiagram(pk.turnover.emax, engine = "ggplot2")
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  modelDiagram(pk.turnover.emax, engine = "DiagrammeR")
}
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

{"x":{"diagram":"digraph model {\n  graph [layout = neato, splines = true, outputorder = edgesfirst, forcelabels = true];\n  node [shape = box, style = \"rounded,filled\", fontname = Helvetica];\n  edge [fontname = Helvetica, fontsize = 10];\n  \"depot\" [pos = \"0,2.2!\", fillcolor = \"#F2C57C\", penwidth = 2];\n  \"gut\" [pos = \"0,1.1!\", fillcolor = \"#FAD7A0\"];\n  \"center\" [pos = \"0,0!\", fillcolor = \"#7FB3D5\"];\n  \"effect\" [pos = \"1.6,0!\", fillcolor = \"#A9DFBF\"];\n  \"depot\" -> \"gut\";\n  \"gut\" -> \"center\";\n  \".elimination3\" [shape = point, style = invis, width = 0.01, pos = \"0,-0.77!\"];\n  \"center\" -> \".elimination3\";\n  \".input4\" [shape = point, style = invis, width = 0.01, pos = \"1.6,0.77!\"];\n  \".input4\" -> \"effect\";\n  \"center\" -> \"effect\" [style = dashed, color = gray40, arrowhead = tee];\n  \".elimination6\" [shape = point, style = invis, width = 0.01, pos = \"1.6,-0.77!\"];\n  \"effect\" -> \".elimination6\";\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}# }
```
