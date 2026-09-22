# Build a compartment graph from a model's differential equations

The differential equations are parsed into additive terms. A term that
is subtracted from one compartment and added (identically) to another is
mass transfer between the two compartments. A remaining subtracted term
that contains the compartment's own amount is an elimination (output); a
remaining term that depends on other compartments is an interaction that
does not transport mass (for example an effect compartment or a
pharmacodynamic stimulation/inhibition); a remaining added term that
does not depend on any other compartment is a (zero-order) input.
Dependencies through intermediate variables (like `cp <- central/v`) are
followed. A compartment that drives a transfer between two other
compartments (like an enzyme) is drawn as an interaction with the
destination. Production or loss driven only by another compartment (like
`ke0*cp` in an effect compartment) is represented by the interaction
arrow alone, without a separate input/output arrow.

## Usage

``` r
modelGraph(object, dosing = NULL, data = NULL)
```

## Arguments

- object:

  model to diagram: a model function, an `rxode2` user interface
  (`rxUi`) object, a compiled `rxode2` model or a fitted `nlmixr2`
  object.

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

## Value

a `nlmixr2ModelGraph` object; a list with:

- `nodes`: data frame with the compartment `name`, its `role`
  (`"dosing"`, `"central"`, `"peripheral"`, `"transit"`, `"metabolite"`,
  `"effect"` or `"other"`), whether it is `dosing` and the layout
  coordinates `x` and `y`, and an `annotation` with the compartment's
  dosing properties (`lag`, `F`, `rate`, `dur`; `""` when there are
  none), which the diagrams show next to the compartment.

- `edges`: data frame with `from`, `to` (`NA` for inputs/eliminations),
  `type` (`"transfer"`, `"elimination"`, `"input"` or `"interaction"`),
  `sign` (`1` when the term is added, `-1` when it is subtracted; for
  interactions `1` is stimulation, `-1` inhibition and `0` an effect
  whose direction cannot be determined from the equations, e.g. through
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) or a conditionally
  assigned variable), `bidirectional` (for transfers) and `label` (the
  model term(s)).

## Details

Some limitations on how equations must be written:

- Mass transfer is only detected when the same term (up to reordering of
  the factors of a product) is subtracted from the source and added to
  the destination, e.g. `d/dt(depot) <- -ka*depot` and
  `d/dt(central) <- ka*depot - ...`. Scaled transfer (like a
  stoichiometric or volume conversion in only one of the equations) is
  shown as an elimination plus an interaction.

- `linCmt()` models are converted to ODEs with
  [`rxode2::linToOde()`](https://nlmixr2.github.io/rxode2/reference/linToOde.html),
  which requires a version of 'rxode2' that provides it.

## See also

Other model diagrams:
[`modelDiagram()`](https://nlmixr2.github.io/nlmixr2plot/reference/modelDiagram.md)

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
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
    d/dt(central) <- ka * depot - cl / v * central
    cp <- central / v
    cp ~ add(add.sd)
  })
}
modelGraph(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> nlmixr2 model graph
#> 
#> compartments:
#>     name    role dosing
#>    depot  dosing   TRUE
#>  central central  FALSE
#> 
#> flows:
#>     from       to        type sign          label
#>    depot  central    transfer    1     ka * depot
#>  central (output) elimination   -1 cl/v * central
# }
```
