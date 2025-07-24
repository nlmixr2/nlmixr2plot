class_nlmixr2PlotList <- S7::new_S3_class("nlmixr2PlotList")

S7::method(`+`, list(class_nlmixr2PlotList, S7::class_any)) <- function(e1, e2)  {
  `+.nlmixr2PlotList`(e1, e1)
}
.onAttach <- function(libname, pkgname) {
  ## nocov start
  S7::methods_register()
  ## nlmixr2SetupMemoize()
  ## options(keep.source = TRUE)
  ## nocov end
}
