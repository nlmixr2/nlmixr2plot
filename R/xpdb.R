#' @export
nmObjGetData.xpdb <- function(x, ...) {
  if (!requireNamespace("xpose.nlmixr", quietly = TRUE)) {
    NULL
  } else {
    xpose.nlmixr::xpose_data_nlmixr2(x[[1]])
  }
}
attr(nmObjGetData.xpdb, "desc") <- "xpose object from nlmixr2"
