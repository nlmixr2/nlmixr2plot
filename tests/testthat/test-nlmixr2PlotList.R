.p <- loadNamespace("nlmixr2plot")
test_that("newNlmixr2PlotList returns the correct class", {
  expect_type(.p$newNlmixr2PlotList(list(A="A")), type="list")
  expect_s3_class(.p$newNlmixr2PlotList(list(A="A")), class="nlmixr2PlotList", exact=TRUE)
})

test_that("newNlmixr2PlotList detects errors correctly", {
  expect_error(.p$newNlmixr2PlotList("A"))
  expect_error(.p$newNlmixr2PlotList(list("A")))
  expect_error(.p$newNlmixr2PlotList(list(A="A", "B")))
})
