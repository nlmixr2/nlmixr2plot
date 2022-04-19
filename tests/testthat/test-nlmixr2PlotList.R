test_that("newNlmixr2PlotList returns the correct class", {
  expect_type(newNlmixr2PlotList(list(A="A")), type="list")
  expect_s3_class(newNlmixr2PlotList(list(A="A")), class="nlmixr2PlotList", exact=TRUE)
})

test_that("newNlmixr2PlotList detects errors correctly", {
  expect_error(newNlmixr2PlotList("A"))
  expect_error(newNlmixr2PlotList(list("A")))
  expect_error(newNlmixr2PlotList(list(A="A", "B")))
})
