test_that("new_nlmixr2PlotList returns the correct class", {
  expect_type(new_nlmixr2PlotList(list(A="A")), type="list")
  expect_s3_class(new_nlmixr2PlotList(list(A="A")), class="nlmixr2PlotList", exact=TRUE)
})

test_that("new_nlmixr2PlotList detects errors correctly", {
  expect_error(new_nlmixr2PlotList("A"))
  expect_error(new_nlmixr2PlotList(list("A")))
  expect_error(new_nlmixr2PlotList(list(A="A", "B")))
})
