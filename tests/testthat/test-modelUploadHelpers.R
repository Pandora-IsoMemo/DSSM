testthat::test_that("function incIndexOfName", {
  expect_equal(incIndexOfName("plotR4(3)"), "plotR4(4)")
  expect_equal(incIndexOfName("plotR4(3357)"), "plotR4(3358)")
  expect_equal(incIndexOfName("plotNew"), "plotNew(1)")
  expect_equal(incIndexOfName("pl(7)otR4(3)"), "pl(7)otR4(4)")
})
