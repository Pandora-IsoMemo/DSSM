context("Import File for Analysis")

test_that("Validate Import", {
  expect_warning(validateImport(NULL))

  expect_warning(validateImport(1:3))
  expect_false(validateImport(1:3))

  d <- data.frame(x = 1, y = 2)
  expect_warning(validateImport(d))
  expect_false(validateImport(d))

  d <- data.frame(Latitude = 1:5, Longitude = 2:6)
  expect_true(validateImport(d))
})
