context("Import File for Analysis")

test_that("Validate Import", {
  # expect_warning(validateImport(NULL))
  #
  # expect_warning(validateImport(1:3))
  # expect_false(validateImport(1:3))
  #
  # d <- data.frame(x = 1, y = 2)
  # expect_warning(validateImport(d))
  # expect_false(validateImport(d))

  d <- data.frame(Latitude = 1:5, Longitude = 2:6)
  expect_true(validateImport(d))
})

test_that("Import Data", {
  d <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  write.csv(d, file = "test.csv", row.names = FALSE)
  write.xlsx(d, file = "test.xlsx")

  data <- readFile("test.csv", "csv", sep = ",", dec = ".")
  data2 <- readFile("test.xlsx", "xlsx")

  # expect_equal(d, data)
  # expect_equal(d, data2)
})
