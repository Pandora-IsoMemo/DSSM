context("data explorer export")

testthat::test_that("function exportFilename", {

  testthat::expect_is(exportFilename(fileending = "csv"), "character")
  testthat::expect_equal(length(exportFilename(fileending = "csv")), 1)

})


testthat::test_that("function exportCSV", {
  dat <- data.frame(a = 1.5:10.5, b = 11:20)

  file <- "datTest.csv"
  exportCSV(file = file, dat = dat, colseparator = ";", decseparator = ",")

  testthat::expect_true(file.exists(file))

  datRead <- read.csv2(file)

  testthat::expect_true(all(datRead == dat))

  unlink(file)
})

testthat::test_that("function exportXLSX", {
  dat <- data.frame(a = 1.5:10.5, b = 11:20)

  file <- "datTest.xlsx"
  exportXLSX(file = file, dat = dat)

  testthat::expect_true(file.exists(file))

  datRead <- openxlsx::read.xlsx(file)

  testthat::expect_true(all(datRead == dat))

  unlink(file)
})
