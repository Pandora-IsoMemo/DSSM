context("description pop up")


testthat::test_that("getDescriptionFull", {

  isoData <- data.frame(id = c("a", "b"),
                        description = c("c", "d"),
                        stringsAsFactors = FALSE)
  colums <- c("id", "description")

  testthat::expect_true(is.null(getDescriptionCells(list(), isoData, colums)))
  testthat::expect_true(is.null(getDescriptionCells(list(col = 0, row = 1), isoData, colums)))
  testthat::expect_equal(getDescriptionCells(list(col = 1, row = 1), isoData, colums), "a")

})



testthat::test_that("getDescriptionCells", {

  isoData <- data.frame(id = c("a", "b"),
                        description = c("c", "d"),
                        descriptionFull = c("ccc", "ddd"),
                        stringsAsFactors = FALSE)

  testthat::expect_equal(getDescriptionFull("a", isoData), "ccc")

})
