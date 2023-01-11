test_that("Test create circle", {
  testRadius <- 3000
  centerLatitude <- 50
  centerLongitude <- 50

  expectedCoords <- readRDS(testthat::test_path("test-savedMapsModule-data.rds"))

  testCoords <- getFullCoordGrid(gridLength = testRadius / 10000)
  expect_identical(testCoords, expectedCoords$expectedGrid)
})
