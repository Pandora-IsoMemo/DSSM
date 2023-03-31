test_that("Test extractZoomFromLongRange", {
  centering <- "Europe"

  expect_equal(extractZoomFromLongRange(rangeLongitude = c(13, 97), mapCentering = centering), 86)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(3, 9), mapCentering = centering), 8)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-10, 84), mapCentering = centering), 96)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-60, 50), mapCentering = centering), 112)

  centering <- "Pacific"

  expect_equal(extractZoomFromLongRange(rangeLongitude = c(13, 97), mapCentering = centering), 86)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(3, 9), mapCentering = centering), 8)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-10, 84), mapCentering = centering), 96)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-60, 50), mapCentering = centering), 252)
  })


test_that("Test getZvalues", {
  testModel <- readRDS(testthat::test_path("averageR_testModel_numeric.rds"))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = ""),
               list(minInput = list(value = 6.1, min = 6.1, max = 15), maxInput = list(
                 value = 15, min = 6.1, max = 15)))

  testModel <- readRDS(testthat::test_path("averageR_testModel_categorical.rds"))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = ""),
               list(minInput = list(value = 0, min = 0, max = 1), maxInput = list(
                 value = 1, min = 0, max = 1)))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = "expert"),
               list(minInput = list(value = -0.09, min = -0.09, max = 1.1),
                    maxInput = list(value = 1.1, min = -0.09, max = 1.1)))
})
