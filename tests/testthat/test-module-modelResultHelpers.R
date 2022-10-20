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
