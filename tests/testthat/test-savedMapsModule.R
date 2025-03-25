test_that("Test create circle", {
  testRadius <- 3000
  centerLatitude <- 50
  centerLongitude <- 50

  expectedCoords <-
    readRDS(testthat::test_path("testdata", "test-savedMapsModule-data.rds"))

  testCoords <- getFullCoordGrid(gridLength = testRadius / 10000)
  expect_equal(testCoords, expectedCoords$expectedGrid)

  testCoordsCircle <- testCoords %>%
    filterCoordCircle(lat = centerLatitude,
                      long = centerLongitude,
                      radius = testRadius / 111)
  expect_equal(testCoordsCircle, expectedCoords$expectedCoordCircle)

  testCoordsSquare <- testCoords %>%
    filterCoordSquare(lat = centerLatitude,
                      long = centerLongitude,
                      length = testRadius / 111 * 2)
  expect_equal(testCoordsSquare, expectedCoords$expectedCoordSquare)

  testCoordsRectangle <- testCoords %>%
    filterCoordRectangle(
      long = centerLongitude,
      lat = centerLatitude,
      latLength = testRadius / 111 * 2,
      longLength = testRadius / 111 * 2
    )

  expect_equal(testCoordsRectangle, testCoordsSquare)
})


test_that("Test getCoordCenter", {
  testLong <- 10
  testLat <- 50
  testRadius <- 3000

  testCenter <-
    getCoordCenter(
      upperLeftLat = testLat - testRadius / 111,
      upperLeftLong = testLong + testRadius / 111,
      lowerRightLat = testLat + testRadius / 111,
      lowerRightLong = testLong - testRadius / 111
    )
  expect_equal(colnames(testCenter), c("lon", "lat"))
  expect_equal(testCenter[1], testLong)
  expect_equal(testCenter[2], testLat)

  testCenter <- getCoordCenter(
    upperLeftLat = 80,
    upperLeftLong = 90,
    lowerRightLat = -70,
    lowerRightLong = -150
  )

  expect_equal(colnames(testCenter), c("lon", "lat"))
  expect_equal(testCenter[1], -30)
  expect_equal(testCenter[2], 5)
})
