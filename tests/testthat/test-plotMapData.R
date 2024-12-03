test_that("shiftLongitudesToPacific shifts values correctly with default parameters", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudesToPacific(longitudes)
  expected <- c(-10, -170, -155, 10, 0)
  expect_equal(result, expected)
})

test_that("shiftLongitudesToPacific handles zero threshold correctly", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudesToPacific(longitudes, threshold = 0)
  expected <- c(-10, -170, -155, 10, 0)
  expect_equal(result, expected)
})

test_that("shiftLongitudesToPacific handles custom threshold correctly", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudesToPacific(longitudes, threshold = 30)
  expected <- c(-40, 160, 175, -20, -30)
  expect_equal(result, expected)
})

test_that("shiftLongitudesToPacific orders values correctly when order = TRUE", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudesToPacific(longitudes, threshold = 20, order = TRUE)
  expected <- c(-175, -30, -20, -10, 170)
  expect_equal(result, expected)
})

test_that("shiftLongitudesToPacific handles edge cases correctly", {
  # Edge case: Single longitude
  longitudes <- c(-190)
  result <- shiftLongitudesToPacific(longitudes, threshold = 20)
  expect_equal(result, c(-30))

  # Edge case: Longitudes exactly at the threshold
  longitudes <- c(20, 180)
  result <- shiftLongitudesToPacific(longitudes, threshold = 20)
  expect_equal(result, c(-180, -20))
})

test_that("shiftLongitudesToPacific returns an empty vector for empty input", {
  longitudes <- numeric(0)
  result <- shiftLongitudesToPacific(longitudes)
  expect_equal(result, numeric(0))
})

test_that("shiftLongitudesToPacific works with negative threshold", {
  longitudes <- c(-190, -10, 25, -170, 180)
  result <- shiftLongitudesToPacific(longitudes, threshold = -10)
  expected <- c(0, -180, -145, 20, 10)
  expect_equal(result, expected)
})

test_that("shiftLongitudesToPacific maintains ordering of unchanged values", {
  longitudes <- c(10, 25, 30)
  result <- shiftLongitudesToPacific(longitudes, threshold = 5)
  expected <- c(-175, -160, -155)  # Adjusted based on threshold = 5
  expect_equal(result, expected)
})
