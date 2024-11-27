test_that("shiftLongitudes shifts values correctly with default parameters", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudes(longitudes)
  expected <- c(-30, 170, -175, -10, -20)
  expect_equal(result, expected)
})

test_that("shiftLongitudes handles zero threshold correctly", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudes(longitudes, threshold = 0)
  expected <- c(-10, -170, -155, 10, 0)
  expect_equal(result, expected)
})

test_that("shiftLongitudes handles custom threshold and shift correctly", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudes(longitudes, threshold = 30, shift = 160)
  expected <- c(-60, 140, 155, -40, -10)
  expect_equal(result, expected)
})

test_that("shiftLongitudes orders values correctly when order = TRUE", {
  longitudes <- c(-190, 10, 25, -170, 180)
  result <- shiftLongitudes(longitudes, order = TRUE)
  expected <- c(-175, -30, -20, -10, 170)
  expect_equal(result, expected)
})

test_that("shiftLongitudes handles edge cases correctly", {
  # Edge case: Single longitude
  longitudes <- c(-190)
  result <- shiftLongitudes(longitudes)
  expect_equal(result, c(-30))

  # Edge case: Longitudes exactly at the threshold
  longitudes <- c(20, 180)
  result <- shiftLongitudes(longitudes)
  expect_equal(result, c(-180, -20))
})

test_that("shiftLongitudes returns an empty vector for empty input", {
  longitudes <- numeric(0)
  result <- shiftLongitudes(longitudes)
  expect_equal(result, numeric(0))
})

test_that("shiftLongitudes works with negative threshold and shift", {
  longitudes <- c(-190, -10, 25, -170, 180)
  result <- shiftLongitudes(longitudes, threshold = -10, shift = 100)
  expected <- c(-80, -100, -65, -60, 90)
  expect_equal(result, expected)
})

test_that("shiftLongitudes maintains ordering of unchanged values", {
  longitudes <- c(10, 25, 30)
  result <- shiftLongitudes(longitudes, threshold = 5, shift = 180)
  expected <- c(-175, -160, -155)  # Adjusted based on threshold = 5
  expect_equal(result, expected)
})
