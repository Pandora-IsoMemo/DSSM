context("Sidebar Plot")

test_that("Clipboard", {
  stats <- statsUnivariate(c(0, 1, 2))
  expect_equal(stats$mean, 1)
  expect_equal(stats$median, 1)
  expect_equal(stats$max, 2)
  expect_equal(stats$min, 0)

  l <- list(
    a = 3,
    b = 4
  )

  raw <- "a\t3\nb\t4"
  expect_equal(statsRaw(l), raw)
})
