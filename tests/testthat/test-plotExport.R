test_that("nameFile", {
  expect_equal(nameFile("spatio-temporal-average", "png", FALSE),
               "spatio-temporal-average.png")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", FALSE),
               "spatio-temporal-average.tif")

  expect_equal(nameFile("spatio-temporal-average", "png", TRUE),
               "spatio-temporal-average.zip")

  expect_equal(nameFile("spatio-temporal-average", "pdf", TRUE, i = 3),
               "spatio-temporal-average_3.pdf")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", TRUE, i = 2),
               "spatio-temporal-average_2.tif")
})
