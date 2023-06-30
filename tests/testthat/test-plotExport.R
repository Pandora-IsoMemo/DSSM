test_that("nameFile", {
  expect_equal(nameFile("spatio-temporal-average", "png", FALSE, "onlyZip"),
               "spatio-temporal-average.png")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", FALSE, "onlyZip"),
               "spatio-temporal-average.tif")
  # for "geo-tiff" there is no series
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", FALSE, "onlyZip", i = 5),
               "spatio-temporal-average.tif")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", TRUE, "onlyZip"),
               "spatio-temporal-average.zip")

  expect_equal(nameFile("spatio-temporal-average", "png", TRUE, "onlyGif"),
               "spatio-temporal-average.gif")

  expect_equal(nameFile("spatio-temporal-average", "svg", TRUE, "gifAndZip", i = 3),
               "spatio-temporal-average_3.svg")
  expect_equal(nameFile("spatio-temporal-average", "svg", TRUE, "gifAndZip"),
               "spatio-temporal-average.zip")

  expect_equal(nameFile("spatio-temporal-average", "pdf", TRUE, "onlyGif", i = 3),
               "spatio-temporal-average_3.pdf")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", TRUE, "onlyZip", i = 2),
               "spatio-temporal-average_2.tif")
})
