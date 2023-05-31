test_that("nameFile", {
  expect_equal(nameFile("spatio-temporal-average", "png", FALSE, "onlyZip"),
               "spatio-temporal-average.png")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", FALSE, "onlyZip"),
               "spatio-temporal-average.tif")

  expect_equal(nameFile("spatio-temporal-average", "png", TRUE, "onlyGif"),
               "spatio-temporal-average.gif")

  expect_equal(nameFile("spatio-temporal-average", "pdf", TRUE, "onlyGif", i = 3),
               "spatio-temporal-average_3.pdf")
  expect_equal(nameFile("spatio-temporal-average", "geo-tiff", TRUE, "onlyZip", i = 2),
               "spatio-temporal-average_2.tif")
})
