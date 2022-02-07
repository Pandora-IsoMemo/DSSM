context("draw interactive map")

testthat::test_that("function draw", {

  testthat::expect_is(draw(data.frame()), "leaflet")
  testthat::expect_is(draw(data.frame(Latitude = NA, ID = "a")), "leaflet")
  testthat::expect_is(draw(data.frame(Latitude = 45,
                                      Longitude = 5, ID = "b")), "leaflet")

})
