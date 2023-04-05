context("draw interactive map")

testthat::test_that("function draw", {
  testthat::expect_is(draw(data.frame()), "leaflet")
  testthat::expect_is(draw(data.frame(Latitude = NA, ID = "a")), "leaflet")
  testthat::expect_is(draw(data.frame(
    Latitude = 45,
    Longitude = 5,
    ID = "b"
  )), "leaflet")

})

testthat::test_that("function drawSymbolsOnMap", {
  testIsoData <- data.frame(
    d13C = c(-20.4, -20.6, -20.8, -20.3, -22.4, -21),
    d15N = c(9, 8.1, 9, 8.1, 5.8, 6.6),
    latitude = c(45.9667, 55.9667, 65.9667, 35.9667, 40.9667, 5.9667),
    longitude = c(15.4833, 25.4833, 5.4833, 35.4833, 55.4833, 15.4833),
    dateMean = c(5365, 5421, 5436, 5485, 4250, 4250),
    dateLower = c(4328L, 4340L, 4344L, 4448L, 4450L, 4450L),
    dateUpper = c(4055L, 4235L, 4244L, 4243L, 4050L, 4050L),
    dateUncertainty = c(31, 30, 30, 50, 100, 100),
    calibratedDate = c(NA, NA, NA, NA, NA, NA),
    calibratedDateLower = c(NA, NA, NA, NA, NA, NA),
    calibratedDateUpper = c(NA, NA, NA, NA, NA, NA),
    source = as.factor(c(
      "LiVES", "LiVES", "Other", "LiVES", "Other",
      "LiVES"
    )),
    id = as.factor(c(
      "1000", "1001", "1002", "1003", "1004", "1005"
    )),
    site = as.factor(
      c(
        "Ajdovska Jama",
        "Ajdovska Jama",
        "Ajdovska Jama",
        "Ajdovska Jama",
        "Ajdovska Jama",
        "Ajdovska Jama"
      )
    ),
    datingType = as.factor(
      c(
        "radiocarbon",
        "radiocarbon",
        "radiocarbon",
        "radiocarbon",
        "expert",
        "expert"
      )
    )
  )

  testMap <- NULL
  testMap <- drawSymbolsOnMap(
    map = leaflet() %>%
      setView(lng = 30,
              lat = 50,
              zoom = 4) %>%
      addProviderTiles("CartoDB.Positron"),
    isoData = testIsoData,
    pointRadius = c(10, 20, 3.5, 80, 10, 20),
    colourPal = colorFactor(
      palette = "Dark2",
      domain = testIsoData[["source"]],
      reverse = FALSE
    ),
    columnForColour = "source",
    pointOpacity = c(1, 1, 0, 0, 0.5, 0.5),
    pointSymbol = c(1, 2, 3, 4, 5, 6)
  )
  testthat::expect_is(testMap, "leaflet")
})
