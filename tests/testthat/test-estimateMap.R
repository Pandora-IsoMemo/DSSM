test_that("prepareDate", {
  testData <-
    structure(
      list(
        longitude = c(15.4833, 15.4833, 15.4833, 15.4833,
                      15.4833, 15.4833),
        latitude = c(45.9667, 45.9667, 45.9667, 45.9667,
                     45.9667, 45.9667),
        source = structure(c(1L, 1L, 1L, 1L, 1L, 1L), levels = "LiVES", class = "factor"),
        id = c("1", "2", "3",
               "4", "5", "6"),
        d13C = c(-20.4, -20.6, -20.8, -20.3, -22.4, -21),
        d15N = c(9, 8.1, 9, 8.1, 5.8, 6.6),
        dateMean = c(5365, 5421,
                     5436, 5485, 4250, 4250),
        dateLower = c(4328L, 4340L, 4344L, 4448L,
                      4450L, 4450L),
        dateUpper = c(4055L, 4235L, 4244L, 4243L, 4050L,
                      4050L),
        dateUncertainty = c(31, 30, 30, 50, 100, 100)
      ),
      row.names = c(NA,
                    6L),
      class = "data.frame"
    )

  prepDateData <- prepareDate(data = testData,
                          DateOne = "dateMean",
                          DateTwo = "",
                          DateType = "Single point",
                          dateUnc = "point")
  expect_equal(prepDateData$Date, c(5365, 5421, 5436, 5485, 4250, 4250))
  expect_equal(prepDateData$Uncertainty, c(0, 0, 0, 0, 0, 0))

  prepDateData <- prepareDate(data = testData,
                              DateOne = "dateLower",
                              DateTwo = "dateUpper",
                              DateType = "Interval",
                              dateUnc = "normal")
  expect_equal(prepDateData$Date, c(4191.5, 4287.5, 4294, 4345.5, 4250, 4250))
  expect_equal(prepDateData$Uncertainty, c(68.25, 26.25, 25, 51.25, 100, 100))

  prepDateData <- prepareDate(data = testData,
                              DateOne = "dateLower",
                              DateTwo = "dateUpper",
                              DateType = "Mean + 1 SD uncertainty",
                              dateUnc = "uniform",
                              useMaxUnc = FALSE)
  expect_equal(prepDateData$Date, c(4328L, 4340L, 4344L, 4448L, 4450L, 4450L))
  expect_equal(prepDateData$Uncertainty, c(4055, 4235, 4244, 4243, 4050, 4050))
})
