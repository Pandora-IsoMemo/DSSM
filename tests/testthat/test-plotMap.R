testthat::test_that("function addFormattedAxis", {
  # we cannot test the output, but we can test if the function runs without error
  # decPlace = 3
  expect_true(is.null({
    plot(data.frame(x = c(1.2435, 4.575, 7.4324), y = c(2.4534, 4.5474, 8.423)), xaxt = "n", xlim = c(0, 10))
    addFormattedAxis(axis = "x", min = 0, max = 10, nLabels = 6, decPlace = 3)
  }))

  # decPlace = 0
  expect_true(is.null({
    plot(data.frame(x = c(1.2435, 4.575, 7.4324), y = c(2.4534, 4.5474, 8.423)), xaxt = "n", xlim = c(0, 10))
    addFormattedAxis(axis = "x", min = 0, max = 10, nLabels = 6, decPlace = 0)
  }))
})

testthat::test_that("function plotMap3D", {
  testModel <- readRDS(testthat::test_path("testdata", "test-plotMap3D-data.rds"))
  testInputs <- list(model = testModel, IndSelect = "expert", time = 5000, estType = "Mean",
                     estQuantile = NULL,
                     rangez = c(0, 1), limitz = NULL, showModel = TRUE, points = FALSE,
                     pointSize = 1L, StdErr = 2.5, rangex = c(-11.19465, 84.80535
                     ), rangey = c(26.22805, 74.22805), mask = TRUE, maskRadius = 500L,
                     addU = 0L, pColor = "#2C2161", pointShape = 4, textLabels = NULL,
                     pointLabels = NULL, pointColLabels = NULL, colorsP = "RdYlGn",
                     fontSize = 1L, fontType = "serif", fontCol = "#2C2161", centerMap = "Europe",
                     resolution = 100L, interior = 2, ncol = 20L, terrestrial = "1",
                     colors = "RdYlGn", reverseColors = FALSE, arrow = TRUE, grid = TRUE,
                     scale = TRUE, titleMain = FALSE, titleScale = FALSE, showScale = TRUE,
                     setAxisLabels = FALSE, mainLabel = "", yLabel = "", xLabel = "",
                     scLabel = "", northSize = 0.2, scalSize = 0.1, scaleX = 0L,
                     scaleY = 0.1, NorthX = 0.025, NorthY = 0.925, AxisSize = 1L,
                     AxisLSize = 1L,
                     pointDat = structure(list(
                       index = numeric(0),
                       y = numeric(0), x = numeric(0), label = character(0),
                       pointSize = numeric(0), pointAlpha = numeric(0), pointColor = character(0)),
                       class = "data.frame", row.names = integer(0)),
                     plotRetNull = TRUE)

  # we cannot test the output, but we can test if the function runs without error
  expect_true(is.null(do.call(plotMap3D, testInputs)))

  testInputs <- list(model = testModel, IndSelect = "expert", time = -2200, estType = "Mean",
                     estQuantile = NULL,
                     rangez = c(0, 1), limitz = NULL, showModel = TRUE, points = FALSE,
                     pointSize = 1L, StdErr = 2.5, rangex = c(-11.19465, 84.80535
                     ), rangey = c(26.22805, 74.22805), mask = TRUE, maskRadius = 500L,
                     addU = 0L, pColor = "#2C2161", pointShape = 4, textLabels = NULL,
                     pointLabels = NULL, pointColLabels = NULL, colorsP = "RdYlGn",
                     fontSize = 1L, fontType = "serif", fontCol = "#2C2161", centerMap = "Europe",
                     resolution = 100L, interior = 2, ncol = 20L, terrestrial = "1",
                     colors = "RdYlGn", reverseColors = FALSE, arrow = TRUE, grid = TRUE,
                     scale = TRUE, titleMain = FALSE, titleScale = FALSE, showScale = TRUE,
                     setAxisLabels = FALSE, mainLabel = "", yLabel = "", xLabel = "",
                     scLabel = "", northSize = 0.2, scalSize = 0.1, scaleX = 0L,
                     scaleY = 0.1, NorthX = 0.025, NorthY = 0.925, AxisSize = 1L,
                     AxisLSize = 1L,
                     pointDat = structure(list(
                       index = numeric(0),
                       y = numeric(0), x = numeric(0), label = character(0),
                       pointSize = numeric(0), pointAlpha = numeric(0), pointColor = character(0)),
                       class = "data.frame", row.names = integer(0)),
                     plotRetNull = TRUE)

  # we cannot test the output, but we can test if the function runs without error
  expect_true(is.null(do.call(plotMap3D, testInputs)))
})
