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

testthat::test_that("function plotMap3D with 3DKernel model", {
  testModel <- readRDS(testthat::test_path("testdata", "test-plotMap3DKernel-data.rds"))

  # clusterAll = "0"
  testInputs <- list(model = testModel, time = 1000, points = TRUE, pointSize = 1.3, rangex = c(-123.6565125,
                                                                             -67.6565125), rangey = c(25.9146065, 53.9146065), estType = "Mean",
                     showModel = FALSE, rangez = c(0, 0.048), limitz = NULL, addU = 0L,
                     centerMap = "Europe", resolution = 100L, interior = 0, mask = FALSE,
                     maskRadius = 500L, ncol = 50L, pColor = "#2C2161", pointShape = 4,
                     textLabels = NULL, pointLabels = NULL, pointColLabels = NULL,
                     colorsP = "RdYlGn", fontSize = 1L, fontType = "serif", fontCol = "#2C2161",
                     terrestrial = "1", colors = "YlOrRd", reverseColors = FALSE,
                     arrow = TRUE, grid = FALSE, scale = TRUE, titleMain = TRUE,
                     titleScale = TRUE, showScale = FALSE, setAxisLabels = FALSE,
                     mainLabel = "", yLabel = "", xLabel = "", scLabel = "", northSize = 0.2,
                     scalSize = 0.1, scaleX = 0.01, scaleY = 0.1, NorthX = 0.16,
                     NorthY = 0.2, AxisSize = 1L, AxisLSize = 1L, cluster = TRUE,
                     clusterAll = "0", clusterResults = "0", clusterCol = "Set1",
                     pointDat = structure(list(index = numeric(0), y = numeric(0),
                                               x = numeric(0), label = character(0), pointSize = numeric(0),
                                               pointAlpha = numeric(0), pointColor = character(0)), class = "data.frame", row.names = integer(0)),
                     plotRetNull = TRUE)

  # we cannot test the output, but we can test if the function runs without error
  expect_true(is.null(do.call(plotMap3D, testInputs)))

  # clusterAll = "-1"
  testInputs <- list(model = testModel, time = 1000, points = TRUE, pointSize = 1.3, rangex = c(-123.6565125,
                                                                                                -67.6565125), rangey = c(25.9146065, 53.9146065), estType = "Mean",
                     showModel = FALSE, rangez = c(0, 0.048), limitz = NULL, addU = 0L,
                     centerMap = "Europe", resolution = 100L, interior = 0, mask = FALSE,
                     maskRadius = 500L, ncol = 50L, pColor = "#2C2161", pointShape = 4,
                     textLabels = NULL, pointLabels = NULL, pointColLabels = NULL,
                     colorsP = "RdYlGn", fontSize = 1L, fontType = "serif", fontCol = "#2C2161",
                     terrestrial = "1", colors = "YlOrRd", reverseColors = FALSE,
                     arrow = TRUE, grid = FALSE, scale = TRUE, titleMain = TRUE,
                     titleScale = TRUE, showScale = FALSE, setAxisLabels = FALSE,
                     mainLabel = "", yLabel = "", xLabel = "", scLabel = "", northSize = 0.2,
                     scalSize = 0.1, scaleX = 0.01, scaleY = 0.1, NorthX = 0.16,
                     NorthY = 0.2, AxisSize = 1L, AxisLSize = 1L, cluster = TRUE,
                     clusterAll = "-1", clusterResults = "0", clusterCol = "Set1",
                     pointDat = structure(list(index = numeric(0), y = numeric(0),
                                               x = numeric(0), label = character(0), pointSize = numeric(0),
                                               pointAlpha = numeric(0), pointColor = character(0)), class = "data.frame", row.names = integer(0)),
                     plotRetNull = TRUE)

  # we cannot test the output, but we can test if the function runs without error
  expect_true(is.null(do.call(plotMap3D, testInputs)))
  })
