test_that("Test module leafletPointSettings if clusterPoints", {
  testServer(leafletPointSettingsServer,
             args = list(loadedData = reactive(data.frame(
               a = 1:3,
               b = 5:7,
               c = 9:11
             ))),
             {
               # Arrange
               print("test leaflet Point Settings if clusterPoints")
               # Act
               session$setInputs(clusterPoints = TRUE,
                                 useJitter = FALSE,
                                 applyPointSettings = 0)

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_true(
                 all(c(
                   "jitterMaxKm",
                   "pointColourPalette",
                   "pointRadius",
                   "pointSymbol",
                   "symbolLegendValues",
                   "clusterPoints",
                   "columnForPointSymbol",
                   "showSymbolLegend"
                 ) %in% names(session$returned))
               )
               expect_true(is.na(session$returned$jitterMaxKm))
               expect_true(session$returned$clusterPoints)
               expect_null(session$returned$pointColourPalette)
             })
})


test_that("Test module-leafletPointSettings if not clusterPoints", {
  testServer(leafletPointSettingsServer,
             args = list(loadedData = reactive(data.frame(
               a = 1:3,
               b = 5:7,
               c = 9:11
             ))),
             {
               # Arrange
               print(paste("Testing leafletPointSettingsServer if not clusterPoints"))

               # Act
               session$setInputs(
                 clusterPoints = FALSE,
                 useJitter = TRUE,
                 jitterMaxKm = 15,
                 pointOpacity = 0.5,
                 applyPointSettings = 1
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_true(
                 all(c(
                   "jitterMaxKm",
                   "pointColourPalette",
                   "pointRadius",
                   "pointSymbol",
                   "symbolLegendValues",
                   "clusterPoints",
                   "columnForPointSymbol",
                   "showSymbolLegend",
                   "pointOpacity"
                 ) %in% names(session$returned))
               )
               expect_false(session$returned$clusterPoints)
               expect_equal(session$returned$pointOpacity, 0.5)
               expect_equal(session$returned$jitterMaxKm, 15)
               expect_null(session$returned$pointColourPalette)
             })
})


test_that("Test module pointColourServer", {
  testServer(pointColourServer,
             args = list(loadedData = reactive(data.frame(
               a = 1:3,
               b = 5:7,
               c = 9:11
             )), apply = reactive(1)),
             {
               # Arrange
               print("test pointColourServer")

               # Act
               session$setInputs(
                 `colour-fixed` = FALSE,
                 `colour-column` = "source",
                 `colour-legend` = FALSE,
                 paletteName = "Dark2",
                 isReversePalette = FALSE
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_true(all(
                 c(
                   "showColourLegend",
                   "columnForPointColour",
                   "pointColourPalette"
                 ) %in% names(session$returned)
               ))
               expect_false(session$returned$showColourLegend)
               expect_equal(session$returned$columnForPointColour, "source")
               expect_true(is.function(session$returned$pointColourPalette))
             })
})

test_that("Test module pointSizeServer", {
  testServer(pointSizeServer,
             args = list(loadedData = reactive(data.frame(
               a = 1:3,
               b = 5:7,
               c = 9:11
             )), apply = reactive(1)),
             {
               # Arrange
               print("test pointSizeServer")

               # Act
               session$setInputs(`size-column` = "b",
                                 sizeFactor = FALSE)

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_true(all(c("pointRadius") %in% names(session$returned)))
               expect_equal(session$returned$pointRadius, c(0, 0, 0))
             })
})

test_that("Test getPointSize", {
  testDf <- data.frame(
    a = c(1, NA, 3),
    b = 5:7,
    c = c(3, 4, 10),
    d = c(4, 4, 4),
    e = c(1.464, 1.9664, 2.0325)
  )

  expect_null(getPointSize(df = NULL, columnForPointSize = "b")$pointSizes)
  expect_equal(getPointSize(df = testDf, columnForPointSize = "b")$pointSizes,
               c(2, 5, 8))
  expect_equal(getPointSize(
    df = testDf,
    columnForPointSize = "b",
    sizeFactor = 2
  )$pointSizes,
  c(4, 10, 16))
  expect_equal(
    getPointSize(df = testDf, columnForPointSize = "c")$pointSizes %>% round(digits = 1),
    c(2.0, 2.9, 8.0)
  )
  expect_equal(getPointSize(df = testDf, columnForPointSize = "a")$pointSizes,
               c(2, 0, 8))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "d")$pointSizes,
               c(5, 5, 5))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "e")$pointSizes,
               c(2, 7.30237467018469, 8))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "e")$sizeLegendValues,
               c(`1.5` = 2.37994722955145, `1.7` = 4.49076517150396, `1.8` = 5.54617414248021,
                 `2` = 7.65699208443271))
})

test_that("Test getPointSymbols", {
  testDf <- data.frame(
    a = c(1, NA, 3),
    b = 5:7,
    c = c(3, 4, 10),
    d = c(4, 4, 4)
  )

  # test case if data is missing
  expect_null(getPointSymbols(NULL, columnForPointSymbol = ""))
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "", symbols = "")$pointSymbols,
    list(19, 19, 19)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "", symbols = "5")$pointSymbols,
    list(5, 5, 5)
  )

  # test columns for symbols
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "a")$pointSymbols,
    list(0, "", 1)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "b")$pointSymbols,
    list(0, 1, 2)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "c")$pointSymbols,
    list(0, 1, 2)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "d", symbols = unlist(pchChoices())[5:10])$pointSymbols,
    list(4, 4, 4)
  )
  # test case if not enough symbols selected
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbol = "c", symbols = unlist(pchChoices())[c(1, 7)])$pointSymbols,
    list(0, 6, 1)
  )
  # test case if not enough symbols available
  expect_equal(
    getPointSymbols(data.frame(x = 1:25), columnForPointSymbol = "x", symbols = unlist(pchChoices()))$pointSymbols,
    list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
         17, 18, 19, 20, 0, 1, 2, 3)
  )
})

# runs only locally:
# test_that("Test getSymbolLegend", {
#   expect_type(getSymbolLegend(c("expert" = 19, "radiocarbon" = 0),
#                               pathToSymbols = file.path("inst", "app", "www")), "character")
#   expect_length(getSymbolLegend(c("expert" = 19, "radiocarbon" = 0),
#                                 pathToSymbols = file.path("inst", "app", "www")), 1)
# })
