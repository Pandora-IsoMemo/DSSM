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
                                 useJitter = FALSE)

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(
                 names(session$returned),
                 c(
                   "jitterMaxKm",
                   "pointColourPalette",
                   "pointRadius",
                   "pointSymbol",
                   "clusterPoints",
                   "columnForPointSymbol",
                   "showSymbolLegend"
                 )
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
                 pointOpacity = 0.5
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(
                 names(session$returned),
                 c(
                   "jitterMaxKm",
                   "pointColourPalette",
                   "pointRadius",
                   "pointSymbol",
                   "clusterPoints",
                   "columnForPointSymbol",
                   "showSymbolLegend",
                   "pointOpacity"
                 )
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
             ))),
             {
               # Arrange
               print("test pointColourServer")

               # Act
               session$setInputs(
                 columnForPointColour = "source",
                 showColourLegend = FALSE,
                 paletteName = "Dark2",
                 isReversePalette = FALSE
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(
                 names(session$returned),
                 c(
                   "showColourLegend",
                   "columnForPointColour",
                   "pointColourPalette"
                 )
               )
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
             ))),
             {
               # Arrange
               print("test pointSizeServer")

               # Act
               session$setInputs(columnForPointSize = "b",
                                 sizeFactor = FALSE)

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(names(session$returned),
                            c("pointRadius"))
               expect_equal(session$returned$pointRadius, c(0, 0, 0))
             })
})

test_that("Test getPointSize", {
  testDf <- data.frame(
    a = c(1, NA, 3),
    b = 5:7,
    c = c(3, 4, 10),
    d = c(4, 4, 4)
  )

  expect_null(getPointSize(df = NULL, columnForPointSize = "b"))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "b"),
               c(2, 5, 8))
  expect_equal(getPointSize(
    df = testDf,
    columnForPointSize = "b",
    sizeFactor = 2
  ),
  c(4, 10, 16))
  expect_equal(
    getPointSize(df = testDf, columnForPointSize = "c") %>% round(digits = 1),
    c(2.0, 2.9, 8.0)
  )
  expect_equal(getPointSize(df = testDf, columnForPointSize = "a"),
               c(2, 0, 8))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "d"),
               c(5, 5, 5))
})

test_that("Test getPointSymbols", {
  testDf <- data.frame(
    a = c(1, NA, 3),
    b = 5:7,
    c = c(3, 4, 10),
    d = c(4, 4, 4)
  )

  # test case if data is missing
  expect_null(getPointSymbols(NULL, columnForPointSymbols = ""))
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "", symbols = ""),
    list(19, 19, 19)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "", symbols = "5"),
    list(5, 5, 5)
  )

  # test columns for symbols
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "a"),
    list(0, "", 1)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "b"),
    list(0, 1, 2)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "c"),
    list(0, 1, 2)
  )
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "d", symbols = unlist(pchChoices())[5:10]),
    list(4, 4, 4)
  )
  # test case if not enough symbols selected
  expect_equal(
    getPointSymbols(testDf, columnForPointSymbols = "c", symbols = unlist(pchChoices())[c(1, 7)]),
    list(0, 6, 1)
  )
  # test case if not enough symbols available
  expect_equal(
    getPointSymbols(data.frame(x = 1:25), columnForPointSymbols = "x", symbols = unlist(pchChoices())),
    list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
         17, 18, 19, 20, 0, 1, 2, 3)
  )
})


test_that("Test getSymbolLegend", {
  expect_type(getSymbolLegend(c("expert" = 19, "radiocarbon" = 0)), "character")
  expect_length(getSymbolLegend(c("expert" = 19, "radiocarbon" = 0)), 1)
})
