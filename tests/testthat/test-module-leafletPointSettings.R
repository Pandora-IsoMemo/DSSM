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
                 c("jitterMaxKm", "pointColourPalette", "pointRadius", "clusterPoints")
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
                 c("jitterMaxKm", "pointColourPalette", "pointRadius", "clusterPoints",
                   "pointOpacity")
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
                 showLegend = FALSE,
                 paletteName = "Dark2",
                 isReversePalette = FALSE
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_true(all(
                 c(
                   "showLegend",
                   "columnForPointColour",
                   "pointColourPalette"
                 ) %in% names(session$returned)
               ))
               expect_false(session$returned$showLegend)
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
               session$setInputs(
                 columnForPointSize = "b",
                 sizeFactor = FALSE
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(
                 names(session$returned),
                 c(
                   "pointRadius"
                 )
               )
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

  expect_equal(getPointSize(df = testDf, columnForPointSize = "b"),
               c(0.1, 4, 8))
  expect_equal(getPointSize(
    df = testDf,
    columnForPointSize = "b",
    sizeFactor = 2
  ),
  c(0.2, 8, 16))
  expect_equal(
    getPointSize(df = testDf, columnForPointSize = "c") %>% round(digits = 1),
    c(0.1, 1.1, 8)
  )
  expect_equal(getPointSize(df = testDf, columnForPointSize = "a"),
               c(0.1, 0, 8))
  expect_equal(getPointSize(df = testDf, columnForPointSize = "d"),
               c(4, 4, 4))
})
