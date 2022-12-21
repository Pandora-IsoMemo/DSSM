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
               session$setInputs(
                 clusterPoints = TRUE,
                 customPoints = FALSE,
                 useJitter = FALSE
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(names(session$returned),
                            c("clusterPoints", "pointColourPalette", "jitterMaxKm"))
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
                 customPoints = TRUE,
                 pointRadiusPxl = 30
               )

               # Assert
               expect_equal(colnames(loadedData()), c("a", "b", "c"))
               expect_equal(names(session$returned),
                            c("jitterMaxKm", "pointColourPalette", "pointRadius", "clusterPoints"
                            ))
               expect_false(session$returned$clusterPoints)
               expect_equal(session$returned$pointRadius, 30)
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
               expect_equal(
                 names(session$returned),
                 c("showLegend", "columnForPointColour", "pointColourPalette")
               )
               expect_false(session$returned$showLegend)
               expect_equal(session$returned$columnForPointColour, "source")
               expect_true(is.function(session$returned$pointColourPalette))
             })
})
