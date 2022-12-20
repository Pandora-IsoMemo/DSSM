test_that("Test module leafletPointSettings", {
  testServer(leafletPointSettingsServer,
             args = list(dataColnames = reactive(NULL)),
             {
               # Arrange
               print("test leaflet Point Settings")
               # Act
               session$setInputs(
                 clusterPoints = TRUE,
                 showLegend = FALSE,
                 pointRadiusPxl = 50,
                 useJitter = FALSE,
                 jitterMaxKm = 30
               )
               expect_equal(
                 names(session$returned()),
                 c(
                   "jitterMaxKm",
                   "showLegend",
                   "pointRadius",
                   "clusterPoints"
                 )
               )
               expect_true(is.na(session$returned()$jitterMaxKm))
               expect_equal(session$returned()$showLegend, FALSE)
               expect_equal(session$returned()$pointRadius, 50)
               expect_equal(session$returned()$clusterPoints, TRUE)
             })
})

test_that("Test module-leafletPointSettings if clusterPoints", {
  testServer(leafletPointSettingsServer,
             args = list(dataColnames = reactive(NULL)),
             {
               # Arrange
               print(paste("Testing leafletPointSettingsServer if clusterPoints"))

               # Act
               session$setInputs(clusterPoints = TRUE)

               # Assert
               expect_true(session$returned()$clusterPoints)
               expect_length(session$returned()$pointRadius, 0)
               expect_length(session$returned()$jitterMaxKm, 0)
             })
})

test_that("Test module-leafletPointSettings if not clusterPoints", {
  testServer(leafletPointSettingsServer,
             args = list(dataColnames = reactive(NULL)),
             {
               # Arrange
               print(paste("Testing leafletPointSettingsServer if clusterPoints"))

               # Act
               session$setInputs(clusterPoints = FALSE,
                                 showLegend = FALSE,
                                 pointRadiusPxl = 30,
                                 useJitter = TRUE,
                                 jitterMaxKm = 15)

               # Assert
               expect_equal(names(session$returned()),
                            c("jitterMaxKm", "showLegend", "pointRadius", "clusterPoints"))
               expect_false(session$returned()$clusterPoints)
               expect_false(session$returned()$showLegend)
               expect_equal(session$returned()$pointRadius, 30)
               expect_equal(session$returned()$jitterMaxKm, 15)
             })
})
