context("Customize Points on Leaflet Map")

test_that("Test module-leafletPointSettings if clusterPoints", {
  testServer(leafletPointSettingsServer,
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
             {
               # Arrange
               print(paste("Testing leafletPointSettingsServer if clusterPoints"))

               # Act
               session$setInputs(clusterPoints = FALSE,
                                 pointRadiusKm = 30,
                                 useJitter = TRUE,
                                 jitterMaxKm = 15)

               # Assert
               expect_false(session$returned()$clusterPoints)
               expect_equal(session$returned()$pointRadius, 30000)
               expect_equal(session$returned()$jitterMaxKm, 15)
             })
})
