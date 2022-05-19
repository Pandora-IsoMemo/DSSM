test_that("Test module leafletPointSettings", {
  testServer(leafletPointSettingsServer,
             {
               # Arrange
               print("test leaflet Point Settings")
               # Act
               session$setInputs(
                 clusterPoints = TRUE,
                 showLegend = FALSE,
                 pointRadiusKm = 50,
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
               expect_equal(session$returned()$pointRadius, 50000)
               expect_equal(session$returned()$clusterPoints, TRUE)
             })
})
