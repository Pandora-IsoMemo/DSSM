test_that("Test module locationFields", {
  testData <-
    structure(
      list(
        Sample.ID = c("LH-1", "LH-12", "LH-13", "LH-2",
                      "LH-3", "LH-4"),
        Sample.date = c(
          "Sept.(2012)",
          "Sept.(2012)",
          "Sept.(2012)",
          "Sept.(2012)",
          "Sept.(2012)",
          "Sept.(2012)"
        ),
        Species = c(
          "Bothriochloa ischaemum",
          "Stipa bungeana",
          "Agropyron cristatum",
          "Artemisia",
          "no identification",
          "Lespedeza davurica"
        ),
        Location = c(
          "Lantian",
          "Lantian",
          "Lantian",
          "Lantian",
          "Lantian",
          "Lantian"
        ),
        Latitude = c(
          "34°14′N",
          "34°25′N",
          "34°25′N",
          "34°15′N",
          "34°16′N",
          "34°17′N"
        ),
        Longitude = c(
          "109°7′E",
          "109°18′E",
          "109°18′E",
          "109°8′E",
          "109°9′E",
          "109°10′E"
        ),
        `Altitude.(m)` = c(619,
                           619, 619, 619, 619, 619),
        Family = c(
          "Poaceae",
          "Poaceae",
          "Poaceae",
          "Asteraceae",
          "Asteraceae",
          "Leguminosae"
        ),
        Class = c(
          "Monocotyledoneae",
          "Monocotyledoneae",
          "Monocotyledoneae",
          NA,
          "Dictyledoneae",
          "Dictyledoneae"
        ),
        Plant.life.forms = c("grass", "grass",
                             "grass", "grass", "shrub", "shrub"),
        Photosynthetic.pathway = c("C4",
                                   "C3", "C3", "C3", NA, "C3"),
        `δDC29.n-alkane(‰)` = c(-207,
                                -222, -185, -224, -158, -172)
      ),
      row.names = c(NA, 6L),
      class = "data.frame"
    )

  testServer(locationFieldsServer, args = list(
    dataRaw = reactive(testData),
    dataSource = reactive("file")),
             {
               # Arrange
               print("test locationFieldsServer")
               # Act
               session$setInputs(coordType = "degrees decimal minutes",
                                 longitude = "Longitude",
                                 latitude = "Latitude")

               # DISABLE TEST for dataSource as long as the condition on datasource is disabled
               #expect_equal(output$dataSource, "file")
               expect_equal(
                 names(session$returned),
                 c("coordType", "longitudeColname", "latitudeColname")
               )
               expect_equal(session$returned$coordType(), "degrees decimal minutes")
               expect_equal(session$returned$longitude(), "Longitude")
               expect_equal(session$returned$latitude(), "Latitude")
             })
})
