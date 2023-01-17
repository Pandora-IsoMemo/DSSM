test_that("Test module numericInputLatAndLongServer", {

  testServer(numericInputLatAndLongServer,
    {
      # Arrange
      print("test numericInputLatAndLongServer")
      # Act
      session$setInputs(latitude = 10,
                        longitude = 50)

      expect_equal(names(session$returned), c("longitude", "latitude"))
      expect_equal(session$returned$longitude(), 50)
      expect_equal(session$returned$latitude(), 10)
    })

  testServer(numericInputLatAndLongServer, args = list(
    valueLat = reactive(25),
    valueLong = reactive(70)),
    {
      # Arrange
      print("test numericInputLatAndLongServer")
      # Act
      session$setInputs(latitude = 10,
                        longitude = 50)

      expect_equal(names(session$returned), c("longitude", "latitude"))
      expect_equal(session$returned$longitude(), 50)
      expect_equal(session$returned$latitude(), 10)
    })
})
