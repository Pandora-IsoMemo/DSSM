testthat::test_that("function augmentData", {
  testData <- structure(
    list(
      d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
      longitude = c(198.2955, 208.5167, 198.2955, 198.7, 203.0014, 200.897),
      latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257),
      Site = 1:6 %>% as.character(),
      d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
      Longitude = c(198.2955, 208.5167, 198.2955, 198.7, 203.0014, 200.897),
      Latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257)
    ),
    row.names = c(1502L, 1139L, 1516L, 2708L, 2398L, 3097L),
    class = "data.frame"
  )

  expect_equal(
    augmentData(testData),
    structure(list(d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9,
                            -15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
                   longitude = c(198.2955, 208.5167, 198.2955, 198.7, 203.0014, 200.897, 198.2955,
                                 208.5167, 198.2955, 198.7, 203.0014, 200.897),
                   latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257, -32.3584,
                                -46.3, -32.3584, -32.4167, -52.1639, -52.257),
                   Site = 1:12,
                   d13C = c(-15.5, -18.8,
                            -14.3, -14.8, -19.2, -19.9, -15.5, -18.8, -14.3, -14.8, -19.2,  -19.9),
                   Longitude = c(198.2955, 208.5167, 198.2955, 198.7, 203.0014, 200.897, -161.7045,
                                 -151.4833, -161.7045, -161.3, -156.9986, -159.103),
                   Latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257, -32.3584,
                                -46.3, -32.3584, -32.4167, -52.1639, -52.257)),
              row.names = c("1502", "1139", "1516", "2708", "2398", "3097",
                            "15021", "11391", "15161", "27081", "23981", "30971"),
              class = "data.frame")
  )
})

testthat::test_that("function centerData", {
  testData <- structure(
    list(
      d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
      longitude = c(198.2955, -151.4833, 198.2955, -161.3000, 203.0014, -159.1030),
      latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257),
      Site = 1:6 %>% as.character(),
      d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
      Longitude = c(198.2955, -151.4833, 198.2955, -161.3000, 203.0014, -159.1030),
      Latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257)
    ),
    row.names = c(1502L, 1139L, 1516L, 2708L, 2398L, 3097L),
    class = "data.frame"
  )

  expect_equal(
    centerData(testData, center = "Pacific"),
    structure(list(d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
                   longitude = c(198.2955, -151.4833, 198.2955, -161.3, 203.0014, -159.103),
                   latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257),
                   Site = c("1", "2", "3", "4", "5", "6"),
                   d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
                   Longitude = c(198.2955, 208.5167, 198.2955, 198.7, 203.0014, 200.897),
                   Latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257)),
              row.names = c(1502L, 1139L, 1516L, 2708L, 2398L, 3097L),
              class = "data.frame")
  )

  expect_equal(
    centerData(testData, center = "Europe"),
    structure(list(d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
                   longitude = c(198.2955, -151.4833, 198.2955, -161.3, 203.0014, -159.103),
                   latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257),
                   Site = c("1", "2", "3", "4", "5", "6"),
                   d13C = c(-15.5, -18.8, -14.3, -14.8, -19.2, -19.9),
                   Longitude = c(-161.7045, -151.4833, -161.7045, -161.3, -156.9986, -159.103),
                   Latitude = c(-32.3584, -46.3, -32.3584, -32.4167, -52.1639, -52.257)),
              row.names = c(1502L, 1139L, 1516L, 2708L, 2398L, 3097L),
              class = "data.frame")
  )
})
