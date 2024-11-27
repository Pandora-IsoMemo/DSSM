# Mock input data for testing
# Create a dummy set of spatial data
dummy_map <- function(name) {
  sp::SpatialPolygonsDataFrame(
    Sr = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(cbind(
      c(-180, -180, 180, 180, -180),
      c(-90, 90, 90, -90, -90)
    ))), ID = "1"))),
    data = data.frame(dummy = 1, row.names = "1")
  )
}

Maps <- list(
  ocean160 = dummy_map("ocean160"),
  ocean200 = dummy_map("ocean200"),
  ocean = dummy_map("ocean"),
  land160 = dummy_map("land160"),
  land200 = dummy_map("land200"),
  land = dummy_map("land"),
  coast160 = dummy_map("coast160"),
  coast200 = dummy_map("coast200"),
  coast = dummy_map("coast"),
  grids160 = dummy_map("grids160"),
  grids200 = dummy_map("grids200"),
  grids = dummy_map("grids"),
  borders160 = dummy_map("borders160"),
  borders200 = dummy_map("borders200"),
  borders = dummy_map("borders")
)

# Tests for addMapLayers
test_that("addMapLayers handles terrestrial = 1 correctly with dummy data", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe", grid = TRUE))
  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific", grid = TRUE))
})

test_that("addMapLayers handles terrestrial = -1 correctly with dummy data", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Europe", grid = TRUE))
  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Pacific", grid = FALSE))
})

test_that("addMapLayers handles missing grid parameter correctly with dummy data", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe"))
  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Pacific"))
})

test_that("addMapLayers handles invalid inputs gracefully with dummy data", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_error(addMapLayers(NULL, terrestrial = 1, centerMap = "Europe"), "'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
  expect_error(addMapLayers(Maps, terrestrial = 2, centerMap = "Europe"), "'terrestrial' must be either 1 (ocean) or -1 (land).", fixed = TRUE)
})

# Load the existing maps
Maps <- loadMaps()

# Tests for addMapLayers
test_that("addMapLayers handles terrestrial = 1 correctly with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe", grid = TRUE))
  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific", grid = TRUE))
})

test_that("addMapLayers handles terrestrial = -1 correctly with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Europe", grid = TRUE))
  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Pacific", grid = FALSE))
})

test_that("addMapLayers handles missing grid parameter correctly with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe"))
  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Pacific"))
})

test_that("addMapLayers handles invalid inputs gracefully with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_error(addMapLayers(NULL, terrestrial = 1, centerMap = "Europe"), "'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
  expect_error(addMapLayers(Maps, terrestrial = 2, centerMap = "Europe"), "'terrestrial' must be either 1 (ocean) or -1 (land).", fixed = TRUE)
})
