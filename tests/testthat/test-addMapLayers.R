# Load the existing maps
Maps <- loadMaps()

# Tests clipping
test_that("clipMap produces valid clipped SpatialLines object", {
  # Test parameters
  xlim <- c(0, 360)
  ylim <- c(10, 70)

  # Run the function
  clipped <- clipMap(Maps$`ocean+180`, layer = "ocean", xlim = xlim, ylim = ylim, mapLand = Maps$`land+180`)

  # Check the class
  expect_s4_class(clipped, "SpatialPolygons")

  # Check the feature count (non-zero)
  expect_true(length(clipped) > 0)

  # Check the extent
  clip_extent <- sp::bbox(clipped)
  tolerance <- clipTolerance() + 1e-0
  expect_true(clip_extent["x", "min"] >= xlim[1] - tolerance)
  expect_true(clip_extent["x", "max"] <= xlim[2] + tolerance)
  expect_true(clip_extent["y", "min"] >= ylim[1] - tolerance)
  expect_true(clip_extent["y", "max"] <= ylim[2] + tolerance)
})

# Tests for addMapLayers
test_that("addMapLayers handles terrestrial = 1 correctly with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-10, 80), ylim = c(-90, 90))  # Define plot window

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe", xlim = c(-10, 40), ylim = c(10, 70)))
  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific"))
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


test_that("addMapLayers to contour with terrestrial maps", {
  # Create example data
  x <- seq(-10, 80, length.out = 50)  # Longitude range
  y <- seq(10, 60, length.out = 50)   # Latitude range
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))  # Generate z-values

  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-40, 120), ylim = c(-90, 90))  # Define plot window

  # add simple contour for testing
  .filled.contour(x, y, z,
                  levels = pretty(range(z), n = 10),  # Define contour levels
                  col = terrain.colors(10))          # Use a color palette

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe", xlim = c(-40, 120), ylim = c(-90, 90)))
  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Europe"))
  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific"))
})

test_that("addMapLayers to contour with terrestrial maps for Pacific", {
  # Create example data
  x <- seq(-190, -130, length.out = 50)  # Longitude range
  y <- seq(20, 60, length.out = 50)   # Latitude range
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))  # Generate z-values

  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(0, 360), ylim = c(-90, 90))  # Define plot window

  # add simple contour for testing
  .filled.contour(x, y, z,
                  levels = pretty(range(z), n = 10),  # Define contour levels
                  col = terrain.colors(10))          # Use a color palette

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific", xlim = c(0, 360), ylim = c(-90, 90)))

  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-200, -120), ylim = c(10, 70))  # Define plot window

  # add simple contour for testing
  .filled.contour(x, y, z,
                  levels = pretty(range(z), n = 10),  # Define contour levels
                  col = terrain.colors(10))          # Use a color palette

  expect_silent(addMapLayers(Maps, terrestrial = 1, centerMap = "Pacific", xlim = c(-200, -120), ylim = c(10, 70)))
})


test_that("addMapLayers to contour with aquatic maps", {
  # Create example data
  x <- seq(-10, 80, length.out = 50)  # Longitude range
  y <- seq(10, 60, length.out = 50)   # Latitude range
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))  # Generate z-values

  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  # add simple contour for testing
  .filled.contour(x, y, z,
                  levels = pretty(range(z), n = 10),  # Define contour levels
                  col = terrain.colors(10))          # Use a color palette

  expect_silent(addMapLayers(Maps, terrestrial = -1, centerMap = "Pacific"))
})

test_that("addMapLayers to contour with full maps", {
  # Create example data
  x <- seq(-10, 80, length.out = 50)  # Longitude range
  y <- seq(10, 60, length.out = 50)   # Latitude range
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))  # Generate z-values

  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  # add simple contour for testing
  .filled.contour(x, y, z,
                  levels = pretty(range(z), n = 10),  # Define contour levels
                  col = terrain.colors(10))          # Use a color palette

  expect_silent(addMapLayers(Maps, terrestrial = 0, centerMap = "Pacific"))
})

test_that("addMapLayers handles invalid inputs gracefully with saved maps", {
  # Initialize a blank plot
  plot.new()  # Start a new plot
  plot.window(xlim = c(-180, 180), ylim = c(-90, 90))  # Define plot window

  expect_error(addMapLayers(NULL, terrestrial = 1, centerMap = "Europe"), "'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
})
