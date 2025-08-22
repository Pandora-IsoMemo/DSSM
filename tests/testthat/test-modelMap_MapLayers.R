# Load the existing maps
Maps <- loadMaps()

# clipMap tests ----

test_that("clipMap (Pacific) produces valid clipped SpatialLines object", {
  xlim <- c(0, 360)
  ylim <- c(10, 70)

  clipped <- clipMap(Maps$`ocean+180`, layer = "ocean",
                     xlim = xlim, ylim = ylim, mapLand = Maps$`land+180`)

  expect_s4_class(clipped, "SpatialPolygons")
  expect_true(length(clipped) > 0)

  clip_extent <- sp::bbox(clipped)
  tolerance <- clipTolerance() + 1e-0
  expect_true(clip_extent["x", "min"] >= xlim[1] - tolerance)
  expect_true(clip_extent["x", "max"] <= xlim[2] + tolerance)
  expect_true(clip_extent["y", "min"] >= ylim[1] - tolerance)
  expect_true(clip_extent["y", "max"] <= ylim[2] + tolerance)
})

test_that("clipMap (Europe) produces valid clipped SpatialLines object", {
  xlim <- c(-180, 180)
  ylim <- c(-90, 90)

  clipped <- Maps$`ocean` %>% clipMap(layer = "ocean",
                                      xlim = xlim, ylim = ylim, mapLand = Maps$`land`)

  expect_s4_class(clipped, "SpatialPolygons")
  expect_true(length(clipped) > 0)

  clip_extent <- sp::bbox(clipped)
  tolerance <- clipTolerance() + 1e-0
  expect_true(clip_extent["x", "min"] >= xlim[1] - tolerance)
  expect_true(clip_extent["x", "max"] <= xlim[2] + tolerance)
  expect_true(clip_extent["y", "min"] >= ylim[1] - tolerance)
  expect_true(clip_extent["y", "max"] <= ylim[2] + tolerance)
})

test_that("clipMap (ocean) works for different ranges", {
  for (z in seq(10, 80, by = 10)) {
    cat(".")
    xlim <- c(-2*z, 180)
    ylim <- c(-z, 90)

    clipped <- Maps$`ocean` %>% clipMap(layer = "ocean", xlim = xlim, ylim = ylim, mapLand = Maps$`land`)
    expect_true(length(clipped) > 0)
  }

  for (z in seq(10, 80, by = 10)) {
    cat(".")
    xlim <- c(-2*z, 180) + 180
    ylim <- c(-z, 90)

    slitted_xlim <- splitXlim(xlim)
    clipped1 <- Maps$`ocean-180` %>%
      clipMap(layer = "ocean", xlim = slitted_xlim$left, ylim = ylim, mapLand = Maps$`land-180`)
    clipped2 <- Maps$`ocean+180` %>%
      clipMap(layer = "ocean", xlim = slitted_xlim$right, ylim = ylim, mapLand = Maps$`land+180`)
    expect_true(length(clipped1) > 0)
    expect_true(length(clipped2) > 0)
  }
})

test_that("clipMap (coast) works for different ranges", {
  for (z in seq(10, 80, by = 10)) {
    cat(".")
    xlim <- c(-2*z, 180)
    ylim <- c(-z, 90)

    clipped <- Maps$`coast` %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim)
    expect_s4_class(clipped, "SpatialLinesDataFrame")
    expect_true(length(clipped) > 0)
  }

  for (z in seq(10, 80, by = 10)) {
    cat(".")
    xlim <- c(-2*z, 180) + 180
    ylim <- c(-z, 90)

    # (kept same as original test; using full xlim for both halves)
    clipped1 <- Maps$`coast-180` %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim)
    clipped2 <- Maps$`coast+180` %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim)
    expect_s4_class(clipped1, "SpatialLinesDataFrame")
    expect_s4_class(clipped2, "SpatialLinesDataFrame")
    expect_true(length(clipped1) > 0)
    expect_true(length(clipped2) > 0)
  }
})

# New tests using MapLayers class + methods ----

test_that("plot.MapLayers handles terrestrial = 1 correctly (Europe & Pacific)", {
  # Europe with limits
  plot.new(); plot.window(xlim = c(-10, 80), ylim = c(-90, 90))
  ml1 <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Europe",
                       xlim = c(-10, 40), ylim = c(10, 70))
  expect_silent(plot(ml1))

  # Pacific without limits (full layer)
  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  ml2 <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific")
  expect_silent(plot(ml2))
})

test_that("plot.MapLayers handles terrestrial = -1 correctly (Europe & Pacific)", {
  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  ml1 <- new_MapLayers(Maps, terrestrial = -1, centerMap = "Europe", grid = TRUE)
  expect_silent(plot(ml1))

  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  ml2 <- new_MapLayers(Maps, terrestrial = -1, centerMap = "Pacific", grid = FALSE)
  expect_silent(plot(ml2))
})

test_that("new_MapLayers defaults (grid missing) behave and plot works", {
  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  ml1 <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Europe")
  expect_silent(plot(ml1))

  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  ml2 <- new_MapLayers(Maps, terrestrial = -1, centerMap = "Pacific")
  expect_silent(plot(ml2))
})

test_that("plot.MapLayers overlays on contour (terrestrial = 1, Europe & Pacific)", {
  # Create example data
  x <- seq(-10, 80, length.out = 50)
  y <- seq(10, 60, length.out = 50)
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))

  # Europe
  plot.new(); plot.window(xlim = c(-40, 120), ylim = c(-90, 90))
  .filled.contour(x, y, z, levels = pretty(range(z), n = 10), col = terrain.colors(10))
  mlE <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Europe",
                       xlim = c(-40, 120), ylim = c(-90, 90))
  expect_silent(plot(mlE))
  # also without limits
  expect_silent(plot(new_MapLayers(Maps, terrestrial = 1, centerMap = "Europe")))

  # Pacific (global window)
  plot.new(); plot.window(xlim = c(0, 360), ylim = c(-90, 90))
  .filled.contour(x, y, z, levels = pretty(range(z), n = 10), col = terrain.colors(10))
  mlP <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific",
                       xlim = c(0, 360), ylim = c(-90, 90))
  expect_silent(plot(mlP))

  # Pacific (local window)
  plot.new(); plot.window(xlim = c(-200, -120), ylim = c(10, 70))
  .filled.contour(seq(-190, -130, length.out = 50), y,
                  outer(y, seq(-190, -130, length.out = 50),
                        function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi)),
                  levels = pretty(range(z), n = 10), col = terrain.colors(10))
  mlP2 <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific",
                        xlim = c(-200, -120), ylim = c(10, 70))
  expect_silent(plot(mlP2))
})

test_that("plot.MapLayers overlays on contour (terrestrial = -1, Pacific)", {
  x <- seq(-10, 80, length.out = 50)
  y <- seq(10, 60, length.out = 50)
  z <- outer(y, x, function(y, x) sin(x / 180 * pi) * cos(y / 90 * pi))

  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  .filled.contour(x, y, z, levels = pretty(range(z), n = 10), col = terrain.colors(10))
  ml <- new_MapLayers(Maps, terrestrial = -1, centerMap = "Pacific")
  expect_silent(plot(ml))
})

test_that("can draw both base types by plotting twice (replacing 'full maps' case)", {
  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  expect_silent(plot(new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific")))
  expect_silent(plot(new_MapLayers(Maps, terrestrial = -1, centerMap = "Pacific")))
})

test_that("layer methods can be called directly without error", {
  plot.new(); plot.window(xlim = c(-20, 60), ylim = c(-50, 80))
  ml <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Europe",
                      grid = TRUE, showBorders = TRUE,
                      xlim = c(-20, 60), ylim = c(-50, 80))
  expect_silent(add_ocean(ml))
  expect_silent(add_coast(ml))
  expect_silent(add_grids(ml))
  expect_silent(add_borders(ml))
  expect_silent(add_center_line(ml))
})

test_that("new_MapLayers handles invalid inputs gracefully", {
  plot.new(); plot.window(xlim = c(-180, 180), ylim = c(-90, 90))
  expect_error(
    new_MapLayers(NULL, terrestrial = 1, centerMap = "Europe"),
    "'Maps' cannot be NULL. Please provide a valid 'Maps' object."
  )
  expect_error(
    new_MapLayers(Maps, terrestrial = 0.5, centerMap = "Europe"),
    "'terrestrial' must be 1 \\(show land, fill ocean\\), -1 \\(show ocean, fill land\\) or any other integer \\(show all, fill none\\)\\."
  )
})
