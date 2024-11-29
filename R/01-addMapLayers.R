# Add map layers to a plot
#
# This function adds map layers to a plot. It is designed to be used in conjunction with the `plot` function.
# Following layers are added: ocean, land, coastlines, grid lines, and borders.
#
# @param Maps A list of spatial objects containing map layers.
# @param terrestrial An integer specifying the type of map layer to add. Possible values are 1 (ocean) and -1 (land).
# @param centerMap A character string specifying the center of the map. Possible values are "Europe" and "Pacific".
# @param grid A logical indicating whether to add grid lines to the plot.
# @param centerLine A logical indicating whether to add a vertical line at the center longitude.
addMapLayers <- function(Maps, terrestrial, centerMap, grid = FALSE, centerLine = FALSE) {
  # Validate input
  if (is.null(Maps)) {
    stop("'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
  }
  if (!terrestrial %in% c(1, -1)) {
    stop("'terrestrial' must be either 1 (ocean) or -1 (land).")
  }

  # Draw base maps
  if (terrestrial == 1) {
    if (centerMap != "Europe") {
      #sp::plot(Maps$ocean160, add = TRUE, col = "lightblue", lwd = 1, border = NA)
      #sp::plot(Maps$ocean200, add = TRUE, col = "lightblue", lwd = 1, border = NA)
      sp::plot(Maps$ocean180, add = TRUE, col = "lightblue", lwd = 1, border = NA)
    } else {
      sp::plot(Maps$ocean, add = TRUE, col = "lightblue", lwd = 1)
    }
  }
  if (terrestrial == -1) {
    if (centerMap != "Europe") {
      #sp::plot(Maps$land160, add = TRUE, col = "grey96", lwd = 1, border = NA)
      #sp::plot(Maps$land200, add = TRUE, col = "grey96", lwd = 1, border = NA)
      sp::plot(Maps$land180, add = TRUE, col = "grey96", lwd = 1, border = NA)
    } else {
      sp::plot(Maps$land, add = TRUE, col = "grey96", lwd = 1, border = NA)
    }
  }

  # Add coastlines
  if (centerMap != "Europe") {
    #sp::plot(Maps$coast160, add = TRUE, lwd = 1)
    #sp::plot(Maps$coast200, add = TRUE, lwd = 1)
    sp::plot(Maps$coast180, add = TRUE, lwd = 1)
  } else {
    sp::plot(Maps$coast, add = TRUE, lwd = 1)
  }

  # Add grid lines
  if (grid) {
    if (centerMap != "Europe") {
      #sp::plot(Maps$grids160, add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
      #sp::plot(Maps$grids200, add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
      sp::plot(Maps$grids180, add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
    } else {
      sp::plot(Maps$grids, add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
    }
  }

  # Add borders
  if (centerMap != "Europe") {
    #sp::plot(Maps$borders160, add = TRUE, col = "darkgrey", lwd = 1)
    #sp::plot(Maps$borders200, add = TRUE, col = "darkgrey", lwd = 1)
    sp::plot(Maps$borders180, add = TRUE, col = "darkgrey", lwd = 1)
  } else {
    sp::plot(Maps$borders, add = TRUE, col = "darkgrey", lwd = 1)
  }

  if (centerLine) {
    # add vertical line at center longitude
    abline(v = 0, col = "blue", lwd = 2)
  }
}
