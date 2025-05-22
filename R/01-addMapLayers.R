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
addMapLayers <- function(Maps, terrestrial, centerMap, grid = FALSE, centerLine = FALSE, xlim = NULL, ylim = NULL) {
  # Validate input
  if (is.null(Maps)) {
    stop("'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
  }

  # Draw base maps
  if (as.numeric(terrestrial) == 1) {
    if (centerMap != "Europe") {
      slitted_xlim <- splitXlim(xlim)
      # x: -360 to 0
      if (!is.null(slitted_xlim$left)) {
        Maps$`ocean-180` %>%
          clipMap(layer = "ocean", xlim = slitted_xlim$left, ylim = ylim, mapLand = Maps$`land-180`) %>%
          sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
      }
      # x: 0 to 360
      if (!is.null(slitted_xlim$right)) {
        Maps$`ocean+180` %>%
          clipMap(layer = "ocean", xlim = slitted_xlim$right, ylim = ylim, mapLand = Maps$`land+180`) %>%
          sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
      }
    } else {
      Maps$`ocean` %>%
        clipMap(layer = "ocean", xlim = xlim, ylim = ylim, mapLand = Maps$`land`) %>%
        sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
    }
  }
  if (as.numeric(terrestrial) == -1) {
    if (centerMap != "Europe") {
      sp::plot(Maps$`land-180` %>% clipMap(layer = "land", xlim = xlim, ylim = ylim), add = TRUE, col = "grey96", lwd = 1, border = NA)
      sp::plot(Maps$`land+180` %>% clipMap(layer = "land", xlim = xlim, ylim = ylim), add = TRUE, col = "grey96", lwd = 1, border = NA)
    } else {
      sp::plot(Maps$land %>% clipMap(layer = "land", xlim = xlim, ylim = ylim), add = TRUE, col = "grey96", lwd = 1, border = NA)
    }
  }

  # Add coastlines
  if (centerMap != "Europe") {
    sp::plot(Maps$`coast-180` %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim), add = TRUE, lwd = 1)
    sp::plot(Maps$`coast+180` %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim), add = TRUE, lwd = 1)
  } else {
    sp::plot(Maps$coast %>% clipMap(layer = "coast", xlim = xlim, ylim = ylim), add = TRUE, lwd = 1)
  }

  # Add grid lines
  if (grid) {
    if (centerMap != "Europe") {
      sp::plot(Maps$`grids-180` %>% clipMap(layer = "grid", xlim = xlim, ylim = ylim), add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
      sp::plot(Maps$`grids+180` %>% clipMap(layer = "grid", xlim = xlim, ylim = ylim), add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
    } else {
      sp::plot(Maps$grids %>% clipMap(layer = "grid", xlim = xlim, ylim = ylim), add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
    }
  }

  # Add borders
  if (centerMap != "Europe") {
    sp::plot(Maps$`borders-180` %>% clipMap(layer = "borders", xlim = xlim, ylim = ylim), add = TRUE, col = "darkgrey", lwd = 1)
    sp::plot(Maps$`borders+180` %>% clipMap(layer = "borders", xlim = xlim, ylim = ylim), add = TRUE, col = "darkgrey", lwd = 1)
  } else {
    sp::plot(Maps$borders %>% clipMap(layer = "borders", xlim = xlim, ylim = ylim), add = TRUE, col = "darkgrey", lwd = 1)
  }

  if (centerLine) {
    # add vertical line at center longitude
    abline(v = 0, col = "blue", lwd = 2)
  }
}

splitXlim <- function(xlim) {
  if (length(xlim) != 2 || xlim[1] >= xlim[2]) {
    return(list(left = xlim, right = xlim))
  }

  # Case 1: Entire range is below 0
  if (xlim[2] <= 0) {
    return(list(left = xlim, right = NULL))
  }

  # Case 2: Entire range is above 0
  if (xlim[1] >= 0) {
    return(list(left = NULL, right = xlim))
  }

  # Case 3: Range spans 0
  left_range <- c(xlim[1], 0)
  right_range <- c(0, xlim[2])

  return(list(left = left_range, right = right_range))
}

# Clip map
#
# This function clips a map to the specified limits.
#
# @param map A spatial object to be clipped.
# @param layer A character specifying the type of map.
# @param xlim A numeric vector of length 2 specifying the x-axis limits.
# @param ylim A numeric vector of length 2 specifying the y-axis limits.
# @param mapLand A spatial object of layer 'land'.
clipMap <- function(map, layer, xlim, ylim, mapLand = NULL) {
  if (is.null(xlim) || is.null(ylim)) {
    # Convert to sp
    return(sf::as_Spatial(map))
  }

  # increase ylim to include that earth is round
  ylim <- ylim + c(-clipTolerance(), clipTolerance())

  # restrict ylim to -90, 90
  ylim <- pmax(-90, pmin(ylim, 90))

  bbox_polygon <- getBoundingBox(xlim, ylim, crs = st_crs(map))

  # Handle special case for ocean (subtract land to define oceans explicitly)
  if (layer == "ocean") {
    # st_union(mapLand) combines all land polygons into a single geometry
    # which allows to subtract the land from the bounding box
    allLand <- st_union(mapLand)

    if (!any(st_intersects(allLand, bbox_polygon, sparse = FALSE))) {
      return(sf::as_Spatial(map))
    }

    bbox_polygon <- st_difference(bbox_polygon, allLand)
    clipped_sf <- st_sf(geometry = st_sfc(bbox_polygon),
                        crs = st_crs(mapLand))
  } else {
    if (!any(st_intersects(map, bbox_polygon, sparse = FALSE))) {
      return(sf::as_Spatial(map))
    }
    # Clip map for other layers
    clipped_sf <- st_intersection(map, bbox_polygon) %>%
      suppressWarnings()
  }

  # Convert to sp
  clipped_sf %>%
    sf::as_Spatial()
}

clipTolerance <- function() {
  15
}

getBoundingBox <- function(xlim, ylim, crs) {
  # Create a bounding box and convert it to a polygon
  bbox <- st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]), crs = crs)
  bbox_polygon <- st_as_sfc(bbox)

  bbox_polygon
}
