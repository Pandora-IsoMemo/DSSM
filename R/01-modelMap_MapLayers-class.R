#' MapLayers S3 Class
#'
#' A lightweight S3 class to manage and draw map layers (ocean, land, coastlines,
#' grid lines, country borders, and an optional center meridian) on an existing
#' plot. Supports "Europe" and "Pacific" map centers. When centered on "Pacific",
#' layers are split into \code{-180} and \code{+180} halves to handle wrapping.
#'
#' @section Expected \code{Maps} structure:
#' For \strong{centerMap == "Europe"}:
#' \itemize{
#'   \item \code{$ocean}, \code{$land}, \code{$coast}, \code{$grids}, \code{$borders}
#' }
#' For \strong{centerMap != "Europe"} (e.g. "Pacific"):
#' \itemize{
#'   \item \code{$`ocean-180`}, \code{$`ocean+180`}, \code{$`land-180`}, \code{$`land+180`}
#'   \item \code{$`coast-180`}, \code{$`coast+180`}, \code{$`grids-180`}, \code{$`grids+180`}
#'   \item \code{$`borders-180`}, \code{$`borders+180`}
#' }
#'
#' All layers are expected as \code{sf} objects (polygons/lines). Internally we
#' convert to \code{sp} for plotting.
#'
#' @param Maps A named list of \code{sf} spatial objects containing map layers.
#' @param terrestrial Integer;  show only estimates on land masses (1), oceans (-1) or all (0);
#'  \code{1} to draw ocean fill, \code{-1} to draw land fill.
#' @param centerMap Character; either \code{"Europe"} or \code{"Pacific"} (or any
#'   non-"Europe" string to trigger split logic).
#' @param showBorders Logical; add country borders.
#' @param grid Logical; add graticule grid lines.
#' @param centerLine Logical; add a vertical line at the center longitude (0 in
#'   the plotting coordinate space you provide).
#' @param xlim,ylim Numeric length-2 vectors with plot limits in degrees.
#'
#' @return An object of class \code{"MapLayers"}.
#' @examples
#' \dontrun{
#' ml <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific",
#'                     showBorders = TRUE, grid = TRUE, centerLine = FALSE,
#'                     xlim = c(-180, 180), ylim = c(-60, 75))
#' plot(0, 0, type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xlab = "lon", ylab = "lat")
#' plot(ml)  # draws ocean, coastlines, optional grid/borders
#' }
#' @export
new_MapLayers <- function(Maps,
                          terrestrial,
                          centerMap,
                          showBorders = TRUE,
                          grid = FALSE,
                          centerLine = FALSE,
                          xlim = NULL,
                          ylim = NULL) {
  if (is.null(Maps)) {
    stop("'Maps' cannot be NULL. Please provide a valid 'Maps' object.")
  }
  if (length(terrestrial) > 0 && is.character(terrestrial)) {
    terrestrial <- as.numeric(terrestrial)
  }

  if (!is.numeric(terrestrial) || !(as.integer(terrestrial) %in% c(1L, -1L))) {
    stop("'terrestrial' must be 1 (ocean) or -1 (land).")
  }
  if (!is.character(centerMap) || length(centerMap) != 1L) {
    stop("'centerMap' must be a single character string (e.g., 'Europe' or 'Pacific').")
  }
  if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2L || xlim[1] >= xlim[2])) {
    stop("'xlim' must be numeric length 2 with xlim[1] < xlim[2].")
  }
  if (!is.null(ylim) && (!is.numeric(ylim) || length(ylim) != 2L || ylim[1] >= ylim[2])) {
    stop("'ylim' must be numeric length 2 with ylim[1] < ylim[2].")
  }

  obj <- structure(
    list(
      Maps = Maps,
      terrestrial = as.integer(terrestrial),
      centerMap = centerMap,
      showBorders = isTRUE(showBorders),
      grid = isTRUE(grid),
      centerLine = isTRUE(centerLine),
      xlim = xlim,
      ylim = ylim
    ),
    class = "MapLayers"
  )
  .validate_MapLayers(obj)
  obj
}

# ---- S3 methods --------------------------------------------------------------

#' @rdname add_ocean
#' @details For \code{MapLayers}, \code{add_ocean()} fills the ocean area
#'   respecting \code{xlim}/\code{ylim} and \code{centerMap} wrapping.
#' @export
add_ocean.MapLayers <- function(x, ...) {
  if (x$terrestrial != 1L) return(invisible(x))

  if (x$centerMap != "Europe") {
    slitted_xlim <- splitXlim(x$xlim)

    if (!is.null(slitted_xlim$left)) {
      x$Maps$`ocean-180` %>%
        clipMap(layer = "ocean",
                xlim = slitted_xlim$left,
                ylim = x$ylim,
                mapLand = x$Maps$`land-180`) %>%
        sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
    }
    if (!is.null(slitted_xlim$right)) {
      x$Maps$`ocean+180` %>%
        clipMap(layer = "ocean",
                xlim = slitted_xlim$right,
                ylim = x$ylim,
                mapLand = x$Maps$`land+180`) %>%
        sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
    }
  } else {
    x$Maps$`ocean` %>%
      clipMap(layer = "ocean",
              xlim = x$xlim,
              ylim = x$ylim,
              mapLand = x$Maps$`land`) %>%
      sp::plot(add = TRUE, col = "lightblue", lwd = 1, border = NA)
  }

  invisible(x)
}

#' @rdname add_land
#' @details For \code{MapLayers}, \code{add_land()} fills land polygons.
#' @export
add_land.MapLayers <- function(x, ...) {
  if (x$terrestrial != -1L) return(invisible(x))

  if (x$centerMap != "Europe") {
    sp::plot(x$Maps$`land-180` %>% clipMap(layer = "land", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey96", lwd = 1, border = NA)
    sp::plot(x$Maps$`land+180` %>% clipMap(layer = "land", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey96", lwd = 1, border = NA)
  } else {
    sp::plot(x$Maps$land %>% clipMap(layer = "land", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey96", lwd = 1, border = NA)
  }

  invisible(x)
}

#' @rdname add_coast
#' @details For \code{MapLayers}, \code{add_coast()} draws coastlines.
#' @export
add_coast.MapLayers <- function(x, ...) {
  if (x$centerMap != "Europe") {
    sp::plot(x$Maps$`coast-180` %>% clipMap(layer = "coast", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, lwd = 1)
    sp::plot(x$Maps$`coast+180` %>% clipMap(layer = "coast", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, lwd = 1)
  } else {
    sp::plot(x$Maps$coast %>% clipMap(layer = "coast", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, lwd = 1)
  }
  invisible(x)
}

#' @rdname add_grids
#' @details For \code{MapLayers}, \code{add_grids()} overlays graticule lines.
#' @export
add_grids.MapLayers <- function(x, ...) {
  if (!isTRUE(x$grid)) return(invisible(x))

  if (x$centerMap != "Europe") {
    sp::plot(x$Maps$`grids-180` %>% clipMap(layer = "grid", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
    sp::plot(x$Maps$`grids+180` %>% clipMap(layer = "grid", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
  } else {
    sp::plot(x$Maps$grids %>% clipMap(layer = "grid", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "grey", lty = 2, xlim = c(0, 1))
  }
  invisible(x)
}

#' @rdname add_borders
#' @details For \code{MapLayers}, \code{add_borders()} draws political borders.
#' @export
add_borders.MapLayers <- function(x, ...) {
  if (!isTRUE(x$showBorders)) return(invisible(x))

  if (x$centerMap != "Europe") {
    sp::plot(x$Maps$`borders-180` %>% clipMap(layer = "borders", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "darkgrey", lwd = 1)
    sp::plot(x$Maps$`borders+180` %>% clipMap(layer = "borders", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "darkgrey", lwd = 1)
  } else {
    sp::plot(x$Maps$borders %>% clipMap(layer = "borders", xlim = x$xlim, ylim = x$ylim),
             add = TRUE, col = "darkgrey", lwd = 1)
  }
  invisible(x)
}

#' @rdname add_center_line
#' @details For \code{MapLayers}, \code{add_center_line()} draws a vertical
#'   center meridian at longitude 0 (plot coords).
#' @export
add_center_line.MapLayers <- function(x, ...) {
  if (isTRUE(x$centerLine)) {
    abline(v = 0, col = "blue", lwd = 2)
  }
  invisible(x)
}

#' Draw all map layers
#'
#' S3 plot method that draws (in order) the filled base (ocean or land),
#' coastlines, optional grids, optional borders, and optional center line.
#' All drawing is done with \code{add = TRUE}; initialize your plotting device
#' beforehand (e.g., \code{plot(0,0,type="n", ...)}).
#'
#' @param x A \code{MapLayers} object.
#' @param ... Unused.
#' @export
plot.MapLayers <- function(x, ...) {
  .validate_MapLayers(x)

  # Base fill
  if (x$terrestrial == 1L) add_ocean(x)
  if (x$terrestrial == -1L) add_land(x)

  # Overlays
  add_coast(x)
  add_grids(x)
  add_borders(x)
  add_center_line(x)

  invisible(x)
}

# ---- Validation helpers ------------------------------------------------------

.validate_MapLayers <- function(x) {
  # Check layer presence depending on center
  need <- if (x$centerMap != "Europe") {
    c("ocean-180", "ocean+180", "land-180", "land+180",
      "coast-180", "coast+180", "grids-180", "grids+180",
      "borders-180", "borders+180")
  } else {
    c("ocean", "land", "coast", "grids", "borders")
  }

  missing <- setdiff(need, names(x$Maps))
  if (length(missing)) {
    stop(sprintf(
      "The 'Maps' list is missing required layers for center '%s': %s",
      x$centerMap, paste(missing, collapse = ", ")
    ))
  }

  # Fast sanity checks on xlim/ylim if provided
  if (!is.null(x$xlim)) {
    if (any(!is.finite(x$xlim))) stop("'xlim' must be finite numbers.")
  }
  if (!is.null(x$ylim)) {
    if (any(!is.finite(x$ylim))) stop("'ylim' must be finite numbers.")
  }

  invisible(TRUE)
}

# ---- Geometry helpers (sf/sp pipeline) --------------------------------------
#' @importFrom sf st_bbox st_as_sfc st_crs st_union st_intersects st_difference st_intersection st_sfc st_sf
#' @importFrom sp plot
#' @importFrom methods is
#' @importFrom magrittr %>%

splitXlim <- function(xlim) {
  if (is.null(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    return(list(left = xlim, right = xlim))
  }
  if (xlim[2] <= 0) {
    return(list(left = xlim, right = NULL))
  }
  if (xlim[1] >= 0) {
    return(list(left = NULL, right = xlim))
  }
  list(left = c(xlim[1], 0), right = c(0, xlim[2]))
}

#' Clip map to bounding box
#'
#' Clips an \code{sf} layer to \code{xlim}/\code{ylim}. For \code{layer="ocean"},
#' subtracts unioned land to explicitly define ocean polygons inside the bbox.
#'
#' @param map \code{sf} object to clip.
#' @param layer One of \code{"ocean"}, \code{"land"}, \code{"coast"},
#'   \code{"grid"}, \code{"borders"}.
#' @param xlim,ylim Numeric length-2 vectors; if \code{NULL}, returns full layer.
#' @param mapLand \code{sf} object of land polygons (required when
#'   \code{layer == "ocean"} to subtract land).
#' @return An \code{sp} object suitable for plotting.
#' @keywords internal
clipMap <- function(map, layer, xlim, ylim, mapLand = NULL) {
  if (is.null(xlim) || is.null(ylim)) {
    return(sf::as_Spatial(map))
  }

  # allow for a bit of vertical slack (earth curvature)
  ylim <- ylim + c(-clipTolerance(), clipTolerance())
  ylim <- pmax(-90, pmin(ylim, 90))

  bbox_polygon <- getBoundingBox(xlim, ylim, crs = sf::st_crs(map))

  if (layer == "ocean") {
    if (is.null(mapLand)) {
      stop("For layer='ocean', 'mapLand' must be supplied to subtract land.")
    }
    allLand <- sf::st_union(mapLand)

    # If no intersection with bbox, just return original (converted)
    if (!any(sf::st_intersects(allLand, bbox_polygon, sparse = FALSE))) {
      return(sf::as_Spatial(map))
    }

    ocean_bbox_minus_land <- sf::st_difference(bbox_polygon, allLand)
    clipped_sf <- sf::st_sf(geometry = sf::st_sfc(ocean_bbox_minus_land),
                            crs = sf::st_crs(mapLand))
  } else {
    if (!any(sf::st_intersects(map, bbox_polygon, sparse = FALSE))) {
      return(sf::as_Spatial(map))
    }
    clipped_sf <- suppressWarnings(sf::st_intersection(map, bbox_polygon))
  }

  sf::as_Spatial(clipped_sf)
}

clipTolerance <- function() 15

getBoundingBox <- function(xlim, ylim, crs) {
  bb <- sf::st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]), crs = crs)
  sf::st_as_sfc(bb)
}
