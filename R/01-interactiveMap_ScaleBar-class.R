#' ScaleBar Class
#'
#' This class represents a scale bar for interactive maps.
#'
#' @param position The position of the scale bar on the map. Options are "topright", "topleft", "bottomright", "bottomleft", "custom", or "none".
#' @param zoom The zoom level at which the scale bar is displayed. If `NULL`, it defaults to the current zoom level of the map.
#' @param size The size of the scale bar in pixels. Default is 100.
#' @param lat The latitude for a custom position of the scale bar. Required if `position` is "custom".
#' @param lng The longitude for a custom position of the scale bar. Required if `position` is "custom".
#' @return An object of class `ScaleBar`.
#'
#' @export
new_ScaleBar <- function(position = "none",
                         zoom = NULL,
                         size = 100,
                         lat = NULL,
                         lng = NULL) {
  structure(list(
    position = position,
    zoom = zoom,
    size = size,
    lat = lat,
    lng = lng
  ),
  class = "ScaleBar")
}

#' Set Scale Bar on Leaflet Map
#'
#' This function sets a scale bar on a Leaflet map based on the properties of a `ScaleBar` object.
#'
#' @param obj An object of class `ScaleBar`.
#' @param map A Leaflet map object.
#' @param ... Additional arguments (not used).
#' @return A Leaflet map object with the scale bar added.
#'
#' @export
set.ScaleBar <- function(obj, map, ...) {
  # reset scale bar
  map <- map %>%
    removeScaleBar() %>%
    clearGroup(group = "customScaleBar")

  is_empty_position <- is.null(obj$position) ||
    obj$position == "none" ||
    (obj$position == "custom" && (is.na(obj$lng) || is.na(obj$lat)))

  if (is_empty_position) {
    return(map)
  }

  if (obj$position %in% c("topright", "topleft", "bottomright", "bottomleft")) {
    map <- leaflet::addScaleBar(
      map,
      position = obj$position,
      options = leaflet::scaleBarOptions(maxWidth = obj$size)
    )
  } else if (obj$position == "custom" &&
             !is.null(obj$lat) && !is.null(obj$lng)) {
    map <- addScaleBarAt(
      map,
      lat = obj$lat,
      lng = obj$lng,
      zoom = obj$zoom,
      size = obj$size
    )
  }
  map
}

# HELPERS ----

getBestScaleValue <- function(max_km) {
  nice_values <- c(10000, 5000, 2000, 1000, 500, 200, 100, 50, 25, 10, 5, 2, 1, 0.5)
  max(nice_values[nice_values <= max_km])
}

addScaleBarAt <- function(map, lat, lng, zoom, size = 100, value = NULL, color = "black", weight = 1, opacity = 1, group = "customScaleBar") {
  if (!is.null(size) && is.numeric(size) && size > 0) {
    # Estimate meters per pixel at current zoom & latitude
    m_per_px <- 156543.03392 * cos(lat * pi / 180) / (2 ^ zoom)
    max_km <- size * m_per_px / 1000  # convert to km

    value <- getBestScaleValue(max_km)
  }

  # Determine default numeric value from zoom level
  if (is.null(value)) {
    value <- switch(as.character(zoom),
                    "1" = 1000, "2" = 1000, "3" = 750,
                    "4" = 500, "5" = 200, "6" = 100, "7" = 50,
                    "8" = 25, "9" = 10, "10" = 5, "11" = 2,
                    "12" = 1, "13" = 1, "14" = 0.5, "15" = 0.2,
                    "16" = 0.1, "17" = 0.1, "18" = 0.05,
                    1
    )
  }

  # Vertical spacing for ticks and labels
  spacing <- switch(as.character(zoom),
                    "1" = 1.5, "2" = 1, "3" = 0.6,
                    "4" = 0.3, "5" = 0.2, "6" = 0.1, "7" = 0.08,
                    "8" = 0.05, "9" = 0.03, "10" = 0.02, "11" = 0.01,
                    "12" = 0.008, "13" = 0.005, "14" = 0.003,
                    "15" = 0.002, "16" = 0.001, "17" = 0.0008,
                    "18" = 0.0005, 0.002
  )

  # Convert scale lengths to degrees longitude
  km_m <- value * 1000
  mi_m <- value * 1609.344

  # # approximation of meters per degree longitude at given latitude
  # # this slightly diverges from Leaflets Mercator projection
  # m_per_deg_long <- 111320 * cos(lat * pi / 180)
  # delta_lng_km <- km_m / m_per_deg_long
  # delta_lng_mi <- mi_m / m_per_deg_long

  # Leaflet's projection uses Mercator, so we can use geosphere to calculate the destination point
  delta_lng_km <- geosphere::destPoint(c(lng, lat), 90, value * 1000)[1] - lng
  delta_lng_mi <- geosphere::destPoint(c(lng, lat), 90, value * 1609.344)[1] - lng

  # Helper to draw background rectangle
  drawBackground <- function(map, lng2, lat1, lat2) {
    addPolygons(map,
                lng = c(lng, lng2, lng2, lng),
                lat = c(lat1, lat1, lat2, lat2),
                fillColor = "white", fillOpacity = 0.5,
                stroke = FALSE, smoothFactor = 0,
                group = group
    )
  }

  # Backgrounds
  map <- map %>%
    drawBackground(lng + delta_lng_mi, lat, lat - spacing * 4) %>%
    drawBackground(lng + delta_lng_km, lat, lat + spacing * 4)

  # Horizontal lines (both at lat)
  map <- map %>%
    addPolylines(lng = c(lng, lng + delta_lng_km), lat = c(lat, lat),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = c(lng, lng + delta_lng_mi), lat = c(lat, lat),
                 color = color, weight = weight, opacity = opacity, group = group)

  # Vertical ticks
  map <- map %>%
    addPolylines(lng = c(lng, lng),
                 lat = c(lat - spacing * 4, lat + spacing * 4),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = rep(lng + delta_lng_km, 2),
                 lat = c(lat, lat + spacing * 4),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = rep(lng + delta_lng_mi, 2),
                 lat = c(lat - spacing * 4, lat),
                 color = color, weight = weight, opacity = opacity, group = group)

  # Invisible icon for labels
  emptyIcon <- icons(
    iconUrl = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8Xw8AApMBgVQe57oAAAAASUVORK5CYII=",
    iconWidth = 1, iconHeight = 1
  )

  # Labels (centered)
  map <- map %>%
    addMarkers(lng = lng + delta_lng_km / 2,
               lat = lat + spacing * 2,
               label = paste(value, "km"),
               labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE),
               icon = emptyIcon, group = group) %>%
    addMarkers(lng = lng + delta_lng_mi / 2,
               lat = lat - spacing * 3,
               label = paste(value, "mi"),
               labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE),
               icon = emptyIcon, group = group)

  return(map)
}
