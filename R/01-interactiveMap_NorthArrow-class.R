#' North Arrow for Interactive Maps
#'
#' Creates a North Arrow object for use in interactive maps.
#'
#' @param position Position of the North Arrow. Options are "topright", "topleft", "bottomright", "bottomleft", "custom", or "none".
#' @param width Width of the North Arrow in pixels.
#' @param height Height of the North Arrow in pixels.
#' @param lat Latitude for custom positioning (required if position is "custom").
#' @param lng Longitude for custom positioning (required if position is "custom").
#' @param layerId Unique identifier for the North Arrow layer.
#' @return A NorthArrow object.
#'
#' @export
new_NorthArrow <- function(position = "none",
                           width = 80,
                           height = 80,
                           lat = NULL,
                           lng = NULL,
                           layerId = "northArrowIcon") {
  structure(
    list(
      position = position,
      width = width,
      height = height,
      lat = lat,
      lng = lng,
      layerId = layerId
    ),
    class = "NorthArrow"
  )
}

#' Set North Arrow on a Leaflet Map
#'
#' Sets the North Arrow on a Leaflet map based on the properties of the NorthArrow object.
#'
#' @param obj A NorthArrow object.
#' @param map A Leaflet map object.
#' @param ... Additional arguments (not used).
#' @return A Leaflet map object with the North Arrow added.
#'
#' @export
set.NorthArrow <- function(obj, map, ...) {
  # Remove previous
  map <- map %>%
    leaflet::removeControl(obj$layerId) %>%
    leaflet::removeMarker(obj$layerId)

  is_empty_position <- is.null(obj$position) ||
    obj$position == "none" ||
    (obj$position == "custom" && (is.na(obj$lng) || is.na(obj$lat)))

  if (is_empty_position) {
    return(map)
  }

  if (obj$position %in% c("topright", "topleft", "bottomright", "bottomleft")) {
    return(
      map %>% leaflet::addControl(
        htmltools::tags$img(
          src = "https://isomemodb.com/NorthArrow.png",
          width = obj$width,
          height = obj$height
        ),
        position = obj$position,
        layerId = obj$layerId,
        className = ""
      )
    )
  }

  if (obj$position == "custom" &&
      !is.null(obj$lat) && !is.null(obj$lng)) {
    return(
      map %>% leaflet::addMarkers(
        lng = obj$lng,
        lat = obj$lat,
        layerId = obj$layerId,
        icon = leaflet::icons(
          iconUrl = "https://isomemodb.com/NorthArrow.png",
          iconWidth = obj$width,
          iconHeight = obj$height
        )
      )
    )
  }

  map
}
