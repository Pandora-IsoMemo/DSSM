#' ui function of leaflet settings module
#'
#' @param id namespace
#' @param title title in tab
leafletSettingsUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    selectizeInput(
      ns("LeafletType"),
      label = "Map type",
      choices = list(
        `borders & names`= list(
        "CartoDB.Positron",
        "OpenStreetMap.Mapnik",
        "OpenStreetMap.DE",
        "OpenTopoMap",
        "Esri",
        "Esri.WorldTopoMap",
        "Esri.OceanBasemap"
      ),
      `only borders`= list(
        "CartoDB.PositronNoLabels"
      ),
      `plain maps`= list(
        "Esri.WorldImagery",
        "Esri.WorldTerrain",
        "Esri.WorldShadedRelief",
        "Esri.WorldPhysical"
      ),
      `custom maps` = list(
        "NASAGIBS.ViirsEarthAtNight2012"
      )
    ),
    options = list(create = TRUE,
                   dropdownParent = 'body')
    ),
    helpText(HTML(paste0("Find more maps ",
                         tags$i(
                           class = "glyphicon glyphicon-info-sign",
                           style = "color:#0072B2;",
                           title = paste(
                             "How to select a new map:",
                             " ",
                             " 1. Select a map from https://leaflet-extras.github.io/leaflet-providers/preview/",
                             "  and copy the name.",
                             " 2. Delete the input of the field 'Map type'.",
                             " 3. Paste the name of the map into the field 'Map type'.",
                             " 4. Click 'Add...', and the custom map will be selected. ",
                             " ",
                             "Some maps are not supported, e.g. they require a sign in. Prefer open",
                             " providers like: 'OpenStreetMap', 'Esri', 'CartoDB', 'NASAGIBS'.",
                             sep = "\n"
                           )),
                         ": <br>",
                         tags$a(href = "https://leaflet-extras.github.io/leaflet-providers/preview/",
                                "https://leaflet-extras.github.io/leaflet-providers/preview/",
                                target = "_blank"),
                         " "
    ))),
    scaleOrNorthArrowUI(ns("northArrow"), label = "North Arrow", sizeValue = 80),
    scaleOrNorthArrowUI(ns("scale"), label = "Scale", sizeValue = 100),
    checkboxInput(ns("fitBounds"), "Zoom into boundaries"),
    conditionalPanel(
      condition = "input.fitBounds == true",
      tags$hr(),
      boundsLatLongNumericUI(ns("bounds"), defaultBounds),
      tags$br(),
      fluidRow(column(5, actionButton(
        ns("applyBounds"), "Zoom"
      )),
      column(
        7, checkboxInput(ns("showBounds"), "Show boundaries", value = TRUE)
      )),
      tags$hr(),
      ns = ns
    )
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
leafletSettings <- function(input, output, session) {
  values <- reactiveValues(applyBounds = 0)

  values$bounds <-
    reactiveValues(
      north = defaultBounds()$lat[["north"]],
      south = defaultBounds()$lat[["south"]],
      east = defaultBounds()$lng[["east"]],
      west = defaultBounds()$lng[["west"]]
    )

  observeEvent(input$LeafletType, {
    values$leafletType <- input$LeafletType
  })

  observe({
    values$scalePosition <- input$`scale-position`
    values$scaleSize <- input$`scale-size`
    values$`scale-lng` <- input$`scale-lng`
    values$`scale-lat` <- input$`scale-lat`
  })

  observe({
    values$northArrowPosition <- input$`northArrow-position`
    values$northArrowSize <- input$`northArrow-size`
    values$`northArrow-lng` <- input$`northArrow-lng`
    values$`northArrow-lat` <- input$`northArrow-lat`
  })

  observeEvent(input$applyBounds, {
    values$applyBounds <- input$applyBounds

    values$bounds <-
      reactiveValues(
        north = input$`bounds-north`,
        south = input$`bounds-south`,
        east = input$`bounds-east`,
        west = input$`bounds-west`,
      )
  })

  observeEvent({
    input$showBounds & input$fitBounds
  }, {
    values$showBounds <- input$showBounds & input$fitBounds
  })

  reactive({
    values
  })
}


scaleOrNorthArrowUI <- function(id, label, sizeValue) {
  ns <- NS(id)
  tagList(
    fluidRow(column(
      6,
      selectInput(
        ns("position"),
        label = label,
        choices = c("none", "topright", "bottomright", "bottomleft", "topleft", "custom"),
        selected = "none"
      )
    ), column(
      6,
      conditionalPanel(
        ns = ns,
        condition = "input.position != 'none'",
        numericInput(
          ns("size"),
          label = "Size",
          value = sizeValue,
          min = 1,
          step = 10
        )
      )
    )),
    conditionalPanel(
      ns = ns,
      condition = "input.position == 'custom'",
      fluidRow(column(
        6,
        numericInput(ns("lng"), "Longitude", value = numeric(0), min = -180, max = 360)
      ), column(
        6,
        numericInput(ns("lat"), "Latitude", value = numeric(0), min = -90, max = 90)
      ))
    )
  )
}

boundsLatLongNumericUI <- function(id, defaultBounds) {
  ns <- NS(id)
  tagList(fluidRow(column(
    6,
    numericInput(
      ns("south"),
      "Latitude: South",
      value = defaultBounds()$lat[1],
      min = -90,
      max = 90
    )
  ), column(
    6,
    numericInput(
      ns("north"),
      "Latitude: North",
      value = defaultBounds()$lat[2],
      min = -90,
      max = 90
    )
  )), fluidRow(column(
    6,
    numericInput(
      ns("west"),
      "Longitude: West",
      value = defaultBounds()$lng[1],
      min = -180,
      max = 360
    )
  ), column(
    6,
    numericInput(
      ns("east"),
      "Longitude: East",
      value = defaultBounds()$lng[2],
      min = -180,
      max = 360
    )
  )))
}



#' Customize Leaflet Map
#'
#' Customize leaflet map for export
#'
#' @param leafletMap leaflet map
#' @param leafletValues map settings, e.g. scalePosition, show/hide bounds
#' @param zoom zoom level
customizeLeafletMap <- function(leafletMap, leafletValues, zoom) {
  leafletMap %>%
    addProviderTiles(leafletValues()$leafletType) %>%
    drawIcons(
      zoom = zoom,
      scalePosition = leafletValues()$scalePosition,
      scaleSize = leafletValues()$scaleSize,
      scaleLng = leafletValues()$`scale-lng`,
      scaleLat = leafletValues()$`scale-lat`,
      northArrowPosition = leafletValues()$northArrowPosition,
      northArrowSize = leafletValues()$northArrowSize,
      northArrowLng = leafletValues()$`northArrow-lng`,
      northArrowLat = leafletValues()$`northArrow-lat`
    ) %>%
    drawFittedBounds(showBounds = leafletValues()$showBounds,
                     bounds = leafletValues()$bounds)

}


#' Draw Fitted Bounds
#'
#' @param map leaflet map
#' @param showBounds logical show/hide fitted bounds
#' @param bounds list of (west, east, south, north) boundaries to be drawn
drawFittedBounds <- function(map, showBounds, bounds) {
  if (showBounds) {
    map <- map %>%
      addRectangles(
        layerId = "mapBoundsFrame",
        lng1 = bounds$west,
        lng2 = bounds$east,
        lat1 = bounds$south,
        lat2 = bounds$north,
        color = "grey",
        weight = 1,
        fillColor = "transparent"
      )
  } else {
    map <- map %>%
      removeShape(layerId = "mapBoundsFrame")
  }

  map
}

defaultCenter <- function(centerType = "atlantic") {
  if (is.null(centerType)) return(list(lng = 0, lat = 30))

  switch(centerType,
         "atlantic" = list(lng = 0, lat = 30),
         "pacific" = list(lng = 180, lat = 0))
}


defaultBounds <- function(center = defaultCenter()) {
  list(lng = c(west = center$lng - 30,
               east = center$lng + 30),
       lat = c(south = center$lat - 15,
               north = center$lat + 15))
}

centerLongitudes <- function(longitude, center) {
  if (is.null(center) || length(longitude) == 0) return(longitude)

  if (center == "pacific") {
    longitude[longitude < 0] <- longitude[longitude < 0] + 360
  }

  if (center == "atlantic") {
    longitude[longitude > 180] <- longitude[longitude > 180] - 360
  }

  longitude
}
