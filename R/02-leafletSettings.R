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
    options = list(create = TRUE)
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
    fluidRow(column(6, checkboxInput(
      ns("includeNorthArrow"), "North Arrow"
    )),
    column(
      6,
      selectInput(
        ns("northArrowPosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    fluidRow(column(6, checkboxInput(
      ns("includeScale"), "Scale"
    )),
    column(
      6,
      selectInput(
        ns("scalePosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    fluidRow(
      column(width = 6,
             checkboxInput(ns("fitBounds"), "Fit boundaries")),
      column(width = 6,
             align = "right",
             actionButton(ns(
               "centerMapButton"
             ), "Data Center", width = "100%"))
    ),
    conditionalPanel(
      condition = "input.fitBounds == true",
      tags$hr(),
      boundsLatLongNumericUI(ns("bounds"), defaultBounds),
      tags$br(),
      fluidRow(column(5, actionButton(
        ns("applyBounds"), "Apply"
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
    values$scalePosition <-
      ifelse(input$includeScale, input$scalePosition, NA_character_)
  })

  observe({
    values$northArrowPosition <-
      ifelse(input$includeNorthArrow,
             input$northArrowPosition,
             NA_character_)
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

  observeEvent(input$centerMapButton, {
    values$centerMapButton <- input$centerMapButton
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


boundsLatLongNumericUI <- function(id, defaultBounds) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             numericInput(
               ns("south"),
               "Latitude: South",
               value = defaultBounds()$lat[1],
               min = -90,
               max = 90
             )
      ),
      column(6,
             numericInput(
               ns("north"),
               "Latitude: North",
               value = defaultBounds()$lat[2],
               min = -90,
               max = 90
             )
      )
    ),
    fluidRow(
      column(6,
             numericInput(
               ns("west"),
               "Longitude: West",
               value = defaultBounds()$lng[1],
               min = -180,
               max = 360
             )
      ),
      column(6,
             numericInput(
               ns("east"),
               "Longitude: East",
               value = defaultBounds()$lng[2],
               min = -180,
               max = 360
             )
      )
    )
  )
}



#' Customize Leaflet Map
#'
#' Customize leaflet map for export
#'
#' @param leafletMap leaflet map
#' @param leafletValues map settings, e.g. scalePosition, show/hide bounds
customizeLeafletMap <- function(leafletMap, leafletValues) {
  leafletMap %>%
    addProviderTiles(leafletValues$leafletType) %>%
    drawIcons(
      scale = !is.na(leafletValues$scalePosition),
      scalePosition = leafletValues$scalePosition,
      northArrow = !is.na(leafletValues$northArrowPosition),
      northArrowPosition = leafletValues$northArrowPosition
    ) %>%
    drawFittedBounds(showBounds = leafletValues$showBounds,
                     bounds = leafletValues$bounds)

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

defaultCenter <- function(center = "atlantic") {
  if (is.null(center)) return(list(lng = 0, lat = 30))

  switch(center,
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

  longitude
}
