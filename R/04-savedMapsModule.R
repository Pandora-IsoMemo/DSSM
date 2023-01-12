#' ui function of saved maps module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
savedMapsTabUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(title,
           id = id,
           value = id,
           sidebarLayout(
             sidebarPanel(
               width = 2,
               div(
                 style = 'display:inline-block',
                 tags$h3("Create Map"),
                 tags$br(),
                 textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
                 numericInput(ns("meanMap"), "Mean of map", value = 0),
                 numericInput(ns("sdMap"), "Sd of map", value = 0, min = 0),
                 radioButtons(
                   ns("userMapType"),
                   "Map Type",
                   choices = c(
                     "all" = "1",
                     "region - circle" = "2",
                     "region - rectangle" = "3"
                   ),
                   selected = 1,
                   inline = FALSE,
                   width = "100%"
                 ),
                 conditionalPanel(
                   ns = ns,
                   condition = "input.userMapType == '2'",
                   numericInput(
                     inputId = ns("userRadius"),
                     label = "Radius in km",
                     min = 1,
                     max = 10000,
                     value = c(3000),
                     width = "100%",
                     step = 100
                   ),
                   numericInputLatAndLongUI(ns("centerCoords"), label = "Center")
                 ),
                 conditionalPanel(
                   ns = ns,
                   condition = "input.userMapType == '3'",
                   tags$strong("Upper left"),
                   tags$br(),
                   numericInput(
                     inputId = ns("upperLeftLat"),
                     label = "Latitude",
                     min = -90,
                     max = 90,
                     value = c(50),
                     width = "40%"
                   ),
                   numericInput(
                     inputId = ns("upperLeftLong"),
                     label = "Longitude",
                     min = -180,
                     max = 180,
                     value = c(10),
                     width = "40%"
                   ),
                   tags$strong("Lower right"),
                   tags$br(),
                   numericInput(
                     inputId = ns("lowerRightLat"),
                     label = "Latitude",
                     min = -90,
                     max = 90,
                     value = c(50),
                     width = "40%"
                   ),
                   numericInput(
                     inputId = ns("lowerRightLong"),
                     label = "Longitude",
                     min = -180,
                     max = 180,
                     value = c(10),
                     width = "40%"
                   )
                 ),
                 actionButton(ns("createMap"), "Create new map")
               )
             ),
             mainPanel(tags$h3("Saved Maps"),
                       tags$br(),
                       uiOutput(ns("mapTable")))
           ))
}



#' server function of saved maps module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param savedMaps saved maps
#'
#' @export
savedMapsTab <- function(input, output, session, savedMaps) {
  ns <- session$ns
  parentSession <- get("session", envir = parent.frame(2))

  output$mapTable <- renderUI({
    mapTable(savedMaps(), ns = ns)
  })

  observeEvent(input$deleteMap, {
    savedMaps(savedMaps()[-input$deleteMap$i])
  })

  circleCenter <- numericInputLatAndLongServer("centerCoords")

  observeEvent(input$createMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == "") {
      alert("Please provide a map name")
      return()
    }

    if (input$userMapType == "1") {
      XPred <- as.numeric(c(input$meanMap, input$sdMap))
    }
    if (input$userMapType == "2") {
      coord <- getFullCoordGrid(gridLength = input$userRadius / 10000)

      coord <- coord %>%
        filterCoordCircle(
          lat = circleCenter$latitude(),
          long = circleCenter$longitude(),
          radius = input$userRadius / 111
        )

      XPred <- data.frame(
        Est = input$meanMap,
        Sd = input$sdMap,
        Longitude = coord[, 1],
        Latitude = coord[, 2]
      )
    }
    if (input$userMapType == "3") {
      center <- getCoordCenter(
        upperLeftLat = input$upperLeftLat,
        upperLeftLong = input$upperLeftLong,
        lowerRightLat = input$lowerRightLat,
        lowerRightLong = input$lowerRightLong
      )
      latLength <- abs(diff(c(input$lowerRightLat, input$upperLeftLat)))
      longLength <- abs(diff(c(input$lowerRightLong, input$upperLeftLong)))

      coord <- getFullCoordGrid(gridLength = min(c(latLength, longLength) / 2) / 10000)

      coord <- coord %>%
        filterCoordRectangle(
          long = center[1],
          lat = center[2],
          latLength = latLength,
          longLength = longLength)

      XPred <- data.frame(
        Est = input$meanMap,
        Sd = input$sdMap,
        Longitude = coord[, 1],
        Latitude = coord[, 2]
      )
    }

    map <- createSavedMap(
      model = NULL,
      predictions = XPred,
      plot =  recordPlot({
        plot.new()
        text(
          x = 0.5,
          y = 0.5,
          paste0("Mean = ", input$meanMap, ", Sd = ", input$sdMap),
          cex = 5
        )
      }),
      type = "user",
      name = mapName
    )
    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })


  observeEvent(savedMaps(), {
    lapply(seq_along(savedMaps()), function(i) {
      rr <- savedMaps()[[i]]
      output[[paste0("thumbnail_", i)]] <-
        renderImage(list(
          src = rr$file,
          height = 100,
          width = 160
        ), deleteFile = FALSE)
    })
  })
}


# Helper functions for saved maps tabs ----

#' Get Full Coord Grid
#'
#' @param gridLength length of the
getFullCoordGrid <- function(gridLength) {
  lo <- seq(-180, 180, by = gridLength)
  la <- seq(-90, 90, by = gridLength)
  expand.grid(lo, la)
}

#' Filter Coord Circle
#'
#' Filter full grid of coordinates to circle area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param radius (numeric) radius
#'
filterCoordCircle <- function(fullGrid, lat, long, radius) {
  fullGrid[sqrt((fullGrid[, 2] - lat) ^ 2 + (fullGrid[, 1] - long) ^ 2) < radius,]
}

#' Filter Coord Square
#'
#' Filter full grid of coordinates to square area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param length (numeric) side length of square
#'
filterCoordSquare <- function(fullGrid, lat, long, length) {
  fullGrid[pmax(abs(fullGrid[, 2] - lat), abs(fullGrid[, 1] - long)) < length / 2,]
}

#' Filter Coord Rectangle
#'
#' Filter full grid of coordinates to square area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param latLength (numeric) length between latitudes
#' @param longLength (numeric) length between longitudes
filterCoordRectangle <-
  function(fullGrid,
           lat,
           long,
           latLength,
           longLength) {
    fullGrid[pmax(abs(fullGrid[, 2] - lat)) < latLength / 2 &
               pmax(abs(fullGrid[, 1] - long)) < longLength / 2,]
  }

#' Get Coord Center
#'
#' Get center of rectangle defined by upper left and lower right coordinates
#'
#' @param upperLeftLat (numeric) latitude of upper left point
#' @param upperLeftLong (numeric) longitude of upper left point
#' @param lowerRightLat (numeric) latitude of lower right point
#' @param lowerRightLong (numeric) longitude of lower right point
getCoordCenter <- function(upperLeftLat,
                           upperLeftLong,
                           lowerRightLat,
                           lowerRightLong) {
  coords <- data.frame(
    lon = c(upperLeftLong, lowerRightLong),
    lat = c(upperLeftLat, lowerRightLat)
  )

  points <- SpatialPoints(coords = coords)
  center <- gCentroid(points)
  res <- center@coords
  colnames(res) <- c("lon", "lat")
  res
}
