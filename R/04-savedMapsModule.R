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
                   numericInputLatAndLongUI(ns("centerCoords"), label = "Center",
                                            valueLat = 50, valueLong = 10)
                 ),
                 conditionalPanel(
                   ns = ns,
                   condition = "input.userMapType == '3'",
                   numericInputLatAndLongUI(ns("upperLeftCoords"), label = "Upper Left",
                                            valueLat = 25, valueLong = 35),
                   numericInputLatAndLongUI(ns("lowerRightCoords"), label = "Lower Right",
                                            valueLat = 75, valueLong = -15)
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
  rectangleUpperLeft <- numericInputLatAndLongServer("upperLeftCoords",
                                                     valueLat = reactive(25),
                                                     valueLong = reactive(35))
  rectangleLowerRight <- numericInputLatAndLongServer("lowerRightCoords",
                                                      valueLat = reactive(75),
                                                      valueLong = reactive(-15))

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
      withProgress(
        coord <- getFullCoordGrid(gridLength = input$userRadius / 10000),
        value = 80,
        message = "Generating grid ..."
      )

      coord <- coord %>%
        filterCoordCircle(
          lat = circleCenter$latitude(),
          long = circleCenter$longitude(),
          radius = input$userRadius / 111
        )

      if (!is.null(coord)) {
      XPred <- data.frame(
        Est = input$meanMap,
        Sd = input$sdMap,
        Longitude = coord[, 1],
        Latitude = coord[, 2]
      )
      } else {
        XPred <- NULL
      }
    }
    if (input$userMapType == "3") {
      center <- getCoordCenter(
        upperLeftLat = rectangleUpperLeft$latitude(),
        upperLeftLong = rectangleUpperLeft$longitude(),
        lowerRightLat = rectangleLowerRight$latitude(),
        lowerRightLong = rectangleLowerRight$longitude()
      )

      latLength <- abs(diff(c(rectangleLowerRight$latitude(), rectangleUpperLeft$latitude())))
      longLength <- abs(diff(c(rectangleLowerRight$longitude(), rectangleUpperLeft$longitude())))

      withProgress(
        coord <- getFullCoordGrid(gridLength = mean(c(latLength, longLength) / 2) / 10000),
        value = 80,
        message = "Generating grid ..."
      )

      coord <- coord %>%
        filterCoordRectangle(
          long = center[1],
          lat = center[2],
          latLength = latLength,
          longLength = longLength)

      if (!is.null(coord)) {
        XPred <- data.frame(
          Est = input$meanMap,
          Sd = input$sdMap,
          Longitude = coord[, 1],
          Latitude = coord[, 2]
        )
      } else {
        XPred <- NULL
      }
    }

    req(XPred)
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

  tryCatch({
    expand.grid(lo, la)
    #stop("test error")
    #warning("test warning")
  },
  error = function(cond) {
    alert(paste("Could not create grid of coordinates:", cond$message))
    # Choose a return value in case of error
    return(NULL)
  },
  warning = function(cond) {
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = NULL)
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
