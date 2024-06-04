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
           tags$h3("Saved Maps"),
                       tags$br(),
                       uiOutput(ns("mapTable"))
           )
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
#' @param gridLength length between two points
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
  fullGrid[sqrt((fullGrid[, 2] - lat) ^ 2 + (fullGrid[, 1] - long) ^ 2) < radius, ]
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
  fullGrid[pmax(abs(fullGrid[, 2] - lat), abs(fullGrid[, 1] - long)) < length / 2, ]
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
               pmax(abs(fullGrid[, 1] - long)) < longLength / 2, ]
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
  df <- data.frame(
    lon = c(upperLeftLong, lowerRightLong),
    lat = c(upperLeftLat, lowerRightLat)
  )

  res <- df %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_combine() %>%
    st_centroid() %>%
    st_coordinates()
  colnames(res) <- c("lon", "lat")
  res
}
