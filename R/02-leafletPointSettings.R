#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("clusterPoints"), "Cluster data points"),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      fluidRow(
        column(8,
               checkboxInput(ns("useJitter"), "Use jitter (in max. km)")
        ),
        column(4,
               conditionalPanel(
                 condition = "input.useJitter == true",
                 numericInput(ns("jitterMaxKm"), label = NULL,
                              value = 25, min = 0, max = 100),
                 ns = ns
               ))
      ),
      pointColourUI(ns("pointColor")),
      checkboxInput(ns("customPoints"), "Customize data points"),
      conditionalPanel(
        condition = "input.customPoints == true",
        tags$hr(),
        sliderInput(ns("pointRadiusPxl"),
                    "Point radius in pixel",
                    value = 4,
                    min = 1,
                    max = 20,
                    step = 1),
        tags$hr(),
        ns = ns
      ),
      ns = ns
    )
  )
}

#' server funtion of leaflet point settings module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param dataColnames (reactive) colnames of loaded data
leafletPointSettingsServer <- function(id, dataColnames){
  moduleServer(
    id,
    function(input, output, session) {
      values <- reactiveValues()

      observe({
        values$clusterPoints <- input$clusterPoints
      })

      pointColorVals <- pointColourServer("pointColor", dataColnames)

      observe({
        for (i in names(pointColorVals)) {
          values[[i]] <- pointColorVals[[i]]
        }
      })

      observeEvent(input$pointRadiusPxl, {
        values$pointRadius <- input$pointRadiusPxl
      })

      observe({
        values$jitterMaxKm <- ifelse(input$useJitter,
                                      input$jitterMaxKm,
                                      NA_real_)
      })

      values
    }
  )
}

#' ui function of leaflet point settings module
#'
#' @param id namespace
pointColourUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(8,
                    selectInput(ns("columnForPointColour"), "Point colour variable",
                                choices = c("Add data ..." = ""))
    ),
    column(4,
           style = "margin-top: 1.5em;",
           checkboxInput(ns("showLegend"), "Legend", value = TRUE)
    )),
    fluidRow(column(8,
                    selectInput(ns("paletteForPointColour"), "Point colour palette",
                                choices = c("Red-Yellow-Green" = "RdYlGn",
                                            "Yellow-Green-Blue" = "YlGnBu",
                                            "Purple-Orange" = "PuOr",
                                            "Pink-Yellow-Green" = "PiYG",
                                            "Red-Yellow-Blue" = "RdYlBu",
                                            "Yellow-Brown" = "YlOrBr",
                                            "Brown-Turquoise" = "BrBG"),
                                selected = "RdYlGn")
    ),
    column(4,
           style = "margin-top: 1.5em;",
           checkboxInput(ns("isReversePalette"), "Reverse", value = FALSE)
    ))
    )
}


#' server funtion of leaflet point settings module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param dataColnames (reactive) colnames of loaded data
pointColourServer <- function(id, dataColnames){
  moduleServer(
    id,
    function(input, output, session) {
      colourValues <- reactiveValues()

      observeEvent(input$showLegend, {
        colourValues$showLegend <- input$showLegend
      })

      observeEvent(dataColnames(), {
        if (!is.null(dataColnames())) {
          selectedDefault <- ifelse("source" %in% dataColnames(),
                                    "source",
                                    dataColnames()[1])
        } else {
          selectedDefault <- character(0)
        }

        updateSelectInput(session = session, "columnForPointColour",
                          choices = dataColnames(),
                          selected = selectedDefault)
      })

      observeEvent(input$columnForPointColour, {
        colourValues$columnForPointColour <- input$columnForPointColour
      })

      observeEvent(input$paletteForPointColour, {
        colourValues$paletteForPointColour <- input$paletteForPointColour
      })

      observeEvent(input$isReversePalette, {
        colourValues$isReversePalette <- input$isReversePalette
      })

      return(colourValues)
    })
  }


# Helper functions ----

#' Update Data On Map
#'
#' @param map reactive leaflet map object
#' @param isoData reactive isoData data
#' @param leafletPointValues reactive settings for points on map
updateDataOnLeafletMap <- function(map, isoData, leafletPointValues) {
  map <- map %>%
    cleanDataFromMap()

  if (is.null(isoData) || is.null(isoData[["latitude"]]) || all(is.na(isoData[["latitude"]])) ||
      is.null(isoData[["longitude"]]) || all(is.na(isoData[["longitude"]]))) return(map)

  isoData <- isoData[(!is.na(isoData[["longitude"]]) & !is.na(isoData[["latitude"]])), ]

  if (leafletPointValues$clusterPoints) {
    return(drawClustersOnMap(map, isoData))
  }

  plotData <- setJitterCoords(isoData,
                              km = leafletPointValues$jitterMaxKm)

  if (!is.null(plotData$Latitude_jit)) plotData$latitude <- plotData$Latitude_jit
  if (!is.null(plotData$Longitude_jit)) plotData$longitude <- plotData$Longitude_jit

  drawCirclesOnMap(map, plotData,
                   pointRadius = leafletPointValues$pointRadius) %>%
    setColorLegend(showLegend = leafletPointValues$showLegend,
                   values = isoData$source)
}


drawClustersOnMap <- function(map, isoData){
  map %>%
    addMarkers(
      data = isoData,
      lat = ~ latitude,
      lng =  ~ longitude,
      group = "dataPoints",
      clusterOptions = markerClusterOptions()
    )
}


setJitterCoords <- function(dat, km) {
  # no jitter should be used: km == NA
  if (is.na(km)) return(dat)

  withProgress({
    set.seed(20180213)

    dat$Latitude_jit <- jitter_latlong(dat$latitude, type = "lat", dat$latitude, km = km)
    dat$Longitude_jit <- jitter_latlong(dat$longitude, type = "long", dat$latitude, km = km)
    dat
  },
  value = 0.9,
  message = 'Add jitter ...'
  )
}


drawCirclesOnMap <- function(map, isoData, pointRadius) {
  numColors <- length(unique(isoData$source))
  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]
  pal <- colorFactor(colors, isoData$Source)

  map %>%
    addCircleMarkers(
      data = isoData,
      lat = ~ latitude,
      lng =  ~ longitude,
      group = "dataPoints",
      stroke = F,
      fillOpacity = 0.7,
      color = pal(isoData$source),
      fillColor = pal(isoData$source),
      radius = pointRadius
    )
}


cleanDataFromMap <- function(map){
  map %>%
    clearGroup("dataPoints") %>%
    clearMarkerClusters() %>%
    removeControl("colorLegend")
}


#' Set Colour Legend
#'
#' @param map leaflet map
#' @param showLegend logical show/hide legend
#' @param values possible values that can be mapped, e.g. isoData$source
setColorLegend <- function(map, showLegend, values){

  if (showLegend) {

    map <- map %>%
      addLegend("topleft",
                pal = getColourPal(values),
                values = values,
                title = "Database",
                layerId = "colorLegend")
  } else {
    map <- map %>% removeControl("colorLegend")
  }

  map
}


#' Get Colour Palette
#'
#' Get colour palette for the points and the legend
#'
#' @inheritParams setColorLegend
getColourPal <- function(values){
  numColors <- length(unique(values))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  colorFactor(colors, values)
}

