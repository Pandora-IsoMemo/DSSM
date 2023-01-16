#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("clusterPoints"), "Cluster data points"),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      ns = ns,
      fluidRow(column(
        8,
        checkboxInput(ns("useJitter"), "Use jitter (in max. km)")
      ),
      column(
        4,
        style = "margin-top: -1em;",
        conditionalPanel(
          condition = "input.useJitter == true",
          numericInput(
            ns("jitterMaxKm"),
            label = NULL,
            value = 25,
            min = 0,
            max = 100
          ),
          ns = ns
        )
      )),
      pointColourUI(ns("pointColor")),
      sliderInput(
        ns("pointRadiusPxl"),
        "Point radius in pixel",
        value = 4,
        min = 1,
        max = 20,
        step = 1
      )
    )
  )
}

#' server funtion of leaflet point settings module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param loadedData (reactive) loaded data
leafletPointSettingsServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 values <- reactiveValues()

                 observe({
                   values$clusterPoints <- input$clusterPoints
                 })

                 pointColorVals <- pointColourServer("pointColor", loadedData)

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
               })
}

#' ui function of leaflet point settings module
#'
#' @param id namespace
pointColourUI <- function(id) {
  ns <- NS(id)

  # using colours from: RColorBrewer::brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
  # adding full names manually
  colourPalettes <- list(
    diverging = c(
      "Brown-BlueGreen" = "BrBG",
      "Pink-Green" = "PiYG",
      "Purple-Green" = "PRGn",
      "Orange-Purple" = "PuOr",
      "Red-Blue" = "RdBu",
      "Red-Yellow-Blue" = "RdYlBu"
    ),
    qualitive = c(
      "Dark" = "Dark2",
      "Paired" = "Paired",
      "Set" = "Set2"
    ),
    sequential = c(
      "Blue" = "Blues",
      "BlueGreen" = "BuGn",
      "BluePurple" = "BuPu",
      "GreenBlue" = "GnBu",
      "Green" = "Greens",
      "Grey" = "Greys",
      "Orange" = "Oranges",
      "OrangeRed" = "OrRd",
      "PurpleBlue" = "PuBu",
      "PurpleBlueGreen" = "PuBuGn",
      "PurpleRed" = "PuRd",
      "Purple" = "Purples",
      "RedPurple" = "RdPu",
      "Red" = "Reds",
      "YellowGreen" = "YlGn",
      "YellowGreenBlue" = "YlGnBu",
      "YellowOrangeBrown" = "YlOrBr",
      "YellowOrangeRed" = "YlOrRd"
    )
  )

  tagList(fluidRow(
    column(8,
           selectInput(
             ns("columnForPointColour"),
             "Point colour variable",
             choices = c("Add data ..." = "")
           )),
    column(4,
           style = "margin-top: 1.5em;",
           checkboxInput(ns("showLegend"), "Legend", value = TRUE))
  ),
  fluidRow(
    column(
      8,
      selectInput(
        ns("paletteName"),
        "Point colour palette",
        choices = colourPalettes,
        selected = "Dark2"
      )
    ),
    column(4,
           style = "margin-top: 1.5em;",
           checkboxInput(
             ns("isReversePalette"), "Reverse", value = FALSE
           ))
  ))
}


#' server funtion of leaflet point settings module
#'
#' @inheritParams leafletPointSettingsServer
pointColourServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 colourValues <- reactiveValues()

                 observeEvent(input$showLegend, {
                   colourValues$showLegend <- input$showLegend
                 })

                 observeEvent(loadedData(), {
                   if (!is.null(loadedData())) {
                     selectedDefault <- ifelse("source" %in% colnames(loadedData()),
                                               "source",
                                               colnames(loadedData())[1])
                   } else {
                     selectedDefault <- character(0)
                   }

                   updateSelectInput(
                     session = session,
                     "columnForPointColour",
                     choices = colnames(loadedData()),
                     selected = selectedDefault
                   )
                 })

                 observeEvent(input$columnForPointColour, {
                   colourValues$columnForPointColour <- input$columnForPointColour
                 })

                 observeEvent(
                   list(
                     input$paletteName,
                     input$isReversePalette,
                     input$columnForPointColour
                   ),
                   {
                     if (is.null(loadedData()) ||
                         is.null(input$columnForPointColour))
                       colourValues$pointColourPalette <- NULL

                     if (!is.null(loadedData()) &&
                         !is.null(input$columnForPointColour)) {
                       colourColumn <- loadedData()[[input$columnForPointColour]]

                       if (is.numeric(colourColumn)) {
                         pal <- colorNumeric(
                           palette = input$paletteName,
                           domain = colourColumn,
                           reverse = input$isReversePalette
                         )
                       } else {
                         pal <- colorFactor(
                           palette = input$paletteName,
                           domain = colourColumn,
                           reverse = input$isReversePalette
                         )
                       }

                       colourValues$pointColourPalette <- pal
                     }
                   }
                 )

                 return(colourValues)
               })
}


# Helper functions ----

#' Update Data On Map
#'
#' @param map reactive leaflet map object
#' @param isoData reactive isoData data
#' @param leafletPointValues reactive settings for points on map
updateDataOnLeafletMap <-
  function(map, isoData, leafletPointValues) {
    map <- map %>%
      cleanDataFromMap()

    if (is.null(isoData) ||
        is.null(isoData[["latitude"]]) ||
        all(is.na(isoData[["latitude"]])) ||
        is.null(isoData[["longitude"]]) ||
        all(is.na(isoData[["longitude"]])))
      return(map)

    isoData <-
      isoData[(!is.na(isoData[["longitude"]]) &
                 !is.na(isoData[["latitude"]])),]

    if (leafletPointValues$clusterPoints) {
      return(drawClustersOnMap(map, isoData))
    }

    plotData <- setJitterCoords(isoData,
                                km = leafletPointValues$jitterMaxKm)

    if (!is.null(plotData$Latitude_jit))
      plotData$latitude <- plotData$Latitude_jit
    if (!is.null(plotData$Longitude_jit))
      plotData$longitude <- plotData$Longitude_jit

    drawCirclesOnMap(
      map,
      plotData,
      pointRadius = leafletPointValues$pointRadius,
      colourPal = leafletPointValues$pointColourPalette,
      columnForColour = leafletPointValues$columnForPointColour
    ) %>%
      setColorLegend(
        showLegend = leafletPointValues$showLegend,
        title = leafletPointValues$columnForPointColour,
        pal = leafletPointValues$pointColourPalette,
        values = isoData[[leafletPointValues$columnForPointColour]]
      )
  }


drawClustersOnMap <- function(map, isoData) {
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
  if (is.na(km))
    return(dat)

  withProgress({
    set.seed(20180213)

    dat$Latitude_jit <-
      jitter_latlong(dat$latitude,
                     type = "lat",
                     dat$latitude,
                     km = km)
    dat$Longitude_jit <-
      jitter_latlong(dat$longitude,
                     type = "long",
                     dat$latitude,
                     km = km)
    dat
  },
  value = 0.9,
  message = 'Add jitter ...')
}


drawCirclesOnMap <-
  function(map,
           isoData,
           pointRadius,
           colourPal,
           columnForColour) {
    if (is.null(colourPal))
      return(map)

    map %>%
      addCircleMarkers(
        data = isoData,
        lat = ~ latitude,
        lng =  ~ longitude,
        group = "dataPoints",
        stroke = F,
        fillOpacity = 0.7,
        color = colourPal(isoData[[columnForColour]]),
        fillColor = colourPal(isoData[[columnForColour]]),
        radius = pointRadius
      )
  }


cleanDataFromMap <- function(map) {
  map %>%
    clearGroup("dataPoints") %>%
    clearMarkerClusters() %>%
    removeControl("colorLegend")
}


#' Set Colour Legend
#'
#' @param map leaflet map
#' @param showLegend logical show/hide legend
#' @param title legend title
#' @param pal colour palette
#' @param values possible values that can be mapped, e.g. isoData$source
setColorLegend <- function(map, showLegend, title, pal, values) {
  if (showLegend && !is.null(pal)) {
    map <- map %>%
      addLegend(
        "topleft",
        pal = pal,
        values = values,
        title = title,
        layerId = "colorLegend"
      )
  } else {
    map <- map %>% removeControl("colorLegend")
  }

  map
}
