#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("clusterPoints"), "Cluster data points"),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      checkboxInput(ns("showLegend"), "Legend", value = TRUE),
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
      checkboxInput(ns("customPoints"), "Customize data points"),
      conditionalPanel(
        condition = "input.customPoints == true",
        tags$hr(),
        sliderInput(ns("pointRadiusKm"),
                    "Point radius in km",
                    value = 20,
                    min = 1,
                    max = 100),
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
leafletPointSettingsServer <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      values <- reactiveValues()

      observe({
        values$clusterPoints <- input$clusterPoints
      })

      observeEvent(input$showLegend, {
        values$showLegend <- input$showLegend
      })

      observeEvent(input$pointRadiusKm, {
        values$pointRadius <- input$pointRadiusKm * 1000
      })

      observe({
        values$jitterMaxKm <- ifelse(input$useJitter,
                                      input$jitterMaxKm,
                                      NA_real_)
      })

      reactive({
        values
      })
    }
  )
}

#' Add Data To Map
#'
#' @param map reactive leaflet map object
#' @param isoData reactive isoData data
#' @param leafletPointValues reactive settings for points on map
addDataToLeafletMap <- function(map, isoData, leafletPointValues) {
  if (is.null(isoData)) return(map)

  if (leafletPointValues$clusterPoints) {
    return(addClustersToMap(map, isoData))
  }

  plotData <- setJitterCoords(isoData,
                              km = leafletPointValues$jitterMaxKm)

  addCirclesToMap(map, plotData,
                  pointRadius = leafletPointValues$pointRadius) %>%
    setColorLegend(showLegend = leafletPointValues$showLegend,
                   values = isoData$source)
}


addClustersToMap <- function(map, isoData){
  if (is.null(isoData$latitude) || all(is.na(isoData$latitude))) return(map)

  isoData <- isoData[!is.na(isoData$longitude), ]

  map <- map %>%
    cleanDataFromMap(layerId = isoData$id)

  map %>%
    addMarkers(
      data = isoData,
      lat = ~ latitude,
      lng =  ~ longitude,
      layerId = ~ id,
      clusterOptions = markerClusterOptions()
    )
}


setJitterCoords <- function(dat, km) {
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


addCirclesToMap <- function(map, isoData, pointRadius){
  if (is.null(isoData$latitude) || all(is.na(isoData$latitude))) return(map)

  isoData <- isoData[!is.na(isoData$longitude), ]

  numColors <- length(unique(isoData$source))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  pal <- colorFactor(colors, isoData$Source)

  if (!is.null(isoData$Latitude_jit)) isoData$latitude <- isoData$Latitude_jit
  if (!is.null(isoData$Longitude_jit)) isoData$longitude <- isoData$Longitude_jit

  map <- map %>%
    cleanDataFromMap(layerId = isoData$id)

  map %>%
    addCircles(data = isoData,
               lat = ~ latitude,
               lng =  ~ longitude,
               layerId = ~ id,
               stroke = F,
               fillOpacity = 0.7,
               color = pal(isoData$source),
               fillColor = pal(isoData$source),
               radius = pointRadius
    )
}


cleanDataFromMap <- function(map, layerId){
  map %>%
    removeShape(layerId = layerId) %>%
    clearMarkerClusters() %>%
    removeControl("colorLegend")
}


#' Add Colour Legend
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

