# Collection of helper modules for the modelling tabs ----

## Formatting of decimal places ----

#' Center Estimate UI
#'
#' UI function of center estimate module
#'
#' @param id namespace
#' @param title title in tab
centerEstimateUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("centerY"),
      label = "Center point latitude",
      min = -180,
      max = 180,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    numericInput(
      inputId = ns("centerX"),
      label = "Center point longitude",
      min = -90,
      max = 90,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.centerY != null && input.centerY != '' && input.centerX != null && input.centerX != ''",
      numericInput(
        inputId = ns("decimalPlace"),
        label = "Decimal places for Mean/Error at the Center point",
        min = 0,
        max = 10,
        value = 2,
        step = 1,
        width = "100%"
      ),
      ns = ns
    ),
    sliderInput(
      inputId = ns("Radius"),
      label = "Radius (km)",
      min = 10,
      max = 300,
      value = 100,
      step = 10,
      width = "100%"
    )
  )
}


#' Center Estimate Server
#'
#' Backend for center estimate module
#'
#' @param id namespace id
#' @param meanCenter (reactive) mean for center
#' @param sdCenter (reactive) error for center
#' @param mapType (reactive) type of plot, either "Map", "Time course", "Time intervals by cluster",
#'  "Spread", "Speed", "Minima/Maxima"
centerEstimateServer <-
  function(id, meanCenter, sdCenter, mapType = reactiveVal("Map")) {
    moduleServer(id,
                 function(input, output, session) {
                   centerEstimateText <- reactiveVal("")

                   centerEstimateMaps <- c("Map", "Spread")
                   # check which maps have the text in the previous version:
                   # currently not included: "Time course", "Time intervals by cluster",
                   # "Speed" (a map but no meanCenter, sdCenter in the output available),
                   # "Minima/Maxima"

                   observeEvent(list(meanCenter(), sdCenter(), input$decimalPlace), {
                     if (is.na(input$centerY) |
                         is.na(input$centerX) |
                         is.na(input$Radius) |
                         !(mapType() %in% centerEstimateMaps)) {
                       centerEstimateText("")
                     }

                     req(
                       input$centerX,
                       input$centerY,
                       input$Radius,
                       (mapType() %in% centerEstimateMaps)
                     )

                     if (is.na(meanCenter()) | is.na(sdCenter())) {
                       centerEstimateText(
                         "Cannot compute mean and sd at your provided coordinates. Please raise the
                       plot resolution or radius such that estimates within the radius are available."
                       )
                     }

                     req(meanCenter(), sdCenter())
                     centerEstimateText(
                       paste0(
                         "Mean: ",
                         round(meanCenter(), digits = input$decimalPlace),
                         ", Standard error of the mean: ",
                         round(sdCenter(), digits = input$decimalPlace),
                         "  at coordinates ",
                         "(",
                         input$centerY,
                         "\u00B0, " ,
                         input$centerX,
                         "\u00B0) for a ",
                         round(input$Radius, 3),
                         " km radius"
                       )
                     )

                   })

                   list(
                     centerX = reactive(input$centerX),
                     centerY = reactive(input$centerY),
                     radius = reactive(input$Radius),
                     text = centerEstimateText
                   )
                 })
  }


#' Format Time Course UI
#'
#' UI function for formatting of the time course plot
#'
#' @param id namespace
#' @param title title in tab
formatTimeCourseUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("axesDecPlace"),
      label = "Decimal places for axes",
      min = 0,
      max = 10,
      value = 0,
      step = 1,
      width = "100%"
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsX"),
          label = "N labels of x axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsY"),
          label = "N labels of y axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      )
    )
  )
}


#' Format Time Course Server
#'
#' Backend for formatting of the time course plot
#'
#' @param id namespace id
formatTimeCourseServer <-
  function(id) {
    moduleServer(id,
                 function(input, output, session) {
                   reactive(
                     list(
                       axesDecPlace = input$axesDecPlace,
                       nLabelsX = input$nLabelsX,
                       nLabelsY = input$nLabelsY
                     )
                   )
                 })
  }


## Time and Map Section ----


#' Time And Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param label label
timeAndMapSectionUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    tags$hr(),
    tags$h4(label),
    sliderAndNumericInputUI(
      ns("timeExtended"),
      label = "Time",
      min = 0,
      max = 15000,
      value = 5000,
      step = 100
    ),
    mapSectionUI(ns("mapSection")),
    fluidRow(
      column(
        width = 3,
        offset = 9,
        style = "margin-top: -60px;",
        actionButton(ns("set"), "Set Time and Map Section")
      )
    ),
    tags$hr(),
  )
}


#' Time And Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param dateMin (reactive) min date
#' @param dateMax (reactive) max date
#' @param dateValue (reactive) value date
#' @param dateStep (reactive) step date
#' @param zoomValue (reactive) default zoom given by model output
timeAndMapSectionServer <- function(id,
                                    dateMin,
                                    dateMax,
                                    dateValue,
                                    dateStep,
                                    zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 mapParams <- reactiveValues(
                   time = 5000,
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50
                 )

                 userInputTime <-
                   sliderAndNumericInputServer(
                     "timeExtended",
                     value = dateValue,
                     min = dateMin,
                     max = dateMax,
                     step = dateStep
                   )

                 mapSectionParams <- mapSectionServer("mapSection",
                                                      zoomValue = zoomValue)

                 # default values depend on model output
                 observeEvent(list(dateValue(),
                                   zoomValue()), {
                   mapParams$time <- dateValue()
                   mapParams$zoom <- zoomValue()
                   mapParams$upperLeftLatitude <-
                     mapSectionParams$upperLeftLatitude
                   mapParams$upperLeftLongitude <-
                     mapSectionParams$upperLeftLongitude
                 })

                 # values given by the user pressing button
                 observeEvent(input$set, {
                   mapParams$time <- userInputTime()
                   mapParams$zoom <- mapSectionParams$zoom
                   mapParams$upperLeftLatitude <-
                     mapSectionParams$upperLeftLatitude
                   mapParams$upperLeftLongitude <-
                     mapSectionParams$upperLeftLongitude
                 })

                 return(mapParams)
               })
}


#' Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
mapSectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderAndNumericInputUI(
      ns("zoom"),
      label = "Zoom/x-Range in degrees Longitude",
      min = 0.1,
      max = 360,
      value = 50,
      step = 1
    ),
    fluidRow(
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLatitude"),
          label = "Latitude of upper left corner",
          min = -90,
          max = 90,
          value = c()
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLongitude"),
          label = "Longitude of upper left corner",
          min = -180,
          max = 180,
          value = c()
        )
      )
    )
  )
}


#' Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param zoomValue (reactive) default zoom given by model output
mapSectionServer <- function(id,
                             zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 mapParams <- reactiveValues(
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50
                 )

                 zoomInput <- sliderAndNumericInputServer(
                   "zoom",
                   value = zoomValue,
                   min = reactive(0.1),
                   max = reactive(360),
                   step = reactive(1)
                 )

                 # update upperLeftLatitude/upperLeftLongitude if values$up/... change ----

                 observe({
                   mapParams$zoom <- zoomInput()
                   mapParams$upperLeftLatitude <-
                     input$upperLeftLatitude
                   mapParams$upperLeftLongitude <-
                     input$upperLeftLongitude
                 })

                 return(mapParams)
               })
}


#' Slider And Input UI
#'
#' UI of the Slider And Input module
#'
#' @param id id of module
#' @param label label
#' @param min (numeric) minumum
#' @param max (numeric) maximum
#' @param value (numeric) default value
#' @param step (numeric) step
sliderAndNumericInputUI <-
  function(id, label, min, max, value, step) {
    ns <- NS(id)
    tagList(fluidRow(
      column(
        width = 10,
        sliderInput(
          inputId = ns("sliderIn"),
          label = label,
          min = min,
          max = max,
          value = value,
          step = step,
          width = "100%"
        )
      ),
      column(
        width = 2,
        style = "margin-top: 30px;",
        numericInput(
          inputId = ns("numIn"),
          label = NULL,
          min = min,
          max = max,
          value = value,
          step = step
        )
      )
    ))
  }

#' Slider And Input Server
#'
#' Server function of the Slider And Input module
#' @param id id of module
#' @param value value of input
#' @param min min of input
#' @param max max of input
#' @param step step of input
sliderAndNumericInputServer <- function(id,
                                        value,
                                        min,
                                        max,
                                        step) {
  moduleServer(id,
               function(input, output, session) {
                 result <- reactiveVal(5000)

                 observe({
                   req(value(), min(), max(), step())

                   updateSliderInput(
                     session = session,
                     "sliderIn",
                     value = value(),
                     min = min(),
                     max = max(),
                     step = step()
                   )
                   updateNumericInput(
                     session = session,
                     "numIn",
                     value = value(),
                     min = min(),
                     max = max(),
                     step = step()
                   )

                   result(value())
                 })

                 observeEvent(input$sliderIn, {
                   req(input$sliderIn != input$numIn)
                   updateNumericInput(session = session,
                                      "numIn",
                                      value = input$sliderIn)
                   result(input$sliderIn)
                 })

                 observeEvent(input$numIn, {
                   req(input$sliderIn != input$numIn)
                   updateSliderInput(session = session,
                                     "sliderIn",
                                     value = input$numIn)
                   result(input$numIn)
                 })

                 return(result)
               })
}


# Collection of helper functions for the modelling tabs ----

#' Extract Zoom From Long Range
#'
#' @param rangeLongitude (numeric) range of longitude vector
#' @param mapCentering (character) centering of the map, either "Europe" or "Pacific"
extractZoomFromLongRange <- function(rangeLongitude, mapCentering) {
  if(mapCentering == "Europe"){
    rangeLong <- diff(range(rangeLongitude, na.rm = TRUE) + c(-1, 1))
  } else {
    longRange <- rangeLongitude
    longRange[rangeLongitude < -20] <- longRange[rangeLongitude < -20] + 200
    longRange[rangeLongitude >= -20] <- (- 160 + longRange[rangeLongitude >= -20])
    rangeLong <- diff(range(longRange, na.rm = TRUE) + c(-1, 1))
  }

  pmin(360, pmax(0, rangeLong, na.rm = TRUE))
}
