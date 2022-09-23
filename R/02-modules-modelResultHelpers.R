# Collection of helper modules ----

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
      column(width = 6,
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
      column(width = 6,
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
                   reactive(list(
                     axesDecPlace = input$axesDecPlace,
                     nLabelsX = input$nLabelsX,
                     nLabelsY = input$nLabelsY
                   ))
                 })
  }


## Time and Map Section ----


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
sliderAndNumericInputUI <- function(id, label, min, max, value, step) {
  ns <- NS(id)
  tagList(
    tags$h4(label),
    div(
      style = "display:flex;",
      div(
        class = "zoom-map",
        sliderInput(inputId = ns("sliderInput"),
                    label = NULL,
                    min = min, max = max, value = value, step = step, width = "100%")
      ),
      div(
        class = "move-map",
        numericInput(inputId = ns("numInput"),
                     label = NULL,
                     min = min, max = max, value = value, step = step, width = "190px")
      ))
  )
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

                 observeEvent(list(value(), min(), max(), step()), {
                   updateNumericInput(session = session, "sliderInput", value = value(),
                                      min = min(), max = max(), step = step())
                   updateNumericInput(session = session, "numInput", value = value(),
                                      min = min(), max = max(), step = step())
                 })

                 observeEvent(input$sliderInput, {
                   req(input$sliderInput != input$numInput)
                   updateNumericInput(session = session, "numInput", value = input$sliderInput)
                   result(input$sliderInput)
                 })

                 observeEvent(input$numInput, {
                   req(input$sliderInput != input$numInput)
                   updateSliderInput(session = session, "sliderInput", value = input$numInput)
                   result(input$numInput)
                 })

                 result
               })
}


#' Map Section UI
#'
#' UI of the Map Section module
#'
#' @param id id of module
#' @param label label
mapSectionUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    tags$h4(label),
    div(
      style = "display:flex;",
      div(
        class = "zoom-map",
        sliderInput(inputId = ns("zoom"),
                    label = "Zoom/x-Range in degrees Longitude",
                    min = 0.1, max = 360, value = 50, width = "100%")
      ),
      div(
        class = "move-map",
        uiOutput(ns("move"))
      )),
    fluidRow(
      column(
        width = 3,
        numericInput(inputId = ns("upperLeftLatitude"),
                     label = "Set Latitude of upper left corner",
                     min = -90, max = 90, value = c(), width = "20%")
      ),
      column(
        width = 3,
        numericInput(inputId = ns("upperLeftLongitude"),
                     label = "Set Longitude of upper left corner",
                     min = -180, max = 180, value = c(), width = "20%")
      ),
      column(
        width = 3,
        numericInput(inputId = ns("zoomSet"),
                     label = "Zoom/x-Range in degrees Longitude (click set button for apply)",
                     min = 0.1, max = 360, value = 50, width = "20%")
      )
    )
  )
}


#' Map Section Server
#'
#' Server function of the Map Section module
#' @param id id of module
mapSectionServer <- function(id, applyButton) {
  moduleServer(id,
               function(input, output, session) {
                 result <- reactiveVal()


                 # observeEvent(input$zoom, {
                 #   zoom <- input$zoom
                 #   values$zoom <- input$zoom
                 # })
                 #
                 # observeEvent(input$up, {
                 #   if(values$set > 0){
                 #     zoom <- values$zoom
                 #   } else {
                 #     zoom <- input$zoom
                 #   }
                 #   values$up <- values$up + zoom / 40
                 # })
                 #
                 # observeEvent(input$down, {
                 #   if(values$set > 0){
                 #     zoom <- values$zoom
                 #   } else {
                 #     zoom <- input$zoom
                 #   }
                 #   values$up <- values$up - zoom / 40
                 # })
                 # observeEvent(input$left, {
                 #   if(values$set > 0){
                 #     zoom <- values$zoom
                 #   } else {
                 #     zoom <- input$zoom
                 #   }
                 #   values$right <- values$right - zoom / 40
                 # })
                 # observeEvent(input$right, {
                 #   if(values$set > 0){
                 #     zoom <- values$zoom
                 #   } else {
                 #     zoom <- input$zoom
                 #   }
                 #   values$right <- values$right + zoom / 40
                 # })
                 # observeEvent(input$center, {
                 #   values$up <- 0
                 #   values$right <- 0
                 # })
                 #
                 # observeEvent(applyButton(), {
                 #   values$set <- 1
                 #   values$up <- 0
                 #   values$right <- 0
                 #   values$zoom <- input$zoomSet
                 #   values$upperLeftLatitude <- input$upperLeftLatitude
                 #   values$upperLeftLongitude <- input$upperLeftLongitude
                 # })


                 result
               })
}
