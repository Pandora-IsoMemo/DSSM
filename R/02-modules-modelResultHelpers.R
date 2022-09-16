# Collection of helper modules

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
