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

#' Place decimal points for TimeCourseplot
#'
#' UI function of for decimal place
#'
#' @param id namespace
#' @param title title in tab
defineDecimalPlace <- function(id, title = "") {
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
#' @param mapType (reactive) type of plot, either "Map", "Time course" or
#'  "Time intervals by cluster"
centerEstimateServer <-
  function(id, meanCenter, sdCenter, mapType = reactiveVal("Map")) {
    moduleServer(id,
                 function(input, output, session) {
                   centerEstimateText <- reactiveVal("")

                   observeEvent(list(meanCenter(), sdCenter(), input$decimalPlace), {
                     if (is.na(input$centerY) |
                         is.na(input$centerX) |
                         is.na(input$Radius) |
                         mapType() != "Map") {
                       centerEstimateText("")
                     }

                     req(input$centerX,
                         input$centerY,
                         input$Radius,
                         (mapType() == "Map"))

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
