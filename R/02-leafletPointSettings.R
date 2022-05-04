#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("customPoints"), "Customize points"),
    conditionalPanel(
      condition = "input.customPoints == true",
      tags$hr(),
      sliderInput(ns("pointRadius"),
                  "Point Radius in km",
                  value = 20,
                  min = 1,
                  max = 100),
      checkboxInput(ns("useJitter"), "Use point jitter (dependent on zoom)"),
      tags$hr(),
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
      values <- reactiveValues(pointRadius = 20,
                               useJitter = FALSE)

      observeEvent(input$pointRadius, {
        values$pointRadius <- input$pointRadius
      })

      observeEvent(input$useJitter, {
        values$useJitter <- input$useJitter
      })

      reactive({
        values
      })
    }
  )
}
