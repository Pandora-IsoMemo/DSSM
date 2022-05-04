#' ui function of leaflet point settings module
#'
#' @param id namespace
#' @param title title in tab
leafletPointSettingsUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    sliderInput(ns("pointRadius"),
                "Point Radius in km",
                value = 20,
                min = 1,
                max = 100),
    checkboxInput(ns("useJitter"), "Use point jitter")
  )
}

#' server funtion of leaflet point settings module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
leafletPointSettingsServer <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      values <- reactiveValues(pointRadius = 20000,
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
