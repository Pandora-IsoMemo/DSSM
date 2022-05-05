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
                  "Point radius in km",
                  value = 20,
                  min = 1,
                  max = 100),
      fluidRow(
        column(8,
               checkboxInput(ns("useJitter"), "Use jitter in km")
               ),
        column(4,
               conditionalPanel(
                 condition = "input.useJitter == true",
                 numericInput(ns("jitterMaxKm"), label = NULL,
                             value = 10, min = 0, max = 100),
                 ns = ns
                 ))
      ),
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
        values$pointRadius <- input$pointRadius * 1000
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
