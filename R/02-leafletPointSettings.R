#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("clusterPoints"), "Cluster points", value = TRUE),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      checkboxInput(ns("showLegend"), "Legend", value = TRUE),
      checkboxInput(ns("customPoints"), "Customize points"),
      conditionalPanel(
        condition = "input.customPoints == true",
        tags$hr(),
        sliderInput(ns("pointRadiusKm"),
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
