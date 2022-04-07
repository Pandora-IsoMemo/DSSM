#' ui function of leaflet settings module
#'
#' @param id namespace
#' @param title title in tab
leafletDataStyleUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    selectInput(
      ns("groupingColumn"),
      "Column with Categories",
      choices = NULL
    ),
    checkboxInput(ns("customizeMarkers"), "Customize Markers", value = FALSE),
    conditionalPanel(ns = ns,
                     "input.customizeMarkers == true",
                     selectInput(
                       ns("dataShapes"),
                       "Fontawesome Icon(s)",
                       choices = c(
                         "circle",
                         "diamond",
                         "square",
                         "leaf",
                         "seedling",
                         "tree",
                         "water",
                         "volcano",
                         "mountain",
                         "fire"
                       ),
                       multiple = TRUE,
                       selected = "circle"
                     ),
                     colourInput(
                       ns("dataColour"), "Colour Palette", value = "red"
                     ),
                     numericInput(ns("dataSize"), "Size(s)", value = 5, min = 1, max = 10)
                     )
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData reactive isoData table
leafletDataStyle <- function(input, output, session, isoData) {
  values <- reactiveValues(categories = NULL,
                           shapes = NULL,
                           colours = NULL,
                           sizes = NULL)

  observe({
    req(isoData(), !is.null(isoData()$source))
    updateSelectInput(session, "groupingColumn", choices = colnames(isoData()),
                      selected = "source")
  })

  observe({
    req(isoData())
    values$categories <- isoData()[[input$groupingColumn]] %>%
      unique()
  })

  observeEvent(input$customizeMarkers, {
    values$customizeMarkers <- input$customizeMarkers
  })

  observe({
    values$shapes <- rep(input$dataShapes, length.out = length(values$categories))
    names(values$shapes) <- values$categories
  })

  reactive({values})
}
