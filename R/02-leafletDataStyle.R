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
    selectInput(
      ns("dataShapes"),
      "Shape(s)",
      choices = c(
        "Type 1" = "1",
        "Type 2" = "2",
        "Type 3" = "3",
        "Type 4" = "4",
        "Type 5" = "5",
        "Type 6" = "6",
        "Type 7" = "7"
      ),
      multiple = TRUE
    ),
    colourInput(
      ns("dataColour"), "Colour Palette", value = "red"
    ),
    numericInput(ns("dataSize"), "Size(s)", value = 5, min = 1, max = 10)
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

  observeEvent(input$groupingColumn, {
    req(isoData())
    values$categories <- isoData()[[input$groupingColumn]] %>%
      unique()
  })

  observeEvent(input$dataShapes, {
    req(values$categories)
    values$shapes <- rep(input$dataShapes, length.out = length(values$categories))
  })

  reactive({values})
}
