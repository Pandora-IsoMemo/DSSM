#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
mergeDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("mergeList"), "Select Datasets", choices = NULL),
    verbatimTextOutput(ns("preview"))
  )
}

#' Merge Data Server
#'
#' Server function of the merge data module
mergeDataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$preview <- renderText({input$mergeList})
    }
  )
}
