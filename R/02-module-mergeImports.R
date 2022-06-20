#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
mergeImportsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectInput(
      ns("mergeNames"),
      "Select datasets",
      choices = NULL,
      multiple = TRUE
    ),
    checkboxInput(ns("showColnames"), "Show column names"),
    conditionalPanel(
      condition = "input.showColnames == true",
      fluidRow(column(12,
                      dataTableOutput(ns(
                        "colNames"
                      )))),
      ns = ns)
  )
}

#' Merge Data Server
#'
#' Server function of the merge data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeImportsServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(length(mergeList()) > 0)
                   updateSelectInput(
                     session,
                     "mergeNames",
                     choices = names(mergeList()),
                     selected = names(mergeList())
                   )
                 })

                 output$colNames <- renderDataTable({
                   req(length(input$mergeNames) > 1)
                   DT::datatable({
                     getColumNames(mergeList()[input$mergeNames])
                   },
                   rownames = FALSE,
                   options = list(scrollX = TRUE))
                 })
               })
}


getColumNames <- function(mergeList) {
  colNamesForMerge <- lapply(mergeList, function(df) {
    data.frame(
      column.id = 1:length(df$dataImport),
      column.name = colnames(df$dataImport)
    )
  }) %>%
    bind_rows(.id = "table.name")

  colNamesForMerge$table.id <- colNamesForMerge %>%
    group_by(.data$table.name) %>%
    group_indices()

  colNamesForMerge$table.id <- paste0("table", colNamesForMerge$table.id)
  colNamesForMerge$column.id <- paste0("col", colNamesForMerge$column.id)

  colNamesForMerge$table.name <- colNamesForMerge$table.name %>%
    cutStrings(cutAt = 20)

  colNamesForMerge %>% select(.data$table.id, everything())
}
