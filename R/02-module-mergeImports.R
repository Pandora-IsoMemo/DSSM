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
    textAreaInput(ns("mergeCommand"),
                  "Merge command",
                  value = "joinedData <- inner_join(table1, table2, by = c(\"col2\" = \"col2\"), na_matches = \"never\")"),
    actionButton(ns("applyMerge"), "Apply"),
    checkboxInput(ns("showColnames"), "Show column names"),
    conditionalPanel(condition = "input.showColnames == true",
                     fluidRow(column(
                       12,
                       dataTableOutput(ns("colNames"))
                     )),
                     ns = ns),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "joinedData"
                    ))))
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
                 columnMapping <- reactiveVal()
                 joinedData <- reactiveVal()

                 observe({
                   req(length(mergeList()) > 0)
                   updateSelectInput(
                     session,
                     "mergeNames",
                     choices = names(mergeList()),
                     selected = names(mergeList())
                   )
                 })

                 observeEvent(input$mergeNames, {
                   req(length(input$mergeNames) > 1)

                   columnMapping(getColumNames(mergeList()[input$mergeNames]))
                 })

                 output$colNames <- renderDataTable({
                   req(columnMapping())
                   DT::datatable(columnMapping(),
                                 rownames = FALSE,
                                 options = list(scrollX = TRUE))
                 })

                 observeEvent(input$applyMerge, {
                   req(input$mergeCommand)

                   # setup data.frames to merge
                   for (i in 1:length(input$mergeNames)) {
                     tableName <- input$mergeNames[i]

                     tableDat <- mergeList()[[tableName]]$dataImport
                     assign(paste0("colNamesTable", i), colnames(tableDat))

                     colnames(tableDat) <-
                       paste0("col", 1:ncol(tableDat))
                     assign(paste0("colIdsTable", i), colnames(tableDat))

                     assign(paste0("table", i), tableDat)
                   }

                   # merge data
                   joinedData <- data.frame()
                   eval(parse(text = input$mergeCommand))

                   # rename columns
                   joinedData <- renameColumnIds(
                     joinedData = joinedData,
                                   mappingTable1 = list(colIds = get("colIdsTable1"),
                                                        colNames = get("colNamesTable1")),
                                   mappingTable2 = list(colIds = get("colIdsTable2"),
                                                        colNames = get("colNamesTable2"))
                     )

                   joinedData(joinedData)

                 })

                 output$joinedData <- renderDataTable({
                   req(joinedData())
                   DT::datatable(joinedData(),
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

  colNamesForMerge$table.id <-
    paste0("table", colNamesForMerge$table.id)
  colNamesForMerge$column.id <-
    paste0("col", colNamesForMerge$column.id)

  colNamesForMerge$table.name <- colNamesForMerge$table.name %>%
    cutStrings(cutAt = 20)

  colNamesForMerge %>% select(.data$table.id, everything())
}


renameColumnIds <- function(joinedData, mappingTable1, mappingTable2) {
  colIdsTable1 <- mappingTable1$colIds
  colNamesTable1 <- mappingTable1$colNames

  colIdsTable2 <- mappingTable2$colIds
  colNamesTable2 <- mappingTable2$colNames

  # rename .x columns
  joinedData <- renameExisting(
    joinedData,
    oldNames = paste0(colIdsTable1, ".x"),
    newNames = colNamesTable1
  )

  # rename joined columns
  joinedData <- renameExisting(
    joinedData,
    oldNames = colIdsTable1,
    newNames = colNamesTable1
  )

  # rename unique .y columns
  uniqueYCols <-
    !(colNamesTable2 %in% colNamesTable1)
  joinedData <- renameExisting(
    joinedData,
    oldNames = paste0(colIdsTable2[uniqueYCols], ".y"),
    newNames = colNamesTable2[uniqueYCols]
  )

  # rename duplicated .y columns
  duplicatedYCols <-
    colNamesTable2 %in% colNamesTable1
  joinedData <- renameExisting(
    joinedData,
    oldNames = paste0(colIdsTable2[duplicatedYCols], ".y"),
    newNames = paste0(colNamesTable2[duplicatedYCols], ".y")
  )

  # rename remaining columns from y
  joinedData <- renameExisting(
    joinedData,
    oldNames = colIdsTable2,
    newNames = colNamesTable2
  )

  joinedData
}


renameExisting <- function(df, oldNames, newNames) {
  existing <- match(oldNames, names(df))
  names(df)[na.omit(existing)] <- newNames[which(!is.na(existing))]

  df
}
