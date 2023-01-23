# Merge Via Command Module ----

#' Merge Via Command UI
#'
#' UI of the merge Via command module
#'
#' @param id id of module
mergeViaCommandUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    dataTableOutput(ns("inMemoryTables")),
    tags$br(),
    dataTableOutput(ns("inMemoryColumns")),
    tags$br(),
    div(style = "margin-top: 1em; margin-bottom: 0.5em;",
        tags$html(
          HTML(
            "<b>SQL query</b> &nbsp;&nbsp; (Please, use the table ID to name tables)"
          )
        )),
    aceEditor(
      ns("sqlCommand"),
      value = NULL,
      mode = "sql",
      theme = "cobalt",
      fontSize = 18,
      autoScrollEditorIntoView = TRUE,
      minLines = 5,
      maxLines = 10,
      autoComplete = "enabled"
    ),
    div(style = "margin-top: 1em;",
        fluidRow(
          column(3, actionButton(ns("applyQuery"), "Apply Query")),
          column(9, align = "right", style = "margin-top: 12px;", textOutput(ns(
            "nRowsQueriedData"
          )))
        )),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview result of query</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(
                      ns("queriedData")
                    )))
  )
}

#' Merge Command Server
#'
#' Server function of the merge Command module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeViaCommandServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 inMemoryDB <- reactiveVal(dbConnect(SQLite(), "file::memory:"))
                 tableIds <- reactiveVal(NULL)

                 result <- reactiveValues(data = NULL,
                                          preview = NULL)

                 observe({
                   req(length(mergeList()) > 0)
                   tmpDB <- inMemoryDB()
                   for (i in 1:length(mergeList())) {
                     dbWriteTable(tmpDB, paste0("t", i), mergeList()[[i]], overwrite = TRUE)
                   }

                   updateCheckboxInput(session = session, "showColumns", value = TRUE)
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))
                   updateAceEditor(session = session, "sqlCommand",
                                   value = "SELECT t1.* FROM t1;")
                 }) %>%
                   bindEvent(mergeList())

                 output$inMemoryTables <- renderDataTable({
                   validate(need(!is.null(tableIds()),
                                 "In-memory tables: Mark files ..."))

                   req(tableIds())
                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Table` = names(mergeList())),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "In-memory tables"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "75px"
                     )
                   )
                 })

                 output$inMemoryColumns <- renderDataTable({
                   validate(need(!is.null(tableIds()),
                                 "In-memory columns: Mark files ..."))

                   req(tableIds())
                   inMemCols <-
                     sapply(mergeList(), function(table) {
                       table %>%
                         colnames() %>%
                         paste(collapse = ", ")
                     })

                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Columns` = inMemCols),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "In-memory columns"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "150px"
                     )
                   )
                 })

                 observe({
                   req(length(mergeList()) > 0)
                   tmpDB <- inMemoryDB()

                   # tryCatch einbauen!!
                   result$data <-
                     dbGetQuery(tmpDB, input$sqlCommand)
                   result$preview <-
                     cutAllLongStrings(result$data[1:2, , drop = FALSE], cutAt = 20)
                 }) %>%
                   bindEvent(input$applyQuery)

                 output$nRowsQueriedData <- renderText({
                   req(result$data)
                   paste("Queried data has ", NROW(result$data), "rows")
                 })

                 output$queriedData <- renderDataTable({
                   req(result$preview)

                   DT::datatable(
                     result$preview,
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       scrollX = TRUE
                     )
                   )
                 })

                 return(reactive(result$data))
               })
}
