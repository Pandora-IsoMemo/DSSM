# Query Data Module ----

#' Query Data UI
#'
#' UI of the query data module
#'
#' @param id id of module
queryDataUI <- function(id) {
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
      autoComplete = "live"
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

#' Query Data Server
#'
#' Server function of the qery data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
queryDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 inMemoryDB <- reactiveVal(dbConnect(SQLite(), "file::memory:"))
                 tableIds <- reactiveVal(NULL)
                 inMemColumns <- reactiveVal(NULL)

                 result <- reactiveValues(
                   data = NULL,
                   preview = NULL,
                   warnings = list(),
                   warningsPopup = list(),
                   errors = list()
                 )

                 observe({
                   req(length(mergeList()) > 0)

                   tmpDB <- inMemoryDB()
                   for (i in 1:length(mergeList())) {
                     dbWriteTable(tmpDB, paste0("t", i), mergeList()[[i]], overwrite = TRUE)
                   }
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))

                   inMemCols <-
                     lapply(mergeList(), function(table) {
                       table %>%
                         colnames()
                     })
                   names(inMemCols) <- tableIds()

                   inMemColumns(inMemCols)

                   if (!is.null(inMemCols[["t1"]])) {
                     colSel <- paste0("`", inMemCols[["t1"]][1], "`")
                   } else {
                     colSel <- "*"
                   }
                   updateAceEditor(session = session, "sqlCommand",
                                   value = paste0("select t1.", colSel, " from t1;"),
                                   autoCompleters = c("snippet", "text", "static", "keyword"),
                                   autoCompleteList = inMemCols)
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
                   inMemColsPasted <-
                     sapply(inMemColumns(), function(colnamesOfTable) {
                       colnamesOfTable %>%
                         paste(collapse = ", ")
                     })

                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Columns` = inMemColsPasted),
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
                   req(length(mergeList()) > 0, input$applyQuery == 1)
                   withProgress({
                     result$data <- NULL
                     result$preview <- NULL
                     result$warningsPopup <- list()
                     result$errors <- list()
                     tmpDB <- inMemoryDB()

                     result$data <-
                       tryCatch({
                         dbGetQuery(tmpDB, input$sqlCommand)
                       },
                       error = function(cond) {
                         result$errors <- "Query failed."
                         alert(paste("Query failed:", cond$message))
                         return(NULL)
                       },
                       warning = function(cond) {
                         result$warningsPopup <- cond$message
                         return(NULL)
                       },
                       finally = NULL)

                     if (!is.null(result$data)) {
                       result$preview <-
                         cutAllLongStrings(result$data[1:2, , drop = FALSE], cutAt = 20)
                     }
                   },
                   value = 0.75,
                   message = 'applying query ...')
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
