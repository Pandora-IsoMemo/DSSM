# Merge Via Command Module ----

#' Merge Via Command UI
#'
#' UI of the merge Via command module
#'
#' @param id id of module
mergeViaCommandUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$strong("List of tables:"),
    tags$br(),
    tableOutput(ns("inMemoryTables")),
    checkboxInput(ns("showColumns"), "Show available columns", value = FALSE),
    conditionalPanel(
      ns = ns,
      condition = "input.showColumns == true",
      dataTableOutput(ns("inMemoryColumns"))
    ),
    tags$br(),
    textAreaInput(
      ns("sqlCommand"),
      "SQL query (Please, use the table id inside the SQL query.)",
      placeholder = "SELECT * FROM t1 INNER JOIN t2 ON t1.`Latitude` = t2.`latitude`;",
      value = NULL,
      width = "100%"
    )
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

                 observe({
                   req(length(mergeList()) > 0)
                   tmpDB <- inMemoryDB()
                   for (i in 1:length(mergeList())) {
                     dbWriteTable(tmpDB, paste0("t", i), mergeList()[[i]], overwrite = TRUE)
                   }

                   updateCheckboxInput(session = session, "showColumns", value = TRUE)
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))
                 }) %>%
                   bindEvent(mergeList())

                 output$inMemoryTables <- renderTable({
                   validate(need(
                     !is.null(tableIds()),
                     "Mark files for merge ..."
                   ))

                   req(tableIds())
                   data.frame(
                     `id` = tableIds(),
                     `name` = names(mergeList())
                   )
                 })

                 output$inMemoryColumns <- renderDataTable({
                   req(tableIds())
                   inMemCols <- sapply(mergeList(), function(table) {
                     table %>%
                       colnames() %>%
                       paste(collapse = ", ")
                     })

                   DT::datatable(
                     data.frame(
                       `id` = tableIds(),
                       `column name` = inMemCols
                     ),
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

                 return(NULL)
               })
}
