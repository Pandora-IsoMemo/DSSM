# Merge Via UI Module ----

#' Merge Via UI UI
#'
#' UI of the merge via UI module
#'
#' @param id id of module
mergeViaUIUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("tableX"),
      "Select tabel x",
      choices = NULL,
      width = "100%"
    ),
    selectInput(
      ns("tableY"),
      "Select tabel y",
      choices = NULL,
      width = "100%"
    ),
    fluidRow(column(
      4,
      selectInput(
        ns("mergeOperation"),
        "Select operation",
        choices = c(
          "all rows in x and y" = "inner_join",
          "all rows in x" = "left_join",
          "all rows in y" = "right_join",
          "all rows in x or y" = "full_join"
        ),
        selected = "left_join"
      )
    ),
    column(
      8,
      checkboxInput(ns("addAllCommonColumns"), "Join on all common columns")
    )),
    fluidRow(column(
      6,
      selectInput(
        ns("columnsX"),
        "Select x columns",
        choices = NULL,
        multiple = TRUE
      )
    ),
    column(
      6,
      selectInput(
        ns("columnsY"),
        "Select y columns",
        choices = NULL,
        multiple = TRUE
      )
    ))
  )
}

#' Merge Via UI Server
#'
#' Server function of the merge via UI module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeViaUIServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 tableId <- reactiveValues(tableX = NULL,
                                           tableY = NULL)
                 commonColumns <- reactiveVal()
                 columnsToJoin <- reactiveValues(tableX = NULL,
                                                 tableY = NULL)
                 mergeCommand <- reactiveVal()
                 #joinedData <- reactiveVal()

                 # update: table selection ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)

                   tableChoices <- extractMergeChoices(mergeList())

                   updateSelectInput(session,
                                     "tableX",
                                     choices = tableChoices,
                                     selected = tableChoices[1])

                   updateSelectInput(session,
                                     "tableY",
                                     choices = tableChoices,
                                     selected = tableChoices[2])
                 })

                 # update: column selection ----
                 observeEvent(input$tableX, {
                   req(mergeList(), input$tableX)
                   updateSelectInput(session, "columnsX",
                                     choices = extractColNames(mergeList()[[input$tableX]]))
                 })

                 observeEvent(input$tableY, {
                   req(mergeList(), input$tableY)
                   updateSelectInput(session, "columnsY",
                                     choices = extractColNames(mergeList()[[input$tableY]]))
                 })

                 observe({
                   req(mergeList(), input$tableX, input$tableY)
                   commonColumns(extractCommonColumns(mergeList(), input$tableX, input$tableY))
                 })

                 observeEvent(input$addAllCommonColumns, {
                   req(commonColumns())
                   if (input$addAllCommonColumns) {
                     updateSelectInput(session, "columnsX",
                                       selected = commonColumns())
                     updateSelectInput(session, "columnsY",
                                       selected = commonColumns())
                   } else {
                     updateSelectInput(session, "columnsX",
                                       selected = list())
                     updateSelectInput(session, "columnsY",
                                       selected = list())
                   }
                 })

                 # create: mergeCommand ----
                 observeEvent(list(input$columnsX, input$columnsY), {
                   req(names(mergeList()))

                   tableId$tableX <-
                     extractTableIds(names(mergeList()))[[input$tableX]]
                   tableId$tableY <-
                     extractTableIds(names(mergeList()))[[input$tableY]]

                   columnsToJoin$tableX <-
                     equalizeLength(input$columnsX, input$columnsY)$xColNames
                   columnsToJoin$tableY <-
                     equalizeLength(input$columnsX, input$columnsY)$yColNames

                   colJoinString <-
                     extractJoinString(columnsToJoin$tableX, columnsToJoin$tableY)

                   if (!is.null(colJoinString) &&
                       (tableId$tableX != tableId$tableY)) {
                     mergeCommand(
                       tmpl(
                         paste0(
                           c(
                             "{{ tableX }} %>% ",
                             "  {{ mergeOperation }}({{ tableY }},",
                             "    by = {{ colJoinString }})"
                           ),
                           collapse = ""
                         ),
                         tableX = tableId$tableX,
                         mergeOperation = input$mergeOperation,
                         tableY = tableId$tableY,
                         colJoinString = colJoinString
                       ) %>% as.character()
                     )
                   } else {
                     mergeCommand("")

                     if (tableId$tableX == tableId$tableY) {
                       alert("Please choose two different table.")
                     }
                   }
                 })

                 # return value for parent module: ----
                 return(mergeCommand)
               })
}


# Merge Via UI Helper Functions ----

## helpers: table selection ----
extractMergeChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(extractTableIds(tableChoices), " --- ", tableChoices)

  tableChoices
}


extractTableIds <- function(namesOfTables) {
  ids <- paste0("table", 1:length(namesOfTables))
  names(ids) <- namesOfTables

  ids
}

## helpers: column selection ----
extractCommonColumns <- function(tableList, tableX, tableY) {
  colnamesX <- extractColNames(tableList[[tableX]])
  colnamesY <- extractColNames(tableList[[tableY]])

  intersect(colnamesX, colnamesY)
}


extractColNames <- function(tableListElement) {
  colnames(tableListElement$dataImport)
}


equalizeLength <- function(xColNames, yColNames) {
  minLength <- min(length(xColNames), length(yColNames))

  if (minLength == 0) {
    return(NULL)
  }

  list(xColNames = xColNames[1:minLength],
       yColNames = yColNames[1:minLength])
}


extractJoinString <- function(xColumns, yColumns) {
  xColumns <- paste0("\"", xColumns, "\"")
  yColumns <- paste0("\"", yColumns, "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}
