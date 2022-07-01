#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
mergeDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
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
    fluidRow(
      column(
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
      )
    ),
    fluidRow(column(
      6,
      selectInput(
        ns("xColumnsToJoin"),
        "Select x colums",
        choices = NULL,
        multiple = TRUE
      )
    ),
    column(
      6,
      selectInput(
        ns("yColumnsToJoin"),
        "Select y columns",
        choices = NULL,
        multiple = TRUE
      )
    )),
    textAreaInput(ns("mergeCommand"),
                  "Merge command",
                  value = NULL,
                  width = "100%"),
    actionButton(ns("applyMerge"), "Apply"),
    actionButton(ns("addMerge"), "Add Table"),
    # checkboxInput(ns("showColnames"), "Show column names"),
    # conditionalPanel(condition = "input.showColnames == true",
    #                  fluidRow(column(
    #                    12,
    #                    dataTableOutput(ns("colNames"))
    #                  )),
    #                  ns = ns),
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
mergeDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 commonColumns <- reactiveVal()
                 #columnMapping <- reactiveVal()
                 mergeCommand <- reactiveVal()
                 joinedData <- reactiveVal()

                 # update choices of tables ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)

                   tableChoices <- getTableChoices(mergeList())

                   updateSelectInput(session,
                                     "tableX",
                                     choices = tableChoices,
                                     selected = tableChoices[1])

                   updateSelectInput(session,
                                     "tableY",
                                     choices = tableChoices,
                                     selected = tableChoices[2])
                 })

                 # update choices of columns ----
                 observeEvent(input$tableX, {
                   req(mergeList(), input$tableX)
                   updateSelectInput(session, "xColumnsToJoin",
                                     choices = getColNames(mergeList()[[input$tableX]]))
                 })

                 observeEvent(input$tableY, {
                   req(mergeList(), input$tableY)
                   updateSelectInput(session, "yColumnsToJoin",
                                     choices = getColNames(mergeList()[[input$tableY]]))
                 })

                 observe({
                   req(mergeList(), input$tableX, input$tableY)
                   commonColumns(getCommonColumns(mergeList(), input$tableX, input$tableY))
                 })

                 # output$commonColumns <-
                 #   renderText({
                 #     paste(commonColumns(), collapse = ", ")
                 #   })

                 observeEvent(input$addAllCommonColumns, {
                   req(commonColumns())
                   if (input$addAllCommonColumns) {
                     updateSelectInput(session, "xColumnsToJoin",
                                       selected = commonColumns())
                     updateSelectInput(session, "yColumnsToJoin",
                                       selected = commonColumns())
                   } else {
                     updateSelectInput(session, "xColumnsToJoin",
                                       selected = list())
                     updateSelectInput(session, "yColumnsToJoin",
                                       selected = list())
                   }
                 })

                 # update mergeCommand ----
                 observeEvent(list(input$xColumnsToJoin, input$yColumnsToJoin), {
                   req(names(mergeList()))

                   namesOfAllTables <- names(mergeList())

                   joinString <- getJoinString(input$xColumnsToJoin, input$yColumnsToJoin)
                   tableX <- getTableId(namesOfAllTables)[[input$tableX]]
                   tableY <- getTableId(namesOfAllTables)[[input$tableY]]

                   if (!is.null(joinString) && (tableX != tableY)) {
                     mergeCommand <-
                       tmpl(
                         paste0(
                           c(
                             "{{ tableX }} %>% ",
                             "  {{ mergeOperation }}({{ tableY }},",
                             "    by = {{ joinString }})"
                           ),
                           collapse = "\n"
                         ),
                         tableX = tableX,
                         mergeOperation = input$mergeOperation,
                         tableY = tableY,
                         joinString = joinString
                       )
                   } else {
                     mergeCommand <- ""

                     if (tableX == tableY) {
                       alert("Please choose two different table.")
                     }
                   }

                   updateTextAreaInput(session, "mergeCommand", value = mergeCommand)
                 })

                 # apply mergeCommand ----
                 observeEvent(input$applyMerge, {
                   req(input$mergeCommand)

                   withProgress({
                     # setup data.frames to merge
                     for (i in 1:length(mergeList())) {
                       tableName <- names(mergeList())[i]

                       tableDat <-
                         mergeList()[[tableName]]$dataImport
                       #assign(paste0("colNamesTable", i), colnames(tableDat))

                       # colnames(tableDat) <-
                       #   paste0("col", 1:ncol(tableDat))
                       # assign(paste0("colIdsTable", i), colnames(tableDat))

                       assign(paste0("table", i), tableDat)
                     }

                     # merge data
                     joinedData <- #try(eval(parse(text = input$mergeCommand)))
                       tryCatch({
                         eval(parse(text = input$mergeCommand))
                         },
                         error=function(cond) {
                           #browser()
                           alert(cond)
                           # Choose a return value in case of error
                           return(NULL)
                         },
                         warning=function(cond) {
                           #browser()
                           alert(cond)
                           # Choose a return value in case of warning
                           return(NULL)
                         },
                         finally= NULL
                       )
                     #browser()
                     if (inherits(joinedData, "try-error")) {
                       browser()
                       alert("Could not merge data")
                       return()
                     }

                     #browser()
                     # rename columns
                     # joinedData <- renameColumnIds(
                     #   joinedData = joinedData,
                     #   mappingTable1 = list(
                     #     colIds = get("colIdsTable1"),
                     #     colNames = get("colNamesTable1")
                     #   ),
                     #   mappingTable2 = list(
                     #     colIds = get("colIdsTable2"),
                     #     colNames = get("colNamesTable2")
                     #   )
                     # )

                     if (nrow(joinedData) > 100000)
                       alert(
                         paste0(
                           "Merged data is very large and has ",
                           nrow(joinedData),
                           "rows.",
                           "The app might be very slow or even crash."
                         )
                       )

                     joinedData(joinedData)
                   },
                   value = 0.75,
                   message = 'merging data ...')
                 })

                 output$joinedData <- renderDataTable({
                   req(joinedData())
                   DT::datatable(joinedData(),
                                 rownames = FALSE,
                                 options = list(scrollX = TRUE))
                 })

                 # return value for parent module: ----
                 return(joinedData)

               })
}


getTableChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(getTableId(tableChoices), " --- ", tableChoices)

  tableChoices
}


getTableId <- function(namesOfTables) {
  res <- paste0("table", 1:length(namesOfTables))
  names(res) <- namesOfTables

  res
}


getCommonColumns <- function(tableList, tableX, tableY) {
  colnamesX <- getColNames(tableList[[tableX]])
  colnamesY <- getColNames(tableList[[tableY]])

  intersect(colnamesX, colnamesY)
}


getColNames <- function(tableListElement) {
  colnames(tableListElement$dataImport)
}





getJoinString <- function(xColumns, yColumns) {
  minLength <- min(length(xColumns), length(yColumns))

  if (minLength == 0) {
    return(NULL)
  }

  xColumns <- paste0("\"", xColumns[1:minLength], "\"")
  yColumns <- paste0("\"", yColumns[1:minLength], "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}


# getColumNames <- function(mergeList) {
#   colNamesForMerge <- lapply(mergeList, function(df) {
#     data.frame(
#       column.id = 1:length(df$dataImport),
#       column.name = colnames(df$dataImport)
#     )
#   }) %>%
#     bind_rows(.id = "table.name")
#
#   colNamesForMerge$table.id <- colNamesForMerge %>%
#     group_by(.data$table.name) %>%
#     group_indices()
#
#   colNamesForMerge$table.id <-
#     paste0("table", colNamesForMerge$table.id)
#   colNamesForMerge$column.id <-
#     paste0("col", colNamesForMerge$column.id)
#
#   colNamesForMerge$table.name <- colNamesForMerge$table.name %>%
#     cutStrings(cutAt = 20)
#
#   colNamesForMerge %>% select(.data$table.id, everything())
# }


# renameColumnIds <-
#   function(joinedData, mappingTable1, mappingTable2) {
#     colIdsTable1 <- mappingTable1$colIds
#     colNamesTable1 <- mappingTable1$colNames
#
#     colIdsTable2 <- mappingTable2$colIds
#     colNamesTable2 <- mappingTable2$colNames
#
#     # rename .x columns
#     joinedData <- renameExisting(joinedData,
#                                  oldNames = paste0(colIdsTable1, ".x"),
#                                  newNames = colNamesTable1)
#
#     # rename joined columns
#     joinedData <- renameExisting(joinedData,
#                                  oldNames = colIdsTable1,
#                                  newNames = colNamesTable1)
#
#     # rename unique .y columns
#     uniqueYCols <-
#       !(colNamesTable2 %in% colNamesTable1)
#     joinedData <- renameExisting(joinedData,
#                                  oldNames = paste0(colIdsTable2[uniqueYCols], ".y"),
#                                  newNames = colNamesTable2[uniqueYCols])
#
#     # rename duplicated .y columns
#     duplicatedYCols <-
#       colNamesTable2 %in% colNamesTable1
#     joinedData <- renameExisting(
#       joinedData,
#       oldNames = paste0(colIdsTable2[duplicatedYCols], ".y"),
#       newNames = paste0(colNamesTable2[duplicatedYCols], ".y")
#     )
#
#     # rename remaining columns from y
#     joinedData <- renameExisting(joinedData,
#                                  oldNames = colIdsTable2,
#                                  newNames = colNamesTable2)
#
#     joinedData
#   }


# renameExisting <- function(df, oldNames, newNames) {
#   existing <- match(oldNames, names(df))
#   names(df)[na.omit(existing)] <- newNames[which(!is.na(existing))]
#
#   df
# }
