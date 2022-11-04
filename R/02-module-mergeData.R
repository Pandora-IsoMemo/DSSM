# Prepare Data Module ----

#' Prepare Data UI
#'
#' UI of the module
#'
#' @param id id of module
prepareDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    renameColumnsUI(ns("renameCols")),
    joinColumnsUI(ns("joinCols")),
    splitColumnsUI(ns("splitCols")),
    deleteColumnsUI(ns("deleteCols")),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
  )
}


#' Prepare Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param selectedData (list) list of data to be merged
prepareDataServer <- function(id, selectedData) {
  moduleServer(id,
               function(input, output, session) {
                 preparedData <- reactiveVal()

                 observeEvent(selectedData(), {
                   preparedData(selectedData())
                 })

                 newColNames <- renameColumnsServer("renameCols",
                                                    columnNames = reactive(colnames(preparedData())))

                 observeEvent(newColNames(), {
                   req(newColNames())
                   tmpData <- preparedData()
                   colnames(tmpData) <- newColNames()
                   preparedData(tmpData)
                 })

                 reducedData <-
                   deleteColumnsServer("deleteCols", preparedData)

                 observeEvent(reducedData(), {
                   req(reducedData())
                   preparedData(reducedData())
                 })

                 joinedData <-
                   joinColumnsServer("joinCols", preparedData)

                 observeEvent(joinedData(), {
                   req(joinedData())
                   preparedData(joinedData())
                 })

                 splittedData <-
                   splitColumnsServer("splitCols", preparedData)

                 observeEvent(splittedData(), {
                   req(splittedData())
                   preparedData(splittedData())
                 })

                 output$preview <- renderDataTable({
                   req(preparedData())

                   previewData <-
                     cutAllLongStrings(preparedData()[1:2, ], cutAt = 20)
                   DT::datatable(
                     previewData,
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

                 preparedData
               })
}


## Rename Columns Module ----

#' Rename Columns UI
#'
#' UI of the module
#'
#' @param id id of module
renameColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(tags$br(),
          fluidRow(
            column(5, selectInput(
              ns("columnToRename"), "Rename a column", choices = NULL
            )),
            column(5, style = "margin-top: 18px;", textInput(
              ns("newName"), label = NULL, placeholder = "New name"
            )),
            column(
              2,
              align = "right",
              style = "margin-top: 18px;",
              actionButton(ns("setColName"), "Set", width = "100%")
            )
          ))
}

#' Rename Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param columnNames (reactive) column names
renameColumnsServer <- function(id, columnNames) {
  moduleServer(id,
               function(input, output, session) {
                 newColumnNames <- reactiveVal()

                 observeEvent(columnNames(), {
                   updateSelectInput(session, "columnToRename", choices = columnNames())
                   updateTextInput(session, "newName", value = "")

                   # by default return current column names
                   newColumnNames(columnNames())
                 })

                 observeEvent(input$setColName, {
                   req(columnNames(), input$newName)

                   tmpNames <- columnNames()
                   tmpNames[tmpNames == input$columnToRename] <-
                     input$newName
                   newColumnNames(tmpNames)
                 })

                 newColumnNames
               })
}


## Delete Columns Module ----

#' Delete Columns UI
#'
#' UI of the module
#'
#' @param id id of module
deleteColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(tags$br(),
          fluidRow(
            column(
              5,
              selectInput(
                ns("columnsToDelete"),
                "Delete column(s)",
                choices = NULL,
                multiple = TRUE
              )
            ),
            column(
              3,
              offset = 4,
              align = "right",
              style = "margin-top: 18px;",
              actionButton(ns("deleteCol"), "Delete")
            )
          ))
}

#' Delete Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) selected data, possibly already modified
deleteColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(
                     session,
                     "columnsToDelete",
                     choices = colnames(preparedData()),
                     selected = c()
                   )

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$deleteCol, {
                   req(preparedData(), input$columnsToDelete)

                   tmpData <- preparedData()
                   tmpData <-
                     tmpData[!(colnames(tmpData) %in% input$columnsToDelete)]
                   newData(tmpData)
                 })

                 newData
               })
}


## Join Columns Module ----

#' Join Columns UI
#'
#' UI of the module
#'
#' @param id id of module
joinColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("column1ToJoin"), "Join column 1", choices = NULL
    )),
    column(2, style = "margin-top: 18px;",
           textInput(
             ns("sep"), label = NULL, value = "; "
           )),
    column(4,
           selectInput(
             ns("column2ToJoin"), "with column 2", choices = NULL
           ))
  ),
  fluidRow(
    column(5, textInput(
      ns("newName"), label = NULL, placeholder = "New name"
    )),
    column(
      4,
      offset = 1,
      style = "margin-top: 14px;",
      checkboxInput(ns("keepOrigColumns"), "Keep input columns", value = TRUE)
    ),
    column(2, align = "right",
           actionButton(ns("join"), "Join", width = "100%"))
  ))
}

#' Join Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) (reactive) selected data, possibly already modified
joinColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(session, "column1ToJoin",
                                     choices = colnames(preparedData()))
                   updateSelectInput(session, "column2ToJoin",
                                     choices = colnames(preparedData()))
                   updateTextInput(session, "newName", value = "")

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$join, {
                   req(preparedData(),
                       input$column1ToJoin,
                       input$column2ToJoin,
                       input$newName)

                   #tmpData <- preparedData()
                   # newName <- input$newName
                   # tmpData[[newName]] <- paste(tmpData[[input$column1ToJoin]],
                   #                             tmpData[[input$column2ToJoin]],
                   #                             sep = input$sep)
                   #
                   # if (!input$keepOrigColumns) {
                   #   tmpData <- tmpData[!(colnames(tmpData) %in% c(input$column1ToJoin, input$column2ToJoin))]
                   # }

                   tmpData <- preparedData() %>%
                     unite(
                       !!input$newName,
                       c(input$column1ToJoin, input$column2ToJoin),
                       sep = input$sep,
                       remove = !input$keepOrigColumns,
                       na.rm = TRUE
                     )

                   newData(tmpData)
                 })

                 newData
               })
}


## Split Columns Module ----

#' Split Columns UI
#'
#' UI of the module
#'
#' @param id id of module
splitColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("columnToSplit"), "Split a column", choices = NULL
    )),
    column(2, style = "margin-top: 18px;",
           textInput(
             ns("sep"), label = NULL, value = "; "
           )),
    column(4, style = "margin-top: 30px;",
           checkboxInput(
             ns("keepOrigColumn"), "Keep input column", value = TRUE
           )),
  ),
  fluidRow(
    column(5, textInput(
      ns("newName1"), label = NULL, placeholder = "New name 1"
    )),
    column(5, textInput(
      ns("newName2"), label = NULL, placeholder = "New name 2"
    )),
    column(2, align = "right",
           actionButton(ns("split"), "Split", width = "100%"))
  ))
}

#' Split Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) (reactive) selected data, possibly already modified
splitColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(session, "columnToSplit",
                                     choices = colnames(preparedData()))
                   updateTextInput(session, "newName1", value = "")
                   updateTextInput(session, "newName1", value = "")

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$split, {
                   req(preparedData(),
                       input$columnToSplit,
                       input$newName1,
                       input$newName2)

                   tmpData <- preparedData() %>%
                     separate(
                       !!input$columnToSplit,
                       c(input$newName1, input$newName2),
                       sep = input$sep,
                       remove = !input$keepOrigColumn
                     )

                   newData(tmpData)
                 })

                 newData
               })
}


# Merge Data Module ----

#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
mergeDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    fluidRow(
      column(
        8,
        selectInput(
          ns("tableX"),
          "Select tabel x",
          choices = NULL,
          width = "100%"
        )
      ),
      column(4, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableX")))
    ),
    fluidRow(
      column(
        8,
        selectInput(
          ns("tableY"),
          "Select tabel y",
          choices = NULL,
          width = "100%"
        )
      ),
      column(4, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableY")))
    ),
    mergeViaUIUI(ns("mergerViaUI")),
    conditionalPanel(
      condition = "input.useMergeViaCommand == true",
      mergeViaCommandUI(ns("mergerViaCommand")),
      ns = ns
    ),
    fluidRow(
      column(3, actionButton(ns("applyMerge"), "Apply Merge")),
      column(4, checkboxInput(
        ns("useMergeViaCommand"), "Check command line"
      )),
      column(5, align = "right", style = "margin-top: 12px;", textOutput(ns(
        "nRowsJoinedData"
      )))
    ),
    #actionButton(ns("addMerge"), "Add Table"),
    conditionalPanel(
      ns = ns,
      condition = "output.showWarning == 'TRUE'",
      tags$br(),
      tags$html(HTML(
        paste0(
          "<p style=\"color:red\">Merged data has more rows than the maximal ",
          " number of rows of the input tables.",
          " One row of one table matches several rows of the other table.<br>",
          " Please check the x and y colums to join on.</p>"
        )
      ))
    ),
    tags$hr(),
    tags$h5("Preview Data"),
    tags$h5("(Long character entries might be cutted in the preview.)"),
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
                 tableId <- reactiveValues(tableX = NULL,
                                           tableY = NULL)

                 tableIds <- reactiveVal()

                 tableXData <- reactiveVal()
                 tableYData <- reactiveVal()

                 joinedData <- reactiveVal()

                 # update: table selection ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)

                   tableIds(extractTableIds(names(mergeList())))

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

                 observeEvent(input$tableX, {
                   tableXData(mergeList()[[input$tableX]])
                 })

                 output$nRowsTableX <- renderText({
                   req(tableXData())
                   paste(NROW(tableXData()), "rows")
                 })

                 observeEvent(input$tableY, {
                   tableYData(mergeList()[[input$tableY]])
                 })

                 output$nRowsTableY <- renderText({
                   req(tableYData())
                   paste(NROW(tableYData()), "rows")
                 })

                 mergeCommandAuto <-
                   mergeViaUIServer(
                     "mergerViaUI",
                     tableXData = tableXData,
                     tableYData = tableYData,
                     tableXId = reactive(tableIds()[input$tableX]),
                     tableYId = reactive(tableIds()[input$tableY])
                   )

                 mergeCommandManual <-
                   mergeViaCommandServer("mergerViaCommand", mergeCommandAuto)

                 # apply: mergeCommand ----
                 observeEvent(input$applyMerge, {
                   joinedData(NULL)

                   req(mergeCommandManual())

                   withProgress({
                     ## create data.frames to merge ----
                     for (i in c(input$tableX, input$tableY)) {
                       assign(tableIds()[i],
                              mergeList()[[i]])
                     }

                     ## match column types ----
                     columsToJoinString <- mergeCommandManual() %>%
                       gsub(pattern = ".*by = ", replacement = "") %>%
                       gsub(pattern = ")$", replacement = "")

                     columsToJoin <-
                       eval(parse(text = columsToJoinString))

                     xColNames <- names(columsToJoin)
                     yColNames <- unname(columsToJoin)

                     assign(
                       tableIds()[input$tableY],
                       matchColClasses(
                         df1 = get(tableIds()[input$tableX]),
                         df2 = get(tableIds()[input$tableY]),
                         xColNames = xColNames,
                         yColNames = yColNames,
                         df1Id = tableIds()[input$tableX]
                       )
                     )

                     ## merge data ----
                     joinedData <-
                       tryCatch({
                         eval(parse(text = mergeCommandManual()))
                       },
                       error = function(cond) {
                         alert(cond$message)
                         # Choose a return value in case of error
                         return(NULL)
                       },
                       warning = function(cond) {
                         alert(cond$message)
                         # Choose a return value in case of warning
                         return(NULL)
                       },
                       finally = NULL)
                     if (inherits(joinedData, "try-error")) {
                       alert("Could not merge data")
                       return(NULL)
                     }

                     if (!is.null(joinedData) &&
                         nrow(joinedData) > 100000)
                       alert(
                         paste0(
                           "Warning: Merged data is very large and has ",
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

                 output$nRowsJoinedData <- renderText({
                   req(joinedData())
                   paste("Merged data has ", NROW(joinedData()), "rows")
                 })

                 output$showWarning <- renderText({
                   req(joinedData())
                   maxRows <-
                     max(NROW(tableXData()), NROW(tableYData()))

                   NROW(joinedData()) > maxRows
                 })
                 outputOptions(output, "showWarning", suspendWhenHidden = FALSE)

                 output$joinedData <- renderDataTable({
                   req(joinedData())

                   previewData <-
                     cutAllLongStrings(joinedData()[1:2, ], cutAt = 20)

                   DT::datatable(previewData,
                                 rownames = FALSE,
                                 options = list(scrollX = TRUE))
                 })

                 # return value for parent module: ----
                 return(joinedData)

               })
}


# Merge Data Helper Functions ----

## helpers: table selection ----
extractMergeChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(extractTableIds(tableChoices), " -- ", tableChoices)

  tableChoices
}


#' Extract Table IDs
#'
#' @param namesOfTables (character) names of loaded tables, often url's to the table file, something
#'  like "https://pandoradata.earth/dataset/.../download/afriarch-isotopic-dataset.xlsx"
#' @return (character) short internal table names
extractTableIds <- function(namesOfTables) {
  ids <- paste0("table", 1:length(namesOfTables))
  names(ids) <- namesOfTables

  ids
}

## helpers: ----
### class matching ----
matchColClasses <-
  function(df1,
           df2,
           xColNames,
           yColNames,
           df1Id = "table1",
           isTest = FALSE) {
    colTypesX <- sapply(df1[, xColNames, drop = FALSE], class)
    colTypesY <- sapply(df2[, yColNames, drop = FALSE], class)

    isAllEqual <- equalColClasses(colTypesX,
                                  colTypesY,
                                  df1Id = df1Id,
                                  isTest = isTest)

    if (!isAllEqual) {
      for (i in 1:length(yColNames)) {
        suppressWarnings(class(df2[, yColNames[i]]) <- colTypesX[i])
      }
    }
    return(df2)
  }


equalColClasses <-
  function(colTypesX,
           colTypesY,
           df1Id = "table1",
           isTest = FALSE) {
    typeMismatch <- colTypesX != colTypesY

    if (any(typeMismatch)) {
      if (!isTest) {
        shinyjs::alert(
          paste0(
            "Warning: Column types not matching for: \n",
            extractJoinString(names(colTypesX)[typeMismatch],
                              names(colTypesY)[typeMismatch]),
            ". \n\n",
            "Using the type of ",
            df1Id,
            " for these columns."
          )
        )
      }
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
