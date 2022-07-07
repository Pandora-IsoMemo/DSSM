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
    checkboxInput(ns("useMergeViaCommand"), "Merge via command line"),
    conditionalPanel(condition = "input.useMergeViaCommand == false",
                     mergeViaUIUI(ns("mergerViaUI")),
                     ns = ns),
    conditionalPanel(
      condition = "input.useMergeViaCommand == true",
      textAreaInput(
        ns("mergeCommand"),
        "Merge command",
        value = NULL,
        width = "100%"
      ),
      ns = ns
    ),
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
                 joinedData <- reactiveVal()

                 mergeCommand <-
                   mergeViaUIServer("mergerViaUI", mergeList = mergeList)

                 # update: mergeCommand ----
                 observeEvent(mergeCommand(), {
                   updateTextAreaInput(session, "mergeCommand", value = mergeCommand())
                 })

                 # apply: mergeCommand ----
                 observeEvent(input$applyMerge, {
                   req(input$mergeCommand, input$applyMerge)

                   withProgress({
                     # setup data.frames to merge
                     for (i in c("tableX", "tableY")) {
                       tableName <- input[[i]]

                       tableDat <-
                         mergeList()[[tableName]]$dataImport

                       assign(tableId[[i]], tableDat)
                     }

                     # match column types
                     assign(
                       tableId$tableY,
                       matchColClasses(
                         df1 = get(tableId$tableX),
                         df2 = get(tableId$tableY),
                         xColNames = columnsToJoin$tableX,
                         yColNames = columnsToJoin$tableY,
                         df1Id = tableId$tableX
                       )
                     )

                     # merge data
                     joinedData <-
                       tryCatch({
                         eval(parse(text = input$mergeCommand))
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
                       return()
                     }

                     if (!is.null(joinedData) &&
                         nrow(joinedData) > 100000)
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


# Merge Data Helper Functions ----

## helpers: class matching----
matchColClasses <-
  function(df1,
           df2,
           xColNames,
           yColNames,
           df1Id = "table1",
           isTest = FALSE) {
    colTypesX <- sapply(df1[, xColNames], class)
    colTypesY <- sapply(df2[, yColNames], class)

    isAllEqual <- equalColClasses(colTypesX,
                                  colTypesY,
                                  df1Id = df1Id,
                                  isTest = isTest)

    if (!isAllEqual) {
      for (i in 1:length(yColNames)) {
        class(df2[, yColNames[i]]) <- colTypesX[i]
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
            "Column types not matching for: \n",
            extractJoinString(names(colTypesX)[typeMismatch],
                              names(colTypesY)[typeMismatch]),
            ". \n",
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
