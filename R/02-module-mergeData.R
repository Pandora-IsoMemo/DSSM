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
    textAreaInput(
      ns("mergeCommand"),
      "Merge command",
      value = NULL,
      width = "100%"
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
                 tableId <- reactiveValues(tableX = NULL,
                                           tableY = NULL)
                 commonColumns <- reactiveVal()
                 columnsToJoin <- reactiveValues(tableX = NULL,
                                                 tableY = NULL)
                 mergeCommand <- reactiveVal()
                 joinedData <- reactiveVal()

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
                   updateSelectInput(session, "xColumnsToJoin",
                                     choices = extractColNames(mergeList()[[input$tableX]]))
                 })

                 observeEvent(input$tableY, {
                   req(mergeList(), input$tableY)
                   updateSelectInput(session, "yColumnsToJoin",
                                     choices = extractColNames(mergeList()[[input$tableY]]))
                 })

                 observe({
                   req(mergeList(), input$tableX, input$tableY)
                   commonColumns(extractCommonColumns(mergeList(), input$tableX, input$tableY))
                 })

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

                 # update: mergeCommand ----
                 observeEvent(list(input$columnsX, input$columnsY), {
                   req(names(mergeList()))

                   tableId$tableX <-
                     extractTableIds(names(mergeList()))[[input$tableX]]
                   tableId$tableY <-
                     extractTableIds(names(mergeList()))[[input$tableY]]

                   columnsToJoin$tableX <-
                     equalizeLength(input$columnsX, input$columnsY)$xColumns
                   columnsToJoin$tableY <-
                     equalizeLength(input$columnsX, input$columnsY)$yColumns

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
                       browser()
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

# helpers: table selection ----
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

# helpers: column selection ----
extractCommonColumns <- function(tableList, tableX, tableY) {
  colnamesX <- extractColNames(tableList[[tableX]])
  colnamesY <- extractColNames(tableList[[tableY]])

  intersect(colnamesX, colnamesY)
}


extractColNames <- function(tableListElement) {
  colnames(tableListElement$dataImport)
}


equalizeLength <- function(xColumns, yColumns) {
  minLength <- min(length(xColumns), length(yColumns))

  if (minLength == 0) {
    return(NULL)
  }

  list(xColumns = xColumns[1:minLength],
       yColumns = yColumns[1:minLength])
}


extractJoinString <- function(xColumns, yColumns) {
  xColumns <- paste0("\"", xColumns, "\"")
  yColumns <- paste0("\"", yColumns, "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}

# helpers: class matching----
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
