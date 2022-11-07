# Merge Via UI Module ----

#' Merge Via UI UI
#'
#' UI of the merge via UI module
#'
#' @param id id of module
mergeViaUIUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      6,
      div(
        style = "margin-bottom: 0px",
        selectInput(
          ns("columnsX"),
          "Select x columns to join",
          choices = NULL,
          multiple = TRUE
        )
      ),
      tags$html(HTML(paste0("(Names are used for joined columns.)"))
      ),
    ),
    column(
      6,
      selectInput(
        ns("columnsY"),
        "Select y columns to join",
        choices = NULL,
        multiple = TRUE
      )
    )),
    tags$br(),
    fluidRow(
    column(
      6,
      checkboxInput(ns("addAllCommonColumns"), "Join on all common columns", value = FALSE)
    ),
    column(
      6,
      #style = "margin-top: 12px;",
      selectInput(
        ns("mergeOperation"),
        "Select join operation",
        choices = c(
          "inner_join: all rows in x and y" = "inner_join",
          "left_join: all rows in x" = "left_join",
          "right_join: all rows in y" = "right_join",
          "full_join: all rows in x or y" = "full_join"
        ),
        selected = "left_join"
      )
    ))
  )
}

#' Merge Via UI Server
#'
#' Server function of the merge via UI module
#' @param id id of module
#' @param tableXData (data.frame) data to be merged
#' @param tableYData (data.frame) data to be merged
#' @param tableXId (character) internal table name, see \link{extractTableIds}
#' @param tableYId (character) internal table name, see \link{extractTableIds}
mergeViaUIServer <-
  function(id,
           tableXData,
           tableYData,
           tableXId,
           tableYId) {
    moduleServer(id,
                 function(input, output, session) {
                   commonColumns <- reactiveVal()
                   columnsToJoin <- reactiveValues(tableX = NULL,
                                                   tableY = NULL)
                   mergeCommandAuto <- reactiveVal()

                   # update: column selection ----
                   observeEvent(tableXData(), {
                     updateSelectInput(session, "columnsX",
                                       choices = colnames(tableXData()),
                                       selected = list())

                     commonColumns(
                       extractCommon(colnames(tableXData()), colnames(tableYData()))
                     )
                   })

                   observeEvent(tableYData(), {
                     updateSelectInput(session, "columnsY",
                                       choices = colnames(tableYData()),
                                       selected = list())

                     commonColumns(
                       extractCommon(colnames(tableXData()), colnames(tableYData()))
                     )
                   })



                   observeEvent(list(input$addAllCommonColumns, commonColumns()), {
                     req(!is.null(input$addAllCommonColumns))
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

                   # create: mergeCommandAuto ----
                   observeEvent(list(input$columnsX, input$columnsY), {
                     columnsToJoin$tableX <-
                       equalizeLength(input$columnsX, input$columnsY)$xColNames
                     columnsToJoin$tableY <-
                       equalizeLength(input$columnsX, input$columnsY)$yColNames

                     colJoinString <-
                       extractJoinString(columnsToJoin$tableX, columnsToJoin$tableY)

                     if (isNotEmptyColumnsAndNonEqualTables(colJoinString, tableXId(), tableYId())) {
                       mergeCommandAuto(
                         tmpl(
                           paste0(
                             c(
                               "{{ tableX }} %>% ",
                               "  {{ mergeOperation }}({{ tableY }},",
                               "    by = {{ colJoinString }})"
                             ),
                             collapse = ""
                           ),
                           tableX = tableXId(),
                           mergeOperation = input$mergeOperation,
                           tableY = tableYId(),
                           colJoinString = colJoinString
                         ) %>% as.character()
                       )
                     } else {
                       mergeCommandAuto("")

                       if (isEqualTables(tableXId(), tableYId())) {
                         alert("Please choose two different tables.")
                       }
                     }
                   })

                   # return value for parent module: ----
                   return(mergeCommandAuto)
                 })
  }


# Merge Via UI Helper Functions ----

## helpers: column selection ----

extractCommon <- function(colnamesX, colnamesY) {
  if (is.null(colnamesX) || is.null(colnamesY) ||
      length(colnamesX) == 0 || length(colnamesY) == 0) return(list())

  intersect(colnamesX, colnamesY)
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

isNotEmptyColumnsAndNonEqualTables <-
  function(colJoinString, tableXId, tableYId) {
    !(colJoinString == "c(\"\"=\"\")") && (tableXId != tableYId)
  }

isEqualTables <- function(tableXId, tableYId) {
  !is.null(tableXId) && !is.null(tableYId) && tableXId == tableYId
}
