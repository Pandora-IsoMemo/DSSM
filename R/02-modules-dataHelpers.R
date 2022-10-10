# Collection of helper modules for the data tab

#' Location Fields UI
#'
#' UI function for the module
#'
#' @param id namespace
#' @param title title in tab
locationFieldsUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$hr(),
    tags$h4(title),
    conditionalPanel(
      condition = "output.dataSource == 'file'",
      ns = ns,
      radioButtons(
        inputId = ns("coordType"),
        label = "Coordinate format",
        choiceNames = c(
          "decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
          "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
          "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"
        ),
        choiceValues = c(
          "decimal degrees",
          "degrees decimal minutes",
          "degrees minutes seconds"
        ),
        selected = "decimal degrees"
      )
    ),
    selectInput(ns("longitude"), "Longitude", choices = NULL),
    selectInput(ns("latitude"), "Latitude", choices = NULL),
    tags$hr()
  )
}


#' Location Fields Server
#'
#' Backend for the module
#'
#' @param id namespace id
#' @param dataRaw (reactive) raw data import
#' @param dataSource (reactive) source of loaded data, either "db" or "file" for database or
#' file upload, respectively.
locationFieldsServer <-
  function(id, dataRaw, dataSource) {
    moduleServer(id,
                 function(input, output, session) {
                   # possibly necessary later, if input$coordType not available from beginning on,
                   # because of the conditionalInput:
                   # coordinateType <- reactiveVal("decimal degrees")
                   #
                   # observeEvent(input$coordType, {
                   #   req(input$coordType)
                   #   coordinateType(input$coordType)
                   # })

                   output$dataSource <- renderText({
                     dataSource()
                   })
                   outputOptions(output, "dataSource", suspendWhenHidden = FALSE)

                   observeEvent(list(input$coordType, dataRaw()), {
                     req(dataRaw())

                     # get possible column names for lat long
                     latLongChoices <- switch(input$coordType,
                                              "decimal degrees" = partialNumericColumns(dataRaw()),
                                              colnames(dataRaw()))

                     # get default column names
                     defaultLongCol <- getDefaultCoordColumn(
                       columnNames = colnames(dataRaw()),
                       tryPattern = c("longitude", "^long$", "^lng$")
                     )

                     defaultLatCol <- getDefaultCoordColumn(
                       columnNames = colnames(dataRaw()),
                       tryPattern = c("latitude", "^lat$")
                     )

                     # update inputs
                     updateSelectInput(session,
                                       "longitude",
                                       choices = latLongChoices,
                                       selected = defaultLongCol)
                     updateSelectInput(session,
                                       "latitude",
                                       choices = latLongChoices,
                                       selected = defaultLatCol)
                   })

                   list(
                     coordType = reactive(input$coordType),
                     longitudeColname = reactive(input$longitude),
                     latitudeColname = reactive(input$latitude)
                   )
                 })
  }


#' Get Default Coord Column
#'
#' @param columnNames (character) column names of loaded data
#' @param tryPattern (character) pattern that should be matched with column names ordered after
#'  priority
getDefaultCoordColumn <-
  function(columnNames,
           tryPattern = c("latitude", "^lat$")) {
    defaultColumn <- ""

    while (length(tryPattern) > 0) {
      isLatitude <- grepl(tryPattern[1], tolower(columnNames))
      if (any(isLatitude)) {
        defaultColumn <- columnNames[isLatitude][[1]]
        tryPattern <- c()
      } else {
        tryPattern <- tryPattern[-1]
      }
    }

    defaultColumn
  }
