#' ui function of dataExplorer module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
dataExplorerUI <- function(id, title = ""){
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(
          "skin", ## no namespace to make it easier to use it across tabs
          "Skin",
          choices = c("Pandora" = "pandora", "IsoMemo" = "isomemo"),
          selected = "pandora"
        ),
        conditionalPanel(
          condition = "input.skin == 'isomemo'",
          pickerInput(
            inputId = ns("database"),
            label = "Database selection:",
            choices = getDatabaseList(),
            options = list(
              `actions-box` = FALSE,
              size = 10,
              `none-selected-text` = "No database selected",
              `selected-text-format` = "count > 8"
            ),
            multiple = TRUE
          ),
          actionButton( ns("load"), "Load data"),
          tags$br(),
          tags$hr(),
          tags$h4("Select categories"),
          unname(lapply(categoryChoices(getMappingTable()), combineCheckboxSelectize, ns = ns))
        ),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
          importDataUI(ns("localData"), "Import Data"),
          tags$hr(),
          tags$h4("Location Fields"),
          selectInput(ns("LongitudePandora"), "Longitude", choices = NULL),
          selectInput(ns("LatitudePandora"), "Latitude", choices = NULL),
          radioButtons(inputId = ns("CoordTypePandora"),
                       label = "Coordinate format",
                       choiceNames = c("decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
                                       "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
                                       "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"),
                       choiceValues = c("decimal degrees", "degrees decimal minutes", "degrees minutes seconds")),
          tags$h4("Radiocarbon Calibration Fields"),
          selectInput(ns("calibrationDatingType"), "Date Type", choices = c("Mean + 1SD uncertainty")),
          conditionalPanel(
            condition = "input.calibrationDatingType == 'Mean + 1SD uncertainty'",
          selectInput(ns("calibrationDateMean"), "Date Mean", choices = NULL),
          selectInput(ns("calibrationDateUncertainty"), "Date Uncertainty", choices = NULL),
          ns = ns
          ),
          conditionalPanel(
            condition = "input.calibrationDatingType == 'Interval'",
            selectInput(ns("calibrationDateIntLower"), "Lower time points", choices = NULL),
            selectInput(ns("calibrationDateIntUpper"), "Upper time points", choices = NULL),
            ns = ns
            )
        ),
        tags$hr(),
        selectInput(inputId = ns("calMethod"),
                    label = "Calibration method (optional):",
                    choices = c("none", "intcal20", "intcal13",
                                "intcal13nhpine16", "shcal20",
                                "shcal13", "shcal13shkauri16",
                                "marine13", "marine20"), width = "100%"),
        numericInput(inputId = ns("calLevel"),
                     label = "Calibration range", min = 0.5,
                     max = 0.99, value = 0.95, step  = 0.01),
        tags$hr(),
        downloadButton(ns("saveOptions"), "Save data selection"),
        fileInput(ns("optionsFile"), label = "", buttonLabel = "Load data selection"),
        tags$hr(),
        actionButton(ns("export"), "Export Data"),
        tags$hr(),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
          selectInput(ns("citationColumns"), "Citation columns", choices = NULL, multiple = TRUE)
        ),
        selectInput(ns("citationType"), "Citation Type", selected = "txt", choices = c("txt", "xml", "json")),
        downloadButton(ns("exportCitation"), "Export Citation")
      ),
      mainPanel(
        div(
          class = "last-updated",
          textOutput(ns("lastUpdate"))
        ),
        DT::dataTableOutput(ns("dataTable"))
      )
    )
  )
}

combineCheckboxSelectize <- function(x, ns){
  choices <- columnChoices(x, getMappingTable(), FALSE)
  selected <- FALSE
  label <- x
  x <- gsub(" ", "", x)
  tagList(
    checkboxInput(
      ns(paste0("selectCategory", x)),
      label = label,
      value = selected
    ),
    conditionalPanel(paste0("input.selectCategory", x, " == true"),
                     pickerInput(
                       inputId = ns(paste0("selectColumns", x)),
                       label = NULL,
                       choices = choices,
                       selected = choices,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         `none-selected-text` = "No fields selected",
                         `selected-text-format` = "count > 8"
                       ),
                       multiple = TRUE
                     ),
                     ns = ns)
  )
}


#' server funtion of data explorer module
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @export
dataExplorer <- function(input, output, session){
  ns <- session$ns


  ## Load Data
  updateDataBase <- reactive({input$load})

  database <- eventReactive(updateDataBase(), ignoreNULL = FALSE, {
    if (is.null(updateDataBase()))
      return(NULL)

    input$database
  })

  mappingTable <- reactive({
    getMappingTable()
  })

  isoDataRaw <- reactiveVal(NULL)

  observe({
    req(database())

    withProgress({
      d <- getRemoteData(database())

      isoDataRaw(d)
    },
      value = 0.75,
      message = 'Get remote data ...'
    )
  })

  ## Load Data from file
  importedData <- callModule(importData, "localData")

  observe({
    req(length(importedData()) > 0)

    d <- importedData()[[1]]

    isoDataRaw(d)
    updateSelectInput(session, "LongitudePandora", choices = partialNumericColumns(isoDataRaw()), selected = "")
    updateSelectInput(session, "LatitudePandora", choices = partialNumericColumns(isoDataRaw()), selected = "")
    updateSelectInput(session, "calibrationDateMean", choices = partialNumericColumns(isoDataRaw()))
    updateSelectInput(session, "calibrationDateUncertainty", choices = partialNumericColumns(isoDataRaw()))
    updateSelectInput(session, "calibrationDateIntLower", choices = partialNumericColumns(isoDataRaw()))
    updateSelectInput(session, "calibrationDateIntUpper", choices = partialNumericColumns(isoDataRaw()))

    #updateSelectInput(session, "calibrationDatingType", choices = characterColumns(isoDataRaw()))
  })

  isoDataFull <- reactive({
    d <- isoDataRaw()

    if (getSkin() == "isomemo") {
      dateFields <- list(
        "dateMean" = "dateMean",
        "dateUncertainty" = "dateUncertainty",
        "datingType" = "datingType"
      )
    } else {
      d$source <- "Pandora"
      if(input$calibrationDatingType == "Mean + 1SD uncertainty"){
        dateFields <- list(
          "dateMean" = input$calibrationDateMean,
          "dateUncertainty" = input$calibrationDateUncertainty,
          "datingType" = "radiocarbon"
        )
      } else {
        dateFields <- list(
          "dateLower" = input$calibrationDateIntLower,
          "dateUpper" = input$calibrationDateIntUpper,
          "datingType" = "radiocarbon"
        )
      }

      if(!is.null(input$LongitudePandora) & !is.null(input$LatitudePandora) & input$LongitudePandora != "" & input$LatitudePandora != ""){
        dCoord <- try({convertLatLong(d, CoordType = input$CoordTypePandora, Latitude = input$LongitudePandora, Longitude = input$LatitudePandora)}, silent = TRUE)

        if(class(dCoord) == "try-error"){
          alert("Conversion of coordinates has failed. Please select appropriate longitude / latitude fields and coordinate type.")
        } else {
        d <- dCoord
        d$longitude <- d[,input$LongitudePandora]
        d$latitude <- d[,input$LatitudePandora]
        d$id <- as.character(1:nrow(d))
        }
      }

    }

    if (calibrate()){
      d <- showMessage(
        calibrateRadiocarbon,
        "Calculating calibrated values")(
        d, calMethod = calibrateMethod(),
        level = calLevel(),
        dateFields = dateFields
      )
    }

    d
  })


  ## Last update
  output$lastUpdate <- renderText({
    if (is.null(isoDataFull()) || is.null(attr(isoDataFull(), "updated"))) NULL
    else paste("Data last updated at", attr(isoDataFull(), "updated"))
  })

  ## Calibration
  calibrate <- reactive({
    input$calMethod != "none"
  })

  calLevel <- reactive({
    input$calLevel
  })


  calibrateMethod <- reactive({
    input$calMethod
  })

  observe({
    lapply(categoryChoices(mappingTable()), function(x){
      choices <- columnChoices(x, mappingTable(), calibrate())
      updateSelectizeInput(session,
                           paste0("selectColumns", gsub(" ", "", x)),
                           choices = choices,
                           selected = choices)
    })
  })


  ## Column selection
  dataColumns <- reactive({
    if (getSkin() == "isomemo"){
      getDataColumns(mappingTable(), input)
    } else{
      names(isoDataFull())
    }
  })

  isoData <- reactive({
    if (is.null(isoDataFull()))
      return(NULL)
    isoDataFull()[input$dataTable_rows_all, names(isoDataFull()) %in% dataColumns()]
  })

  isoDataExport <- reactive({
    if (is.null(isoDataFull()))
      return(NULL)
    dCols <- dataColumns()
    if("description" %in% dCols){
      dCols[dCols == "description"] <- "descriptionFull"
    }

    isoDataFull()[input$dataTable_rows_all, names(isoDataFull()) %in% dCols]
  })

  ## Export
  colseparator <- reactive({
    input$colseparator
   })

  decseparator <- reactive({
    input$decseparator
  })

  observeEvent(input$export, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        session$ns("exportType"),
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        ns = session$ns,
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("colseparator"), "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("decseparator"), "decimal separator:", value = "."))
      ),
      downloadButton(session$ns("exportExecute"), "Export")
    ))
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      exportFilename(fileending = input$exportType)
    },
    content = function(file){
      switch(
        input$exportType,
        csv = exportCSV(file, isoDataExport(), colseparator(), decseparator()),
        xlsx = exportXLSX(file, isoDataExport()),
        json = exportJSON(file, isoDataExport())
      )
    }
  )

  ## Save / load options
  output$saveOptions <- downloadHandler(
    filename = "options.json",
    content = function(file){
      options <- list(
        database = input$database,
        columns = dataColumns(),
        calibrateMethod = input$calMethod
      )
      write(toJSON(options), file = file)
    }
  )

  observe({
    optionsFile <- input$optionsFile

    if (is.null(optionsFile))
      return(NULL)

    opt <- fromJSON(paste0(readLines(optionsFile$datapath)))

    loadOptions(session, opt, mappingTable())
 })


  ## Output
  output$dataTable <- renderDataTable({
    validate(
      need(!is.null(isoDataFull()), "Please select a database in the sidebar panel.")
    )
    datTable(isoDataFull(), columns = dataColumns())
  })

  decriptionTableClick <- reactive({
    getDescriptionCells(input$dataTable_cell_clicked, isoData(), dataColumns())
  })

  observeEvent(decriptionTableClick(), ignoreInit = TRUE, ignoreNULL = FALSE, {

    if (length(decriptionTableClick()) == 0) return(NULL)

    showModal(modalDialog(
      getDescriptionFull(decriptionTableClick(), isoDataFull()),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })

  observe({
    if (is.null(isoDataFull())) shinyjs::disable("exportCitation")
    else shinyjs::enable("exportCitation")
  })

  observe({
    req(isoDataFull())
    updateSelectInput(session, "citationColumns", choices = names(isoDataFull()))
  })



  output$exportCitation <- downloadHandler(
    filename = function() {
      paste0("isoMemoCitation.", input$citationType)
    },
    content = function(filename) {
      if (getSkin() == "isomemo"){
        citationColumns <- c(
          "databaseReference", "databaseDOI",
          "compilationReference", "compilationDOI",
          "originalDataReference", "originalDataDOI"
        )
      } else{
        citationColumns <- input$citationColumns
      }

      if (!all(citationColumns %in% colnames(isoDataFull()))) {
        alert("You need to select all citation columns from 'References' first")
        return()
      }

      if (length(citationColumns) != 6) {
        alert("You need to choose exactly 6 columns for exporting citations.")
        return()
      }

      data <- isoDataFull()[citationColumns]
      generateCitation(data, input$citationType, file = filename)
    }
  )

  return(isoData)
}


getDescriptionCells <- function(clickList, isoData, columns){
  if (length(clickList) == 0) return(NULL)
  if (! "description" %in% columns) return(NULL)

  if (clickList$col + 1 != which(names(isoData) == "description")) return(NULL)

  isoData$id[clickList$row]
}

getDescriptionFull <- function(id, isoDataFull){

  isoDataFull$descriptionFull[match(id, isoDataFull$id)]

}

loadOptions <- function(session, opt, mapping){
  if (any(names(opt) != c("database", "columns", "calibrateMethod"))){
    showModal(modalDialog(
      "Could not read file with saved options",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }

  updateCheckboxGroupInput(
    session,
    "database",
    selected = opt$database
  )

  updateSelectInput(
    session,
    "calMethod",
    selected = opt$calibrateMethod
  )

  calibrate <- opt$calibrateMethod != "none"

  lapply(categoryChoices(mapping), function(x){
    choices <- columnChoices(x, mapping, calibrate)
    selected <- intersect(choices, opt$columns)

    updateCheckboxInput(
      session,
      paste0("selectCategory", gsub(" ", "", x)),
      value = (length(selected) > 0)
    )

    updateSelectizeInput(
      session,
      paste0("selectColumns", gsub(" ", "", x)),
      selected = selected
    )
  })

  clickElement(session$ns("load"), delay = 500)
}
