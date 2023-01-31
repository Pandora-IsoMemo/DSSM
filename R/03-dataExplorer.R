#' ui function of dataExplorer module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
dataExplorerUI <- function(id, title = "") {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "position:fixed; width:20%; max-width:350px; overflow-y:auto; height:88%",
        radioButtons(
          "skin",
          ## no namespace to make it easier to use it across tabs
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
          actionButton(ns("load"), "Load data"),
          tags$br(),
          tags$hr(),
          tags$h4("Select categories"),
          unname(lapply(
            categoryChoices(getMappingTable()),
            combineCheckboxSelectize,
            ns = ns
          ))
        ),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
          importDataUI(ns("localData"), "Import Data"),
          locationFieldsUI(ns("locationFieldsId"), title = "Location Fields"),
          tags$h4("Radiocarbon Calibration Fields"),
          selectInput(
            ns("calibrationDatingType"),
            "Date Type",
            choices = c("Mean + 1SD uncertainty")
          ),
          conditionalPanel(
            condition = "input.calibrationDatingType == 'Mean + 1SD uncertainty'",
            selectInput(ns("calibrationDateMean"), "Date Mean", choices = NULL),
            selectInput(
              ns("calibrationDateUncertainty"),
              "Date Uncertainty",
              choices = NULL
            ),
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
        selectInput(
          inputId = ns("calMethod"),
          label = "Calibration method (optional):",
          choices = c(
            "none",
            "intcal20",
            "intcal13",
            "intcal13nhpine16",
            "shcal20",
            "shcal13",
            "shcal13shkauri16",
            "marine13",
            "marine20"
          ),
          width = "100%"
        ),
        numericInput(
          inputId = ns("calLevel"),
          label = "Calibration range",
          min = 0.5,
          max = 0.99,
          value = 0.95,
          step  = 0.01
        ),
        tags$hr(),
        detectDuplicatesUI(id = ns("detectDuplicates")),
        tags$hr(),
        downloadButton(ns("saveOptions"), "Save data selection"),
        fileInput(
          ns("optionsFile"),
          label = "",
          buttonLabel = "Load data selection"
        ),
        tags$hr(),
        actionButton(ns("export"), "Export Data"),
        tags$hr(),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
          selectInput(
            ns("citationColumns"),
            "Citation columns",
            choices = NULL,
            multiple = TRUE
          )
        ),
        selectInput(
          ns("citationType"),
          "Citation Type",
          selected = "txt",
          choices = c("txt", "xml", "json")
        ),
        downloadButton(ns("exportCitation"), "Export Citation")
      ),
      mainPanel(
        div(class = "last-updated",
            textOutput(ns("lastUpdate"))),
        DT::dataTableOutput(ns("dataTable"))
      )
    )
  )
}

combineCheckboxSelectize <- function(x, ns) {
  choices <- columnChoices(x, getMappingTable(), FALSE)
  selected <- FALSE
  label <- x
  x <- gsub(" ", "", x)
  tagList(
    checkboxInput(ns(paste0(
      "selectCategory", x
    )),
    label = label,
    value = selected),
    conditionalPanel(
      paste0("input.selectCategory", x, " == true"),
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
      ns = ns
    )
  )
}


#' server funtion of data explorer module
#'
#' @param id namespace id
#' @return reactive dataframe with loaded or imported isoData
#'
#' @export
dataExplorerServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 mappingTable <- reactive({
                   getMappingTable()
                 })

                 isoDataRaw <- reactiveVal(NULL)
                 isoDataFull <- reactiveVal(NULL)
                 isoData <- reactiveVal(NULL)
                 dataColumns <- reactiveVal(NULL)

                 # detectDuplicates module
                 detectDuplicatesServer(
                   id = "detectDuplicates",
                   inputData = isoDataFull
                 )

                 ## Load Data (isomemo skin) ----
                 observeEvent(input$load, {
                   # reset isoData
                   isoDataRaw(NULL)
                   isoDataFull(NULL)
                   dataColumns(NULL)
                   isoData(NULL)

                   withProgress({
                     d <- getRemoteData(input$database)

                     isoDataRaw(d)
                   },
                   value = 0.75,
                   message = 'Get remote data from database ...'
                   )
                 })

                 ## Load Data from file (pandora skin) ----
                 importedData <- importDataServer("localData")

                 observeEvent(importedData(), {
                   req(length(importedData()) > 0)

                   # reset isoData
                   isoDataRaw(NULL)
                   isoDataFull(NULL)
                   dataColumns(NULL)
                   isoData(NULL)

                   d <- importedData()[[1]] %>%
                     convertNumeric()

                   isoDataRaw(d)

                   updateSelectInput(session, "calibrationDateMean",
                                     choices = partialNumericColumns(isoDataRaw()))
                   updateSelectInput(session,
                                     "calibrationDateUncertainty",
                                     choices = partialNumericColumns(isoDataRaw()))
                   updateSelectInput(session,
                                     "calibrationDateIntLower",
                                     choices = partialNumericColumns(isoDataRaw()))
                   updateSelectInput(session,
                                     "calibrationDateIntUpper",
                                     choices = partialNumericColumns(isoDataRaw()))

                   #updateSelectInput(session, "calibrationDatingType", choices = characterColumns(isoDataRaw()))
                 })

                 locationFields <- locationFieldsServer(
                   "locationFieldsId",
                   dataRaw = isoDataRaw,
                   dataSource = reactive(switch(getSkin(),
                                                "pandora" = "file",
                                                "isomemo" = "db",
                                                "file"))
                 )

                 # Extract isoDataFull (both skins) ----
                 observeEvent(list(locationFields$longitude(),
                                   locationFields$latitude(),
                                   locationFields$coordType(),
                                   calibrateMethod(),
                                   calLevel(),
                                   isoDataRaw()), {
                   req(isoDataRaw())
                   d <- isoDataRaw()

                   if (getSkin() == "isomemo") {
                     dateFields <- list(
                       "dateMean" = "dateMean",
                       "dateUncertainty" = "dateUncertainty",
                       "datingType" = "datingType"
                     )
                   } else {
                     d$source <- "Pandora"
                     if (input$calibrationDatingType == "Mean + 1SD uncertainty") {
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

                     if (!is.null(locationFields$longitude()) &
                         !is.null(locationFields$latitude()) &
                         locationFields$longitude() != "" &
                         locationFields$latitude() != "") {
                       dCoord <-
                         try({
                           convertLatLong(
                             d,
                             CoordType = locationFields$coordType(),
                             Latitude = locationFields$longitude(),
                             Longitude = locationFields$latitude()
                           )
                         }, silent = TRUE)

                       if (class(dCoord) == "try-error") {
                         ### Conversion failure ----
                         if (locationFields$longitude() == "longitude" ||
                             locationFields$latitude() == "latitude") {
                           if (locationFields$longitude() == "longitude") {
                             # rename original to avoid name conflicts
                             tmpIsoDataRaw <- isoDataRaw()
                             tmpIsoDataRaw[[paste0(locationFields$longitude(), "_orig")]] <-
                               tmpIsoDataRaw[["longitude"]]
                             tmpIsoDataRaw[["longitude"]] <- NULL
                             isoDataRaw(tmpIsoDataRaw)
                           }

                           if (locationFields$latitude() == "latitude") {
                             # rename original to avoid name conflicts
                             tmpIsoDataRaw <- isoDataRaw()
                             tmpIsoDataRaw[[paste0(locationFields$latitude(), "_orig")]] <-
                               tmpIsoDataRaw[["latitude"]]
                             tmpIsoDataRaw[["latitude"]] <- NULL
                             isoDataRaw(tmpIsoDataRaw)
                           }
                         } else {
                           alert(
                             paste0(
                               "Conversion of coordinates has failed. Please select appropriate ",
                               "longitude / latitude fields and coordinate format. ",
                               "Columns longitude and latitude were removed (renamed)."
                             )
                           )

                           d$longitude <- NULL
                           d$latitude <- NULL
                         }
                       } else {
                         ### Conversion success ----
                         showNotification(
                           paste0(
                             "Conversion of coordinates succeeded. ",
                             "Columns longitude and latitude set successfully."
                           )
                         )
                         d <- dCoord
                         d$id <- as.character(1:nrow(d))
                         d$longitude <- d[, locationFields$longitude()]
                         d$latitude <- d[, locationFields$latitude()]

                         if (locationFields$longitude() != "longitude") {
                           # remove original
                           d[[locationFields$longitude()]] <- NULL
                         }

                         if (locationFields$latitude() != "latitude") {
                           # remove original
                           d[[locationFields$latitude()]] <- NULL
                         }

                         # put lng/lat to beginning
                         oldColNames <- colnames(d)
                         oldColNames <- oldColNames[!(oldColNames %in% c("longitude", "latitude"))]
                         d <- d[, c("longitude", "latitude", oldColNames)]
                       }
                     }

                   }

                   if (calibrate()) {
                     d <- showMessage(calibrateRadiocarbon,
                                      "Calculating calibrated values")(
                                        d,
                                        calMethod = calibrateMethod(),
                                        level = calLevel(),
                                        dateFields = dateFields
                                      )
                   }

                   isoDataFull(d)
                 })


                 ## Last update (both skins) ----
                 output$lastUpdate <- renderText({
                   if (is.null(isoDataFull()) ||
                       is.null(attr(isoDataFull(), "updated")))
                     NULL
                   else
                     paste("Data last updated at", attr(isoDataFull(), "updated"))
                 })

                 ## Calibration (both skins) ----
                 calibrate <- reactive({
                   input$calMethod != "none"
                 })

                 calLevel <- reactive({
                   input$calLevel
                 })


                 calibrateMethod <- reactive({
                   input$calMethod
                 })

                 ## Column selection (isomemo skin) ----
                 observe({
                   req(getSkin() == "isomemo")
                   lapply(categoryChoices(mappingTable()), function(x) {
                     choices <- columnChoices(x, mappingTable(), calibrate())
                     updateSelectizeInput(
                       session,
                       paste0("selectColumns", gsub(" ", "", x)),
                       choices = choices,
                       selected = choices
                     )
                   })
                 })

                 observe({
                   req(isoDataFull())
                   if (getSkin() == "isomemo") {
                     dataColumns(getDataColumns(mappingTable(), input))
                   } else {
                     dataColumns(names(isoDataFull()))
                   }
                 })

                 observe({
                   if (is.null(isoDataFull()) || is.null(input$dataTable_rows_all)) {
                     isoData(NULL)
                   } else {
                     isoData(isoDataFull()[input$dataTable_rows_all,
                                           names(isoDataFull()) %in% dataColumns(),
                                           drop = FALSE])
                   }
                 })

                 # IsoData export ----
                 isoDataExport <- reactive({
                   if (is.null(isoDataFull()))
                     return(NULL)
                   dCols <- dataColumns()
                   if ("description" %in% dCols) {
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
                   showModal(
                     modalDialog(
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
                         div(
                           style = "display: inline-block;horizontal-align:top; width: 80px;",
                           textInput(session$ns("colseparator"), "column separator:", value = ",")
                         ),
                         div(
                           style = "display: inline-block;horizontal-align:top; width: 80px;",
                           textInput(
                             session$ns("decseparator"),
                             "decimal separator:",
                             value = "."
                           )
                         )
                       ),
                       downloadButton(session$ns("exportExecute"), "Export")
                     )
                   )
                 })

                 output$exportExecute <- downloadHandler(
                   filename = function() {
                     exportFilename(fileending = input$exportType)
                   },
                   content = function(file) {
                     switch(
                       input$exportType,
                       csv = exportCSV(file, isoDataExport(), colseparator(), decseparator()),
                       xlsx = exportXLSX(file, isoDataExport()),
                       json = exportJSON(file, isoDataExport())
                     )
                   }
                 )

                 ## Save / load options ----
                 output$saveOptions <- downloadHandler(
                   filename = "options.json",
                   content = function(file) {
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

                   opt <-
                     fromJSON(paste0(readLines(optionsFile$datapath)))

                   loadOptions(session, opt, mappingTable())
                 })


                 ## Output table ----
                 output$dataTable <- renderDataTable({
                   validate(need(
                     !is.null(isoDataFull()),
                     "Please select a database in the sidebar panel."
                   ))
                   req(dataColumns())
                   datTable(isoDataFull(), columns = dataColumns())
                 })

                 decriptionTableClick <- reactive({
                   getDescriptionCells(input$dataTable_cell_clicked, isoData(), dataColumns())
                 })

                 observeEvent(decriptionTableClick(),
                              ignoreInit = TRUE,
                              ignoreNULL = FALSE,
                              {
                                if (length(decriptionTableClick()) == 0)
                                  return(NULL)

                                showModal(
                                  modalDialog(
                                    getDescriptionFull(decriptionTableClick(), isoDataFull()),
                                    size = "l",
                                    easyClose = TRUE,
                                    footer = modalButton("OK")
                                  )
                                )
                              })

                 # Citation export ----
                 observe({
                   if (is.null(isoDataFull()))
                     shinyjs::disable("exportCitation")
                   else
                     shinyjs::enable("exportCitation")
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
                     if (getSkin() == "isomemo") {
                       citationColumns <- c(
                         "databaseReference",
                         "databaseDOI",
                         "compilationReference",
                         "compilationDOI",
                         "originalDataReference",
                         "originalDataDOI"
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

                 # return isoData ----
                 return(isoData)
               })
}

getDescriptionCells <- function(clickList, isoData, columns) {
  if (length(clickList) == 0)
    return(NULL)
  if (!"description" %in% columns)
    return(NULL)

  if (clickList$col + 1 != which(names(isoData) == "description"))
    return(NULL)

  isoData$id[clickList$row]
}

getDescriptionFull <- function(id, isoDataFull) {
  isoDataFull$descriptionFull[match(id, isoDataFull$id)]

}

loadOptions <- function(session, opt, mapping) {
  if (any(names(opt) != c("database", "columns", "calibrateMethod"))) {
    showModal(
      modalDialog(
        "Could not read file with saved options",
        easyClose = TRUE,
        footer = modalButton("OK")
      )
    )
    return(NULL)
  }

  updateCheckboxGroupInput(session,
                           "database",
                           selected = opt$database)

  updateSelectInput(session,
                    "calMethod",
                    selected = opt$calibrateMethod)

  calibrate <- opt$calibrateMethod != "none"

  lapply(categoryChoices(mapping), function(x) {
    choices <- columnChoices(x, mapping, calibrate)
    selected <- intersect(choices, opt$columns)

    updateCheckboxInput(session,
                        paste0("selectCategory", gsub(" ", "", x)),
                        value = (length(selected) > 0))

    updateSelectizeInput(session,
                         paste0("selectColumns", gsub(" ", "", x)),
                         selected = selected)
  })

  clickElement(session$ns("load"), delay = 500)
}

convertNumeric <- function(data){
  suppressWarnings(data.num <- as.data.frame(lapply(1:ncol(data), function(x){
    y <- as.numeric(data[,x])
    if(sum(is.na(y)) == sum(is.na(data[,x]))){
      return(y)
    } else {
      return(data[,x])
    }
  } )))
  names(data.num) <- names(data)
  data.num
}
