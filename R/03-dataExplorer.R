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
          choices = c("Pandora" = "pandora", "Data networks" = "isomemo"),
          selected = "pandora"
        ),
        tags$hr(),
        conditionalPanel(
          condition = "input.skin == 'isomemo'",
          selectInput(ns("mappingId"), "Select data network", choices = c("IsoMemo - Humans" = "IsoMemo")),
          pickerInput(
            inputId = ns("database"),
            label = "Database selection",
            choices = character(0),
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
          uiOutput(ns("categorySelection"))
        ),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
          importDataUI(ns("localData"), "Import Data"),
          locationFieldsUI(ns("locationFieldsId"), title = "Location Fields")
        ),
        tags$hr(),
        tags$h4("Radiocarbon Calibration Fields"),
        conditionalPanel(
          condition = "input.skin == 'pandora'",
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
        selectInput(
          inputId = ns("calMethod"),
          label = "Method (optional)",
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
          label = "Range",
          min = 0.5,
          max = 0.99,
          value = 0.95,
          step  = 0.01
        ),
        tags$hr(),
        conditionalPanel(
          condition = "input.skin == 'isomemo'",
          downloadButton(ns("saveOptions"), "Save data selection"),
          fileInput(
            ns("optionsFile"),
            label = "",
            buttonLabel = "Load data selection"
          ),
          tags$hr()
        ),
        detectDuplicatesUI(id = ns("detectDuplicates")),
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
        downloadButton(ns("exportCitation"), "Export Citation"),
        hr(),
        numericInput(
          inputId = ns("maxCharLength"),
          label = "Maximum Length Character Columns",
          value = NA,
          min = 1,
          max = NA,
          step = 1
        )
      ),
      mainPanel(
        div(class = "last-updated",
            textOutput(ns("lastUpdate"))),
        shinyjs::hidden(
          div(HTML("<b>Preview</b> &nbsp;&nbsp; (Long characters are cutted in the preview)<br><br>"), id = ns("previewText"))
        ),
        DT::dataTableOutput(ns("dataTable")) %>% withSpinner(color = "#20c997")
      )
    )
  )
}

combineCheckboxSelectize <- function(x, ns, mappingTbl) {
  choices <- columnChoices(x, mappingTbl, FALSE)
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

updateCheckboxSelectize <- function(x, session, mappingTbl) {
  choices <- columnChoices(x, mappingTbl, FALSE)
  x <- gsub(" ", "", x)
  updatePickerInput(session, paste0("selectColumns", x), choices = choices, selected = choices)
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

                 mappingTable <- reactiveVal()
                 observe({
                   newVal <- mappingTable() %>%
                     reloadMappingTable(mappingId = input[["mappingId"]])
                   mappingTable(newVal)
                 }) %>%
                   bindEvent(input[["mappingId"]])

                 isoDataRaw <- reactiveVal(NULL)
                 isoDataFull <- reactiveVal(NULL)
                 isoData <- reactiveVal(NULL)
                 dataColumns <- reactiveVal(NULL)

                 # detectDuplicates module
                 detectDuplicatesServer(
                   id = "detectDuplicates",
                   inputData = isoDataFull
                 )

                 output$categorySelection <- renderUI({
                   validate(need(mappingTable(), "No mapping table!"))

                   errorMsg <- "An error occurred. No mapping table!"
                   if (!is.null(attr(mappingTable(), "errorApi"))) {
                     errorMsg <- paste(errorMsg, attr(mappingTable(), "errorApi"))
                   }
                   validate(need(length(mappingTable()) > 0, errorMsg))

                   unname(lapply(
                     categoryChoices(mappingTable()),
                     combineCheckboxSelectize,
                     ns = ns,
                     mappingTbl = mappingTable()
                   ))
                 })

                 observe({
                   lapply(
                     categoryChoices(mappingTable()),
                     updateCheckboxSelectize,
                     session = session,
                     mappingTbl = mappingTable()
                   )
                 }) %>%
                   bindEvent(mappingTable())

                 ## Load Data (isomemo skin) ----
                 observeEvent(input$load, {
                   # reset isoData
                   isoDataRaw(NULL)
                   isoDataFull(NULL)
                   dataColumns(NULL)
                   isoData(NULL)

                   # try reload mappingTable if empty
                   if (length(mappingTable()) == 0 || !has_internet()) {
                     newVal <- mappingTable() %>%
                       reloadMappingTable(mappingId = input[["mappingId"]])
                     mappingTable(newVal)
                   }

                   req(input$database, has_internet())
                   withProgress({
                     d <- getData(db = input$database, mapping = input[["mappingId"]])  %>%
                       fillIsoData(mapping = getFields(mapping = input[["mappingId"]],
                                                       colnamesAPI = TRUE)) %>%
                       handleDescription(maxChar = 20) %>%
                       suppressWarnings()
                     isoDataRaw(d)
                   },
                   value = 0.75,
                   message = 'Get remote data from database ...'
                   )
                 })

                 # show preview text when maxCharLength is selected
                observe({
                  if (!is.na(input[["maxCharLength"]])) {
                    shinyjs::show(id = "previewText")
                  } else {
                    shinyjs::hide(id = "previewText")
                  }
                }) %>%
                  bindEvent(
                    input[["maxCharLength"]]
                  )

                 ## Load Data from file (pandora skin) ----
                 importedData <- importDataServer("localData",
                                                  defaultSource = config()[["defaultSourceData"]],
                                                  ckanFileTypes = config()[["ckanFileTypes"]],
                                                  rPackageName = config()[["rPackageName"]])

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

                   if (!is.null(attr(isoDataRaw(), "error"))) {
                     calibrationChoices <- c("")
                     names(calibrationChoices) <- attr(isoDataRaw(), "error")
                   } else {
                     calibrationChoices <- partialNumericColumns(isoDataRaw())
                   }

                   updateSelectInput(session, "calibrationDateMean",
                                     choices = calibrationChoices)
                   updateSelectInput(session,
                                     "calibrationDateUncertainty",
                                     choices = calibrationChoices)
                   updateSelectInput(session,
                                     "calibrationDateIntLower",
                                     choices = calibrationChoices)
                   updateSelectInput(session,
                                     "calibrationDateIntUpper",
                                     choices = calibrationChoices)

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
                   req(length(isoDataRaw()) > 0)

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

                     ### Convert Lat/Long (pandora skin) ----
                     d <- d %>%
                       convertLatLongWrapper(Longitude = locationFields$longitude(),
                                             Latitude = locationFields$latitude(),
                                             CoordType = locationFields$coordType())
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

                 ## Column selection (isomemo skin: "Select categories") ----
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
                   ### filter isoData ----
                   if (is.null(isoDataFull()) || is.null(input$dataTable_rows_all)) {
                     isoData(NULL)
                   } else {
                     isoData(isoDataFull()[input$dataTable_rows_all,
                                           names(isoDataFull()) %in% dataColumns(),
                                           drop = FALSE])
                   }
                 }) %>%
                   bindEvent(list(isoDataFull(), input$dataTable_rows_all, dataColumns()),
                             ignoreNULL = FALSE, ignoreInit = TRUE)

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
                       network = input$mappingId,
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
                 }) %>%
                   bindEvent(input$optionsFile)

                 ## Output TABLE ----
                 output$dataTable <- renderDataTable({
                   validate(need(
                     !is.null(isoDataFull()),
                     "Please import data (Skin: Pandora) or select a database (Skin: Data networks) in the sidebar panel."
                   ))

                   if(!is.na(input[["maxCharLength"]]) && length(isoDataFull()) > 0){
                     # cut long strings
                     tabData <- cutAllLongStrings(isoDataFull(), cutAt = input[["maxCharLength"]])
                     # use uncut column names
                     names(tabData) <- names(isoDataFull())
                   } else {
                     tabData <- isoDataFull()
                   }
                   req(dataColumns())
                   tblOut <- datTable(tabData, columns = dataColumns())

                   errorMsg <- "An error occurred. No data loaded!"
                   validate(need(tblOut, errorMsg))

                   if (!is.null(attr(tblOut, "error"))) {
                     errorMsg <- attr(tblOut, "error")
                   }
                   validate(need(length(tblOut) > 0, errorMsg))

                   tblOut
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

reloadMappingTable <- function(oldMapping, mappingId) {
  if (length(oldMapping) > 0) return(oldMapping)

  if (!has_internet()) {
    res <- list()
    attr(res, "errorApi") <- "Try 'Load data' ..."
    return(res)
  }

  getFields(mapping = mappingId, colnamesAPI = TRUE)
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
  if (
    # new format
    !all(names(opt) %in% c("network", "database", "columns", "calibrateMethod")) ||
    # old format
      !all(c("database", "columns", "calibrateMethod") %in% names(opt))) {
    showModal(
      modalDialog(
        "Could not read file with saved options",
        easyClose = TRUE,
        footer = modalButton("OK")
      )
    )
    return(NULL)
  }

  updateSelectInput(session,
                    "mappingId",
                    selected = opt$network)

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
