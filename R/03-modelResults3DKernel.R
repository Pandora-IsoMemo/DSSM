#' ui function of modelResults3DKernel module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResults3DKernelUI <- function(id, title = ""){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      # left sidebar ----
      sidebarPanel(
        width = 2,
        style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
        importUI(ns("modelUpload"), label = "Import Model"),
        downloadDSSMModelUI(ns = ns),
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = c("Database" = "db",
                                "Upload file" = "file",
                                "Saved map" = "model"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'file'",
          importDataUI(ns("importData"), "Import Data"),
          tags$br(),
          tags$br(),
          radioButtons(
            inputId = ns("CoordType"),
            label = "Coordinate format",
            choiceNames = c("decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
                            "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
                            "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"),
            choiceValues = c("decimal degrees", "degrees decimal minutes", "degrees minutes seconds")),
          tags$hr(),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
          ns = ns,
          selectInput(
            ns("savedModel"),
            "Saved Map",
            choices = NULL,
            selected = NULL
          )
        ),
        conditionalPanel(
          condition = "input.dataSource != 'model'",
          ns = ns,
          selectInput(inputId = ns("DateType"),
                      label = "Date type:",
                      choices = c("Interval", "Mean + 1 SD uncertainty",
                                  "Single point"), selected = "Interval"),
          selectInput(inputId = ns("DateOne"),
                      label = "Mean or lower time point:",
                      choices = c("")),
          conditionalPanel(
            condition = "input.DateType == 'Interval' || input.DateType == 'Mean + 1 SD uncertainty'",
            selectInput(inputId = ns("DateTwo"),
                        label = "Standard deviation or upper time point:",
                        choices = c("")),
            ns = ns),
          selectInput(inputId = ns("Longitude"),
                      label = "Longitude variable:",
                      choices = c("Longitude")),
          selectInput(inputId = ns("Latitude"),
                      label = "Latitude variable:",
                      choices = c("Latitude")),
          selectInput(inputId = ns("IndependentX"),
                      label = "Presence/Absence variable (optional):",
                      choices = c("")),
          selectInput(inputId = ns("Weighting"),
                      label = "Weighting variable (optional):",
                      choices = c("")),
          tags$br(),
          clusterMethodUI(ns = ns, timeRangeInput = TRUE),
          tags$br(),
          checkboxInput(inputId = ns("modelUnc"),
                        label = "Include dating uncertainty", value = TRUE),
          conditionalPanel(
            condition = "input.modelUnc == true",
          conditionalPanel(
            condition = "input.DateType == 'Interval' || input.DateType == 'Mean + 1 SD uncertainty'",
            radioButtons(inputId = ns("dateUnc"),
                         label = "Distribution of date uncertainty",
                         choices = c("normal (sd = 1/4 width)" = "normal",
                                     "normal (sd = 1/2 width)" = "normal2",
                                     "uniform (full width)" = "uniform",
                                     "mid point" = "point")),
            ns = ns),
          ns = ns),
          sliderInput(inputId = ns("nSim"), label = "Number of simulations (for ci/se prediction)",
          min = 5, max = 100, step = 1, value = 10),
          dataCenterUI(ns, displayCondition = "true", hideCorrection = TRUE),
          checkboxInput(inputId = ns("modelArea"),
                        label = "Restrict model area",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.modelArea == true",
            tags$strong("Latitude restriction:"),
            numericInput(inputId = ns("mALat1"),
                         label = "Lower",
                         min = -90, max = 90, value = c(-90), width = "80%"),
            numericInput(inputId = ns("mALat2"),
                         label = "Upper",
                         min = -90, max = 90, value = c(90), width = "80%"),
            tags$strong("Longitude restriction:"),
            numericInput(inputId = ns("mALong1"),
                         label = "Lower",
                         min = -180, max = 180, value = c(-180), width = "80%"),
            numericInput(inputId = ns("mALong2"),
                         label = "Upper",
                         min = -180, max = 180, value = c(180), width = "80%"),
            ns = ns
          )
        ),
        sliderInput(ns("smoothParam"), value = 1.0, min = 0.1, max = 5.0, step = 0.1, label = "Adjust smoothness (optional)"),
        radioButtons(ns("kdeType"), "Bandwidth matrix type",
                     choices = c("Correlated" = "1", "Diagonal" = "2", "Diagonal + equal in longitudes and latitudes" = "3")),
        actionButton( ns("start"), "Start"),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          checkboxInput(inputId = ns("fixCol"),
                        label = "Fix colours and ranges ",
                        value = FALSE, width = "100%")
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.dataSource != 'model'",
          tags$hr(),
          tags$h4("Batch Modeling"),
          batchModelingUI(ns("batchModeling"))
        )
      ),
      # main panel ----
      mainPanel(
        width = 8,
        previewDataUI(id = ns("preview"), title = sprintf("%s Input Data", title)),
        tags$hr(),
        tags$h4("Map"),
        div(class = "aspect-16-9", div(
          plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
        )),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          htmlOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
          tags$br(),
          tags$br(),
          fluidRow(column(width = 3,
                          conditionalPanel(
                            condition = "input.mapType == 'Map'",
                            ns = ns,
                            div(
                              class = "move-map",
                              uiOutput(ns("move"))
                            ))
          ),
          column(width = 3,
                 offset = 6,
                 align = "right",
                 plotExportButton(ns("export"))
          )),
          conditionalPanel(
            condition = "input.mapType == 'Map'",
            ns = ns,
            timeAndMapSectionUI(ns("sectionOfMap"), label = "Time and Map Section"),
            div(div(
              style = 'display:inline-block',
              class = "save-plot-container",
              textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
              actionButton(ns("saveMap"), "Save map"),
              dataExportButton(ns("exportData"))
            )),
            actionButton(ns("add_btn2D"), "Add data point"),
            actionButton(ns("rm_btn2D"), "Remove data point"),
            actionButton(ns("ok"), "Ok"),
            uiOutput(ns("pointInput2D"))
          )
        ),
        conditionalPanel(
          condition = "input.mapType == 'Time course' || input.mapType == 'Time intervals by temporal group or cluster'",
          ns = ns,
          # possibly add input for timerange also later ----
          sliderInput(inputId = ns("trange"),
                      label = "Time range",
                      min = 0, max = 15000, value = c(0, 15000), width = "100%"),
          actionButton(ns("add_btn"), "Add data point"),
          actionButton(ns("rm_btn"), "Remove data point"),
          # colourInput(ns("col_btn"), "Select colour of data point"),
          # sliderInput(ns("size_btn"), "Size data point", min = 0.1, max = 10, value = 1),
          uiOutput(ns("pointInput")),
          conditionalPanel(
            condition = "input.pointsTime == true",
            dataExportButton(ns("exportDataTimeCourse"), title = "Export time course plot data"),
            dataExportButton(ns("exportDataTimeCoursePred"), title = "Export time course plot prediction data"),

            ns = ns)
        )
      ),
      # right sidebar ----
        sidebarPanel(
          width = 2,
          style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
          radioButtons(inputId = ns("Centering"),
                       label = "Map Centering",
                       choices = c("0th meridian" = "Europe", "180th meridian" = "Pacific")),
          helpTextCenteringUI(ns),
          zScaleUI(ns("zScale")),
          radioButtons(inputId = ns("mapType"), label = "Plot type", inline = TRUE,
                       choices = c("Map", "Time course", "Time intervals by temporal group or cluster"),
                       selected = "Map"),
          conditionalPanel(
            condition = "input.mapType == 'Time course'",
            ns = ns,
            tags$hr(),
            selectInput(inputId = ns("intervalType"), label = "Uncertainty Interval Type",
                        choices = list("none" = "1",
                                       "1 SE" = "2",
                                       "2 SE" = "6"), selected = "2"),
            checkboxInput(inputId = ns("pointsTime"),
                          label = "Show nearby points",
                          value = TRUE, width = "100%"),
            conditionalPanel(
              condition = "input.pointsTime == true",
              ns = ns,
              checkboxInput(inputId = ns("intTime"),
                            label = "Show nearby points unc. intervals",
                            value = FALSE, width = "100%"),
              sliderInput(inputId = ns("rangePointsTime"),
                          label = "Show nearby points / intervals range in km",
                          min = 10, max = 2500, value = 250, step = 10)
              ),
            formatTimeCourseUI(ns("timeCourseFormat")),
            tags$hr()
            ),
          conditionalPanel(
            condition = "input.mapType != 'Time course'",
            ns = ns,
            tags$hr(),
            mapLayerSettingsUI(ns("mapLayerSettings")),
            checkboxInput(inputId = ns("scale"),
                          label = "Show map scale",
                          value = TRUE, width = "100%"),
            checkboxInput(inputId = ns("arrow"),
                          label = "Show north arrow",
                          value = TRUE, width = "100%"),
            checkboxInput(inputId = ns("titleMain"),
                          label = "Show plot title",
                          value = TRUE),
            checkboxInput(inputId = ns("titleScale"),
                          label = "Show colour scale title",
                          value = TRUE),
            checkboxInput(inputId = ns("showScale"),
                          label = "Show colour scale",
                          value = TRUE),
            checkboxInput(inputId = ns("setAxisLabels"),
                          label = "Set axis labels",
                          value = FALSE),
            conditionalPanel(
              condition = "input.setAxisLabels == true",
              ns = ns,
              tags$hr(),
              textInput(ns("mainLabel"), NULL, placeholder = "main title"),
              textInput(ns("yLabel"), NULL, placeholder = "y-axis"),
              textInput(ns("xLabel"), NULL, placeholder = "x-axis"),
              textInput(ns("scLabel"), NULL, placeholder = "colour scale title"),
              tags$hr()
            ),
            checkboxInput(inputId = ns("setNorth"),
                          label = "Set north arrow and scale size and position",
                          value = FALSE),
            conditionalPanel(
              condition = "input.setNorth == true",
              ns = ns,
              tags$hr(),
              sliderInput(ns("northSize"), "Size north arrow", min = 0, max = 1, value = 0.2),
              sliderInput(ns("scalSize"), "Size scale", min = 0, max = 1, value = 0.1),
              sliderInput(ns("scaleX"), "Scale x orientation", min = 0, max = 1, value = 0),
              sliderInput(ns("scaleY"), "Scale y orientation", min = 0, max = 1, value = 0.1),
              sliderInput(ns("NorthX"), "North arrow x orientation", min = 0, max = 1, value = 0.025),
              sliderInput(ns("NorthY"), "North arrow y orientation", min = 0, max = 1, value = 0.925),
              tags$hr()
            ),
            checkboxInput(inputId = ns("points"),
                          label = "Show locations on map",
                          value = TRUE, width = "100%"),
            conditionalPanel(
              condition = "input.points == true",
              ns = ns,
              tags$hr(),
              sliderInput(inputId = ns("pointSize"),
                          label = "Location mark size",
                          min = 0.1, max = 2, value = 1, width = "100%",
                          step = 0.1),
              checkboxInput(inputId = ns("cluster"),
                            label = "Show Clustering",
                            value = FALSE, width = "100%"),
              conditionalPanel(
                condition = "input.cluster == true",
                # checkboxInput(inputId = ns("clusterAll"),
                #               label = "Show all cluster locations",
                #               value = FALSE, width = "100%"),
                radioButtons(inputId = ns("clusterResults"),
                             label = "Select grouping:",
                             choices = c("Temporal Grouping" = 0, "Spatial Clustering" = 1),
                             selected = 0),
                radioButtons(inputId = ns("clusterAll"),
                             label = "Cluster visibility",
                             choices = c("Show only centroids" = "-1", "Show points for all times" = "0", "Show only points for time slice" = "1"),
                             selected = "0", width = "100%"),
                selectInput(inputId = ns("clusterCol"), label = "Colour palette for points",
                            choices = list("Set1" = "Set1",
                                           "Set2" = "Set2",
                                           "Set3" = "Set3",
                                           "Pastel1" = "Pastel1",
                                           "Pastel2" = "Pastel2",
                                           "Paired" = "Paired",
                                           "Dark2" = "Dark2",
                                           "Accent" = "Accent"),
                            selected = "Set1"),
                ns = ns),
              conditionalPanel(
                condition = "input.cluster == false",
                ns = ns,
                checkboxInput(inputId = ns("colPSettings"),
                              label = "Set colour and shape of location marks",
                              value = FALSE, width = "100%")
              ),
              conditionalPanel(
                condition = "input.colPSettings == true & input.cluster == false",
                radioButtons(inputId = ns("colFix"),
                             label = "Fixed or variable location mark colour?", choices = c("fixed", "variable")),
                conditionalPanel(
                  condition = "input.colFix == 'fixed'",
                  colourInput(inputId = ns("pointCol"),
                              label = "Colour of location marks",
                              value = "#2C2161"), ns = ns),
                conditionalPanel(
                  condition = "input.colFix == 'variable'",
                  selectInput(inputId = ns("pointLabelsVarCol"),
                              label = "Select point color variable",
                              choices = c("")),
                  selectInput(inputId = ns("colorsP"), label = "Colour palette for points",
                              choices = list("Red-Yellow-Green" = "RdYlGn",
                                             "Yellow-Green-Blue" = "YlGnBu",
                                             "Purple-Orange" = "PuOr",
                                             "Pink-Yellow-Green" = "PiYG",
                                             "Red-Yellow-Blue" = "RdYlBu",
                                             "Yellow-Brown" = "YlOrBr",
                                             "Brown-Turquoise" = "BrBG"),
                              selected = "RdYlGn"),
                  ns = ns),
                selectInput(inputId = ns("pointShape"), label = "Shape of location marks",
                            choices = pchChoices(), selected = 4),
                ns = ns),
              tags$hr()
            ),
            sliderInput(inputId = ns("AddU"),
                        label = "Location marks and convex hull: Add time uncertainty in years",
                        min = 0, max = 1000, value = 0, step = 10),
            radioButtons(inputId = ns("interior"),
                         label = "Apply convex hull",
                         choices = c("none" = "0", "spatio-temporal" = "1", "time-sliced spatial" = "2"),
                         selected = "0", width = "100%"),

            checkboxInput(inputId = ns("mask"),
                          label = "Mask / Show output within range of points",
                          value = FALSE, width = "100%"),
            conditionalPanel(
              condition = "input.mask == true",
              sliderInput(inputId = ns("maskRadius"),
                          label = "Mask radius in km",
                          min = 10, max = 2500, value = 500, width = "100%",
                          step = 10), ns = ns),

            selectInput(inputId = ns("Colours"), label = "Colour palette",
                        choices = list("Yellow-Red" = "YlOrRd",
                                       "Purple-Red" = "PuRd",
                                       "Red" = "Reds",
                                       "Purple" = "Purples",
                                       "Orange" = "Oranges",
                                       "Grey" = "Greys",
                                       "Blue" = "Blues",
                                       "Green" = "Greens",
                                       "Yellow-Green" = "YlGn",
                                       "Red-Purple" = "RdPu",
                                       "Orange-Red" = "OrRd",
                                       "Green-Blue" = "GnBu",
                                       "Blue-Green" = "BuGn",
                                       "Purple-Blue" = "PuBu"),
                        selected = "RdYlGn"),
            checkboxInput(inputId = ns("reverseCols"),
                          label = "Reverse colors",
                          value = FALSE, width = "100%"),
            sliderInput(inputId = ns("ncol"),
                        label = "Approximate number of colour levels",
                        min = 4, max = 50, value = 50, step = 2, width = "100%"),
            centerEstimateUI(ns("centerEstimateParams")),
            tags$hr()
            ),
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("resolution"),
                      label = "Plot resolution (px)",
                      min = 20, max = 500, value = 100, width = "100%",
                      step = 20),
          checkboxInput(inputId = ns("pointLabels"),
                        label = "Scale point size by variable",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.pointLabels == true",
            selectInput(inputId = ns("pointLabelsVar"),
                        label = "Select point size variable",
                        choices = c("")), ns = ns
          ),
          checkboxInput(inputId = ns("textLabels"),
                        label = "Add location mark text labels",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.textLabels == true",
            selectInput(inputId = ns("textLabelsVar"),
                        label = "Select text label variable",
                        choices = c("")),
            sliderInput(inputId = ns("fontSize"),
                        label = "Font size)",
                        min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
            selectInput(inputId = ns("fontType"),
                        label = "Select font type",
                        choices = names(pdfFonts())),
            colourInput(inputId = ns("fontCol"),
                        label = "Colour of font",
                        value = "#2C2161"), ns = ns),
          sliderInput(inputId = ns("AxisSize"),
                      label = "Axis title font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          sliderInput(inputId = ns("AxisLSize"),
                      label = "Axis label font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),

          batchPointEstimatesUI(ns("batch"))
        )
    )
  )
}


#' server function of model Results module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData data
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#'
#' @export
modelResults3DKernel <- function(input, output, session, isoData, savedMaps, fruitsData){
  observeSavedMaps(input, output, session, savedMaps, type = c("kernel3d"))

  Model <- reactiveVal()

  observeEvent(input$saveMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == ""){
      alert("Please provide a map name")
      return()
    }

    map <- createSavedMap(
      model = Model(),
      predictions = values$predictions,
      plot = values$plot,
      plotFUN = plotFun(),
      type = "kernel3d",
      name = mapName
    )
    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })


  output$centerEstimate <- renderUI({
    centerEstimate$text()
  })

  data <- reactiveVal()
  observe({
    activeData <- switch(
      input$dataSource,
      db = isoData(),
      file = fileImport()
    )

    req(!is.null(activeData), !identical(data(), activeData))
    logDebug("modelResults3DKernel: Update data")

    # reset model
    Model(NULL)
    data(activeData)
    log_object_size(data())
  })

  coordType <- reactive({
    # reset model
    Model(NULL)

    switch(
      input$dataSource,
      db = "decimal degrees",
      file = input$CoordType
    )
  })

  outputHelpTextCentering(input, output, session)

  # MODEL DOWN- / UPLOAD ----
  uploadedNotes <- reactiveVal(NULL)
  subFolder <- "KernelTimeR"

  downloadDSSMModel(input, output, session,
                    dat = data,
                    model = Model(),
                    #savedMaps = savedMaps(),
                    subFolder = subFolder,
                    tabId = "model3DKernel",
                    uploadedNotes = uploadedNotes)

  uploadedValues <- importServer("modelUpload",
                                     title = "Import Model",
                                     importType = "model",
                                     ckanFileTypes = config()[["ckanModelTypes"]],
                                     subFolder = subFolder,
                                     ignoreWarnings = TRUE,
                                     defaultSource = config()[["defaultSourceModel"]],
                                     fileExtension = config()[["fileExtension"]],
                                     options = importOptions(rPackageName = config()[["rPackageName"]]))

  observe(priority = 100, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["data"]]))

    # reset model
    Model(NULL)
    fileImport(uploadedValues()[[1]][["data"]])
    data(uploadedValues()[[1]][["data"]])
    log_object_size(data())

    # update notes in tab "Estimates" model download ----
    uploadedNotes(uploadedValues()[[1]][["notes"]])
  }) %>%
    bindEvent(uploadedValues())

  previewDataServer(id = "preview", dat = data)

  observe(priority = 50, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
    uploadedInputs <- uploadedValues()[[1]][["inputs"]]

    ## update inputs ----
    inputIDs <- names(uploadedInputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedInputs[[inputIDs[i]]]) )
    }
  }) %>%
    bindEvent(uploadedValues())

  observe(priority = 10, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["model"]]))
    ## update model ----
    Model(unpackModel(uploadedValues()[[1]][["model"]]))
    log_object_size(Model())

    uploadedSavedMaps <- unpackSavedMaps(uploadedValues()[[1]][["model"]], currentSavedMaps = savedMaps())
    savedMaps(c(savedMaps(), uploadedSavedMaps))
  }) %>%
    bindEvent(uploadedValues())

  # RUN MODEL ----
  observeEvent(input$start, {
    if (input$dataSource == "model") {
      if (length(savedMaps()) == 0) return(NULL)

      Model(savedMaps()[[as.numeric(input$savedModel)]]$model)
      log_object_size(Model())
      return()
    }

    if (input$Latitude == "" |
        input$Longitude == "" | input$DateOne == "" |
        (input$DateTwo == "" & input$DateType != "Single point")) {
      Model(NULL)
      return()
    }
    values$set <- 0

    if(input$modelArea){
      restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
      restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
    } else {
      restriction <- c(-90, 90, -180, 180)
    }

    data <- data()

      model <- withProgress({
        estimateMap3DKernel(data = data, independent = input$IndependentX,
                      Longitude = input$Longitude, Latitude = input$Latitude,
                      CoordType = coordType(), DateOne = input$DateOne,
                      DateTwo = input$DateTwo, DateType = input$DateType,
                      Weighting = input$Weighting,
                      clusterMethod = input$clusterMethod,
                      dateUnc = input$dateUnc,
                      kMeansAlgo = input$kMeansAlgo,
                      nClust = input$nClust,
                      nClustRange = input$nClustRange,
                      trimRatio = input$trimRatio,
                      restr.fact = input$restr.fact,
                      clusterTimeRange = input$timeClust,
                      modelUnc = input$modelUnc,
                      restriction = restriction,
                      nSim = input$nSim,
                      smoothness = input$smoothParam,
                      kdeType = input$kdeType) %>%
          shinyTryCatch()
        },
        value = 0,
        message = "Generating spatio-temporal kernel density"
      )
      Model(model)
      log_object_size(Model())
      updateSelectInput(session, "Centering", selected = input$centerOfData)
  })

  Independent <- reactive({
    if (input$dataSource == "model") names(Model()$data)[1]
    else input$IndependentX
  })

  zoomFromModel <- reactiveVal(50)

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      newZoom <- extractZoomFromLongRange(
        rangeLongitude = range(Model()$data$Longitude, na.rm = TRUE),
        mapCentering = input$Centering
      )

      isolate({
        zoomFromModel(newZoom)
        values$zoom <- newZoom
        values$up <- 0
        values$right <- 0
      })
    }
  })

  output$move <- renderUI({
    moveButtons(ns = session$ns)
  })

  zSettings <- zScaleServer("zScale",
                            Model = Model,
                            fixCol = reactive(input$fixCol),
                            estimationTypeChoices =
                              reactive(c("Mean", "1 SE", "2 SE", "Quantile")),
                            restrictOption = reactive("hide"),
                            zValuesFun = getZValuesKernel,
                            zValuesFactor = 1.5)

  observeEvent(input$up, {
    values$up <- values$up + values$zoom / 40
  })

  observeEvent(input$down, {
    values$up <- values$up - values$zoom / 40
  })

  observeEvent(input$left, {
    values$right <- values$right - values$zoom / 40
  })

  observeEvent(input$right, {
    values$right <- values$right + values$zoom / 40
  })

  observeEvent(input$Centering, {
    values$upperLeftLatitude <- NA
    values$upperLeftLongitude <- NA
    values$up <- 0
    values$right <- 0
  })

  dateExtent <- reactiveValues(
    min = 0,
    max = 15000,
    mean = 5000,
    range = c(0, 15000),
    step = 100
  )

  mapSection <- timeAndMapSectionServer("sectionOfMap",
                                        dateMin = reactive(dateExtent$min),
                                        dateMax = reactive(dateExtent$max),
                                        dateValue = reactive(dateExtent$mean),
                                        dateStep = reactive(dateExtent$step),
                                        zoomValue = zoomFromModel,
                                        mapCenter = reactive(input$Centering))

  observeEvent(mapSection$set, {
    mapSectionVars <- names(mapSection)
    for (i in mapSectionVars[mapSectionVars != "set"]) {
      values[[i]] <- mapSection[[i]]
    }

    values$up <- 0
    values$right <- 0
  })

  observe({
    if(input$DateType == "Interval"){
      updateRadioButtons(session, "dateUnc", choices = c("normal (sd = 1/4 width)" = "normal",
                                                         "normal (sd = 1/2 width)" = "normal2",
                                                         "uniform (full width)" = "uniform",
                                                         "mid point" = "point"))
    }
    if(input$DateType == "Mean + 1 SD uncertainty"){
      updateRadioButtons(session, "dateUnc", choices = c("uniform (2xSD input)" = "uniform",
                                                         "uniform (1xSD input)" = "uniform2",
                                                         "mean point" = "point",
                                                         "normal (sd = 1/4 width)" = "normal"))
    }
  })

  observe({
    validate(validInput(data()))
    try({
      # check if dateOne is numeric
      dateOne <- data()[, (input$DateOne)]
      if (!is.numeric(dateOne)) {
        dateOne <- dateOne %>%
          as.numeric() %>%
          na.omit()
      }
      req(length(dateOne))
      if(input$DateType == "Single point"){
        d <- dateOne
      }

      # check if dateTwo is numeric
      dateTwo <- data()[, (input$DateTwo)]
      if (!is.numeric(dateTwo)) {
        dateTwo <- dateTwo %>%
          as.numeric() %>%
          na.omit()
      }
      req(length(dateTwo))
      if(input$DateType == "Interval"){
        d <- c(dateOne,
               dateTwo)
      }
      if(input$DateType == "Mean + 1 SD uncertainty"){
        d <- c(dateOne + 2 *
                 dateTwo,
               dateOne - 2 *
                 dateTwo)
      }
    }, silent = TRUE)

    if(exists("d")){
      d <- na.omit(d)
      step <- signif(roundUpNice(diff(range(d)),
                                 nice = c(1,10)) / 10000, digits = 2)
      minD <- min(d) - diff(range(d)) * 0.1
      maxD <- max(d) + diff(range(d)) * 0.1

  updateSliderInput(
    session,
    "timeClust",
    value = signif(c(minD, maxD), digits = 2),
    min = signif(minD, digits = 2),
    max = signif(maxD, digits = 2),
    step = step
  )
    }
  })

  observe({
  if(input[["clusterMethod"]] %in% c("kmeans","mclust","tclust")){
  value <- TRUE
  } else {
  value <- FALSE
  }
  updateCheckboxInput(
    session,
    "cluster",
    value = value
  )
  }) %>%
    bindEvent(input[["clusterMethod"]])

  observe({
    validate(validInput(Model()))

    dateExtentValues <- update_date_extent(
      input_data = data(),
      model_data = Model()$data,
      input = input
    )

    if (length(dateExtentValues) == 0) return()

    # update date extent reactive values
    dateExtent$mean <- dateExtentValues$mean
    dateExtent$range <- dateExtentValues$range
    dateExtent$step <- dateExtentValues$step
    dateExtent$min <- dateExtentValues$min
    dateExtent$max <- dateExtentValues$max

    # update plot time
    values$time <- dateExtent$mean

    # time range update ----
    updateSliderInput(
      session,
      "trange",
      value = dateExtent$range,
      min = dateExtent$min,
      max = dateExtent$max,
      step = dateExtent$step
    )
  })

  ### Add Points
  pointDat2D <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(Model(), ignoreNULL = FALSE, {
    pointDat2D(data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })

  addRow2D <- function(df) {
    rbind(df, data.frame(index = nrow(df) + 1, y = NA,
                         x = NA, label = "",
                         pointColor = "black", pointSize = 1,
                         pointAlpha = 0.5, stringsAsFactors = FALSE))
  }

  rmRow2D <- function(df) {
    if (nrow(df) > 0) df[- nrow(df), , drop = FALSE]
    else df
  }

  observeEvent(input$add_btn2D, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    pointDat2D(addRow2D(pointDat2D()))
  })

  observeEvent(input$rm_btn2D, {
    pointDat2D(rmRow2D(pointDat2D()))
  })

  inputGroup2D <- reactive({
    createPointInputGroup2D(pointDat2D(), ns = session$ns)
  })

  pointDatOK <- eventReactive(input$ok, ignoreNULL = FALSE, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    return(pointDat2D())
  })

  centerEstimate <- centerEstimateServer("centerEstimateParams",
                                         predictions = reactive(values$predictions),
                                         mapType = reactive(input$mapType))

  formatTimeCourse <- formatTimeCourseServer("timeCourseFormat")

  plotFun <- reactive({
    function(model, time = values$time, returnPred = FALSE, ...){
      pointDat = pointDat()
      pointDatOK = pointDatOK()

      if(input$fixCol == FALSE){
        zoom <- values$zoom

        rangey <- model$data %>%
          extractRangeFromData(column = "Latitude", move = values$up) %>%
          zoomLatitudeRange(zoom = zoom,
                            upperLeftLatitude = values$upperLeftLatitude,
                            move = values$up) %>%
          constrainLatitudeRange()

        rangex <- model$data %>%
          shiftDataToCenter(centerMap = input$Centering) %>%
          extractRangeFromData(column = "Longitude", move = values$right) %>%
          zoomLongitudeRange(zoom = zoom,
                             upperLeftLongitude = values$upperLeftLongitude,
                             center = input$Centering,
                             move = values$right) %>%
          constrainLongitudeRange(zoom = zoom, center = input$Centering)

        values$rangex <- rangex
        values$rangey <- rangey

      }
      if(input$smoothCols){
        values$ncol <- 200
      } else {
        if(input$fixCol == FALSE){
          values$ncol <- input$ncol
        }
      }
      textLabels <- NULL
      if(input$textLabels & !is.null(input$textLabelsVar) & input$textLabelsVar != ""){
        textLabels <- (data())[, input$textLabelsVar, drop = FALSE]
        if(nrow(textLabels) == 0){
          textLabels = NULL
        }
      }

      pointLabels <- NULL
      if(input$pointLabels & !is.null(input$pointLabelsVar) & input$pointLabelsVar != ""){
        pointLabels <- (data())[, input$pointLabelsVar, drop = FALSE]
        if(nrow(pointLabels) == 0){
          pointLabels = NULL
        }
      }
      pointColLabels <- NULL
      if(input$colFix == "variable" & !is.null(input$pointLabelsVarCol) & input$pointLabelsVarCol != ""){
        pointColLabels <- (data())[, input$pointLabelsVarCol, drop = FALSE]
        if(nrow(pointColLabels) == 0){
          pointColLabels = NULL
        }
      }

      if(input$mapType == "Time course"){
        plotTimeCourse(model,
                       trange = input$trange,
                       independent = isolate(Independent()),
                       resolution = input$resolution,
                       centerX = centerEstimate$centerX(),
                       centerY = centerEstimate$centerY(),
                       rangey = c(input$rangezMin, input$rangezMax),
                       pointDat = pointDat,
                       seType = input$intervalType,
                       returnPred = returnPred,
                       pointsTime = input$pointsTime,
                       rangePointsTime = input$rangePointsTime,
                       intTime = input$intTime,
                       limitz = NULL,
                       formatTimeCourse = formatTimeCourse(),
                       ...)
      } else {
      if(input$mapType == "Time intervals by temporal group or cluster"){
        withProgress({
          plotTimeIntervals(model,
                            trange = input$trange,
                            AxisSize = input$AxisSize,
                            AxisLSize = input$AxisLSize,
                            cluster = input$cluster,
                            clusterCol = input$clusterCol,
                            clusterResults = input$clusterResults,
                            ...)
        },
          value = 0,
          message = "Creating plot (takes some seconds)"
        )
      }

        req(zSettings$estType)

        # PLOT MAP ----
      if(input$mapType == "Map"){
        plotMap3D(
          model,
          time = time,
          points = input$points,
          pointSize = input$pointSize,
          rangex = values$rangex,
          rangey = values$rangey,
          estType = zSettings$estType,
          showModel = zSettings$showModel,
          rangez = zSettings$range,
          limitz = zSettings$limit,
          addU = input$AddU,
          centerMap = input$Centering,
          resolution = input$resolution,
          interior = as.numeric(input$interior),
          mask = input$mask,
          maskRadius = input$maskRadius,
          ncol = values$ncol,
          pColor = input$pointCol,
          pointShape = as.numeric(input$pointShape),
          textLabels = textLabels,
          pointLabels = pointLabels,
          pointColLabels = pointColLabels,
          colorsP = input$colorsP,
          fontSize = input$fontSize,
          fontType = input$fontType,
          fontCol = input$fontCol,
          terrestrial = input[["mapLayerSettings-terrestrial"]],
          grid = input[["mapLayerSettings-grid"]],
          showBorders = input[["mapLayerSettings-showBorders"]],
          colors = input$Colours,
          reverseColors = input$reverseCols,
          arrow = input$arrow,
          scale = input$scale,
          titleMain = !input$titleMain,
          titleScale = !input$titleScale,
          showScale = input$showScale,
          setAxisLabels = input$setAxisLabels,
          mainLabel = input$mainLabel,
          yLabel =  input$yLabel,
          xLabel =  input$xLabel,
          scLabel =  input$scLabel,
          northSize = input$northSize,
          scalSize = input$scalSize,
          scaleX = input$scaleX,
          scaleY = input$scaleY,
          NorthX = input$NorthX,
          NorthY = input$NorthY,
          AxisSize = input$AxisSize,
          AxisLSize = input$AxisLSize,
          cluster = input$cluster,
          clusterAll = input$clusterAll,
          clusterResults = input$clusterResults,
          clusterCol = input$clusterCol,
          pointDat = pointDatOK,
          ...
        ) %>%
          shinyTryCatch(errorTitle = "Plotting failed")
      }
      }
    }
  })

  output$DistMap <- renderPlot({
    validate(validInput(Model()))
    withProgress({
      res <- plotFun()(Model())
    }, min = 0, max = 1, value = 0.8, message = "Plotting map ...")
    values$predictions <- res$XPred
    log_object_size(values$predictions)
    values$plot <- recordPlot()
  })

  values <- reactiveValues(plot = NULL, predictions = NULL,
                           set = 0,
                           upperLeftLongitude = NA,
                           upperLeftLatitude = NA,
                           zoom = 50)

  observe(priority = 75, {
    logDebug("Update input choices")
    numVars <- get_num_vars(data())
    timeVars <- get_time_vars(data())

    selectedLongitude <- select_if_db_and_exists(input, data(), "longitude")
    selectedLatitude  <- select_if_db_and_exists(input, data(), "latitude")

    updateSelectInput(session, "IndependentX",  choices = c("", setdiff(numVars, timeVars)))

    updateSelectInput(session, "Longitude", choices = c("", names(data())),
                      selected = selectedLongitude)
    updateSelectInput(session, "Latitude", choices = c("", names(data())),
                      selected = selectedLatitude)
    updateSelectInput(session, "Weighting", choices = c("", numVars))
    updateSelectInput(session, "textLabelsVar", choices = c("", names(data())),
                      selected = character(0))
    updateSelectInput(session, "pointLabelsVar", choices = c("", names(data())),
                      selected = character(0))
    updateSelectInput(session, "pointLabelsVarCol", choices = c("", names(data())),
                      selected = character(0))

    if (input$dataSource == "db"){
      updateSelectInput(session, "DateOne", choices = c("", numVars))
      updateSelectInput(session, "DateTwo", choices = c("", numVars))
    } else {
      updateSelectInput(session, "DateOne", choices = c("", numVars))
      updateSelectInput(session, "DateTwo", choices = c("", numVars))
    }
  }) %>%
    bindEvent(data())

  ## Import Data ----
  importedDat <- importDataServer("importData")

  fileImport <- reactiveVal(NULL)
  observe({
    # reset model
    Model(NULL)
    if (length(importedDat()) == 0 ||  is.null(importedDat()[[1]])) fileImport(NULL)

    req(length(importedDat()) > 0, !is.null(importedDat()[[1]]))
    data <- importedDat()[[1]]
    valid <- validateImport(data, showModal = TRUE)

    if (!valid){
      showNotification("Import is not valid.")
      fileImport(NULL)
    } else {
      fileImport(data)
    }
  }) %>% bindEvent(importedDat())

  dataFun <- reactive({
    req(Model())
    function() {
      if(!is.null(Model()$data$spatial_cluster)){
        allData <- data()
        allData$rNames <- rownames(allData)
        modelData <- Model()$data
        modelData$rNames <- rownames(modelData)
        modelData <- merge(modelData[, c("spatial_cluster",
                                         "temporal_group",
                                         "long_centroid_spatial_cluster",
                                         "lat_centroid_spatial_cluster",
                                         "long_temporal_group_reference_point",
                                         "lat_temporal_group_reference_point",
                                         "rNames")], allData, all.y = FALSE, sort = FALSE)
        modelData$rNames <- NULL
        # filter data that was filtered out for clustering
        modelData <- modelData[!is.na(modelData$long_centroid_spatial_cluster),]
        return(modelData)
      } else {
        allData <- data()
        allData$rNames <- rownames(allData)
        modelData <- Model()$data
        modelData$rNames <- rownames(modelData)
        modelData <- merge(modelData[, c("rNames"), drop = FALSE], allData, all.y = FALSE, sort = FALSE)
        modelData$rNames <- NULL
        return(modelData)
      }
    }
  })

  dataTimeCourse <- reactive({
    req(Model())
    function(){
      tData <- plotFun()(Model())
      if(!is.null(tData)){
        allData <- data()
        tData <- allData[which(rownames(allData) %in% rownames(tData)), ]
        return(tData)
      } else {
        return(data.frame())
      }
    }
  })

  dataTimeCoursePred <- reactive({
    req(Model())
    function(){
      tData <- plotFun()(Model(), returnPred = TRUE)
      if(!is.null(tData)){
        return(tData)
      } else {
        return(data.frame())
      }
    }
  })

  ### Add Points

  pointDat <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0),
      x = numeric(0),
      xmin = numeric(0),
      xmax = numeric(0),
      group = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(Model(), ignoreNULL = FALSE, {
    pointDat(data.frame(
      index = numeric(0),
      y = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0),
      x = numeric(0),
      xmin = numeric(0),
      xmax = numeric(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })
  addRow <- function(df) {
    rbind(df, data.frame(index = nrow(df) + 1, y = NA,
                         ymin = NA, ymax = NA, x = NA,
                         xmin = NA, xmax = NA,
                         pointColor = "black", pointSize = 1,
                         pointAlpha = 0.5, stringsAsFactors = FALSE))
  }

  rmRow <- function(df) {
    if (nrow(df) > 0) df[- nrow(df), , drop = FALSE]
    else df
  }

  observeEvent(input$add_btn, {
    df <- pointDat()
    indices <- df$index

    lapply(indices, function(index) {
      yval <- input[[paste("yT", index, sep = "_")]]
      yminval <- input[[paste("yminT", index, sep = "_")]]
      ymaxval <- input[[paste("ymaxT", index, sep = "_")]]
      xminval <- input[[paste("xminT", index, sep = "_")]]
      xmaxval <- input[[paste("xmaxT", index, sep = "_")]]
      xval <- input[[paste("xT", index, sep = "_")]]
      pointColor <- input[[paste("pointColorT", index, sep = "_")]]
      pointSize <- input[[paste("pointSizeT", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlphaT", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "ymin"] <<- if (is.null(yminval)) NA else yminval
      df[index, "ymax"] <<- if (is.null(ymaxval)) NA else ymaxval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "xmin"] <<- if (is.null(xminval)) NA else xminval
      df[index, "xmax"] <<- if (is.null(xmaxval)) NA else xmaxval
    })

    pointDat(df)
    pointDat(addRow(pointDat()))
  })

  observeEvent(input$rm_btn, {
    pointDat(rmRow(pointDat()))
  })

  inputGroup <- reactive({
    createPointInputGroup(pointDat(), ns = session$ns)
  })

  output$pointInput <- renderUI(inputGroup())
  output$n <- reactive(nrow(pointDat()))
  outputOptions(output, "n", suspendWhenHidden = FALSE)
  ###
  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)

  callModule(dataExport, "exportData", dataFun = dataFun, filename = "modelData")
  callModule(dataExport, "exportDataTimeCourse", dataFun = dataTimeCourse, filename = "timeCourseData")
  callModule(dataExport, "exportDataTimeCoursePred", dataFun = dataTimeCoursePred, filename = "timeCourseData")

  callModule(plotExport, "export", reactive(values$plot), "spatio-temporal-average",
             predictions = reactive(values$predictions),
             plotFun = plotFun,
             Model = Model,
             mapType = reactive(input$mapType)
  )
  callModule(batchPointEstimates, "batch", plotFun, time = TRUE, fruitsData = fruitsData, Model = Model)
  modelParams <- reactive({
    params <- reactiveValuesToList(input)
    params$coordType <- coordType()
    params
  })
  batchModel <- callModule(batchModeling, "batchModeling", data = data, plotFun = plotFun,
             modelParams = modelParams, type = "kernel3d",
             savedMaps = savedMaps, estimateWrapper = estimateMap3DKernelWrapper,
             variableNames = "Presence/Absence Variables")

  observeEvent(batchModel(), {
    Model(batchModel())
  })

}
