
#' ui function of modelResultsDiffSim module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResultsSimUI <- function(id, title = ""){
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
        # !!! Uploading of inputs is not working: inputs are cleaned when updating "values" ----
        # importDataUI(ns("modelUpload"), label = "Import Model"),
        # checkboxInput(ns("useDownload"), label = "Download model"),
        # conditionalPanel(
        #   ns = ns,
        #   condition = "input.useDownload == true",
        #   downloadModelUI(ns("modelDownload"), label = "Download")
        # ),
        # tags$hr(),
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = c("Create map" = "create",
                                "Saved map" = "model"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
        selectInput(ns("savedModel"),
                    "Select similarity Map",
                    choices = c(""),
                    selected = ""),
        actionButton( ns("load"), "Load"),
        ns = ns),
        conditionalPanel(
          condition = "input.dataSource == 'create'",
          pickerInput(
          inputId = ns("SimMapSelect"),
          label = "Select baseline maps:",
          choices = NULL,
          options = list(
            `actions-box` = FALSE,
            size = 10,
            `none-selected-text` = "No maps selected",
            `selected-text-format` = "count > 8"
          ),
          multiple = TRUE
        ),
        actionButton(ns("simDataImport"), "Provide data values"),
        tags$hr(),
        checkboxInput(ns("normalize"), "Normalize values"),
        conditionalPanel(
          condition = "input.normalize == true",
          radioButtons(ns("normalType"), "Type of normalisation",
                       choices = c("Max value equal to 1" = "1", "Volume equal to 1" = "2")),
          ns = ns),
        checkboxInput(ns("weightProb"), "Weight values", value = FALSE),
        conditionalPanel(
          condition = "input.weightProb == true",
          selectInput(ns("savedMap"),
                      "Select Map for Weighting",
                      choices = c(""),
                      selected = ""),
          checkboxInput(ns("negZero"), "Set negative values to zero weight", value = TRUE),
          checkboxInput(ns("invWeight"), "Inverse exponential weighting", value = FALSE),
          conditionalPanel(
            condition = "input.invWeight == true",
          numericInput(ns("weightDecay"), "Exponential weighting decay half-life", value = 1000, min = 0, max = Inf),
          ns = ns),
          ns = ns),
        actionButton(ns("start"), "Create probability map"),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          checkboxInput(inputId = ns("fixCol"),
                        label = "Fix colours and ranges ",
                        value = FALSE, width = "100%")
        ),
        ns = ns
        )
      ),
      # main panel ----
      mainPanel(
        width = 8,
        div(class = "aspect-16-9", div(
          plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
        )),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          htmlOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
          tags$br(),
          tags$br(),
          fluidRow(column(width = 3,
                          div(
                            class = "move-map",
                            uiOutput(ns("move"))
                          )
          ),
          column(width = 3,
                 offset = 6,
                 align = "right",
                 plotExportButton(ns("export"))
          )),
          tags$hr(),
          tags$h4("Map Section"),
          mapSectionUI(ns("mapSection")),
          fluidRow(
            column(
              width = 3,
              offset = 9,
              style = "margin-top: -60px;",
              align = "right",
              actionButton(ns("set"), "Set Map Section")
            )
          ),
          tags$hr(),
          div(
            div(
              style = 'display:inline-block',
              class = "save-plot-container",
              textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
              actionButton(ns("saveMap"), "Save map")
            ),
          ),
          actionButton(ns("add_btn2D"), "Add data point"),
          actionButton(ns("rm_btn2D"), "Remove data point"),
          actionButton(ns("ok"), "Ok"),
          uiOutput(ns("pointInput2D"))
        )
      ),
      # right sidebar ----
        sidebarPanel(
          width = 2,
          style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
          radioButtons(inputId = ns("Centering"),
                       label = "Map Centering",
                       choices = c("0th meridian" = "Europe", "160th meridian" = "Pacific")),
          zScaleUI(ns("zScale")),
          radioButtons(inputId = ns("terrestrial"), label = "", inline = TRUE,
                       choices = list("Terrestrial " = 1, "All" = 3, "Aquatic" = -1),
                       selected = 1),
          checkboxInput(inputId = ns("grid"),
                        label = "Show map grid",
                        value = TRUE, width = "100%"),
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
            textInput(ns("mainLabel"), NULL, placeholder = "main title"),
            textInput(ns("yLabel"), NULL, placeholder = "y-axis"),
            textInput(ns("xLabel"), NULL, placeholder = "x-axis"),
            textInput(ns("scLabel"), NULL, placeholder = "colour scale title"), ns = ns
          ),
          checkboxInput(inputId = ns("setNorth"),
                        label = "Set north arrow and scale size and position",
                        value = FALSE),
          conditionalPanel(
            condition = "input.setNorth == true",
            sliderInput(ns("northSize"), "Size north arrow", min = 0, max = 1, value = 0.2),
            sliderInput(ns("scalSize"), "Size scale", min = 0, max = 1, value = 0.1),
            sliderInput(ns("scaleX"), "Scale x orientation", min = 0, max = 1, value = 0),
            sliderInput(ns("scaleY"), "Scale y orientation", min = 0, max = 1, value = 0.1),
            sliderInput(ns("NorthX"), "North arrow x orientation", min = 0, max = 1, value = 0.025),
            sliderInput(ns("NorthY"), "North arrow y orientation", min = 0, max = 1, value = 0.925),
            ns = ns
          ),
          selectInput(inputId = ns("Colours"), label = "Colour palette",
                      choices = list("Red-Yellow-Green" = "RdYlGn",
                                     "Yellow-Green-Blue" = "YlGnBu",
                                     "Purple-Orange" = "PuOr",
                                     "Pink-Yellow-Green" = "PiYG",
                                     "Red-Yellow-Blue" = "RdYlBu",
                                     "Yellow-Brown" = "YlOrBr",
                                     "Brown-Turquoise" = "BrBG"),
                      selected = "RdYlGn"),
          checkboxInput(inputId = ns("showValues"),
                        label = "Show data values in plot",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("reverseCols"),
                        label = "Reverse colors",
                        value = FALSE, width = "100%"),
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("ncol"),
                      label = "Approximate number of colour levels",
                      min = 4, max = 50, value = 20, step = 2, width = "100%"),
          centerEstimateUI(ns("centerEstimateParams")),
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
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#' @param config (list) list of configuration parameters
#'
#' @export
mapSim <- function(input, output, session, savedMaps, fruitsData, config){
  values <- reactiveValues(
    simDataList = list(),
    simDataTemp = list(),
    predictionList = list(),
    set = 0,
    upperLeftLongitude = NA,
    upperLeftLatitude = NA,
    zoom = 50,
    weightMap = NULL
  )

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), "similarity")

    updateSelectInput(session, "savedModel", choices = choices)
  })

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), c("localAvg", "temporalAvg", "spread", "difference",
                                            "similarity", "kernel2d", "kernel3d", "user"))

    updateSelectInput(session, "savedMap", choices = choices)
  })


  mapChoices <- reactive(
    getMapChoices(savedMaps(), c("localAvg", "temporalAvg"))
  )

  observe({
    updatePickerInput(session, "SimMapSelect", choices = mapChoices())
  })

  observeEvent(input$SimMapSelect, ignoreNULL = FALSE, {
    if (is.null(input$SimMapSelect) || length(input$SimMapSelect) == 0)
      shinyjs::disable("simDataImport")
    else
      shinyjs::enable("simDataImport")
  })

  observeEvent(input$simDataImport, {
    mapNames <- names(mapChoices())[match(input$SimMapSelect, mapChoices())]
    m <- listToDoubleMatrix(values$simDataList, mapNames)

    if (nrow(m) == 0) m <- rbind(m, NA)
    mode(m) <- "character"

    m[is.na(m)] <- ""

    showModal(modalDialog(
      title = "Data values",
      matrixInput(
        session$ns("simDataValues"),
        value = m,
        class = "numeric",
        cols = list(
          names = TRUE,
          createHeader = "MpiIsoApp.doubleHeader.create",
          updateHeader = "MpiIsoApp.doubleHeader.update",
          getHeader = "MpiIsoApp.doubleHeader.get"
        ),
        rows = list(extend = TRUE),
      ),
      footer = tagList(
        actionButton(session$ns("simDataImportCancel"), "Cancel"),
        tags$button("Submit", class = "btn btn-default", type = "button",
                    onClick = paste0("setTimeout(function(){Shiny.setInputValue('",
                                     session$ns("simDataImportSubmit"), "', Math.random())}, 300)"))
      )
    ))
  })

  observe({
    req(input$simDataValues)
    values$simDataTemp <- doubleMatrixToList(input$simDataValues)
  })

  observeEvent(input$SimMapSelect, {
    res <- lapply(input$SimMapSelect, function(m) {
      savedMaps()[[as.numeric(m)]]$predictions
    })
    values$predictionList <- res
    # values are cleaned here -> uploading of inputs is not working
    values$simDataList <- list()
    values$simDataListM <- list()
  })

  observeEvent(input$savedMap, {
    values$weightMap <- savedMaps()[[as.numeric(input$savedMap)]]
  })


  observeEvent(input$simDataImportCancel, removeModal())
  observeEvent(input$simDataImportSubmit, {
    values$simDataList <- values$simDataTemp
    removeModal()
  })

  Model <- reactiveVal(NULL)

  # !!! Uploading of inputs is not working: inputs are cleaned when updating "values" ----
  # # MODEL DOWN- / UPLOAD ----

  # uploadedNotes <- reactiveVal(NULL)
  # subFolder <- "LocateR"
  # downloadModelServer("modelDownload",
  #                     dat = savedMaps,
  #                     inputs = input,
  #                     model = Model,
  #                     rPackageName = config$rPackageName,
  #                     subFolder = subFolder,
  #                     fileExtension = config$fileExtension,
  #                     helpHTML = getHelp(id = "similarity"),
  #                     modelNotes = uploadedNotes,
  #                     triggerUpdate = reactive(TRUE),
  #                     compressionLevel = 1)
  #
  # uploadedValues <- importDataServer("modelUpload",
  #                                    title = "Import Model",
  #                                    defaultSource = config$defaultSourceModel,
  #                                    importType = "model",
  #                                    rPackageName = config$rPackageName,
  #                                    subFolder = subFolder,
  #                                    ignoreWarnings = TRUE,
  #                                    fileExtension = config$fileExtension)
  #
  # observe(priority = 100, {
  #   req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["data"]]))
  #
  #   # reset model
  #   Model(NULL)
  #   savedMaps(uploadedValues()[[1]][["data"]])
  #
  #   # update notes in tab "Estimates" model download ----
  #   uploadedNotes(uploadedValues()[[1]][["notes"]])
  # }) %>%
  #   bindEvent(uploadedValues())
  #
  # observe(priority = 50, {
  #   req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
  #   uploadedInputs <- uploadedValues()[[1]][["inputs"]]
  #
  #   ## update inputs ----
  #   inputIDs <- names(uploadedInputs)
  #   inputIDs <- inputIDs[inputIDs %in% names(input)]
  #
  #   for (i in 1:length(inputIDs)) {
  #     session$sendInputMessage(inputIDs[i],  list(value = uploadedInputs[[inputIDs[i]]]) )
  #   }
  # }) %>%
  #   bindEvent(uploadedValues())
  #
  # observe(priority = 10, {
  #   req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["model"]]))
  #   ## update model ----
  #   Model(uploadedValues()[[1]][["model"]])
  # }) %>%
  #   bindEvent(uploadedValues())

  # RUN MODEL ----
  observeEvent(input$start, {
    values$simDataListM <- values$simDataList

    if (length(values$simDataListM) == 0) return(NULL)
    if (any(is.na(do.call("rbind", values$simDataListM)[, 1]))) return(NULL)
    if (any(is.na(do.call("rbind", values$simDataListM)[, 2]))){
      withProgress({
        model <- createSimilarityMap(values$predictionList,
                                        values$simDataListM, includeUncertainty = FALSE,
                                        normalize = input$normalize,
                                        normalType = input$normalType,
                                        weightProb = input$weightProb,
                                        weightMap = values$weightMap,
                                        negZero = input$negZero,
                                        invWeight = input$invWeight,
                                        weightDecay = input$weightDecay) %>%
          tryCatchWithWarningsAndErrors()
        Model(model)
        },
        value = 0,
        message = 'Creating similarity map ...'
      )
    } else {
      withProgress({
        model <- createSimilarityMap(values$predictionList,
                                        values$simDataListM,
                                        normalize = input$normalize,
                                        normalType = input$normalType,
                                        weightProb = input$weightProb,
                                        weightMap = values$weightMap,
                                        negZero = input$negZero,
                                        invWeight = input$invWeight,
                                        weightDecay = input$weightDecay) %>%
          tryCatchWithWarningsAndErrors()
        Model(model)
        },
        value = 0,
        message = 'Creating similarity map ...'
      )
    }
  })

  observeEvent(input$load, {
    Model(savedMaps()[[as.numeric(input$savedModel)]]$model)
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
                                         predictions = reactive(values$predictions))

  plotFun <-  reactive({
    validate(validInput(Model()))
    pointDatOK = pointDatOK()

    if(input$fixCol == FALSE){
      zoom <- values$zoom

      rangey <- - diff(range(Model()$Latitude, na.rm = TRUE)) / 2 +
        max(Model()$Latitude, na.rm = TRUE) + values$up
      if(!is.na(values$upperLeftLatitude)){
        rangey <- values$upperLeftLatitude + c(- zoom / 2 , 0) + values$up
      } else {
        rangey <- rangey + c( - zoom / 4, zoom / 4)
      }
      if(input$Centering == "Europe"){
        rangex <- - diff(range(Model()$Longitude, na.rm = TRUE)) / 2 +
          max(Model()$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude)){
          rangex <- values$upperLeftLongitude + values$right
          rangex <- rangex + c(0, zoom)
        } else {
          rangex <- rangex + c( - zoom / 2, zoom / 2)
        }
      } else{
        dataPac <- Model()[, c("Longitude", "Latitude")]
        dataPac$Longitude[Model()$Longitude < -20] <- dataPac$Longitude[Model()$Longitude < -20] + 200
        dataPac$Longitude[Model()$Longitude >= -20] <- (- 160 + dataPac$Longitude[Model()$Longitude >= -20])
        rangex <- - diff(range(dataPac$Longitude, na.rm = TRUE)) / 2 +
          max(dataPac$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude)){
          rangex <- values$upperLeftLongitude + values$right
          if(rangex < -20) rangex <- rangex + 200
          if(rangex >= -20) rangex <- rangex - 160
          rangex <- rangex + c(0, zoom)
        } else {
          rangex <- rangex + c( - zoom / 2, zoom / 2)
        }
        }
      if(rangex[2] > 180){
        rangex <- c(180 - zoom, 180)
      }
      if(rangex[1] < -180){
        rangex <- c(-180, -180 + zoom)
      }
      if(rangey[2] > 90){
        coordDiff <- rangey[2] - 90
        rangey <- pmin(90, pmax(-90, rangey - coordDiff))
      }
      if(rangey[1] < -90){
        coordDiff <- rangey[1] + 90
        rangey <- pmin(90, pmax(-90, rangey - coordDiff))
      }
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

    req(zSettings$estType)

    # PLOT MAP ----
    function(...){
      plotDS(Model(),
             type = "similarity",
             independent = "",
             rangex = values$rangex,
             rangey = values$rangey,
             estType = zSettings$estType,
             estQuantile = zSettings$Quantile,
             rangez = zSettings$range,
             showModel = zSettings$showModel,
             colors = input$Colours,
             ncol = values$ncol,
             reverseColors = input$reverseCols,
             terrestrial = input$terrestrial,
             grid = input$grid,
             centerMap = input$Centering,
             arrow = input$arrow,
             scale = input$scale,
             simValues = values$simDataListM,
             showValues = input$showValues,
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
             pointDat = pointDatOK,
             ...
             )
    }
  })

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
      type = "similarity",
      name = mapName
    )

    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })

  output$DistMap <- renderPlot({
    validate(validInput(Model()))
    res <- plotFun()()
    if(inherits(res, "character")){
      alert(res)
    } else {
    values$predictions <- res$XPred
    values$plot <- recordPlot()
    }
  })

  output$centerEstimate <- renderUI({
    centerEstimate$text()
  })

  zoomFromModel <- reactiveVal(50)

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      val <- signif(max(Model()$Sd, na.rm = TRUE), 2)
      updateSliderInput(session, "StdErr", value = signif(val * 8, 2),
                        min = 0, max = signif(val * 8, 2),
                        step = signif(roundUpNice(val, nice = c(1,10)) / 1000, 1))

      newZoom <- extractZoomFromLongRange(
        rangeLongitude = range(Model()$Longitude, na.rm = TRUE),
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
                            estimationTypeChoices = reactive(c(
                              "Mean", "1 SE", "2 SE", "Quantile"
                            )),
                            restrictOption = reactive("hide"),
                            zValuesFun = getZValuesMapSim,
                            zValuesFactor = 1)

  mapSettings <- mapSectionServer("mapSection", zoomValue = zoomFromModel)

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

  observeEvent(input$center, {
    values$upperLeftLatitude <- NA
    values$upperLeftLongitude <- NA
    values$up <- 0
    values$right <- 0
  })
  observeEvent(input$set, {
    values$zoom <- mapSettings$zoom
    values$upperLeftLatitude <- mapSettings$upperLeftLatitude
    values$upperLeftLongitude <- mapSettings$upperLeftLongitude

    values$up <- 0
    values$right <- 0
  })

  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)
  callModule(plotExport, "export", reactive(values$plot), "similarity",
             reactive(values$predictions))
  callModule(batchPointEstimates, "batch", plotFun, fruitsData = fruitsData)
}
