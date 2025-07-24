plotExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plot")
}

plotExport <- function(input,
                       output,
                       session,
                       plotObj,
                       modelType,
                       predictions = function(){NULL},
                       plotFun = NULL,
                       Model = NULL,
                       mapType = reactive("Map")){
  ns <- session$ns
  previewImagePath <- reactiveVal(NULL)
  width <- reactiveVal(1280)
  height <- reactiveVal(800)

  exportObj <- reactiveVal(NULL)

  observeEvent(input$export, {
    withProgress(message = "Generating preview...", value = 0.7, {
      previewImagePath(createPlotPreview(replayPlot(plotObj()), width(), height()))
    })

    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      conditionalPanel(
        condition = "input.exportType != 'geo-tiff'",
        ns = ns,
        fluidRow(
          column(4, numericInput(ns("preview-width"), "Width (px)", value = width())),
          column(4, numericInput(ns("preview-height"), "Height (px)", value = height())),
          column(4, align = "right", style = "margin-top: 1.75em", actionButton(ns("generatePreview"), "Preview"))
        ),
        tags$br(),
        div(
          style = "height: 310px; max-width: 600px; overflow-x: auto; overflow-y: hidden; margin: auto; white-space: nowrap;",
          imageOutput(ns("previewImage"), height = "300px") %>% withSpinner(color = "#20c997")
        )
      ),
      fluidRow(column(
        8,
        textInput(
          ns("exportFilename"),
          "Filename (without extension)",
          value = getFileName(plotType = modelType, isTimeSeries = FALSE),
          width = "100%"
        )),
        column(
          4,
          selectInput(
            ns("exportType"), "Filetype",
            choices = c(
              "jpeg", "png", "pdf", "svg", "tiff",
              if(!is.null(predictions())) "geo-tiff" else NULL
            )
          ))
      ),
      conditionalPanel(
        condition = paste0("'", modelType, "' == 'spatio-temporal-average' & ",
                           "'", mapType(), "' == 'Map'"), #& input.exportType != 'geo-tiff'
        ns = ns,
        checkboxInput(ns("isTimeSeries"), "Export time series"),
        conditionalPanel(
          condition = "input.isTimeSeries",
          ns = ns,
          fluidRow(column(
            width = 4, style = "margin-bottom: 1em;",
            numericInput(ns("minTime"), "Time begin of series", value = 0)),
          column(
            width = 4,
            numericInput(ns("maxTime"), "Time end of series", value = 5000)),
          column(
            width = 4,
            numericInput(ns("intTime"), "Time interval length", value = 1000))
          ),
          actionButton(ns("generateFiles"), "Generate Plot Files"),
          fluidRow(
            column(width = 4, style = "margin-top: 1em; margin-bottom: 1em;",
                   selectInput(ns("typeOfSeries"), "Type of time series",
                               choices = c(
                                 "Gif + graphic files" = "gifAndZip",
                                 "Graphic files" = "onlyZip",
                                 "Gif file" = "onlyGif",
                                 "MapR files" = "mapr")),
                   conditionalPanel(
                     condition = "input.typeOfSeries == 'mapr'",
                     ns = ns,
                     helpText("MapR only supports .png export. Ignoring 'Filetype' input.")
                   )
                   ),
            column(width = 4, style = "margin-top: 1em;",
                   conditionalPanel(
                     condition = "input.typeOfSeries == 'gifAndZip' || input.typeOfSeries == 'onlyGif'",
                     ns = ns,
                     numericInput(ns("fpsGif"), "Frames per second", value = 2, min = 1, max = 10)
                   ),
                   conditionalPanel(
                     condition = "input.typeOfSeries == 'mapr'",
                     ns = ns,
                     textInput(ns("mapr-group"), "Group", value = "Groupname"),
                     textInput(ns("mapr-measure"), "Measure", value = "Mean")
                   )
                   ),
                   conditionalPanel(
                     condition = "input.typeOfSeries == 'gifAndZip' || input.typeOfSeries == 'onlyGif'",
                     ns = ns,
                     column(width = 4, style = "margin-top: 2em;",
                     checkboxInput(ns("reverseGif"), "Reverse time order")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.typeOfSeries == 'mapr'",
                     ns = ns,
                     column(width = 4,
                     textInput(ns("mapr-variable"), "Variable", value = "Variable"),
                     textInput(ns("mapr-measureunit"), "Measure unit", value = "Measure Unit")
                   )
                   )
            )
        )
      ),
      downloadButton(ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  observeEvent(input$generatePreview, {
    req(input$`preview-width`, input$`preview-height`)
    withProgress(message = "Generating preview...", value = 0.7, {
      previewImagePath(createPlotPreview(replayPlot(plotObj()), input$`preview-width`, input$`preview-height`))
    })
  })

  output$previewImage <- renderImage({
    validate(
      need(previewImagePath(), "Click 'Preview' to generate map preview.")
    )

    list(
      src = previewImagePath(),
      contentType = "image/png",
      height = 300,
      alt = "Map preview"
    )
  }, deleteFile = TRUE)

  # enable/disable generateFiles button
  observe({
    shinyjs::disable("generateFiles")

    req(input$`preview-width`, input$`preview-height`,
        input$exportType,
        input$minTime, input$maxTime, input$intTime)
    shinyjs::enable("generateFiles")
  })

  observe({
    times <- seq(input$minTime, input$maxTime, by = abs(input$intTime))

    obj <- new_PlotSeriesExport(plotFun = plotFun(),
                                Model = Model(),
                                times = times,
                                exportType = input$exportType,
                                modelType = modelType,
                                width = input$`preview-width`,
                                height = input$`preview-height`)

    # Generate plots
    withProgress(message = "Generating time series plots ...", value = 0, {
      obj <- generate(obj) %>%
        shinyTryCatch(errorTitle = "Plot generation failed")
    })

    exportObj(obj)  # Save it for later use
  }) %>%
    bindEvent(input$generateFiles)

  observe({
    shinyjs::disable("exportExecute")

    isTimeSeries <- isTruthy(input$isTimeSeries)  # assumes you use checkboxInput
    type <- input$typeOfSeries
    obj <- exportObj()

    # Only enforce for time series
    if (isTimeSeries && type %in% c("onlyZip", "onlyGif", "gifAndZip", "mapr")) {
      # Check that object exists, status is completed, and files exist
      if (!is.null(obj) && obj$status == "completed") {
        validFiles <- switch(type,
                             mapr = all(file.exists(obj$pngFileNames)),
                             onlyZip = all(file.exists(obj$mainFileNames)),
                             onlyGif = all(file.exists(obj$mainFileNames)),  # still uses main for gif source now
                             gifAndZip = all(file.exists(obj$mainFileNames)), # same here
                             FALSE
        )

        if (validFiles) {
          shinyjs::enable("exportExecute")
        }
      }
    } else {
      # For non-time-series, allow export always (assuming the plot is ready)
      shinyjs::enable("exportExecute")
    }
  }) %>%
    bindEvent(input$isTimeSeries, input$typeOfSeries, exportObj())

  observe({
  if(any(c(input$`mapr-group`,input$`mapr-variable`,input$`mapr-measure`,input$`mapr-measureunit`) == "") && input$typeOfSeries == "mapr"){
    shinyjs::disable("exportExecute")
  } else {
  shinyjs::enable("exportExecute")
  }
  }) %>% bindEvent(c(input$typeOfSeries,input$`mapr-group`,input$`mapr-variable`,input$`mapr-measure`,input$`mapr-measureunit`),
                  ignoreInit = TRUE)

  isTimeSeriesInput <- reactiveVal(FALSE)
  exportType <- reactiveVal("png")

  observe({
    if (input$isTimeSeries && input$typeOfSeries == "onlyGif") exportType("gif") else exportType(input$exportType)
  }) %>%
    bindEvent(input$exportType)

  observe({
    req(!is.null(input$isTimeSeries))
    if (mapType() == "Map") isTimeSeriesInput(input$isTimeSeries) else isTimeSeriesInput(FALSE)
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      paste0(input$exportFilename, '.', getExtension(exportType(), isTimeSeriesInput(), input$typeOfSeries))
    },
    content = function(file){
      if (!isTimeSeriesInput()) {
        exportGraphicSingle(exportType = exportType(),
                            file = file,
                            width = input$`preview-width`,
                            height = input$`preview-height`,
                            plotObj = plotObj(),
                            predictions = predictions()) %>%
          suppressWarnings() %>%
          shinyTryCatch(errorTitle = "Export of graphic failed")
      } else {
        obj <- exportObj()
        req(!is.null(obj), obj$status == "completed")

        if (input$typeOfSeries == "mapr") {
          exportMapR(obj, file = file, input = input) %>%
            suppressWarnings() %>%
            shinyTryCatch(errorTitle = "Export of series of graphics failed")
        } else {
          exportSeries(obj,
                       file = file,
                       modelType = modelType,
                       typeOfSeries = input$typeOfSeries,
                       fpsGif = input$fpsGif,
                       reverseGif = input$reverseGif) %>%
          suppressWarnings() %>%
          shinyTryCatch(errorTitle = "Export of series of graphics failed")
        }
      }
    }
  )
}

createPlotPreview <- function(plot, width, height) {
  tmp_png <- tempfile(fileext = ".png")
  png(tmp_png, width = width, height = height)
  plot
  dev.off()

  return(tmp_png)
}

# Get File Name
#
# @param plotType (character) plot specification
# @param isTimeSeries (logical) if TRUE, set file names for a series of plots
# @param i (numeric) number of i-th plot of a series of plots
getFileName <- function(plotType, isTimeSeries, i = NULL) {
  if (isTimeSeries && !is.null(i)) return(paste0(plotType, "_", i))

  plotType
}

getExtension <- function(exportType, isTimeSeries, typeOfSeries = "gifAndZip", i = NULL) {
  if (!isTimeSeries || !is.null(i)) {
    ## file extension for single plots: from user input 'exportType'

    # use 'tif' instead of 'geo-tiff'
    if (exportType == 'geo-tiff') {
      fileExt <- "tif"
    } else {
      fileExt <- exportType
    }
  } else {
    ## file extension for series of plots: from user input 'typeOfSeries'
    fileExt <- switch(typeOfSeries,
                      gifAndZip = "zip",
                      onlyZip = "zip",
                      onlyGif = "gif",
                      mapr = "zipm")
  }
}

exportGraphicSingle <- function(exportType, file, width, height, plotObj, predictions) {
  if (exportType == "geo-tiff"){
    writeGeoTiff(predictions, file)
    return()
  }

  writeGraphics(exportType = exportType,
                plot = replayPlot(plotObj),
                filename = file,
                width = width,
                height = height)
}

writeGeoTiff <- function(XPred, file){
  if(is.null(XPred)) return()
  longLength <- length(unique((XPred$Longitude)))
  latLength <- length(unique((XPred$Latitude)))

  # is filter for time i possible?
  vals <- matrix(XPred$Est, nrow = longLength, byrow = TRUE)
  vals <- vals[nrow(vals) : 1, ]
  r <- raster(nrows = longLength,
              ncols = latLength,
              xmn = min(XPred$Longitude),
              ymn = min(XPred$Latitude),
              xmx = max(XPred$Longitude),
              ymx = max(XPred$Latitude),
              vals  = vals)
  writeRaster(r, filename = "out.tif", format="GTiff",
              options = c('TFW=YES'), overwrite = TRUE)
  file.rename("out.tif", file)
}

# Write Graphics
#
# @param exportType (character) file type of exported plot
# @param plot (object) plot object
# @inheritParams grDevices::png
writeGraphics <- function(exportType, plot, filename, width, height) {
  switch(
    exportType,
    png = png(filename, width = width, height = height),
    jpeg = jpeg(filename, width = width, height = height),
    pdf = pdf(filename, width = width / 72, height = height / 72),
    tiff = tiff(filename, width = width, height = height),
    svg = svg(filename, width = width / 72, height = height / 72)
  )

  plot

  dev.off()
}
