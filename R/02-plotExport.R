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
            width = 4,
            numericInput(ns("minTime"), "Time begin of series", value = 0)),
          column(
            width = 4,
            numericInput(ns("maxTime"), "Time end of series", value = 5000)),
          column(
            width = 4,
            numericInput(ns("intTime"), "Time interval length", value = 1000))
          ),
          actionButton(ns("generateFiles"), "Generate Files"),
          fluidRow(
            column(width = 4,
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
            column(width = 4,
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
      obj <- generate.PlotSeriesExport(obj)
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
          # exportMapRFiles(file = file,
          #                 plotFun = plotFun(),
          #                 Model = Model(),
          #                 input = input) %>%
          exportMapR(obj, file = file, input = input) %>%
            suppressWarnings() %>%
            shinyTryCatch(errorTitle = "Export of series of graphics failed")
        } else {
        # exportGraphicSeries(exportType = exportType(),
        #                     file = file,
        #                     width = input$`preview-width`,
        #                     height = input$`preview-height`,
        #                     plotFun = plotFun(),
        #                     Model = Model(),
        #                     predictions = predictions(),
        #                     modelType = modelType,
        #                     minTime = input$minTime,
        #                     maxTime = input$maxTime,
        #                     intTime = input$intTime,
        #                     typeOfSeries = input$typeOfSeries,
        #                     reverseGif = input$reverseGif,
        #                     fpsGif = input$fpsGif) %>%
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

# Name File
#
# @param plotType (character) plot specification
# @param exportType (character) file type of exported plot
# @param isTimeSeries (logical) if TRUE, set file names for a series of plots
# @param typeOfSeries one of "gifAndZip", "onlyZip", "onlyGif"
# @param i (numeric) number of i-th plot of a series of plots
nameFile <- function(plotType, exportType, isTimeSeries, typeOfSeries, i = NULL) {
  # set file name
  fileName <- getFileName(plotType = plotType, isTimeSeries = isTimeSeries, i = i)

  # set file extension
  fileExt <- getExtension(exportType, isTimeSeries, typeOfSeries, i)

  # return file name with extension
  paste0(fileName, ".", fileExt)
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

exportMapRFiles <- function(file, plotFun, Model, input) {
  withProgress(message = "Generating series ...", value = 0, {
    times <- seq(input$minTime, input$maxTime, by = abs(input$intTime))

    # create all file names to be put into a zip
    figFileNames <- sapply(times,
                           function(i) {
                             paste0("data","/",
                                    gsub(" ", "", input$`mapr-group`),"/",
                                    gsub(" ", "", input$`mapr-variable`),"/",
                                    gsub(" ", "", input$`mapr-measure`),"/",
                                    i,".png")
                           })

    lapply(unique(dirname(figFileNames)), dir.create, recursive = TRUE, showWarnings = FALSE)

    for (i in times) {
      incProgress(1 / length(times), detail = paste("time: ", i))
      figFilename <- figFileNames[[which(times == i)]]

      # save desired file type
      writeGraphics(exportType = "png",
                    plot = plotFun(model = Model, time = i),
                    filename = figFilename,
                    width = input$`preview-width`,
                    height = input$`preview-height`)
    }

    json_list <- create_image_list_json(input, figFileNames, times)

    # Save JSON content to a temporary file
    json_file <- file.path("image_list.json")
    jsonlite::write_json(json_list, json_file, pretty = TRUE)

    zip(file, files = c(json_file, figFileNames))

    # Clean up the temporary files
    file.remove(figFileNames)
    file.remove(json_file)
    unlink(file.path("data"), recursive = TRUE)
  })
}

create_image_list_json <- function(input, figFileNames, times){
  image_list <- list(
    Selections = list(
      list(
        Group = input$`mapr-group`,
        Group_DOI = 1,
        Variable = list(
          list(
            Variable_name = input$`mapr-variable`,
            Variable_DOI = 1,
            Measure = list(
              list(
                Measure_name = input$`mapr-measure`,
                Measure_unit = input$`mapr-measureunit`,
                images = list(
                )
              )
            )
          )
        )
      )
    )
  )

  for (image in figFileNames){

    time <- as.character(times[[which(figFileNames == image)]])

    single_image <- list(
      x_display_value = time,
      file_type = "png",
      location_type = "local",
      address = gsub("data/","",image)
    )

    # Add the images to the list
    image_list$Selections[[1]]$Variable[[1]]$Measure[[1]]$images <- append(image_list$Selections[[1]]$Variable[[1]]$Measure[[1]]$images, list(single_image))
  }

  image_list
}


exportGraphicSeries <- function(exportType, file,
                                width, height, plotFun, Model, predictions,
                                modelType, minTime, maxTime, intTime,
                                typeOfSeries, reverseGif, fpsGif) {
  withProgress(message = "Generating series ...", value = 0, {
    times <- seq(minTime, maxTime, by = abs(intTime))
    if (reverseGif && typeOfSeries != "onlyZip") times <- rev(times)

    # create all file names to be put into a zip
    figFileNames <- sapply(times,
                           function(i) {
                             nameFile(plotType = modelType, exportType = exportType,
                                      isTimeSeries = TRUE, typeOfSeries = typeOfSeries, i = i)
                           })

    # create all file names to be put into a gif, they have always .jpeg format
    gifFileNames <- sapply(times,
                           function(i) {
                             paste0(getFileName(plotType = modelType, isTimeSeries = TRUE, i = i),
                                    ".jpeg")
                           })

    for (i in times) {
      incProgress(1 / length(times), detail = paste("time: ", i))
      figFilename <- figFileNames[[which(times == i)]]

      if (exportType == "geo-tiff") {
        # filter for i ??? <- probably one needs to re-create the plot using: res <- plotFun(model = Model, time = i) and get the predictions from res$XPred
        # LOGIC FOR TIME SERIES WILL BE ADDED SOON
        NULL
      } else {
        # save desired file type
        writeGraphics(exportType = exportType,
                      plot = plotFun(model = Model, time = i),
                      filename = figFilename,
                      width = width,
                      height = height)

        # save jpeg for .gif if desired file type is not .jpeg (else we already stored that file)
        if (typeOfSeries != "onlyZip" && exportType != "jpeg") {
          jpeg(gifFileNames[[which(times == i)]], width = width, height = height)
          plotFun(model = Model, time = i)
          dev.off()
        }
      }
    }

    if (typeOfSeries == "onlyZip") {
      # zip file to be downloaded:
      zipr(zipfile = file, files = figFileNames)
    }
    if (typeOfSeries == "onlyGif") {
      # gif file to be downloaded:
      generateGif(gifFile = file, files = gifFileNames, fps = fpsGif)
    }
    if (typeOfSeries == "gifAndZip") {
      generateGif(gifFile = paste0(modelType, ".gif"), files = gifFileNames, fps = fpsGif)
      # zip file to be downloaded containing the gif file:
      zipr(zipfile = file, files = c(paste0(modelType, ".gif"), figFileNames))
      unlink(paste0(modelType, ".gif"))
    }
    # clean up all single files
    unlink(figFileNames)
    unlink(gifFileNames)
  })
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

# Generate GIF
#
# @param gifFile The gif file to create
# @param files a list of files, url's, or raster objects or bitmap arrays
# @param exportType (character) file type of exported plot
# @param fps frames per second
generateGif <- function(gifFile = "animated.gif", files, fps = 1) {
  image_list <- lapply(files, image_read)

  image_list %>%
    image_join() %>%
    image_animate(fps = fps, loop = 0) %>%
    image_write(path = gifFile)
}

# Add GIF
#
# @param gifFile The gif file to add a slide to
# @param file the file, url, or raster object or bitmap array to be added to gifFileSource
# addGif <- function(gifFile, file) {
#   #Create image object for new slide
#   new_slide <- image_read(file)
#
#   #Read an existing gif
#   existing_gif <- image_read(gifFile)
#
#   #Append new slide to existing gif
#   final_gif <- c(existing_gif, new_slide)
#
#   #Write new gif
#   image_write(final_gif, path = gifFile)
# }
