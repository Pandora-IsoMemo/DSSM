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
  observeEvent(input$export, {
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutput(session$ns("plot"), height = "300px"),
      selectInput(
        session$ns("exportType"), "Filetype",
        choices = c(
          "png", "pdf", "svg", "tiff",
          if(!is.null(predictions())) "geo-tiff" else NULL
        ),
        width = "50%"
      ),
      conditionalPanel(
        condition = "input.exportType != 'geo-tiff'",
        ns = session$ns,
        fluidRow(column(width = 6,
                        numericInput(session$ns("width"), "Width (px)", value = 1280)
        ),
        column(width = 6,
               numericInput(session$ns("height"), "Height (px)", value = 800)
        ))
      ),
      conditionalPanel(
        condition = paste0("'", modelType, "' == 'spatio-temporal-average' & ",
                           "'", mapType(), "' == 'Map'"),
        ns = session$ns,
        checkboxInput(session$ns("isTimeSeries"), "Export time series"),
        conditionalPanel(
          condition = "input.isTimeSeries",
          ns = session$ns,
          fluidRow(column(
            width = 4,
            numericInput(session$ns("minTime"), "Time begin of series", value = 0)),
          column(
            width = 4,
            numericInput(session$ns("maxTime"), "Time end of series", value = 5000)),
          column(
            width = 4,
            numericInput(session$ns("intTime"), "Time interval length", value = 1000))
          ),
          fluidRow(
            column(width = 6,
                   selectInput(session$ns("typeOfSeries"), "Type of time series",
                               choices = c(
                                 "Zip: Gif + graphic files" = "gifAndZip",
                                 "Zip: graphic files" = "onlyZip",
                                 "Gif file" = "onlyGif"))),
            column(width = 6,
                   style = "margin-top: 1.5em;",
                   conditionalPanel(
                     condition = "input.typeOfSeries != 'onlyZip'",
                     ns = session$ns,
                     checkboxInput(session$ns("reverseGif"), "Reverse time order of animation")
                   ))
            )
        )
      ),
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$plot <- renderPlot({
    replayPlot(plotObj())
  })

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
      nameFile(plotType = modelType, exportType = exportType(), isTimeSeries = isTimeSeriesInput())
    },
    content = function(file){
      if (input$isTimeSeries && input$typeOfSeries == "onlyGif") {
        ## option gif + graphic files is missing ----
        ## extract code of all cases into single functions... ----
        ## export gif ----
        minTime <- input$minTime
        maxTime <- input$maxTime
        intTime <- abs(input$intTime)
        if(input$reverseGif){
          minTime <- input$maxTime
          maxTime <- input$minTime
          intTime <- sign(-1) * abs(input$intTime)
        }
        ## APLY new fun here ----
        withProgress(message = "Generating gif ...", value = 0, {
          times <- seq(minTime, maxTime, by = intTime)
          saveGIF({
            for (i in times) {
              incProgress(1 / length(times), detail = paste("time: ", i))
              plotFun()(model = Model(), time = i, plotRetNull = TRUE)
            }
          }, movie.name = file, ani.width = input$width, ani.height = input$height)
        })
      } else if (!isTimeSeriesInput()) {
        ## export single graphic ----
        if (exportType() == "geo-tiff"){
          writeGeoTiff(predictions(), file)
          return()
        }

        switch(
          exportType(),
          png = png(file, width = input$width, height = input$height),
          pdf = pdf(file, width = input$width / 72, height = input$height / 72),
          tiff = tiff(file, width = input$width, height = input$height),
          svg = svg(file, width = input$width / 72, height = input$height / 72)
        )
        replayPlot(plotObj())

        dev.off()
      } else {
        ## export series of graphic files as zip ----
        withProgress(message = "Generating series ...", value = 0, {
          times <- seq(input$minTime, input$maxTime, by = abs(input$intTime))

          figFileNames <- sapply(times,
                                 function(i) {
                                   nameFile(plotType = modelType, exportType = exportType(),
                                            isTimeSeries = isTimeSeriesInput(), i = i)
                                 })

          for (i in times) {
            incProgress(1 / length(times), detail = paste("time: ", i))
            figFilename <- figFileNames[[which(times == i)]]

            if (exportType() == "geo-tiff"){
              writeGeoTiff(predictions(), figFilename)
            } else {
              switch(
                exportType(),
                png = png(figFilename, width = input$width, height = input$height),
                pdf = pdf(figFilename, width = input$width / 72, height = input$height / 72),
                tiff = tiff(figFilename, width = input$width, height = input$height),
                svg = svg(figFilename, width = input$width / 72, height = input$height / 72)
              )
              plotFun()(model = Model(), time = i)
              dev.off()
            }
          }

          zipr(file, figFileNames)
          unlink(figFileNames)
        })
      }
    }
  )
}


#' Name File
#'
#' @param plotType (character) plot specification
#' @param exportType (character) file type of exported plot
#' @param isTimeSeries (logical) if TRUE, set file names for a series of plots
#' @param i (numeric) number of i-th plot of a series of plots
nameFile <- function(plotType, exportType, isTimeSeries, i = NULL) {
  if (exportType == 'geo-tiff') {
    exportType <- "tif"
  }

  if (!isTimeSeries || exportType == "gif") return(paste0(plotType, ".", exportType))

  if (!is.null(i)) {
    paste0(plotType, "_", i, ".", exportType)
  } else {
    paste0(plotType, ".zip")
  }
}


writeGeoTiff <- function(XPred, file){
  if(is.null(XPred)) return()
  longLength <- length(unique((XPred$Longitude)))
  latLength <- length(unique((XPred$Latitude)))

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

generateGif <- function(files) {
  #files <- c("1.jpg", "2.jpg")
  image_list <- lapply(files, image_read)
  image_animate(image_list, loop = 0)
  image_write(image_list, path = "animated.gif")
}

addGif <- function(file, gif) {
  #file <- "3.jpg"
  #gif <- "animated.gif"
  #after the above is done a loop can sequentially add new images. Below I illustrate only how to add one image
  #Create image object for new slide
  new_slide <- image_read(file)

  #Read in existing gif
  existing_gif <- image_read(gif)

  #Append new slide to existing gif
  final_gif <- c(existing_gif, new_slide)

  #Write new gif
  image_write(final_gif, path = "animated.gif")
}
