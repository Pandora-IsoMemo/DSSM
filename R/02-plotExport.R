plotExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plot")
}

plotExport <- function(input,
                       output,
                       session,
                       plotObj,
                       type,
                       predictions = function(){NULL},
                       plotFun = NULL,
                       Model = NULL){
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
        )
      ),
      conditionalPanel(
        condition = "input.exportType != 'geo-tiff'",
        ns = session$ns,
        numericInput(session$ns("width"), "Width (px)", value = 1280),
        numericInput(session$ns("height"), "Height (px)", value = 800)
      ),
      conditionalPanel(
        condition = paste0("'", type, "' == 'spatio-temporal-average'"),
        ns = session$ns,
        checkboxInput(session$ns("isTimeSeries"), "Export time series"),
        conditionalPanel(
          condition = "input.isTimeSeries",
          ns = session$ns,
          numericInput(session$ns("minTime"), "Time begin of series", value = 0),
          numericInput(session$ns("maxTime"), "Time end of series", value = 5000),
          numericInput(session$ns("intTime"), "Time interval length of series", value = 1000)
        )
      ),
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$plot <- renderPlot({
    replayPlot(plotObj())
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      nameFile(plotType = type, exportType = input$exportType, isTimeSeries = input$isTimeSeries)
    },
    content = function(file){
      if (!input$isTimeSeries) {
        if (input$exportType == "geo-tiff"){
          writeGeoTiff(predictions(), file)
          return()
        }

        switch(
          input$exportType,
          png = png(file, width = input$width, height = input$height),
          pdf = pdf(file, width = input$width / 72, height = input$height / 72),
          tiff = tiff(file, width = input$width, height = input$height),
          svg = svg(file, width = input$width / 72, height = input$height / 72)
        )
        replayPlot(plotObj())

        dev.off()
        return()
      }

      minTime <- input$minTime
      maxTime <- input$maxTime
      intTime <- abs(input$intTime)

      withProgress(message = "Generating series ...", value = 0, {
        times <- seq(minTime, maxTime, by = intTime)

        figFileNames <- sapply(times,
                               function(i) {
                                 nameFile(plotType = type, exportType = input$exportType,
                                          isTimeSeries = input$isTimeSeries, i = i)
                               })

        for (i in times) {
          incProgress(1 / length(times), detail = paste("time: ", i))
          figFilename <- figFileNames[[which(times == i)]]

          if (input$exportType == "geo-tiff"){
            writeGeoTiff(predictions(), figFilename)
          } else {
            switch(
              input$exportType,
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

  if (!isTimeSeries) return(paste0(plotType, ".", exportType))

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
