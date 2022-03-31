# TO DO: use structure of plot export to create a leaflet export pop up

#' ui function of leaflet map export module
#'
#' @param id namespace
leafletExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("exportLeafletPopUp"), "Export Map")
}


#' server funtion of leaflet map export module
#'
#' @param input input
#' @param output output
#' @param session session
leafletExport <- function(input, output, session,
                          #map,
                          isoData,
                          # zoom, center,
                          leafletValues,
                          #plotObj, type, predictions = function(){NULL}, plotFun = NULL, Model = NULL
                          ){
  observeEvent(input$exportLeafletMap, {
    showModal(modalDialog(
      title = "Export Map",
      footer = modalButton("OK"),
      #plotOutput(session$ns("mapToExport"), height = "300px"),
      # selectInput(
      #   session$ns("exportType"), "Filetype",
      #   choices = c(
      #     "png", "pdf", "svg", "tiff",
      #     if(!is.null(predictions())) "geo-tiff" else NULL,
      #     if(type == "spatio-temporal-average") "gif" else NULL
      #   )
      # ),
      selectInput(ns("exportType"), "Filetype", choices = c("png", "pdf", "jpeg")),
      # conditionalPanel(
      #   condition = "input.exportType != 'geo-tiff'",
      #   ns = session$ns,
      #   numericInput(session$ns("width"), "Width (px)", value = 1280),
      #   numericInput(session$ns("height"), "Height (px)", value = 800)
      # ),
      # conditionalPanel(
      #   condition = "input.exportType == 'gif'",
      #   ns = session$ns,
      #   numericInput(session$ns("minTime"), "Time begin of animation", value = 0),
      #   numericInput(session$ns("maxTime"), "Time end of animation", value = 5000),
      #   checkboxInput(session$ns("reverseGif"), "Reverse time order of animation"),
      #   numericInput(session$ns("intTime"), "Time interval length of animation", value = 1000)
      # ),
      downloadButton(session$ns("exportLeafletMap"), "Export"),
      easyClose = TRUE
    ))
  })


  output$exportLeaflet <- downloadHandler(
    filename = function() {
      paste0('plot-', Sys.Date(), '.', input$exportType)
    },
    content = function(filename) {

      m <- draw(
        isoData(),
        zoom = input$map_zoom,
        center = input$map_center,
        type = leafletValues()$leafletType,
        scale = !is.na(leafletValues()$scalePosition),
        scalePosition = leafletValues()$scalePosition,
        northArrow = !is.na(leafletValues()$northArrowPosition),
        northArrowPosition = leafletValues()$northArrowPosition,
        logoPosition = leafletValues()$logoPosition
      )
      mapview::mapshot(
        m, file = filename,
        remove_controls = "zoomControl",
        vwidth = input$map_width,
        vheight = input$map_height)
    }
  )

  # output$mapToExport <- renderLeaflet({
  #   draw(
  #     isoData(),
  #     zoom = input$map_zoom,
  #     center = input$map_center,
  #     type = leafletValues()$leafletType,
  #     scale = !is.na(leafletValues()$scalePosition),
  #     scalePosition = leafletValues()$scalePosition,
  #     northArrow = !is.na(leafletValues()$northArrowPosition),
  #     northArrowPosition = leafletValues()$northArrowPosition,
  #     logoPosition = leafletValues()$logoPosition
  #   )
  # })

  # output$exportExecute <- downloadHandler(
  #   filename = function(){
  #     if (input$exportType == 'geo-tiff')
  #       return(paste0(type, ".", "tif"))
  #
  #     paste0(type, ".", input$exportType)
  #   },
  #   content = function(file){
  #     if (input$exportType == "geo-tiff"){
  #       writeGeoTiff(predictions(), file)
  #       return()
  #     }
  #
  #     if (input$exportType == "gif"){
  #       minTime <- input$minTime
  #       maxTime <- input$maxTime
  #       intTime <- abs(input$intTime)
  #       if(input$reverseGif){
  #         minTime <- input$maxTime
  #         maxTime <- input$minTime
  #         intTime <- sign(-1) * abs(input$intTime)
  #       }
  #       withProgress(message = "Generating gif ...", value = 0, {
  #         saveGIF({
  #           times <- seq(minTime, maxTime, by = intTime)
  #           for (i in times) {
  #             incProgress(1 / length(times), detail = paste("time: ", i))
  #             plotFun()(model = Model(), time = i, plotRetNull = TRUE)
  #           }
  #         }, movie.name = file, ani.width = input$width, ani.height = input$height)
  #       })
  #       return()
  #     }
  #
  #     switch(
  #       input$exportType,
  #       png = png(file, width = input$width, height = input$height),
  #       pdf = pdf(file, width = input$width / 72, height = input$height / 72),
  #       tiff = tiff(file, width = input$width, height = input$height),
  #       svg = svg(file, width = input$width / 72, height = input$height / 72)
  #     )
  #     replayPlot(plotObj())
  #
  #     dev.off()
  #   }
  # )
}

# writeGeoTiff <- function(XPred, file){
#   if(is.null(XPred)) return()
#   longLength <- length(unique((XPred$Longitude)))
#   latLength <- length(unique((XPred$Latitude)))
#
#   vals <- matrix(XPred$Est, nrow = longLength, byrow = TRUE)
#   vals <- vals[nrow(vals) : 1, ]
#   r <- raster(nrows = longLength,
#               ncols = latLength,
#               xmn = min(XPred$Longitude),
#               ymn = min(XPred$Latitude),
#               xmx = max(XPred$Longitude),
#               ymx = max(XPred$Latitude),
#               vals  = vals)
#   writeRaster(r, filename = "out.tif", format="GTiff",
#               options = c('TFW=YES'), overwrite = TRUE)
#   file.rename("out.tif", file)
# }
