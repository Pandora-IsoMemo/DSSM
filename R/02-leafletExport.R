# TO DO: use structure of plot export to create a leaflet export pop up

#' ui function of leaflet map export module
#'
#' @param id namespace
leafletExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("exportLeaflet"), "Export Map")
}


#' server funtion of leaflet map export module
#'
#' @param input input
#' @param output output
#' @param session session
leafletExport <- function(input, output, session,
                          isoData,
                          zoom, center,
                          width, height,
                          leafletValues
                          ){
  ns <- session$ns

  observeEvent(input$exportLeaflet, {
    showModal(modalDialog(
      title = "Export Map",
      footer = modalButton("OK"),
      selectInput(ns("exportType"), "Filetype", choices = c("png", "pdf", "jpeg")),
      numericInput(ns("width"), "Width (px)", value = width()),
      numericInput(ns("height"), "Height (px)", value = height()),
      downloadButton(session$ns("exportLeafletMap"), "Export"),
      easyClose = TRUE
    ))
  })

  output$exportLeafletMap <- downloadHandler(
    filename = function() {
      paste0('plot-', Sys.Date(), '.', input$exportType)
    },
    content = function(filename) {
      m <- draw(
        isoData(),
        zoom = zoom(),
        center = center(),
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
        vwidth = input$width,
        vheight = input$height)
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
}
