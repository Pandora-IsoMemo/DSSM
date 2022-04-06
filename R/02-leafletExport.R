#' ui function of leaflet map export module
#'
#' @param id namespace
leafletExportButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("exportLeaflet"), "Export Map")
}


#' server funtion of leaflet map export module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param leafletMap reactive leaflet map object
#' @param width reactive width of map in px
#' @param height reactive height of map in px
leafletExport <- function(input,
                          output,
                          session,
                          leafletMap,
                          width,
                          height) {
  ns <- session$ns

  observeEvent(input$exportLeaflet, {
    showModal(
      modalDialog(
        title = "Export Map",
        footer = modalButton("OK"),
        selectInput(
          ns("exportType"),
          "Filetype",
          choices = c("png", "pdf", "jpeg")
        ),
        numericInput(ns("width"), "Width (px)", value = width()),
        numericInput(ns("height"), "Height (px)", value = height()),
        downloadButton(session$ns("exportLeafletMap"), "Export"),
        easyClose = TRUE
      )
    )
  })

  output$exportLeafletMap <- downloadHandler(
    filename = function() {
      paste0('plot-', Sys.Date(), '.', input$exportType)
    },
    content = function(filename) {
      m <- leafletMap()

      mapview::mapshot(
        m,
        file = filename,
        remove_controls = "zoomControl",
        vwidth = input$width,
        vheight = input$height
      )
    }
  )
}
