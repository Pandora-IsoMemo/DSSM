#' ui function of leaflet map export module
#'
#' @param id namespace
leafletExportButton <- function(id) {
  ns <- NS(id)

  actionButton(ns("exportLeaflet"), "Export map")
}


#' server funtion of leaflet map export module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param leafletMap reactive leaflet map object
#' @param width reactive width of map in px
#' @param height reactive height of map in px
#' @param zoom map zoom
#' @param center map center (list of lat and lng)
#' @inheritParams updateDataOnLeafletMap
#' @inheritParams customizeLeafletMap
leafletExport <- function(input,
                          output,
                          session,
                          leafletMap,
                          width,
                          height,
                          zoom,
                          center,
                          isoData,
                          leafletValues,
                          leafletPointValues) {
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
        textInput(ns("exportFilename"), "Filename (without extension)", value = sprintf("plot-%s", Sys.Date())),
        tags$br(),
        downloadButton(session$ns("exportLeafletMap"), "Export"),
        easyClose = TRUE
      )
    )
  })

  output$exportLeafletMap <- downloadHandler(
    filename = function() {
      paste0(input$exportFilename, '.', input$exportType)
    },
    content = function(filename) {
      withProgress({
        m <- leafletMap() %>%
          setView(lng = center()$lng,
                  lat = center()$lat,
                  zoom = zoom())

        m <- m %>%
          customizeLeafletMap(leafletValues())

        m <- m %>%
          updateDataOnLeafletMap(isoData = isoData(),
                                 leafletPointValues = leafletPointValues)

        temp_file <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(m,
                                file = temp_file,
                                selfcontained = TRUE)
        webshot2::webshot(temp_file, file = filename, vwidth = input$width, vheight = input$height)
        unlink(temp_file)
      },
      value = 0.9,
      message = "Exporting ...")
    }
  )
}
