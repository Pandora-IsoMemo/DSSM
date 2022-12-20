#' ui function of leaflet map export module
#'
#' @param id namespace
leafletExportButton <- function(id) {
  ns <- NS(id)

  tagList(actionButton(ns("exportLeaflet"), "Export Map"),
          div(
            id = ns("phantomjsHelp"),
            helpText(
              "To export map you need to install PhantomJS (https://www.rdocumentation.org/packages/webshot/versions/0.5.2/topics/install_phantomjs)"
            )
          ))
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

  observe({
    if (webshot::is_phantomjs_installed()) {
      shinyjs::enable("exportLeaflet")
      shinyjs::hide("phantomjsHelp")
    } else {
      shinyjs::disable("exportLeaflet")
    }
  })

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

        mapview::mapshot(
          m,
          file = filename,
          remove_controls = "zoomControl",
          vwidth = input$width,
          vheight = input$height
        )
      },
      value = 0.9,
      message = "Exporting ...")
    }
  )
}
