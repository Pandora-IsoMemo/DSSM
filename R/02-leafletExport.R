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

        m %>%
          exportWidgetSnapshot(filename = filename,
                               fileext = input$exportType,
                               width = input$width,
                               height = input$height)
      },
      value = 0.9,
      message = "Exporting ...")
    }
  )
}

exportWidgetSnapshot <- function(widget, filename, fileext, width, height) {
  # Create temporary HTML file
  temp_file <- tempfile(fileext = ".html")
  saveWidget(widget, file = temp_file, selfcontained = TRUE)

  if (fileext == "pdf") {
    # Save temporary PNG file
    temp_png <- tempfile(fileext = ".png")
    webshot(temp_file, file = temp_png, vwidth = width, vheight = height)

    # Use magick to read PNG and convert to raster for PDF export
    img <- image_read(temp_png)
    # We cannot use image_write to write pdf, it is blocked by ImageMagick for shiny, docker, ...
    #magick::image_write(img, path = filename, format = "pdf")

    bitmap <- as.raster(img)
    dims <- image_info(img)

    pdf(filename, width = dims$width / 72, height = dims$height / 72)
    grid::grid.raster(bitmap)
    dev.off()

    # Clean up temp PNG
    unlink(temp_png)

  } else if (fileext %in% c("png", "jpeg", "jpg")) {
    # Direct export for image formats
    webshot(temp_file, file = filename, vwidth = width, vheight = height)

  } else {
    stop("Unsupported file extension: must be one of 'png', 'jpeg', or 'pdf'")
  }

  # Clean up temp HTML
  unlink(temp_file)
}

