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
  previewImagePath <- reactiveVal(NULL)

  finalLeafletMap <- reactive({
    leafletMap() %>%
      setView(lng = center()$lng, lat = center()$lat, zoom = zoom()) %>%
      customizeLeafletMap(leafletValues()) %>%
      updateDataOnLeafletMap(isoData = isoData(), leafletPointValues = leafletPointValues)
  })

  observeEvent(input$exportLeaflet, {
    withProgress(message = "Generating preview...", value = 0.7, {
     previewImagePath(createPreview(finalLeafletMap(), width(), height()))
    })

    showModal(
      modalDialog(
        title = "Export Map",
        footer = modalButton("OK"),
        fluidRow(
          column(4, numericInput(ns("width"), "Width (px)", value = width())),
          column(4, numericInput(ns("height"), "Height (px)", value = height())),
          column(4, align = "right", style = "margin-top: 1.75em", actionButton(ns("generatePreview"), "Preview"))
        ),
        tags$br(),
        div(
          style = "height: 310px; max-width: 600px; overflow-x: auto; overflow-y: hidden; margin: auto; white-space: nowrap;",
          imageOutput(ns("previewImage"), height = "300px") %>% withSpinner(color = "#20c997")
        ),
        tags$br(),
        fluidRow(
          column(5, textInput(ns("exportFilename"), "Filename (without extension)", value = sprintf("plot-%s", Sys.Date()))),
          column(3, selectInput(ns("exportType"), "Filetype",choices = c("png", "pdf", "jpeg"))),
          column(4, align = "right", style = "margin-top: 1.75em", downloadButton(session$ns("exportLeafletMap"), "Export"))
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$generatePreview, {
    req(input$width, input$height)
    withProgress(message = "Generating preview...", value = 0.7, {
      previewImagePath(createPreview(finalLeafletMap(), input$width, input$height))
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

  output$exportLeafletMap <- downloadHandler(
    filename = function() {
      paste0(input$exportFilename, '.', input$exportType)
    },
    content = function(filename) {
      withProgress(message = "Exporting ...", value = 0.7, {
        finalLeafletMap() %>%
          exportWidgetSnapshot(filename = filename,
                               fileext = input$exportType,
                               width = input$width,
                               height = input$height)
      })
    }
  )
}

createPreview <- function(finalLeafletMap, width, height) {
  tmp_html <- tempfile(fileext = ".html")
  tmp_png <- tempfile(fileext = ".png")
  saveWidget(finalLeafletMap, file = tmp_html, selfcontained = FALSE)
  webshot(tmp_html, file = tmp_png, vwidth = width, vheight = height)

  return(tmp_png)
}

exportWidgetSnapshot <- function(widget, filename, fileext, width, height) {
  # Create temporary HTML file
  temp_file <- tempfile(fileext = ".html")
  saveWidget(widget, file = temp_file, selfcontained = FALSE)

  if (fileext == "pdf") {
    # Direct export to pdf: here the format is not correctly and possibly needs better "options" settings
    #pagedown::chrome_print(temp_file, output = filename)

    # convert to PNG and then to PDF using magick
    # Save temporary PNG file
    temp_png <- tempfile(fileext = ".png")
    webshot(temp_file, file = temp_png, vwidth = width, vheight = height)

    # Use magick to read PNG and convert to raster for PDF export
    img <- magick::image_read(temp_png)
    # We cannot use image_write to write pdf, it is blocked by ImageMagick for shiny, docker, ...
    #magick::image_write(img, path = filename, format = "pdf")

    bitmap <- as.raster(img)
    dims <- magick::image_info(img)

    pdf(filename, width = dims$width / 72, height = dims$height / 72)
    grid.raster(bitmap)
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

