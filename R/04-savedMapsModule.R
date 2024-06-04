#' ui function of saved maps module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
savedMapsTabUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(title,
           id = id,
           value = id,
           tags$h3("Saved Maps"),
           tags$br(),
           uiOutput(ns("mapTable")),
           savedMapsExportButton(ns("exportSavedMapsPlots"))
  )
}



#' server function of saved maps module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param savedMaps saved maps
#'
#' @export
savedMapsTab <- function(input, output, session, savedMaps) {
  ns <- session$ns
  parentSession <- get("session", envir = parent.frame(2))

  output$mapTable <- renderUI({
    mapTable(savedMaps(), ns = ns)
  })

  observeEvent(input$deleteMap, {
    savedMaps(savedMaps()[-input$deleteMap$i])
  })

  observeEvent(savedMaps(), {
    lapply(seq_along(savedMaps()), function(i) {
      rr <- savedMaps()[[i]]
      output[[paste0("thumbnail_", i)]] <-
        renderImage(list(
          src = getThumbnail(rr),
          alt = "Thumbnail missing ...",
          height = 100,
          width = 160
        ), deleteFile = FALSE)
    })
  })

  savedMapsExportServer("exportSavedMapsPlots", savedMaps = savedMaps)
}

# Helper functions for saved maps tabs ----

getThumbnail <- function(savedMap) {
  if (is.null(savedMap$plotFUN)) return(savedMap$file)

  # if thumbnail does not exist, create it
  if (!file.exists(savedMap$file)) {
    dir_path <- dirname(savedMap$file)

    # Create the directory if it does not exist
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # create the plot
    png(savedMap$file, width = 800, height = 500)
    savedMap$plotFUN(savedMap$model)
    dev.off()
  }

  return(savedMap$file)
}

#' Get Full Coord Grid
#'
#' @param gridLength length between two points
getFullCoordGrid <- function(gridLength) {
  lo <- seq(-180, 180, by = gridLength)
  la <- seq(-90, 90, by = gridLength)

  tryCatch({
    expand.grid(lo, la)
    #stop("test error")
    #warning("test warning")
  },
  error = function(cond) {
    alert(paste("Could not create grid of coordinates:", cond$message))
    # Choose a return value in case of error
    return(NULL)
  },
  warning = function(cond) {
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = NULL)
}

#' Filter Coord Circle
#'
#' Filter full grid of coordinates to circle area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param radius (numeric) radius
#'
filterCoordCircle <- function(fullGrid, lat, long, radius) {
  fullGrid[sqrt((fullGrid[, 2] - lat) ^ 2 + (fullGrid[, 1] - long) ^ 2) < radius, ]
}

#' Filter Coord Square
#'
#' Filter full grid of coordinates to square area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param length (numeric) side length of square
#'
filterCoordSquare <- function(fullGrid, lat, long, length) {
  fullGrid[pmax(abs(fullGrid[, 2] - lat), abs(fullGrid[, 1] - long)) < length / 2, ]
}

#' Filter Coord Rectangle
#'
#' Filter full grid of coordinates to square area
#'
#' @param fullGrid grid of coordinates
#' @param lat (numeric) latitude of center
#' @param long (numeric) longitude of center
#' @param latLength (numeric) length between latitudes
#' @param longLength (numeric) length between longitudes
filterCoordRectangle <-
  function(fullGrid,
           lat,
           long,
           latLength,
           longLength) {
    fullGrid[pmax(abs(fullGrid[, 2] - lat)) < latLength / 2 &
               pmax(abs(fullGrid[, 1] - long)) < longLength / 2, ]
  }

#' Get Coord Center
#'
#' Get center of rectangle defined by upper left and lower right coordinates
#'
#' @param upperLeftLat (numeric) latitude of upper left point
#' @param upperLeftLong (numeric) longitude of upper left point
#' @param lowerRightLat (numeric) latitude of lower right point
#' @param lowerRightLong (numeric) longitude of lower right point
getCoordCenter <- function(upperLeftLat,
                           upperLeftLong,
                           lowerRightLat,
                           lowerRightLong) {
  df <- data.frame(
    lon = c(upperLeftLong, lowerRightLong),
    lat = c(upperLeftLat, lowerRightLat)
  )

  res <- df %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_combine() %>%
    st_centroid() %>%
    st_coordinates()
  colnames(res) <- c("lon", "lat")
  res
}

# savedMapsExport ----

#' Saved Maps Export Button
#'
#' UI of the module
#'
#' @param id id of module
savedMapsExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plots of Saved Maps")
}

#' Saved Maps Export Server
#'
#' Server function of the module
#'
#' @param id id of module
#' @inheritParams savedMapsTab
savedMapsExportServer <- function(id, savedMaps) {
  moduleServer(
    id,
    function(input, output, session) {

      withPredictions <- reactive({
        if (length(savedMaps()) > 0) {
          all(sapply(savedMaps(), function(x) !is.null(x$predictions)))
        } else {
          FALSE
        }
      })

      mapChoices <- reactiveVal(c("Please save maps first" = ""))
      observe({
        if (length(savedMaps()) == 0) {
          logDebug("modelResultsSavedMaps: disable export button")
          shinyjs::disable("export")
          mapChoices(c("Please save maps first" = ""))
        } else {
          logDebug("modelResultsSavedMaps: enable export button")
          shinyjs::enable("export")
          mapChoices(getMapChoices(savedMaps()))
        }
      }) %>%
        bindEvent(savedMaps())

      observeEvent(input$export, {
        logDebug("modelResultsSavedMaps: export button clicked")
        showModal(modalDialog(
          title = "Export Graphic",
          footer = modalButton("OK"),
          selectInput(session$ns("displayMap"), "Show Map", choices = mapChoices()),
          plotOutput(session$ns("plot"), height = "300px"),
          tags$br(),
          pickerInput(
            inputId = session$ns("mapsToExport"),
            label = "Export Maps",
            choices = mapChoices(),
            selected = mapChoices(),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `none-selected-text` = "No maps selected",
              `selected-text-format` = "count > 8"
            ),
            multiple = TRUE
          ),
          fluidRow(column(width = 4,
                          selectInput(
                            session$ns("exportType"), "Filetype",
                            choices = c(
                              "jpeg", "png", "pdf", "svg", "tiff",
                              if (withPredictions()) "geo-tiff" else NULL
                            )
                          )),
                   column(width = 4,
                          conditionalPanel(
                            condition = "input.exportType != 'geo-tiff'",
                            ns = session$ns,
                            numericInput(session$ns("width"), "Width (px)", value = 1280)
                          )),
                   column(width = 4,
                          conditionalPanel(
                            condition = "input.exportType != 'geo-tiff'",
                            ns = session$ns,
                            numericInput(session$ns("height"), "Height (px)", value = 800)
                          ))
          ),
          textInput(session$ns("userFileName"),
                    "File name (without extension)",
                    value = "savedMaps_dssm" %>% prefixSysTime(),
                    placeholder = "savedMaps_dssm" %>% prefixSysTime(),
                    width = "100%"),
          downloadButton(session$ns("exportExecute"), "Export"),
          easyClose = TRUE
        ))
      })

      output$plot <- renderImage({
        validate(need(length(savedMaps()) > 0, "Please save maps first ..."))
        validate(need(input[["displayMap"]], "Please select a map to display ..."))

        mapToDisplay <- savedMaps()[[as.numeric(input[["displayMap"]])]]
        list(
          src = getThumbnail(mapToDisplay),
          height = 3*100,
          width = 3*160
        )
      }, deleteFile = FALSE)

      output$exportExecute <- downloadHandler(
        filename = function(){
          setZipFileName(fileName = input[["userFileName"]],
                         defaultFileName = "savedMaps_dssm" %>% prefixSysTime())
        },
        content = function(file){
          exportSavedMapsPlots(file = file,
                               savedMaps = savedMaps()[as.numeric(input[["mapsToExport"]])],
                               width = input$width,
                               height = input$height,
                               exportType = input$exportType) %>%
            suppressWarnings() %>%
            tryCatchWithWarningsAndErrors(errorTitle = "Export of Saved Maps faild")
        }
      )
    })
}

setZipFileName <- function(fileName, defaultFileName) {
  newName <- defaultFileName

  # set custom name
  if (length(fileName) > 0 && fileName != "") {
    newName <- fileName
  }

  newName <- sprintf("%s.zip", newName)

  return(newName)
}

exportSavedMapsPlots <- function(file, savedMaps, width, height, exportType) {
  withProgress(message = "Exporting plots ...", value = 0, {
    # create all file names to be put into a zip
    figFileNames <- sapply(seq_along(savedMaps),
                           function(i) {
                             sprintf("%s_%s.%s", savedMaps[[i]]$name, savedMaps[[i]]$type, exportType)
                           })

    for (i in seq_along(savedMaps)) {
      incProgress(1 / length(savedMaps), detail = paste("plot: ", i))
      figFilename <- figFileNames[[i]]

      if (exportType == "geo-tiff") {
        writeGeoTiff(savedMaps[[i]]$predictions, figFilename)
      } else {
        # save desired file type
        writeGraphics(exportType = exportType,
                      plot = savedMaps[[i]]$plotFUN(savedMaps[[i]]$model), #replayPlot(savedMaps[[i]]$plot),
                      filename = figFilename,
                      width = width,
                      height = height)
      }
    }

    # zip file to be downloaded:
    zipr(zipfile = file, files = figFileNames)

    # clean up all single files
    unlink(figFileNames)
  })
}
