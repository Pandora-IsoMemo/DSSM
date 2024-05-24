createSavedMap <- function(model, predictions, plot, type, name) {
  tmp <- tempfile("plot", fileext = ".png")
  png(tmp, width = 800, height = 500)
  replayPlot(plot)
  dev.off()

  list(model = model, predictions = predictions, plot = plot, file = tmp , type = type, name = name)
}

savedMapsExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plots of Saved Maps")
}

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

      savedMapsNames <- reactiveVal(NULL)
      mapChoices <- reactiveVal(c("Please save maps first" = ""))
      observe({
        logDebug("modelResultsSavedMaps: disable export button")
        if (length(savedMaps()) == 0) {
          shinyjs::disable("export")
          mapChoices(c("Please save maps first" = ""))
        } else {
          shinyjs::enable("export")
          names <- sapply(savedMaps(), function(x) x$name)
          savedMapsNames(names)
          mapChoices(c("Please select saved maps" = "", names))
        }
      }) %>%
        bindEvent(savedMaps())

      mapsSelected <- reactiveVal(NULL)
      observe({
        logDebug("modelResultsSavedMaps: observe mapsToExport")
        if (is.null(input[["mapsToExport"]]) || input[["mapsToExport"]] == "") {
          mapsSelected(NULL)
        } else {
          mapsSelected(extractSelectedMaps(savedMaps(), input[["mapsToExport"]]))
        }
      }) %>%
        bindEvent(input[["mapsToExport"]], ignoreNULL = FALSE)

      observeEvent(input$export, {
        showModal(modalDialog(
          title = "Export Graphic",
          footer = modalButton("OK"),
          plotOutput(session$ns("plot"), height = "300px"),
          tags$br(),
          fluidRow(column(width = 4,
                          selectInput(session$ns("mapsToExport"), "Saved Maps to Export",
                                      choices = mapChoices(),
                                      selected = "",
                                      multiple = TRUE)
                          ),
                   column(width = 4,
                          selectInput(session$ns("displayMap"), "Show Map",
                                      choices = mapChoices(),
                                      selected = "")
                   )
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

      output$plot <- renderPlot({
        validate(need(length(savedMaps()) > 0, "Please save maps first ..."))
        validate(need(input[["displayMap"]], "Please select a map to display ..."))

        mapToDisplay <- extractSelectedMaps(savedMaps(), input[["displayMap"]])
        replayPlot(mapToDisplay[[1]]$plot)
      })

      exportType <- reactiveVal("png")

      observe({
        exportType(input$exportType)
      }) %>%
        bindEvent(input$exportType)

      output$exportExecute <- downloadHandler(
        filename = function(){
          setZipFileName(fileName = input[["userFileName"]],
                         defaultFileName = "savedMaps_dssm" %>% prefixSysTime())
        },
        content = function(file){
          exportSavedMapsPlots(file = file,
                               savedMaps = extractSelectedMaps(savedMaps(), input[["mapsToExport"]]),
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

extractSelectedMaps <- function(savedMaps, selectedMaps) {
  savedMaps[sapply(savedMaps, function(x) x$name %in% selectedMaps)]
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
        switch(
          exportType,
          png = png(figFilename, width = width, height = height),
          jpeg = jpeg(figFilename, width = width, height = height),
          pdf = pdf(figFilename, width = width / 72, height = height / 72),
          tiff = tiff(figFilename, width = width, height = height),
          svg = svg(figFilename, width = width / 72, height = height / 72)
        )
        replayPlot(savedMaps[[i]]$plot)
        dev.off()
      }
    }

    # zip file to be downloaded:
    zipr(zipfile = file, files = figFileNames)

    # clean up all single files
    unlink(figFileNames)
  })
}
