batchPointEstimatesUI <- function(id){
  ns <- NS(id)
  actionButton(ns("batch"), "Batch estimates")
}

batchPointEstimates <- function(input, output, session, estimator, time = FALSE, fruitsData, Model){
  ns <- session$ns

  fileDescription <- if (time) "File with Coordinates and Time"
  else "File with Corrdinates"

  timeVarInput <- if (time) selectInput(inputId = ns("DateOne"),
                                       label = "Date variable:",
                                       choices = character(0))
  else NULL


  observeEvent(input$batch, {
    showModal(modalDialog(
      title = "Batch Calculate Estimates",
      footer = modalButton("OK"),
      fileInput(ns("file"), fileDescription),
      selectInput(ns("fileType"),
                  "File type",
                  choices = c("xlsx", "csv"),
                  selected = "xlsx"
                  ),
      conditionalPanel(
        condition = "input.fileType == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(ns("colseparator"), "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(ns("decseparator"), "decimal separator:", value = ".")),
        ns = ns
      ),
      helpText(
        "The first row in your file need to contain variable names."
      ),
      div(class = "text-danger", textOutput(ns("fileImportWarning"))),
      tags$hr(),
      selectInput(inputId = ns("Longitude"),
                  label = "Longitude variable:",
                  choices = character(0)),
      selectInput(inputId = ns("Latitude"),
                  label = "Latitude variable:",
                  choices = character(0)),
      timeVarInput,
      radioButtons(inputId = ns("CoordType"),
                   label = "Coordinate format",
                   choiceNames = c("decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
                                   "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
                                   "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"),
                   choiceValues = c("decimal degrees", "degrees decimal minutes", "degrees minutes seconds")),
      sliderInput(inputId = ns("RadiusBatch"),
                  label = "Radius (km)",
                  min = 10, max = 300, value = 100, step = 10, width = "100%"),
      div(class = "text-danger", textOutput(ns("coordTransformWarning"))),
      tags$h4("Preview"),
      checkboxInput(ns("previewAllCols"), "Show all columns in preview"),
      div(style = "overflow-x: auto;", tableOutput(ns("preview"))),
      tags$h4("Export"),
      selectInput(
        session$ns("exportType"),
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        ns = ns,
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(ns("expcolseparator"), "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(ns("expdecseparator"), "decimal separator:", value = "."))
      ),
      downloadButton(ns("export"), "Download / Export")
      #tags$hr(),
      # if (!reSourcesInstalled()) helpText(paste("To export data to fruits you need the package ReSources installed in version >=", reSourcesVersion()))
      # else NULL,
      # if (reSourcesInstalled()) actionButton(ns("fruits"), "Export to FRUITS")
      # else shinyjs::disabled(actionButton(ns("fruits"), "Export to FRUITS"))
    ))
  })

  fileImport <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    readBatchFile(inFile$datapath, input$fileType, input$colseparator,
                     input$decseparator)
  })

  importValid <- reactive({!is.null(fileImport())})

  coordinatesSelected <- reactive({
    if (input$Latitude == "" || input$Longitude == "")
      return(FALSE)

    return(TRUE)
  })

  coordinatesValid <- reactive({
    if (input$Latitude == "" || input$Longitude == "")
      return(TRUE)
    if (!all(c(input$Latitude, input$Longitude) %in% names(fileImport())))
      return(FALSE)

    if (input$Latitude == input$Longitude) return(FALSE)

    d <- tryCatch({
      convertLatLong(
      isoData = fileImport(),
      CoordType = input$CoordType,
      Latitude = input$Latitude,
      Longitude  = input$Longitude
      )
    }, error = function(w){
      return(FALSE)
    })

    if (all(d == FALSE)) return(FALSE)
    if (all(is.na(d))) return(FALSE)

    return(TRUE)
  })

  timeValid <- reactive({
    if (!time) return(TRUE)

    if (!input$DateOne %in% names(fileImport())) return(FALSE)

    if (!is.numeric(fileImport()[[input$DateOne]])) return(FALSE)

    return(TRUE)
  })

  data <- reactive({
    if (!importValid()) return(NULL)
    if (!coordinatesValid()) return(fileImport())
    if (time && !timeValid()) return(fileImport())

    df <- convertLatLong(
      isoData = fileImport(),
      CoordType = input$CoordType,
      Latitude = input$Latitude,
      Longitude  = input$Longitude
    )
    if (!time){
    withProgress(
      estimateBatch(
        data = df,
        estimator = estimator(),
        model = Model(),
        longitude = input$Longitude,
        latitude = input$Latitude,
        RadiusBatch = input$RadiusBatch
      ),
      value = 0.1,
      message = 'Computing Batch estimates ...'
    )
      } else {
      withProgress(
        estimateBatch(
          data = df,
          estimator = estimator(),
          model = Model(),
          longitude = input$Longitude,
          latitude = input$Latitude,
          RadiusBatch = input$RadiusBatch,
          time = input$DateOne
        ),
        value = 0.1,
        message = 'Computing Batch estimates ...'
      )
    }
  })

  output$preview <- renderTable({
    if (!coordinatesSelected()) return(NULL)
    if (!coordinatesValid()) return(NULL)
    if (time && !timeValid()) return(NULL)

    df <- data()

    if (input$previewAllCols)
      head(df)
    else
      head(df[, names(df) %in% c(input$Longitude, input$Latitude, input$DateOne,
                                 "Mean", "Sd","SdTotal", "Int_Lower_2.5", "Int_Upper_97.5",
                                 "Int_Lower_Total_2.5", "Int_Upper_Total_97.5")])

  }, striped = TRUE, bordered = TRUE)

  output$fileImportWarning <- renderText({
    if (is.null(fileImport()))
      "Please select file for import."
    else if (is.null(fileImport())){
      "Could not import file. Please check file type and separators."
      }
    else NULL
  })

  output$coordTransformWarning <- renderText({
    if (importValid() && !coordinatesSelected())
      "Please select longitude and latitude variables."
    else if (importValid() && !coordinatesValid())
      "Could not transform coordinates."
    else if (time && !timeValid())
      "Please select time variable"
    else NULL
  })

  observeEvent(importValid(), {
    updateSelectInput(session, "Longitude", choices = names(fileImport()),
                      selected = character(0))
    updateSelectInput(session, "Latitude", choices = names(fileImport()),
                      selected = character(0))

    if (time)
      updateSelectInput(session, "DateOne", choices = names(fileImport()),
                        selected = character(0))
  })
  output$export <- downloadHandler(
    filename = function(){
      exportFilename(fileending = input$exportType)
    },
    content = function(file){
      switch(
        input$exportType,
        csv = exportCSV(file, data(), input$expcolseparator, input$expdecseparator),
        xlsx = exportXLSX(file, data()),
        json = exportJSON(file, data())
      )
    }
  )

  rootSession <- get("session", envir = parent.frame(4))

  observeEvent(input$fruits, {
    df <- data()[c("Mean", "Sd")]

    fruitsData(list(
      data = df,
      event = runif(1)
    ))

    showTab("tab", "fruits", select = TRUE, session = rootSession)

    removeModal()
  })
}

estimateBatch <- function(data, estimator, longitude, latitude, RadiusBatch, time = NULL, model){
  if (is.null(time)){
    dataCenter <- data.frame(centerX = data[, longitude], centerY = data[, latitude])
    naS <- apply(dataCenter, 1, function(x) sum(is.na(x)) > 0)
    dataCenter <- dataCenter[!naS, ]
  } else {
      dataCenter <- data.frame(centerX = data[, longitude], centerY = data[, latitude], time = data[, time])
      naS <- apply(dataCenter, 1, function(x) sum(is.na(x)) > 0)
      dataCenter <- dataCenter[!naS, ]
  }
  est <- estimator(model = model, dataCenter = dataCenter, RadiusBatch = RadiusBatch)

  if(!is.null(est$IntLowerTotal)){
    return(cbind(data[!naS, ], Mean = est$mean,
                 SEM = est$sd, Total_Error = est$sdTotal,
                 SD = est$SDPop,
                 Int_Lower_2.5 = est$IntLower, Int_Upper_97.5 = est$IntUpper,
                 Int_Lower_Total_2.5 = est$IntLowerTotal,
                 Int_Upper_Total_97.5 = est$IntUpperTotal))
  } else {
  return(cbind(data[!naS, ], Mean = est$mean, SEM = est$sd,
        Int_Lower_2.5 = est$IntLower, Int_Upper_97.5 = est$IntUpper))
  }
}

readBatchFile <- function(file, type, sep = ",", dec = "."){
  tryCatch(
    switch(
      type,
      csv = read.csv(file, sep = sep, dec = dec, stringsAsFactors = FALSE),
      xlsx = read.xlsx(file)
    ),
    error = function(e){
      warning("Could not read file")
      NULL
    }
  )
}
