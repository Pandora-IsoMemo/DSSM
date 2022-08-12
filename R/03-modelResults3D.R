#' ui function of modelResults3D module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResults3DUI <- function(id, title = ""){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      sidebarPanel(
        width = 2,
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = c("Database" = "db",
                                "Upload file" = "file",
                                "Saved map" = "model"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'file'",
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
          radioButtons(inputId = ns("CoordType"),
                          label = "Coordinate format",
                        choiceNames = c("decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
                                        "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
                                        "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"),
                        choiceValues = c("decimal degrees", "degrees decimal minutes", "degrees minutes seconds")),
          fileInput(ns("file"), "Upload file"),
          tags$hr(),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
          ns = ns,
          selectInput(
            ns("savedModel"),
            "Saved Map",
            choices = NULL,
            selected = NULL
          )
        ),
        conditionalPanel(
          condition = "input.dataSource != 'model'",
          ns = ns,
          selectInput(inputId = ns("Independent"),
                      label = "Dependent variable:",
                      choices = c("d15N", "d13C")),
          selectInput(inputId = ns("IndependentUnc"),
                      label = "Uncertainty(optional) of dep. var.:",
                      choices = c("")),
          selectInput(inputId = ns("DateType"),
                      label = "Date type:",
                      choices = c("Interval", "Mean + 1 SD uncertainty",
                                  "Single point"), selected = "Interval"),
          selectInput(inputId = ns("DateOne"),
                      label = "Mean or lower time point:",
                      choices = c("")),
          conditionalPanel(
            condition = "input.DateType == 'Interval' || input.DateType == 'Mean + 1 SD uncertainty'",
            selectInput(inputId = ns("DateTwo"),
                        label = "Standard deviation or upper time point:",
                        choices = c("")),
            ns = ns),
          selectInput(inputId = ns("Longitude"),
                      label = "Longitude variable:",
                      choices = c("Longitude")),
          selectInput(inputId = ns("Latitude"),
                      label = "Latitude variable:",
                      choices = c("Latitude")),
          selectInput(inputId = ns("Site"),
                      label = "Site/ID (optional):",
                      choices = c("")),
          radioButtons(inputId = ns("SplineType"),
                      label = "Smooth type",
                      choices = c("planar" = "1", "spherical" = "2"),
                      selected = "1"),
          conditionalPanel(
            condition = "input.SplineType == '1'",
            checkboxInput(inputId = ns("correctionPac"),
                          label = "Border correction for pacific",
                          value = FALSE),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.SplineType == 2",
            sliderInput(inputId = ns("Smoothing"),
                        label = "Number of spatial basis functions",
                        min = 10, max = 250, value = 30, step = 5),
            sliderInput(inputId = ns("SmoothingT"),
                        label = "Number of time basis functions",
                        min = 4, max = 50, value = 12, step = 1),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.SplineType == 1",
            sliderInput(inputId = ns("SmoothingClassic"),
                        label = "Number of spatial basis functions",
                        min = 10, max = 1000, value = 150, step = 5),
            ns = ns
          ),
          radioButtons(inputId = ns("Penalty"),
                      label = "Extrapolation behaviour",
                      choices = c("constant" = "1", "linear" = "2"),
                      selected = "2"),
          checkboxInput(inputId = ns("Outlier"),
                        label = "Remove model outliers",
                        value = FALSE, width = "100%"),
          checkboxInput(inputId = ns("OutlierD"),
                        label = "Remove data outliers",
                        value = FALSE, width = "100%"),

          conditionalPanel(
            condition = "input.Outlier == true",
            sliderInput(inputId = ns("OutlierValue"),
                        label = "Model outlier threshold in standard deviations",
                        min = 2, max = 8, value = 4, step = 0.1),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.OutlierD == true",
            sliderInput(inputId = ns("OutlierValueD"),
                        label = "Data outlier threshold in standard deviations",
                        min = 2, max = 8, value = 4, step = 0.1),
            ns = ns
          ),
          checkboxInput(inputId = ns("modelArea"),
                        label = "Restrict model area",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.modelArea == true",
            numericInput(inputId = ns("mALat1"),
                        label = "Set lower latitude restriction",
                        min = -90, max = 90, value = c(-90), width = "80%"),
            numericInput(inputId = ns("mALat2"),
                        label = "Set upper latitude restriction",
                        min = -90, max = 90, value = c(90), width = "80%"),
            numericInput(inputId = ns("mALong1"),
                        label = "Set lower longitude restriction",
                        min = -180, max = 180, value = c(-180), width = "80%"),
            numericInput(inputId = ns("mALong2"),
                        label = "Set upper longitude restriction",
                        min = -180, max = 180, value = c(180), width = "80%"),
            ns = ns
          ),
          checkboxInput(inputId = ns("Bayes"),
                        label = "Bayesian model",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            checkboxInput(inputId = ns("sdVar"),
                          label = "Varying standard error",
                          value = FALSE, width = "100%"),
            condition = "input.Bayes == true",
            conditionalPanel(
              condition = "input.DateType == 'Interval' || input.DateType == 'Mean + 1 SD uncertainty'",
              radioButtons(inputId = ns("dateUnc"),
                           label = "Distribution of date uncertainty",
                           choices = c("normal (sd = 1/4 width)" = "normal",
                                       "normal (sd = 1/2 width)" = "normal2",
                                       "uniform (full width)" = "uniform",
                                       "mid point" = "point")),
              ns = ns),
            sliderInput(inputId = ns("Iter"),
                        label = "Number of MCMC iterations",
                        min = 100, max = 100000, value = 3000, step = 100),
            sliderInput(inputId = ns("burnin"), label = "Number of burnin iterations",
                        value = 1000, min = 100, max = 10000, step = 100),
            sliderInput(inputId = ns("nChains"), label = "Number of MCMC chains",
                        value = 1, min = 1, max = 16, step = 1),
            sliderInput(inputId = ns("thinning"), label = "MCMC thinning (keep every x-th sample)",
                        value = 10, min = 1, max = 20, step = 1),

            sliderInput(inputId = ns("smoothConst"),
                        label = "Amount of smoothing",
                        min = 0.2, max = 5, value = 1, step = 0.1),
            ns = ns)
          ),
          actionButton( ns("start"), "Start"),
          conditionalPanel(
            condition = conditionPlot(ns("DistMap")),
            checkboxInput(inputId = ns("fixCol"),
                          label = "Fix colours and ranges ",
                          value = FALSE, width = "100%")
          ),
          conditionalPanel(
            ns = ns,
            condition = "input.dataSource != 'model'",
            tags$hr(),
            tags$h4("Batch Modeling"),
            batchModelingUI(ns("batchModeling"))
          )
        ),
        mainPanel(
          width = 8,
          div(class = "aspect-16-9", div(
            plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
          )),
          conditionalPanel(
            condition = conditionPlot(ns("DistMap")),
            textOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
            div(plotExportButton(ns("export"))),
            tags$br(),
            conditionalPanel(
              condition = "input.mapType == 'Map'",
              ns = ns,
              sliderAndNumericInputUI(ns("timeExtended"),
                                      label = "Time selection",
                                      min = 0, max = 15000, value = 5000, step = 100),
              div(
                style = "display:flex;",
                div(
                  class = "zoom-map",
                  sliderInput(inputId = ns("zoom"),
                              label = "Zoom/x-Range in degrees Longitude",
                              min = 0.1, max = 360, value = 50, width = "100%")
                ),
                div(
                  class = "move-map",
                  uiOutput(ns("move"))
                )),
              numericInput(inputId = ns("upperLeftLatitude"),
                          label = "Set Latitude of upper left corner",
                          min = -90, max = 90, value = c(), width = "20%"),
              numericInput(inputId = ns("upperLeftLongitude"),
                          label = "Set Longitude of upper left corner",
                          min = -180, max = 180, value = c(), width = "20%"),
              numericInput(inputId = ns("zoomSet"),
                          label = "Zoom/x-Range in degrees Longitude (click set button for apply)",
                          min = 0.1, max = 360, value = 50, width = "20%"),
              actionButton( ns("set"), "Set"),
              div(div(
                style = 'display:inline-block',
                class = "save-plot-container",
                textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
                actionButton(ns("saveMap"), "Save map"),
                dataExportButton(ns("exportData")),
                conditionalPanel(
                  condition = "input.Bayes == true",
                  modelDiagButton(ns("modelDiag")),
                  ns = ns)
              )),
              actionButton(ns("add_btn2D"), "Add data point"),
              actionButton(ns("rm_btn2D"), "Remove data point"),
              actionButton(ns("ok"), "Ok"),
              uiOutput(ns("pointInput2D"))
            )
          ),
          # add input for timerange also ? ----
          conditionalPanel(
            condition = "input.mapType == 'Time course'",
            ns = ns,
            sliderInput(inputId = ns("trange"),
                        label = "Time range",
                        min = 0, max = 15000, value = c(0, 15000), width = "100%"),
            actionButton(ns("add_btn"), "Add data point"),
            actionButton(ns("rm_btn"), "Remove data point"),
            # colourInput(ns("col_btn"), "Select colour of data point"),
            # sliderInput(ns("size_btn"), "Size data point", min = 0.1, max = 10, value = 1),
            uiOutput(ns("pointInput")),
            conditionalPanel(
              condition = "input.pointsTime == true",
            dataExportButton(ns("exportDataTimeCourse"), title = "Export time course plot data"),
            dataExportButton(ns("exportDataTimeCoursePred"), title = "Export time course plot predictions data"),
            ns = ns)
          )
      ),
      sidebarPanel(
        width = 2,
        radioButtons(inputId = ns("Centering"),
                     label = "Map Centering",
                     choices = c("0th meridian" = "Europe", "160th meridian" = "Pacific")),
        selectInput(inputId = ns("estType"), label = "Estimation type",
                    choices = c("Mean" = "Mean",
                                "1 SEM" = "1 SE",
                                "1 Total_Error" = "1 SETOTAL",
                                "2 SEM" = "2 SE",
                                "2 Total_Error" = "2 SETOTAL",
                                "1 SD" = "1 SD Population",
                                "2 SD" = "2 SD Population",
                                "Quantile_Mean" = "Quantile",
                                "Quantile_Total" = "QuantileTOTAL"),
                    selected = "Mean"),
        conditionalPanel(
          ns = ns,
          condition = "input.estType == 'Quantile' || input.estType == 'QuantileTOTAL'",
          sliderInput(inputId = ns("Quantile"),
                      label = "Estimation quantile",
                      min = 0.01, max = 0.99, value = c(0.9), width = "100%")
        ),
        checkboxInput(inputId = ns("showModel"), label = "Show model estimates", value = T),
        numericInput(ns("rangezMin"), "Min value of range dependent variable", value = 0),
        numericInput(ns("rangezMax"), "Max value of range dependent variable", value = 10),
        selectInput(inputId = ns("limitz"), label = "Restrict range dependent variable",
                      choices = list("No restriction" = "No restriction",
                                     "0-1" = "0-1",
                                     "0-100" = "0-100")),
          radioButtons(inputId = ns("mapType"), label = "Plot type", inline = TRUE,
                       choices = c("Map", "Time course"),
                       selected = "Map"),
        conditionalPanel(
          condition = "input.mapType == 'Time course'",
          selectInput(inputId = ns("intervalType"), label = "Uncertainty Interval Type",
                      choices = list("none" = "1",
                                     "1 SEM" = "2",
                                     "1 SD" = "5",
                                     "1 Total_Error" = "3",
                                     "1 SEM & Total_Error" = "4",
                                     "2 SEM" = "6",
                                     "2 SD" = "7",
                                     "2 Total_Error" = "8",
                                     "2 SEM & Total_Error" = "9"), selected = "2"),
          checkboxInput(inputId = ns("pointsTime"),
                        label = "Show nearby points",
                        value = TRUE, width = "100%"),
          conditionalPanel(
            condition = "input.pointsTime == true",
            checkboxInput(inputId = ns("intTime"),
                          label = "Show nearby points unc. intervals",
                          value = FALSE, width = "100%"),
            sliderInput(inputId = ns("rangePointsTime"),
                        label = "Show nearby points / intervals range in km",
                        min = 10, max = 2500, value = 250, step = 10),
            ns = ns),
          ns = ns),
          radioButtons(inputId = ns("terrestrial"), label = "", inline = TRUE,
                       choices = list("Terrestrial " = 1, "All" = 3, "Aquatic" = -1),
                       selected = 1),
          checkboxInput(inputId = ns("points"),
                        label = "Show locations on map",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("grid"),
                        label = "Show map grid",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("scale"),
                        label = "Show map scale",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("arrow"),
                        label = "Show north arrow",
                        value = TRUE, width = "100%"),
        checkboxInput(inputId = ns("titleMain"),
                      label = "Show plot title",
                      value = TRUE),
        checkboxInput(inputId = ns("titleScale"),
                      label = "Show colour scale title",
                      value = TRUE),
        checkboxInput(inputId = ns("showScale"),
                      label = "Show colour scale",
                      value = TRUE),
        checkboxInput(inputId = ns("setAxisLabels"),
                      label = "Set axis labels",
                      value = FALSE),
        conditionalPanel(
          condition = "input.setAxisLabels == true",
          textInput(ns("mainLabel"), NULL, placeholder = "main title"),
          textInput(ns("yLabel"), NULL, placeholder = "y-axis"),
          textInput(ns("xLabel"), NULL, placeholder = "x-axis"),
          textInput(ns("scLabel"), NULL, placeholder = "colour scale title"), ns = ns
        ),
        checkboxInput(inputId = ns("setNorth"),
                      label = "Set north arrow and scale size and position",
                      value = FALSE),
        conditionalPanel(
          condition = "input.setNorth == true",
          sliderInput(ns("northSize"), "Size north arrow", min = 0, max = 1, value = 0.2),
          sliderInput(ns("scalSize"), "Size scale", min = 0, max = 1, value = 0.1),
          sliderInput(ns("scaleX"), "Scale x orientation", min = 0, max = 1, value = 0),
          sliderInput(ns("scaleY"), "Scale y orientation", min = 0, max = 1, value = 0.1),
          sliderInput(ns("NorthX"), "North arrow x orientation", min = 0, max = 1, value = 0.025),
          sliderInput(ns("NorthY"), "North arrow y orientation", min = 0, max = 1, value = 0.925),
          ns = ns
        ),
          conditionalPanel(
            condition = "input.points == true",
            sliderInput(inputId = ns("pointSize"),
                        label = "Location mark size",
                        min = 0.1, max = 2, value = 1, width = "100%",
                        step = 0.1), ns = ns),
        checkboxInput(inputId = ns("colPSettings"),
                      label = "Set colour and shape of location marks",
                      value = FALSE, width = "100%"),
        conditionalPanel(
          condition = "input.colPSettings == true",
          radioButtons(inputId = ns("colFix"),
                       label = "Fixed or variable location mark colour?", choices = c("fixed", "variable")),
          conditionalPanel(
            condition = "input.colFix == 'fixed'",
            colourInput(inputId = ns("pointCol"),
                        label = "Colour of location marks",
                        value = "#2C2161"), ns = ns),
          conditionalPanel(
            condition = "input.colFix == 'variable'",
            selectInput(inputId = ns("pointLabelsVarCol"),
                        label = "Select point color variable",
                        choices = c("")),
            selectInput(inputId = ns("colorsP"), label = "Colour palette for points",
                        choices = list("Red-Yellow-Green" = "RdYlGn",
                                       "Yellow-Green-Blue" = "YlGnBu",
                                       "Purple-Orange" = "PuOr",
                                       "Pink-Yellow-Green" = "PiYG",
                                       "Red-Yellow-Blue" = "RdYlBu",
                                       "Yellow-Brown" = "YlOrBr",
                                       "Brown-Turquoise" = "BrBG"),
                        selected = "RdYlGn"),
            ns = ns),
          selectInput(inputId = ns("pointShape"), label = "Shape of location marks",
                      choices = 0:25, selected = 4),
          ns = ns),
          sliderInput(inputId = ns("AddU"),
                      label = "Location marks and convex hull: Add time uncertainty in years",
                      min = 0, max = 1000, value = 0, step = 10),
          sliderInput(inputId = ns("StdErr"),
                      label = "Display up to max standard error",
                      min = 0, max = 10000, value = 10000, width = "100%"),
          radioButtons(inputId = ns("interior"),
                        label = "Apply convex hull",
                       choices = c("none" = "0", "spatio-temporal" = "1", "time-sliced spatial" = "2"),
                       selected = "2", width = "100%"),
        checkboxInput(inputId = ns("mask"),
                      label = "Mask / Show output within range of points",
                      value = FALSE, width = "100%"),
        conditionalPanel(
          condition = "input.mask == true",
          sliderInput(inputId = ns("maskRadius"),
                      label = "Mask radius in km",
                      min = 10, max = 2500, value = 500, width = "100%",
                      step = 10), ns = ns),

          selectInput(inputId = ns("Colours"), label = "Colour palette",
                       choices = list("Red-Yellow-Green" = "RdYlGn",
                                      "Yellow-Green-Blue" = "YlGnBu",
                                      "Purple-Orange" = "PuOr",
                                      "Pink-Yellow-Green" = "PiYG",
                                      "Red-Yellow-Blue" = "RdYlBu",
                                      "Yellow-Brown" = "YlOrBr",
                                      "Brown-Turquoise" = "BrBG"),
                       selected = "RdYlGn"),
          checkboxInput(inputId = ns("reverseCols"),
                        label = "Reverse colors",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("ncol"),
                      label = "Approximate number of colour levels",
                      min = 4, max = 50, value = 20, step = 2, width = "100%"),
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("resolution"),
                      label = "Plot resolution (px)",
                      min = 20, max = 500, value = 100, width = "100%",
                      step = 20),
        checkboxInput(inputId = ns("pointLabels"),
                      label = "Scale point size by variable",
                      value = FALSE, width = "100%"),
        conditionalPanel(
          condition = "input.pointLabels == true",
          selectInput(inputId = ns("pointLabelsVar"),
                      label = "Select point size variable",
                      choices = c("")), ns = ns
        ),
        checkboxInput(inputId = ns("textLabels"),
                      label = "Add location mark text labels",
                      value = FALSE, width = "100%"),
        conditionalPanel(
          condition = "input.textLabels == true",
          selectInput(inputId = ns("textLabelsVar"),
                      label = "Select text label variable",
                      choices = c("")),
          sliderInput(inputId = ns("fontSize"),
                      label = "Font size)",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          selectInput(inputId = ns("fontType"),
                      label = "Select font type",
                      choices = names(pdfFonts())),
          colourInput(inputId = ns("fontCol"),
                      label = "Colour of font",
                      value = "#2C2161")
          , ns = ns),
          numericInput(inputId = ns("centerY"),
                       label = "Center point latitude",
                       min = -180, max = 180, value = c(), step = 0.5, width = "100%"),
          numericInput(inputId = ns("centerX"),
                       label = "Center point longitude",
                       min = -90, max = 90, value = c(), step = 0.5, width = "100%"),
            sliderInput(inputId = ns("Radius"),
                        label = "Radius (km)",
                        min = 10, max = 300, value = 100, step = 10, width = "100%"),
        sliderInput(inputId = ns("AxisSize"),
                    label = "Axis title font size",
                    min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
        sliderInput(inputId = ns("AxisLSize"),
                    label = "Axis label font size",
                    min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
            batchPointEstimatesUI(ns("batch"))
        )
    )
  )
}

#' server function of model Results module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData data
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#'
#' @export
modelResults3D <- function(input, output, session, isoData, savedMaps, fruitsData){
  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), "temporalAvg")

    updateSelectInput(session, "savedModel", choices = choices)
  })

  observeEvent(input$saveMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == ""){
      alert("Please provide a map name")
      return()
    }

    map <- createSavedMap(
      model = Model(),
      predictions = values$predictions,
      plot = values$plot,
      type = "temporalAvg",
      name = mapName
    )
    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })


  output$centerEstimate <- renderText({
    if (is.na(input$centerY) | is.na(input$centerX) | is.na(input$Radius) | input$mapType == "Time course") return("")

    if (is.na(values$meanCenter) | is.na(values$sdCenter)) {
      return("Cannot compute mean and sd at your provided coordinates.
             Please raise the plot resolution or radius such that estimates within the radius are available.")
    }

    paste0("Mean: ", values$meanCenter,
           ", Standard error of the mean: ", values$sdCenter,
           "  at coordinates ",  "(",
           input$centerY, "\u00B0, " , input$centerX,
           "\u00B0) for a ", round(input$Radius, 3),
           " km radius")
  })


  data <- reactive({
    switch(
      input$dataSource,
      db = isoData(),
      file = fileImport()
    )
  })

  coordType <- reactive({
    switch(
      input$dataSource,
      db = "decimal degrees",
      file = input$CoordType
    )
  })

  observeEvent(input$Bayes, {
    if (input$Bayes) alert(alertBayesMessage()) else NULL
  })

  Model <- reactiveVal()

  observeEvent(input$start, ignoreNULL = FALSE, {
    if (input$dataSource == "model") {
      if (length(savedMaps()) == 0) return(NULL)

      Model(savedMaps()[[as.numeric(input$savedModel)]]$model)
      return()
    }
    values$set <- 0

    if (input$Independent == "" | input$Latitude == "" |
          input$Longitude == "" | input$DateOne == "" |
        (input$DateTwo == "" & input$DateType != "Single point")) {
      Model(NULL)
      return()
    }

    params <- reactiveValuesToList(input)
    params$coordType <- coordType()

    model <- estimateMap3DWrapper(data(), params)

    Model(model)
  })

  Independent <- reactive({
    names(Model()$data)[1]
  })

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      val <- sd(Model()$data[, isolate(Independent())], na.rm = TRUE)
      updateSliderInput(session, "StdErr", value = signif(5 * val, 2),
                        min = 0, max = signif(5 * val, 2),
                        step = signif(roundUpNice(val, nice = c(1,10)) / 1000, 1))
      if(input$Centering == "Europe"){
        rangeLong <- diff(range(Model()$data$Longitude, na.rm = TRUE) + c(-1, 1))

        updateSliderInput(session, "zoom",
                          value = pmin(360, pmax(0, rangeLong, na.rm = TRUE)))
      } else {
        longRange <- Model()$data$Longitude
        longRange[Model()$data$Longitude < -20] <- longRange[Model()$data$Longitude < -20] + 200
        longRange[Model()$data$Longitude >= -20] <- (- 160 + longRange[Model()$data$Longitude >= -20])
        rangeLong <- diff(range(longRange, na.rm = TRUE) + c(-1, 1))
        updateSliderInput(session, "zoom",
                          value = pmin(360, pmax(0, rangeLong, na.rm = TRUE)))
      }
      values$up <- 0
      values$right <- 0
    }
  })

  output$move <- renderUI({
    moveButtons(ns = session$ns)
  })

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      if(input$estType %in% c("1 SETOTAL", "2 SETOTAL", "1 SD Population", "2 SD Population") && input$mapType != "Time course"){
        sdVal <- ifelse(grepl("2", input$estType), 2, 1)
        val <- signif(1.1 * max(Model()$model$range$seTotal) * sdVal, 2)
        updateNumericInput(session, "rangezMin", value = 0, min = 0, max = val * 3)
        updateNumericInput(session, "rangezMax", value = val, min = 0, max = val * 3)
      }
      if(input$estType %in% c("1 SE", "2 SE") && input$mapType != "Time course"){
        sdVal <- ifelse(grepl("2", input$estType), 2, 1)
        val <- signif(1.1 * max(Model()$model$range$se) * sdVal, 2)
        updateNumericInput(session, "rangezMin", value = 0, min = 0, max = val * 3)
        updateNumericInput(session, "rangezMax", value = val, min = 0, max = val * 3)
      }
      if(!(input$estType %in% c("1 SE", "1 SETOTAL", "2 SE", "2 SETOTAL", "1 SD Population", "2 SD Population"))){
        minValue <- signif(Model()$model$range$mean[1] - 0.1 * diff(Model()$model$range$mean), which(round(abs(diff(Model()$model$range$mean) / Model()$model$range$mean[1] * 10^(0:10)), 0) > 1)[1])
        maxValue <- signif(Model()$model$range$mean[2] + 0.1 * diff(Model()$model$range$mean), which(round(abs(diff(Model()$model$range$mean) / Model()$model$range$mean[2] * 10^(0:10)), 0) > 1)[1])
        updateNumericInput(session, "rangezMin", value = minValue, min = minValue, max = maxValue)
        updateNumericInput(session, "rangezMax", value = maxValue, min = minValue, max = maxValue)
      }
    }
  })

  observeEvent(input$zoom, {
    zoom <- input$zoom
    values$zoom <- input$zoom
  })

  observeEvent(input$up, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$up <- values$up + zoom / 40
  })

  observeEvent(input$down, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$up <- values$up - zoom / 40
  })
  observeEvent(input$left, {
    if(values$set > 0){
    zoom <- values$zoom
  } else {
    zoom <- input$zoom
  }
    values$right <- values$right - zoom / 40
  })
  observeEvent(input$right, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$right <- values$right + zoom / 40
  })
  observeEvent(input$center, {
    values$up <- 0
    values$right <- 0
  })

  observeEvent(input$set, {
    values$set <- 1
    values$up <- 0
    values$right <- 0
    values$zoom <- input$zoomSet
    values$upperLeftLatitude <- input$upperLeftLatitude
    values$upperLeftLongitude <- input$upperLeftLongitude
  })

  dateExtent <- reactiveValues(
    min = 0,
    max = 15000,
    mean = 5000,
    range = c(0, 15000),
    step = 100
  )

  observe({
    validate(validInput(Model()))

    if(input$dataSource != "model"){
      try({
        if(input$DateType == "Interval"){
          d <- c(data()[, isolate(input$DateOne)],
                 data()[, isolate(input$DateTwo)])
        }
        if(input$DateType == "Mean + 1 SD uncertainty"){
          d <- c(data()[, isolate(input$DateOne)] + 2 *
                   data()[, isolate(input$DateTwo)],
                 data()[, isolate(input$DateOne)] - 2 *
                   data()[, isolate(input$DateTwo)])
        }
        if(input$DateType == "Single point"){
          d <- data()[, isolate(input$DateOne)]
        }
      }, silent = TRUE)
    } else {
      try({d <- Model()$data[, "Date"]}, silent = TRUE)
    }

    if(exists("d")){
      d <- na.omit(d)
      dateExtent$mean <- signif(mean(d), digits = 1)
      dateExtent$range <- signif(range(d), digits = 1)
      dateExtent$step <- signif(roundUpNice(diff(range(d)),
                                            nice = c(1,10)) / 10000,
                                digits = 2)
      dateExtent$min <- signif(min(d) - diff(range(d)) * 0.1, digits = 2)
      dateExtent$max <- signif(max(d) + diff(range(d)) * 0.1, digits = 2)

      # time range update ----
      updateSliderInput(
        session,
        "trange",
        value = dateExtent$range,
        min = dateExtent$min,
        max = dateExtent$max,
        step = dateExtent$step
      )
    }
  })

  ### Add Points
  pointDat2D <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(Model(), ignoreNULL = FALSE, {
    pointDat2D(data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })

  addRow2D <- function(df) {
    rbind(df, data.frame(index = nrow(df) + 1, y = NA,
                         x = NA, label = "",
                         pointColor = "black", pointSize = 1,
                         pointAlpha = 0.5, stringsAsFactors = FALSE))
  }

  rmRow2D <- function(df) {
    if (nrow(df) > 0) df[- nrow(df), , drop = FALSE]
    else df
  }

  observeEvent(input$add_btn2D, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    pointDat2D(addRow2D(pointDat2D()))
  })

  observeEvent(input$rm_btn2D, {
    pointDat2D(rmRow2D(pointDat2D()))
  })

  inputGroup2D <- reactive({
    createPointInputGroup2D(pointDat2D(), ns = session$ns)
  })

  pointDatOK <- eventReactive(input$ok, ignoreNULL = FALSE, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    return(pointDat2D())
  })

  userInputTime <- sliderAndNumericInputServer("timeExtended",
                                               value = reactive(dateExtent$mean),
                                               min = reactive(dateExtent$min),
                                               max = reactive(dateExtent$max),
                                               step = reactive(dateExtent$step))
  plotFun <- reactive({
    function(model, time = userInputTime(), returnPred = FALSE,...){
      pointDat = pointDat()
      pointDatOK = pointDatOK()
      if(input$fixCol == FALSE){
        if(values$set > 0){
          zoom <- values$zoom
        } else {
          zoom <- input$zoom
        }
        rangey <- - diff(range(model$data$Latitude, na.rm = TRUE)) / 2 +
          max(model$data$Latitude, na.rm = TRUE) + values$up
        if(!is.na(values$upperLeftLatitude) & values$set > 0){
          rangey <- values$upperLeftLatitude + c(- zoom / 2 , 0) + values$up
        } else {
          rangey <- rangey + c( - zoom / 4, zoom / 4)
        }
        if(input$Centering == "Europe"){
          rangex <- - diff(range(model$data$Longitude, na.rm = TRUE)) / 2 +
            max(model$data$Longitude, na.rm = TRUE) + values$right
          if(!is.na(values$upperLeftLongitude) & values$set > 0){
            rangex <- values$upperLeftLongitude + values$right
            rangex <- rangex + c(0, zoom)
          } else {
            rangex <- rangex + c( - zoom / 2, zoom / 2)
          }
        } else{
          dataPac <- model$data
          dataPac$Longitude[model$data$Longitude < -20] <- dataPac$Longitude[model$data$Longitude < -20] + 200
          dataPac$Longitude[model$data$Longitude >= -20] <- (- 160 + dataPac$Longitude[model$data$Longitude >= -20])
          rangex <- - diff(range(dataPac$Longitude, na.rm = TRUE)) / 2 +
            max(dataPac$Longitude, na.rm = TRUE) + values$right
          if(!is.na(values$upperLeftLongitude) & values$set > 0){
            rangex <- values$upperLeftLongitude + values$right
            if(rangex < -20) rangex <- rangex + 200
            if(rangex >= -20) rangex <- rangex - 160
            rangex <- rangex + c(0, zoom)
          } else {
            rangex <- rangex + c( - zoom / 2, zoom / 2)
          }
          }
        if(rangex[2] > 180){
          rangex <- c(180 - zoom, 180)
        }
        if(rangex[1] < -180){
          rangex <- c(-180, -180 + zoom)
        }
        if(rangey[2] > 90){
          coordDiff <- rangey[2] - 90
          rangey <- pmin(90, pmax(-90, rangey - coordDiff))
        }
        if(rangey[1] < -90){
          coordDiff <- rangey[1] + 90
          rangey <- pmin(90, pmax(-90, rangey - coordDiff))
        }
        values$rangex <- rangex
        values$rangey <- rangey

      }
      if(input$smoothCols){
        values$ncol <- 200
      } else {
        if(input$fixCol == FALSE){
          values$ncol <- input$ncol
        }
      }

      rangez = c(input$rangezMin, input$rangezMax)
      if(input$limitz == "0-1"){
        rangez <- pmax(0, pmin(1, rangez))
        if(rangez[1] == rangez[2]){
          rangez <- c(0,1)
        }
        updateNumericInput(session, "rangezMin", value = min(rangez))
        updateNumericInput(session, "rangezMax", value = max(rangez))
      }
      if(input$limitz == "0-100"){
        rangez <- pmax(0, pmin(100, rangez))
        if(rangez[1] == rangez[2]){
          rangez <- c(0,100)
        }
        updateNumericInput(session, "rangezMin", value = min(rangez))
        updateNumericInput(session, "rangezMax", value = max(rangez))
      }

      textLabels <- NULL
      if(input$textLabels & !is.null(input$textLabelsVar) & input$textLabelsVar != ""){
        textLabels <- (data())[, input$textLabelsVar, drop = FALSE]
        if(nrow(textLabels) == 0){
          textLabels = NULL
        }
      }

      pointLabels <- NULL
      if(input$pointLabels & !is.null(input$pointLabelsVar) & input$pointLabelsVar != ""){
        pointLabels <- (data())[, input$pointLabelsVar, drop = FALSE]
        if(nrow(pointLabels) == 0){
          pointLabels = NULL
        }
      }
      pointColLabels <- NULL
      if(input$colFix == "variable" & !is.null(input$pointLabelsVarCol) & input$pointLabelsVarCol != ""){
        pointColLabels <- (data())[, input$pointLabelsVarCol, drop = FALSE]
        if(nrow(pointColLabels) == 0){
          pointColLabels = NULL
        }
      }

      if(input$mapType == "Time course"){
        plotTimeCourse(model,
                       trange = input$trange,
                       independent = isolate(Independent()),
                       resolution = input$resolution,
                       centerX = input$centerX,
                       centerY = input$centerY,
                       Radius = input$Radius,
                       rangey = rangez,
                       seType = input$intervalType,
                       pointDat = pointDat,
                       pointsTime = input$pointsTime,
                       returnPred = returnPred,
                       rangePointsTime = input$rangePointsTime,
                       intTime = input$intTime,
                       limitz = input$limitz,
                       ...)
      } else {
        plotMap3D(
          model,
          time = time,
          estType = input$estType,
          estQuantile = input$Quantile,
          points = input$points,
          pointSize = input$pointSize,
          StdErr = input$StdErr,
          rangex = values$rangex,
          rangey = values$rangey,
          rangez = rangez,
          limitz = input$limitz,
          mask = input$mask,
          maskRadius = input$maskRadius,
          addU = input$AddU,
          showModel = input$showModel,
          pColor = input$pointCol,
          pointShape = as.numeric(input$pointShape),
          textLabels = textLabels,
          pointLabels = pointLabels,
          pointColLabels = pointColLabels,
          colorsP = input$colorsP,
          fontSize = input$fontSize,
          fontType = input$fontType,
          fontCol = input$fontCol,
          centerMap = input$Centering,
          resolution = input$resolution,
          interior = as.numeric(input$interior),
          ncol = values$ncol,
          centerX = input$centerX,
          centerY = input$centerY,
          Radius = input$Radius,
          terrestrial = input$terrestrial,
          colors = input$Colours,
          reverseColors = input$reverseCols,
          arrow = input$arrow,
          grid = input$grid,
          scale = input$scale,
          titleMain = !input$titleMain,
          titleScale = !input$titleScale,
          showScale = input$showScale,
          setAxisLabels = input$setAxisLabels,
          mainLabel = input$mainLabel,
          yLabel =  input$yLabel,
          xLabel =  input$xLabel,
          scLabel =  input$scLabel,
          northSize = input$northSize,
          scalSize = input$scalSize,
          scaleX = input$scaleX,
          scaleY = input$scaleY,
          NorthX = input$NorthX,
          NorthY = input$NorthY,
          AxisSize = input$AxisSize,
          AxisLSize = input$AxisLSize,
          pointDat = pointDatOK,
          ...
        )
      }
    }
  })

  output$DistMap <- renderPlot({
    validate(validInput(Model()))
    res <- plotFun()(Model())
    values$predictions <- res$XPred
    values$meanCenter <- res$meanCenter
    values$sdCenter <- res$sdCenter
    values$plot <- recordPlot()
  })

  values <- reactiveValues(plot = NULL, predictions = NULL,
                           set = 0,
                           upperLeftLongitude = NA,
                           upperLeftLatitude = NA,
                           zoom = 50)

  observe({
    numVars <- unlist(lapply(names(data()), function(x){
      if (
        (is.integer(data()[[x]]) | is.numeric(data()[[x]]) | sum(!is.na(as.numeric((data()[[x]])))) > 2) #&
        #!(x %in% c("Latitude", "Longitude"))
      )
        x
      else
        NULL
    }))

    timeVars <- unlist(lapply(names(data()), function(x){
      if (grepl("date", x, ignore.case = TRUE)
      )
        x
      else
        NULL
    }))
    selectedTextLabel <- NULL

    selectedLongitude <- NULL
    if (input$dataSource == "db" & ("longitude" %in% names(data()))){
      selectedLongitude <- "longitude"
    }
    selectedLatitude <- NULL
    if (input$dataSource == "db" & ("latitude" %in% names(data()))){
      selectedLatitude <- "latitude"
    }
    selectedSite <- NULL
    if (input$dataSource == "db" & ("site" %in% names(data()))){
      selectedSite <- "site"
    }
    updateSelectInput(session, "Independent", choices = c("", setdiff(numVars, timeVars)))
    updateSelectInput(session, "IndependentUnc", choices = c("", setdiff(numVars, timeVars)))

    updateSelectInput(session, "Longitude", choices = c("", names(data())),
                      selected = selectedLongitude)
    updateSelectInput(session, "Latitude", choices = c("", names(data())),
                      selected = selectedLatitude)
    updateSelectInput(session, "Site", choices = c("", names(data())),
                      selected = selectedSite)
    updateSelectInput(session, "textLabelsVar", choices = c("", names(data())),
                      selected = selectedTextLabel)
    updateSelectInput(session, "pointLabelsVar", choices = c("", names(data())),
                      selected = selectedTextLabel)
    updateSelectInput(session, "pointLabelsVarCol", choices = c("", names(data())),
                      selected = selectedTextLabel)

    # if (input$dataSource == "db"){
    #   updateSelectInput(session, "DateOne", choices = c("", timeVars))
    #   updateSelectInput(session, "DateTwo", choices = c("", timeVars))
    # } else {
      updateSelectInput(session, "DateOne", choices = c("", numVars))
      updateSelectInput(session, "DateTwo", choices = c("", numVars))
    #}
  })

  observe({
    if(input$DateType == "Interval"){
      updateRadioButtons(session, "dateUnc", choices = c("normal (sd = 1/4 width)" = "normal",
                                                         "normal (sd = 1/2 width)" = "normal2",
                                                         "uniform (full width)" = "uniform",
                                                         "mid point" = "point"))
    }
    if(input$DateType == "Mean + 1 SD uncertainty"){
      updateRadioButtons(session, "dateUnc", choices = c("uniform (2xSD input)" = "uniform",
                                                         "uniform (1xSD input)" = "uniform2",
                                                         "mean point" = "point",
                                                         "normal (sd = 1/4 width)" = "normal"))
    }
  })


  ## Import Data
  fileImport <- reactive({
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)

    decseparator = input$decseparator
    if (decseparator == "" & input$colseparator == ";") decseparator <- ","
    if (decseparator == "" & input$colseparator == ",") decseparator <- "."

    data <- readFile(inFile$datapath, input$fileType, input$colseparator,
                     decseparator)

    valid <- validateImport(data, showModal = TRUE)

    if (!valid){
      reset("file")
      NULL
    }
    else data
  })
  dataFun <- reactive({
    req(Model())
    function() {
      outlier <- Model()$outlier
      outlierDR <- Model()$outlierDR
      allData <- data()
      modelData <- Model()$data
      filtered <- which(rownames(allData) %in% rownames(modelData))
      allData <- allData[which(rownames(allData) %in% unique(c(filtered, outlier))), ]
      allData$Outlier <- "non-outlier"
      allData$Outlier[which(rownames(allData) %in% outlier)] <- "model outlier"
      allData$Outlier[which(rownames(allData) %in% outlierDR)] <- "data outlier"
      return(allData)
    }
  })

  dataTimeCourse <- reactive({
    req(Model())
    function(){
      tData <- plotFun()(Model())
      if(!is.null(tData)){
        allData <- data()
        tData <- allData[which(rownames(allData) %in% rownames(tData)), ]
        return(tData)
      } else {
        return(data.frame())
      }
    }
  })

  dataTimeCoursePred <- reactive({
    req(Model())
    function(){
      tData <- plotFun()(Model(), returnPred = TRUE)
      if(!is.null(tData)){
        return(tData)
      } else {
        return(data.frame())
      }
    }
  })

  ### Add Points

  pointDat <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0),
      x = numeric(0),
      xmin = numeric(0),
      xmax = numeric(0),
      group = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(Model(), ignoreNULL = FALSE, {
    pointDat(data.frame(
      index = numeric(0),
      y = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0),
      x = numeric(0),
      xmin = numeric(0),
      xmax = numeric(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })
  addRow <- function(df) {
    rbind(df, data.frame(index = nrow(df) + 1, y = NA,
                         ymin = NA, ymax = NA, x = NA,
                         xmin = NA, xmax = NA,
                         pointColor = "black", pointSize = 1,
                         pointAlpha = 0.5, stringsAsFactors = FALSE))
  }

  rmRow <- function(df) {
    if (nrow(df) > 0) df[- nrow(df), , drop = FALSE]
    else df
  }

  observeEvent(input$add_btn, {
    df <- pointDat()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("yT", index, sep = "_")]]
      yminval <- input[[paste("yminT", index, sep = "_")]]
      ymaxval <- input[[paste("ymaxT", index, sep = "_")]]
      xminval <- input[[paste("xminT", index, sep = "_")]]
      xmaxval <- input[[paste("xmaxT", index, sep = "_")]]
      xval <- input[[paste("xT", index, sep = "_")]]
      pointColor <- input[[paste("pointColorT", index, sep = "_")]]
      pointSize <- input[[paste("pointSizeT", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlphaT", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "ymin"] <<- if (is.null(yminval)) NA else yminval
      df[index, "ymax"] <<- if (is.null(ymaxval)) NA else ymaxval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "xmin"] <<- if (is.null(xminval)) NA else xminval
      df[index, "xmax"] <<- if (is.null(xmaxval)) NA else xmaxval
    })
      pointDat(df)
    pointDat(addRow(pointDat()))
  })

  observeEvent(input$rm_btn, {
    pointDat(rmRow(pointDat()))
  })


  inputGroup <- reactive({
    createPointInputGroup(pointDat(), ns = session$ns)
  })

  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)


  output$pointInput <- renderUI(inputGroup())
  output$n <- reactive(nrow(pointDat()))
  outputOptions(output, "n", suspendWhenHidden = FALSE)

  ###
  callModule(modelDiagnostics, "modelDiag", model = Model)
  callModule(dataExport, "exportData", data = dataFun, filename = "modelData")
  callModule(dataExport, "exportDataTimeCourse", data = dataTimeCourse, filename = "timeCourseData")
  callModule(dataExport, "exportDataTimeCoursePred", data = dataTimeCoursePred, filename = "timeCourseDataPred")

  callModule(plotExport, "export", reactive(values$plot), "spatio-temporal-average",
             predictions = reactive(values$predictions),
             plotFun = plotFun,
             Model = Model
             )
  callModule(batchPointEstimates, "batch", plotFun, time = TRUE, fruitsData = fruitsData, Model = Model)

  modelParams <- reactive({
    params <- reactiveValuesToList(input)
    params$coordType <- coordType()
    params
  })
  batchModel <- callModule(batchModeling, "batchModeling", data = data, plotFun = plotFun,
             modelParams = modelParams, type = "temporalAvg",
             savedMaps = savedMaps, estimateWrapper = estimateMap3DWrapper)

  observeEvent(batchModel(), {
    Model(batchModel())
  })
}


# Slider And Input Selection Module ----

#' Slider And Input UI
#'
#' UI of the Slider And Input module
#'
#' @param id id of module
#' @param label label
#' @param min (numeric) minumum
#' @param max (numeric) maximum
#' @param value (numeric) default value
#' @param step (numeric) step
sliderAndNumericInputUI <- function(id, label, min, max, value, step) {
  ns <- NS(id)
  tagList(
    div(
      style = "display:flex;",
      div(
        class = "zoom-map",
        sliderInput(inputId = ns("sliderInput"),
                    label = label,
                    min = min, max = max, value = value, step = step, width = "100%")
      ),
      div(
        class = "move-map",
        numericInput(inputId = ns("numInput"),
                     label = label,
                     min = min, max = max, value = value, step = step, width = "190px")
      ))
  )
}

#' Slider And Input Server
#'
#' Server function of the Slider And Input module
#' @param id id of module
#' @param value value of input
#' @param min min of input
#' @param max max of input
#' @param step step of input
sliderAndNumericInputServer <- function(id,
                                        value,
                                        min,
                                        max,
                                        step) {
  moduleServer(id,
               function(input, output, session) {
                 result <- reactiveVal(5000)

                 observeEvent(list(value(), min(), max(), step()), {
                   updateNumericInput(session = session, "sliderInput", value = value(),
                                      min = min(), max = max(), step = step())
                   updateNumericInput(session = session, "numInput", value = value(),
                                      min = min(), max = max(), step = step())
                 })

                 observeEvent(input$sliderInput, {
                   req(input$sliderInput != input$numInput)
                   updateNumericInput(session = session, "numInput", value = input$sliderInput)
                   result(input$sliderInput)
                 })

                 observeEvent(input$numInput, {
                   req(input$sliderInput != input$numInput)
                   updateSliderInput(session = session, "sliderInput", value = input$numInput)
                   result(input$numInput)
                 })

                 result
               })
}

