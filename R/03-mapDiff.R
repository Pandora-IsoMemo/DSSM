
#' ui function of modelResultsDiffSim module (OperateR)
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResultsDiffUI <- function(id, title = ""){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      # left sidebar ----
      sidebarPanel(
        width = 2,
        style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
        importUI(ns("modelUpload"), label = "Import Map"),
        downloadDSSMModelUI(ns = ns),
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = c("Create new map from existing" = "create",
                                "Create new map from scratch" = "createN",
                                "Load OperatoR map" = "model",
                                "Geotiff file" = "geotiff"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
        selectInput(ns("savedModel"),
                    "Select Map",
                    choices = c(""),
                    selected = ""),
        actionButton(ns("load"), "load"),
        ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'create'",
        selectInput(inputId = ns("targetMap1"),
                    label = "Map1:",
                    choices = NULL),
        selectInput(inputId = ns("targetMap2"),
                    label = "Map2:",
                    choices = NULL),
        selectInput(inputId = ns("operation"),
                    label = "Operation",
                    choices = c("Difference" = "-", "Sum" = "+",
                                "Multiply" = "*", "Ratio" = "/",
                                "Mean" = "mean", "Min" = "pMin",
                                "Max" = "pMax",
                                "Weight" = "weight",
                                "Weighted mean" = "weightedMean")),
        actionButton( ns("createDiffMap"), "Run"),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          checkboxInput(inputId = ns("fixCol"),
                        label = "Fix colours and ranges ",
                        value = FALSE, width = "100%"),
          ns = ns
        ),
        ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'createN'",
          #textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
          radioButtons(
            ns("userMapType"),
            "Map Type",
            choices = c(
              "all" = "1",
              "region - circle" = "2",
              "region - rectangle" = "3",
              "elevation" = "4",
              "custom" = "5"
            ),
            selected = 1,
            inline = FALSE,
            width = "100%"
          ),
          conditionalPanel(
            condition = "input.userMapType == '2' ||  input.userMapType == '3'",
          radioButtons(ns("customCircles"), "type", c("single", "multiple")),
          ns = ns
          ),
          conditionalPanel(
            condition = "(input.customCircles == 'single' || input.userMapType == '1') && input.userMapType != '4'&& input.userMapType != '5'",
            numericInput(ns("meanMap"), "Mean of map", value = 0),
            numericInput(ns("sdMap"), "Sd of map", value = 0, min = 0),
            ns = ns),
          conditionalPanel(
            condition = "input.userMapType == '2'",
            conditionalPanel(
              condition = "input.customCircles == 'single'",
            numericInput(
              inputId = ns("userRadius"),
              label = "Radius in km",
              min = 1,
              max = 10000,
              value = c(3000),
              width = "100%",
              step = 100
            ),
            numericInputLatAndLongUI(
              ns("centerCoords"),
              label = "Center",
              valueLat = 50,
              valueLong = 10
            ),
            ns = ns
            ),
            ns = ns
          ),

          conditionalPanel(
            condition = "input.userMapType == '4'",
              numericInput(
                inputId = ns("elevRes"),
                label = "Resolution (px)",
                min = 50,
                max = 2000,
                value = c(100),
                width = "100%",
                step = 50
              ),
              numericInputLatAndLongUI(
                ns("upperBoundElev"),
                label = "Upper Left",
                valueLat = 65,
                valueLong = -20
              ),
            numericInputLatAndLongUI(
              ns("lowerBoundElev"),
              label = "Lower Right",
              valueLat = 25,
              valueLong = 80
            ),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.userMapType == '5'",
            importDataUI(ns("importData"), "Import Data"),
            selectInput(inputId = ns("LongitudeC"),
                        label = "Longitude variable:",
                        choices = c("Longitude")),
            selectInput(inputId = ns("LatitudeC"),
                        label = "Latitude variable:",
                        choices = c("Latitude")),
            selectInput(inputId = ns("MeanC"),
                        label = "Value variable:",
                        choices = c("Mean")),
            selectInput(inputId = ns("SdC"),
                        label = "SD variable (optional):",
                        choices = c("SD")),
            ns = ns
          ),


          conditionalPanel(
            condition = "input.userMapType == '3'",
            conditionalPanel(
              condition = "input.customCircles == 'single'",
            numericInputLatAndLongUI(
              ns("upperLeftCoords"),
              label = "Upper Left",
              valueLat = 65,
              valueLong = -20
            ),
            numericInputLatAndLongUI(
              ns("lowerRightCoords"),
              label = "Lower Right",
              valueLat = 25,
              valueLong = 80
            ),
            ns = ns
            ),
            ns = ns
          ),
          conditionalPanel(
          condition = "(input.userMapType == '2' ||  input.userMapType == '3') && (input.customCircles == 'multiple')",
          importDataUI(ns("importData"), "Import Data"),
          ns = ns
          ),
          conditionalPanel(
            condition = "input.customCircles == 'multiple' && input.userMapType == '2'",
          selectInput(inputId = ns("LongitudeO"),
                      label = "Longitude variable:",
                      choices = c("Longitude")),
          selectInput(inputId = ns("LatitudeO"),
                      label = "Latitude variable:",
                      choices = c("Latitude")),
          selectInput(inputId = ns("MeanO"),
                      label = "Mean variable:",
                      choices = c("Mean")),
          selectInput(inputId = ns("SdO"),
                      label = "SD variable:",
                      choices = c("SD")),
          selectInput(inputId = ns("RadiusO"),
                      label = "Radius variable:",
                      choices = c("Radius")),
          ns = ns
          ),
          conditionalPanel(
            condition = "input.customCircles == 'multiple' && input.userMapType == '3'",
            selectInput(inputId = ns("LongitudeU"),
                        label = "Upper Left Longitude variable:",
                        choices = c("Longitude")),
            selectInput(inputId = ns("LatitudeU"),
                        label = "Upper Left Latitude variable:",
                        choices = c("Latitude")),
            selectInput(inputId = ns("LongitudeL"),
                        label = "Lower Left Longitude variable:",
                        choices = c("Longitude")),
            selectInput(inputId = ns("LatitudeL"),
                        label = "Lower Left Latitude variable:",
                        choices = c("Latitude")),
            selectInput(inputId = ns("MeanR"),
                        label = "Mean variable:",
                        choices = c("Mean")),
            selectInput(inputId = ns("SdR"),
                        label = "SD variable:",
                        choices = c("SD")),
            ns = ns
          ),

          actionButton( ns("createNewMap"), "Run"),
          ns = ns
        )
      ),
      # main panel ----
      mainPanel(
        width = 8,
        div(class = "aspect-16-9", div(
          plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
        )),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          htmlOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
          tags$br(),
          tags$br(),
          fluidRow(column(width = 3,
                          div(
                            class = "move-map",
                            uiOutput(ns("move"))
                          )
          ),
          column(width = 3,
                 offset = 6,
                 align = "right",
                 plotExportButton(ns("export"))
          )),
          tags$hr(),
          tags$h4("Map Section"),
          mapSectionUI(ns("mapSection")),
          fluidRow(
            column(
              width = 3,
              offset = 9,
              style = "margin-top: -60px;",
              align = "right",
              actionButton(ns("set"), "Set Map Section")
            )
          ),
          tags$hr(),
          div(
            div(
              style = 'display:inline-block',
              class = "save-plot-container",
              textInput(ns("saveMapNameMain"), NULL, placeholder = "Name for Map"),
              actionButton(ns("saveMap"), "Save map")
            ),
          ),
          actionButton(ns("add_btn2D"), "Add data point"),
          actionButton(ns("rm_btn2D"), "Remove data point"),
          actionButton(ns("ok"), "Ok"),
          uiOutput(ns("pointInput2D"))
        )
      ),
      # right sidebar ----
        sidebarPanel(
          width = 2,
          style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
          radioButtons(inputId = ns("Centering"),
                       label = "Map Centering",
                       choices = c("0th meridian" = "Europe", "180th meridian" = "Pacific")),
          zScaleUI(ns("zScale")),
          selectInput(ns("contourType"),
                      label = "Contour type",
                      choices = c("Filled counter" = "filled.contour",
                                  "Simple counter" = "contour")),
          radioButtons(inputId = ns("terrestrial"), label = "", inline = TRUE,
                       choices = list("Terrestrial " = 1, "All" = 3, "Aquatic" = -1),
                       selected = 1),
          checkboxInput(inputId = ns("grid"),
                        label = "Show map grid",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("showBorders"),
                        label = "Show country borders",
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
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("ncol"),
                      label = "Approximate number of colour levels",
                      min = 4, max = 50, value = 20, step = 2, width = "100%"),
          sliderInput(inputId = ns("AxisSize"),
                      label = "Axis title font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          sliderInput(inputId = ns("AxisLSize"),
                      label = "Axis label font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          centerEstimateUI(ns("centerEstimateParams")),
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
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#'
#' @export
mapDiff <- function(input, output, session, savedMaps, fruitsData){
  data <- reactiveVal()
  fileImport <- reactiveVal(NULL)
  MapDiff <- reactiveVal(NULL)

  values <- reactiveValues(plot = NULL,
                           predictions = NULL,
                           set = 0,
                           upperLeftLongitude = NA,
                           upperLeftLatitude = NA,
                           zoom = 50)

  observeSavedMaps(input, output, session, savedMaps, type = c("difference", "user"))

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), c("localAvg", "temporalAvg", "spread", "difference",
                                            "similarity", "kernel2d", "kernel3d", "user"))

    updateSelectInput(session, "targetMap1", choices = choices)
    updateSelectInput(session, "targetMap2", choices = choices)
  })

  observeEvent(input$saveMap, {
    mapName <- trimws(input$saveMapNameMain)
    if (mapName == "") {
      alert("Please provide a map name")
      return()
    }

    map <- createSavedMap(
      model = MapDiff(),
      predictions = values$predictions,
      plot = values$plot,
      plotFUN = plotFun(),
      type = "difference",
      name = mapName
    )

    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapNameMain", value = "")
  })

  # MODEL DOWN- / UPLOAD ----

  uploadedNotes <- reactiveVal(NULL)
  subFolder <- "OperatoR"

  downloadDSSMModel(input, output, session,
                   dat = savedMaps,
                   model = MapDiff(),
                   #savedMaps = savedMaps(),
                   subFolder = subFolder,
                   tabId = "difference",
                   uploadedNotes = uploadedNotes)

  uploadedValues <- importServer("modelUpload",
                                     title = "Import Model",
                                     importType = "model",
                                     ckanFileTypes = config()[["ckanModelTypes"]],
                                     subFolder = subFolder,
                                     ignoreWarnings = TRUE,
                                     defaultSource = config()[["defaultSourceModel"]],
                                     fileExtension = config()[["fileExtension"]],
                                     options = importOptions(rPackageName = config()[["rPackageName"]]))

  observe(priority = 100, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["data"]]))

    # reset model
    MapDiff(NULL)
    fileImport(uploadedValues()[[1]][["data"]])
    savedMaps(uploadedValues()[[1]][["data"]])

    # update notes in tab "Estimates" model download ----
    uploadedNotes(uploadedValues()[[1]][["notes"]])
  }) %>%
    bindEvent(uploadedValues())

  observe(priority = 50, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
    uploadedInputs <- uploadedValues()[[1]][["inputs"]]

    ## update inputs ----
    inputIDs <- names(uploadedInputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedInputs[[inputIDs[i]]]) )
    }
  }) %>%
    bindEvent(uploadedValues())

  observe(priority = 10, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["model"]]))
    ## update model ----
    MapDiff(unpackModel(uploadedValues()[[1]][["model"]]))

    uploadedSavedMaps <- unpackSavedMaps(uploadedValues()[[1]][["model"]], currentSavedMaps = savedMaps())
    savedMaps(c(savedMaps(), uploadedSavedMaps))
  }) %>%
    bindEvent(uploadedValues())

  observe({
    updateSelectInput(session, "LongitudeO", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LatitudeO", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LongitudeL", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LatitudeL", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LongitudeU", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LatitudeU", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LongitudeC", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "LatitudeC", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "MeanO", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "SdO", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "MeanR", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "SdR", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "MeanC", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "SdC", choices = c("", names(fileImport())),
                      selected = NULL)
    updateSelectInput(session, "RadiusO", choices = c("", names(fileImport())),
                      selected = NULL)
  }) %>% bindEvent(fileImport())

  # RUN MODEL ----
  observeEvent(input$createDiffMap, {
    if (!is.null(input$targetMap1) & !is.null(input$targetMap2)) {
      withProgress({
        model <- createDifferenceMap(savedMaps()[[as.numeric(input$targetMap1)]]$predictions,
                                          savedMaps()[[as.numeric(input$targetMap2)]]$predictions,
                                          operation = input$operation) %>%
          shinyTryCatch()
        MapDiff(model)
        },
        value = 0,
        message = "Generating map"
      )
    }
  })

  circleCenter <- numericInputLatAndLongServer("centerCoords")
  rectangleUpperLeft <-
    numericInputLatAndLongServer("upperLeftCoords",
                                 valueLat = reactive(65),
                                 valueLong = reactive(-20))
  rectangleLowerRight <-
    numericInputLatAndLongServer("lowerRightCoords",
                                 valueLat = reactive(25),
                                 valueLong = reactive(80))

  #elevation boundary box
  ElevUpperLeft <-
    numericInputLatAndLongServer("upperBoundElev",
                                 valueLat = reactive(65),
                                 valueLong = reactive(-20))
  ElevLowerRight <-
    numericInputLatAndLongServer("lowerBoundElev",
                                 valueLat = reactive(25),
                                 valueLong = reactive(80))

  observeEvent(input$createNewMap, {
    # mapName <- trimws(input$saveMapName)
    # if (mapName == "") {
    #   alert("Please provide a map name")
    #   return()
    # }
    if (input$userMapType == "1") {
      XPred <- as.numeric(c(input$meanMap, input$sdMap))
    }
    if (input$userMapType == "4") {
      if (ElevLowerRight$longitude() <= ElevUpperLeft$longitude()) {
        alert("Upper left longitude must be smaller than upper right longitude")
      }
      if (ElevLowerRight$latitude() >= ElevUpperLeft$latitude()) {
        alert("Upper left latitude must be larger than upper right longitude")
      }

      Longitude <- seq(ElevLowerRight$longitude(), ElevUpperLeft$longitude(), length.out = input$elevRes)
      Latitude <- seq(ElevUpperLeft$latitude(), ElevLowerRight$latitude(), length.out = input$elevRes)
      pred_data <- data.frame(expand.grid(Longitude, Latitude))
      colnames(pred_data) <- c("x", "y")

      withProgress({
        elev <- get_elev_point(pred_data,prj = "+proj=longlat +datum=WGS84",
                               src = "aws")$elevation
      },
      value = 0.5,
      message = "Getting elevation data",
      detail = "This may take a while..."
      )
      XPred <- data.frame(Est = elev, Sd = 0, Longitude = pred_data$x, Latitude = pred_data$y)
    }

    if (input$userMapType == "5") {
    columnsC <- c(input$MeanC, input$LongitudeC, input$LatitudeC)
    if (any(is.null(columnsC)) ||
        any(columnsC == "")) {
      alert("Import is not valid.")
      return()
    } else {
      dat <- fileImport()
      if (all(columnsC %in% colnames(dat)) && all(!is.null(colnames(dat)))) {
        if (input$SdC %in% colnames(dat)) {
          XPred <- data.frame(
            Est = dat[, input$MeanC],
            Sd = dat[, input$SdC],
            Longitude = dat[, input$LongitudeC],
            Latitude = dat[, input$LatitudeC]
          )
        } else {
          XPred <- data.frame(
            Est = dat[, input$MeanC],
            Sd = 0,
            longitude = dat[, input$LongitudeC],
            latitude = dat[, input$LatitudeC]
          )
        }
    }
    }
    if (!all(table(XPred$Longitude, XPred$Latitude) == 1)) {
      alert("For each longitude entry all unique latitude entries must be supplied and vice versa")
      return()
    }
    }
    if (input$userMapType == "2") {
      if (input$customCircles == "multiple") {
        columnsC <- c(input$MeanO, input$SdO, input$LongitudeO, input$LatitudeO, input$RadiusO)
        if (any(is.null(columnsC)) ||
            any(columnsC == "")) {
          alert("Import is not valid.")
          return()
        } else {
          dat <- fileImport()
          if (all(columnsC %in% colnames(dat)) && all(!is.null(colnames(dat)))) {
            XPred <- data.frame(
              Est = dat[, input$MeanO],
              Sd = dat[, input$SdO],
              longitude = dat[, input$LongitudeO],
              latitude = dat[, input$LatitudeO],
              radius = dat[,input$RadiusO]
            )
          } else {
            alert("Not all variables defined")
            return()
          }

          if (nrow(XPred) > nrow(na.omit(XPred))) {
            alert("data contains missing values")
            return()
          }
          withProgress({
            coord <- getFullCoordGrid(gridLength = max(XPred$radius) / 10000)
          },
          value = 80,
          message = "Generating grid ..."
          )
          coord <- lapply(1:nrow(XPred), function(x) coord %>%
            filterCoordCircle(
              lat = XPred$latitude[x],
              long = XPred$longitude[x],
              radius = XPred$radius[x] / 111
            ))
          if (!is.null(coord) && !is.null(XPred) && nrow(XPred) != 0) {
            XPred <- do.call("rbind", lapply(1:length(coord), function(x) data.frame(
              Est = XPred$Est[x],
              Sd = XPred$Sd[x],
              Longitude = coord[[x]][, 1],
              Latitude = coord[[x]][, 2]
            ))) %>% group_by(Longitude, Latitude) %>%
              summarise(Est = mean(.data$Est), Sd = mean(.data$Sd)) %>% ungroup() %>% as.data.frame()
          } else {
            alert("No relevant data")
            return()
          }

        }
      } else {
        withProgress({
          coord <- getFullCoordGrid(gridLength = input$userRadius / 10000)
        },
        value = 80,
        message = "Generating grid ..."
        )

        coord <- coord %>%
          filterCoordCircle(
            lat = circleCenter$latitude(),
            long = circleCenter$longitude(),
            radius = input$userRadius / 111
          )
        if (!is.null(coord) || !is.null(XPred) || nrow(XPred) == 0) {
            XPred <- data.frame(
              Est = input$meanMap,
              Sd = input$sdMap,
              Longitude = coord[, 1],
              Latitude = coord[, 2]
            )
          } else {
            XPred <- NULL
          }
      }
      if (!is.null(XPred)) {
        expanded <- expand.grid(unique(XPred$Longitude), unique(XPred$Latitude))
        colnames(expanded) <- c("Longitude", "Latitude")
        XPred <- left_join(expanded, XPred, by = c("Longitude", "Latitude")) %>% arrange(Latitude, Longitude)
      }
      }
    if (input$userMapType == "3") {
      if (input$customCircles == "multiple") {
        columnsR <- c(input$MeanR, input$SdR, input$LongitudeL, input$LatitudeL, input$LongitudeU, input$LatitudeU)

        if (any(is.null(columnsR)) ||
            any(columnsR == "")) {
          alert("Import is not valid.")
          return()
        } else {
          dat <- fileImport()
          if (all(columnsR %in% colnames(dat)) && all(!is.null(colnames(dat)))) {
            XPred <- data.frame(
            Est = dat[, input$MeanR],
            Sd = dat[, input$SdR],
            upperLeftLong = dat[, input$LongitudeU],
            upperLeftLat = dat[, input$LatitudeU],
            lowerRightLong = dat[,input$LongitudeL],
            lowerRightLat = dat[,input$LatitudeL]
          )} else {
            alert("Not all variables defined")
            return()
          }

          center <- lapply(1:nrow(XPred), function(x) getCoordCenter(
            upperLeftLat = XPred[x,"upperLeftLat"],
            upperLeftLong = XPred[x,"upperLeftLong"],
            lowerRightLat = XPred[x,"lowerRightLat"],
            lowerRightLong = XPred[x,"lowerRightLong"]
          ))

          latLength <-
            lapply(1:nrow(XPred), function(x) abs(diff(
              c(XPred[x,"lowerRightLat"],
                XPred[x,"upperLeftLat"]
              )
            )))
          longLength <-
            lapply(1:nrow(XPred),function(x) abs(diff(
              c(
                XPred[x,"lowerRightLong"],
                XPred[x,"upperLeftLong"]
              )
            )))
          if (nrow(XPred) > nrow(na.omit(XPred))) {
            alert("data contains missing values")
            return()
          }

          withProgress({
            coord <- getFullCoordGrid(gridLength = mean(c(
                max(unlist(latLength)), max(unlist(longLength))
              ) / 2) * 111 / 10000)
          },
          value = 80,
          message = "Generating grid ..."
          )
          coord <- lapply(1:nrow(XPred), function(x) coord %>%
             filterCoordRectangle(
              long = center[[x]][1],
              lat = center[[x]][2],
              latLength = latLength[[x]],
              longLength = longLength[[x]]
            ))
          if (!is.null(coord) && !is.null(XPred) && nrow(XPred) != 0) {
            XPred <- do.call("rbind", lapply(1:length(coord), function(x) data.frame(
              Est = XPred$Est[x],
              Sd = XPred$Sd[x],
              Longitude = coord[[x]][, 1],
              Latitude = coord[[x]][, 2]
            ))) %>% group_by(Longitude, Latitude) %>%
              summarise(Est = mean(.data$Est), Sd = mean(.data$Sd)) %>% ungroup() %>% as.data.frame()
          } else {
            alert("No relevant data")
            return()
          }
        }
      }
      else {
        center <- getCoordCenter(
          upperLeftLat = rectangleUpperLeft$latitude(),
          upperLeftLong = rectangleUpperLeft$longitude(),
          lowerRightLat = rectangleLowerRight$latitude(),
          lowerRightLong = rectangleLowerRight$longitude()
        )

        latLength <-
          abs(diff(
            c(
              rectangleLowerRight$latitude(),
              rectangleUpperLeft$latitude()
            )
          ))
        longLength <-
          abs(diff(
            c(
              rectangleLowerRight$longitude(),
              rectangleUpperLeft$longitude()
            )
          ))

        withProgress({
          coord <-
            getFullCoordGrid(gridLength = mean(c(
              latLength, longLength
            ) / 2) * 111 / 10000)
        },
        value = 80,
        message = "Generating grid ..."
        )

        coord <- coord %>%
          filterCoordRectangle(
            long = center[1],
            lat = center[2],
            latLength = latLength,
            longLength = longLength
          )

        if (!is.null(coord) || !is.null(XPred) || nrow(XPred) == 0) {
          XPred <- data.frame(
            Est = input$meanMap,
            Sd = input$sdMap,
            Longitude = coord[, 1],
            Latitude = coord[, 2]
          )
        } else {
          XPred <- NULL
        }
      }
      if (!is.null(XPred)) {
        expanded <- expand.grid(unique(XPred$Longitude), unique(XPred$Latitude))
        colnames(expanded) <- c("Longitude", "Latitude")
        XPred <- left_join(expanded, XPred, by = c("Longitude", "Latitude")) %>% arrange(Latitude, Longitude)
      }

    }
    XPred <- XPred[order(XPred$Latitude, XPred$Longitude),]
    req(XPred)
    MapDiff(XPred)
  })
  ####

  observeEvent(input$load, {
    MapDiff(savedMaps()[[as.numeric(input$savedModel)]]$model)
  })

  zoomFromModel <- reactiveVal(50)

  observe({
    validate(validInput(MapDiff()))
    if (input$fixCol == FALSE) {
      val <- signif(max(MapDiff()$Sd, na.rm = TRUE), 2)
      updateSliderInput(session, "StdErr", value = signif(val * 5, 2),
                        min = 0, max = signif(val * 5, 2),
                        step = signif(roundUpNice(val, nice = c(1,10)) / 1000, 1))

      newZoom <- extractZoomFromLongRange(
        rangeLongitude = range(MapDiff()$Longitude, na.rm = TRUE),
        mapCentering = input$Centering
      )

      isolate({
        zoomFromModel(newZoom)
        values$zoom <- newZoom
        values$up <- 0
        values$right <- 0
      })
    }
  })

  output$move <- renderUI({
    moveButtons(ns = session$ns)
  })

  mapSettings <- mapSectionServer("mapSection", zoomValue = zoomFromModel, mapCenter = reactive(input$Centering))

  observeEvent(input$up, {
    values$up <- values$up + values$zoom / 40
  })

  observeEvent(input$down, {
    values$up <- values$up - values$zoom / 40
  })

  observeEvent(input$left, {
    values$right <- values$right - values$zoom / 40
  })

  observeEvent(input$right, {
    values$right <- values$right + values$zoom / 40
  })

  observeEvent(input$Centering, {
    values$upperLeftLatitude <- NA
    values$upperLeftLongitude <- NA
    values$up <- 0
    values$right <- 0
  })

  observeEvent(input$set, {
    values$zoom <- mapSettings$zoom
    values$upperLeftLatitude <- mapSettings$upperLeftLatitude
    values$upperLeftLongitude <- mapSettings$upperLeftLongitude

    values$up <- 0
    values$right <- 0
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

  observeEvent(MapDiff(), ignoreNULL = FALSE, {
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
    if (nrow(df) > 0) df[-nrow(df), , drop = FALSE]
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

  zSettings <- zScaleServer("zScale",
                            Model = MapDiff,
                            fixCol = reactive(input$fixCol),
                            estimationTypeChoices = reactive(c(
                              "Mean",
                              "1 SE",
                              "2 SE",
                              "Quantile",
                              "Significance (p-value)",
                              "Significance (z-value)",
                              "Significance (Overlap)"
                            )),
                            restrictOption = reactive("hide"),
                            zValuesFun = getZValuesMapDiff,
                            zValuesFactor = 1)

  centerEstimate <- centerEstimateServer("centerEstimateParams",
                                         predictions = reactive(values$predictions))

  plotFun <-  reactive({
    validate(validInput(MapDiff()))
    pointDatOK = pointDatOK()

    if (input$fixCol == FALSE) {
      zoom <- values$zoom

      rangey <- MapDiff() %>%
        extractRangeFromData(column = "Latitude", move = values$up) %>%
        zoomLatitudeRange(zoom = zoom,
                          upperLeftLatitude = values$upperLeftLatitude,
                          move = values$up) %>%
        constrainLatitudeRange()

      rangex <- MapDiff() %>%
        shiftDataToCenter(centerMap = input$Centering) %>%
        extractRangeFromData(column = "Longitude", move = values$right) %>%
        zoomLongitudeRange(zoom = zoom,
                           upperLeftLongitude = values$upperLeftLongitude,
                           center = input$Centering,
                           move = values$right) %>%
          constrainLongitudeRange(zoom = zoom, center = input$Centering)

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

    req(zSettings$estType)

    # PLOT MAP ----
    function(...){
      plotDS(MapDiff(),
             type = "difference",
             independent = "",
             rangex = values$rangex,
             rangey = values$rangey,
             estType = zSettings$estType,
             estQuantile = zSettings$Quantile,
             rangez = zSettings$range,
             showModel = zSettings$showModel,
             contourType = input$contourType,
             colors = input$Colours,
             ncol = values$ncol,
             centerMap = input$Centering,
             reverseColors = input$reverseCols,
             terrestrial = input$terrestrial,
             grid = input$grid,
             showBorders = input$showBorders,
             arrow = input$arrow,
             scale = input$scale,
             titleMain = input$titleMain,
             titleScale = input$titleScale,
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
      ) %>%
        shinyTryCatch(errorTitle = "Plotting failed")
    }
  })

  output$DistMap <- renderPlot({
    validate(validInput(MapDiff()))
    withProgress({
      res <- plotFun()()
    }, min = 0, max = 1, value = 0.8, message = "Plotting map ...")
    values$predictions <- res$XPred
    values$plot <- recordPlot()
  })

  output$centerEstimate <- renderUI({
    centerEstimate$text()
  })

  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)

  callModule(plotExport, "export", reactive(values$plot), "similarity",
             reactive(values$predictions))
  callModule(batchPointEstimates, "batch", plotFun, fruitsData = fruitsData)


  ## Import Data ----
  importedDat <- importDataServer("importData")

  observe({
    # reset model
    if (length(importedDat()) == 0 ||  is.null(importedDat()[[1]])) fileImport(NULL)

    req(length(importedDat()) > 0, !is.null(importedDat()[[1]]))
    data <- importedDat()[[1]]
    valid <- validateImport(data, showModal = TRUE, minRows = 2)

    if (!valid) {
      showNotification("Import is not valid.")
      fileImport(NULL)
    } else {
      fileImport(data)
    }
  }) %>% bindEvent(importedDat())

}
