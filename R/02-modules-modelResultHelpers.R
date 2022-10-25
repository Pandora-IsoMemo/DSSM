# Collection of helper modules for the modelling tabs ----

## Formatting of decimal places ----

#' Center Estimate UI
#'
#' UI function of center estimate module
#'
#' @param id namespace
#' @param title title in tab
centerEstimateUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("centerY"),
      label = "Center point latitude",
      min = -180,
      max = 180,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    numericInput(
      inputId = ns("centerX"),
      label = "Center point longitude",
      min = -90,
      max = 90,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.centerY != null && input.centerY != '' && input.centerX != null && input.centerX != ''",
      numericInput(
        inputId = ns("decimalPlace"),
        label = "Decimal places for Mean/Error at the Center point",
        min = 0,
        max = 10,
        value = 2,
        step = 1,
        width = "100%"
      ),
      ns = ns
    ),
    sliderInput(
      inputId = ns("Radius"),
      label = "Radius (km)",
      min = 10,
      max = 300,
      value = 100,
      step = 10,
      width = "100%"
    )
  )
}


#' Center Estimate Server
#'
#' Backend for center estimate module
#'
#' @param id namespace id
#' @param meanCenter (reactive) mean for center
#' @param sdCenter (reactive) error for center
#' @param mapType (reactive) type of plot, either "Map", "Time course", "Time intervals by cluster",
#'  "Spread", "Speed", "Minima/Maxima"
centerEstimateServer <-
  function(id, meanCenter, sdCenter, mapType = reactiveVal("Map")) {
    moduleServer(id,
                 function(input, output, session) {
                   centerEstimateText <- reactiveVal("")

                   centerEstimateMaps <- c("Map", "Spread")
                   # check which maps have the text in the previous version:
                   # currently not included: "Time course", "Time intervals by cluster",
                   # "Speed" (a map but no meanCenter, sdCenter in the output available),
                   # "Minima/Maxima"

                   observeEvent(list(meanCenter(), sdCenter(), input$decimalPlace), {
                     if (is.na(input$centerY) |
                         is.na(input$centerX) |
                         is.na(input$Radius) |
                         !(mapType() %in% centerEstimateMaps)) {
                       centerEstimateText("")
                     }

                     req(
                       input$centerX,
                       input$centerY,
                       input$Radius,
                       (mapType() %in% centerEstimateMaps)
                     )

                     if (is.na(meanCenter()) | is.na(sdCenter())) {
                       centerEstimateText(
                         "Cannot compute mean and sd at your provided coordinates. Please raise the
                       plot resolution or radius such that estimates within the radius are available."
                       )
                     }

                     req(meanCenter(), sdCenter())
                     centerEstimateText(
                       paste0(
                         "Mean: ",
                         round(meanCenter(), digits = input$decimalPlace),
                         ", Standard error of the mean: ",
                         round(sdCenter(), digits = input$decimalPlace),
                         "  at coordinates ",
                         "(",
                         input$centerY,
                         "\u00B0, " ,
                         input$centerX,
                         "\u00B0) for a ",
                         round(input$Radius, 3),
                         " km radius"
                       )
                     )

                   })

                   list(
                     centerX = reactive(input$centerX),
                     centerY = reactive(input$centerY),
                     radius = reactive(input$Radius),
                     text = centerEstimateText
                   )
                 })
  }


#' Format Time Course UI
#'
#' UI function for formatting of the time course plot
#'
#' @param id namespace
#' @param title title in tab
formatTimeCourseUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("axesDecPlace"),
      label = "Decimal places for axes",
      min = 0,
      max = 10,
      value = 0,
      step = 1,
      width = "100%"
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsX"),
          label = "N labels of x axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsY"),
          label = "N labels of y axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      )
    )
  )
}


#' Format Time Course Server
#'
#' Backend for formatting of the time course plot
#'
#' @param id namespace id
formatTimeCourseServer <-
  function(id) {
    moduleServer(id,
                 function(input, output, session) {
                   reactive(
                     list(
                       axesDecPlace = input$axesDecPlace,
                       nLabelsX = input$nLabelsX,
                       nLabelsY = input$nLabelsY
                     )
                   )
                 })
  }


## Time and Map Section ----


#' Time And Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param label label
timeAndMapSectionUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    tags$hr(),
    tags$h4(label),
    sliderAndNumericInputUI(
      ns("timeExtended"),
      label = "Time",
      min = 0,
      max = 15000,
      value = 5000,
      step = 100
    ),
    mapSectionUI(ns("mapSection")),
    fluidRow(
      column(
        width = 4,
        offset = 8,
        style = "margin-top: -60px;",
        align = "right",
        actionButton(ns("set"), "Set Time and Map Section")
      )
    ),
    tags$hr(),
  )
}


#' Time And Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param dateMin (reactive) min date
#' @param dateMax (reactive) max date
#' @param dateValue (reactive) value date
#' @param dateStep (reactive) step date
#' @param zoomValue (reactive) default zoom given by model output
timeAndMapSectionServer <- function(id,
                                    dateMin,
                                    dateMax,
                                    dateValue,
                                    dateStep,
                                    zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 mapAndTimeSettings <- reactiveValues(
                   time = 5000,
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50,
                   set = 0
                 )

                 userInputTime <-
                   sliderAndNumericInputServer(
                     "timeExtended",
                     value = dateValue,
                     min = dateMin,
                     max = dateMax,
                     step = dateStep
                   )

                 mapSectionParams <-
                   mapSectionServer("mapSection", zoomValue = zoomValue)

                 # default values depend on model output
                 observeEvent(list(dateValue(),
                                   zoomValue()), {
                                     mapAndTimeSettings$time <- dateValue()
                                     mapAndTimeSettings$zoom <-
                                       zoomValue()
                                     mapAndTimeSettings$upperLeftLatitude <-
                                       mapSectionParams$upperLeftLatitude
                                     mapAndTimeSettings$upperLeftLongitude <-
                                       mapSectionParams$upperLeftLongitude
                                   })

                 # values given by the user pressing button
                 observeEvent(input$set, {
                   mapAndTimeSettings$time <- userInputTime()
                   mapAndTimeSettings$zoom <- mapSectionParams$zoom
                   mapAndTimeSettings$upperLeftLatitude <-
                     mapSectionParams$upperLeftLatitude
                   mapAndTimeSettings$upperLeftLongitude <-
                     mapSectionParams$upperLeftLongitude
                   mapAndTimeSettings$set <- input$set
                 })

                 return(mapAndTimeSettings)
               })
}


#' Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
mapSectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderAndNumericInputUI(
      ns("zoom"),
      label = "Zoom/x-Range in degrees Longitude",
      min = 0.1,
      max = 360,
      value = 50,
      step = 1
    ),
    fluidRow(
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLatitude"),
          label = "Latitude of upper left corner",
          min = -90,
          max = 90,
          value = c()
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLongitude"),
          label = "Longitude of upper left corner",
          min = -180,
          max = 180,
          value = c()
        )
      )
    )
  )
}


#' Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param zoomValue (reactive) default zoom given by model output
mapSectionServer <- function(id,
                             zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 mapSettings <- reactiveValues(
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50
                 )

                 zoomInput <- sliderAndNumericInputServer(
                   "zoom",
                   value = zoomValue,
                   min = reactive(0.1),
                   max = reactive(360),
                   step = reactive(1)
                 )

                 # update upperLeftLatitude/upperLeftLongitude if values$up/... change ----

                 observe({
                   mapSettings$zoom <- zoomInput()
                   mapSettings$upperLeftLatitude <-
                     input$upperLeftLatitude
                   mapSettings$upperLeftLongitude <-
                     input$upperLeftLongitude
                 })

                 return(mapSettings)
               })
}

## Z-scale settings ----

#' Z Scale UI
#'
#' UI of the module
#'
#' @param id id of module
zScaleUI <-
  function(id) {
    ns <- NS(id)
    tagList(
      selectInput(
        inputId = ns("estType"),
        label = "Estimation type",
        choices = NULL,
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.estType == 'Quantile' || input.estType == 'QuantileTOTAL'",
        sliderInput(
          inputId = ns("Quantile"),
          label = "Estimation quantile",
          min = 0.01,
          max = 0.99,
          value = c(0.9),
          width = "100%"
        )
      ),
      checkboxInput(
        inputId = ns("showModel"),
        label = "Show model estimates",
        value = T
      ),
      tags$hr(),
      tags$b("Range of dependent variable:"),
      fluidRow(
        column(width = 6,
               numericInput(
                 inputId = ns("min"),
                 label = "Min",
                 value = 0
               )),
        column(width = 6,
               numericInput(
                 inputId = ns("max"),
                 label = "Max",
                 value = 10
               ))
      ),
      conditionalPanel(
        ns = ns,
        condition = "output.restrictOption == 'show'",
        selectInput(
          inputId = ns("limit"),
          label = "Restriction",
          choices = list(
            "No restriction" = "No restriction",
            "0-1" = "0-1",
            "0-100" = "0-100"
          )
        )
      ),
      tags$hr(),
    )
  }

#' Z Scale Server
#'
#' Server function of the module
#' @param id id of module
#' @param Model (reactive) model output
#' @param fixCol (reactive) user input if columns should be fixed, TRUE or FALSE
#' @param estimationTypeChoices (reactive) named characters of choices of estimation types
#' @param restrictOption (reactive) either "hide" or "show". If "show" than add user input to
#' restrict the z scale.
zScaleServer <- function(id,
                         Model,
                         fixCol,
                         estimationTypeChoices,
                         restrictOption,
                         zValuesFun) {
  moduleServer(id,
               function(input, output, session) {
                 zValues <- reactiveVal(NULL)

                 values <- reactiveValues(
                   estType = NULL,
                   Quantile = NULL,
                   showModel = NULL,
                   range = NULL,
                   limit = NULL
                 )

                 observeEvent(estimationTypeChoices(), {
                   updateSelectInput(session,
                                     "estType",
                                     choices = estimationTypeChoices(),
                                     selected = "Mean")
                 })

                 observeEvent(list(input$estType, Model()), {
                   req(input$estType)
                   validate(validInput(Model()))

                   if(!fixCol()) zValues(zValuesFun(input$estType, Model()$model)) else
                     zValues(NULL)
                 })

                 observeEvent(zValues(), {
                   req(zValues())

                   updateNumericInput(
                     session,
                     "min",
                     value = zValues()$valueMin,
                     min = zValues()$min,
                     max = zValues()$max
                   )

                   updateNumericInput(
                     session,
                     "max",
                     value = zValues()$valueMax,
                     min = zValues()$min,
                     max = zValues()$max
                   )

                   values$estType <- input$estType
                   values$range <- c(zValues()$valueMin, zValues()$valueMax)

                   req(input$Quantile)
                   values$Quantile <- input$Quantile
                 })

                 output$restrictOption <-
                   renderText({
                     restrictOption()
                   })
                 outputOptions(output, "restrictOption", suspendWhenHidden = FALSE)

                 observeEvent(input$limit, {
                   req(restrictOption() == "show")

                   rangez <- c(input$min, input$max)

                   if (identical(input$limit, "0-1")) {
                     rangez <- pmax(0, pmin(1, rangez))
                     if (rangez[1] == rangez[2]) {
                       rangez <- c(0, 1)
                     }
                   }

                   if (identical(input$limit, "0-100")) {
                     rangez <- pmax(0, pmin(100, rangez))
                     if (rangez[1] == rangez[2]) {
                       rangez <- c(0, 100)
                     }
                   }

                   updateNumericInput(session, "min", value = min(rangez))
                   updateNumericInput(session, "max", value = max(rangez))

                   values$range <- c(min(rangez), max(rangez))
                   values$limit <- input$limit
                 })

                 observeEvent(input$Quantile, {
                   values$estType <- input$estType
                   values$Quantile <- input$Quantile
                 })

                 observeEvent(input$showModel, {
                   values$showModel <- input$showModel
                 })

                 values
               })
}


#' Get Z Values Kernel
#'
#' @param estimationType (character) type of estimate
#' @param model (list) model output
getZValuesKernel <- function(estimationType, model) {
  if (is.null(model))
    return(NULL)

  if (estimationType %in% c("1 SE", "2 SE")) {
    sdVal <- ifelse(grepl("2", estimationType), 2, 1)
    zValues <-
      as.vector(apply(sapply(1:length(model), function(x)
        model[[x]]$estimate), 1, sd)) * sdVal
  } else {
    zValues <-
      as.vector(rowMeans(sapply(1:length(model), function(x)
        model[[x]]$estimate))) * 1.25
  }

  maxValue <- signif(max(zValues, na.rm = TRUE), 2)

  return(list(
    valueMin = 0,
    valueMax = maxValue,
    min = 0,
    max = maxValue
  ))
}


#' Get Z Values
#'
#' @param estimationType (character) type of estimate
#' @param model (list) model output
getZvalues <- function(estimationType, model) {
  if (is.null(model))
    return(NULL)

  if (estimationType %in% c("Mean", "Quantile", "QuantileTotal")) {
    defaultMin <- getDefaultZMin(model$range$mean)
    defaultMax <- getDefaultZMax(model$range$mean)

    return(
      list(
        valueMin = defaultMin,
        valueMax = defaultMax,
        min = defaultMin,
        max = defaultMax
      )
    )
  }

  if (estimationType %in% c("1 SETOTAL", "2 SETOTAL", "1 SD Population", "2 SD Population")) {
    val <- getDefaultZError(estimationType, model$range$seTotal)
  }

  if (estimationType %in% c("1 SE", "2 SE")) {
    val <- getDefaultZError(estimationType, model$range$se)
  }

  return(list(
    valueMin = 0,
    valueMax = val,
    min = 0,
    max = val * 3
  ))
}


#' Get Default Z Error
#'
#' @param estType (character) type of estimate
#' @param range (numeric) range from model output
getDefaultZError <- function(estType, range) {
  sdVal <- ifelse(grepl("2", estType), 2, 1)
  3 * signif(1.1 * max(range) * sdVal, 2)
}

#' Get Default Z Min
#'
#' @param mean mean from model output
getDefaultZMin <- function(mean) {
  signif(mean[1] - 0.1 * diff(mean), which(round(abs(
    diff(mean) / mean[1] * 10 ^ (0:10)
  ), 0) > 1)[1])
}

#' Get Default Z Max
#'
#' @param mean mean from model output
getDefaultZMax <- function(mean) {
  signif(mean[2] + 0.1 * diff(mean), which(round(abs(
    diff(mean) / mean[2] * 10 ^ (0:10)
  ), 0) > 1)[1])
}

## Combined Input ----

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
sliderAndNumericInputUI <-
  function(id, label, min, max, value, step) {
    ns <- NS(id)
    tagList(fluidRow(
      column(
        width = 10,
        sliderInput(
          inputId = ns("sliderIn"),
          label = label,
          min = min,
          max = max,
          value = value,
          step = step,
          width = "100%"
        )
      ),
      column(
        width = 2,
        style = "margin-top: 30px;",
        numericInput(
          inputId = ns("numIn"),
          label = NULL,
          min = min,
          max = max,
          value = value,
          step = step
        )
      )
    ))
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

                 observe({
                   req(value(), min(), max(), step())

                   updateSliderInput(
                     session = session,
                     "sliderIn",
                     value = value(),
                     min = min(),
                     max = max(),
                     step = step()
                   )
                   updateNumericInput(
                     session = session,
                     "numIn",
                     value = value(),
                     min = min(),
                     max = max(),
                     step = step()
                   )

                   result(value())
                 })

                 observeEvent(input$sliderIn, {
                   req(!identical(input$sliderIn, input$numIn))
                   updateNumericInput(session = session,
                                      "numIn",
                                      value = input$sliderIn)
                   result(input$sliderIn)
                 })

                 observeEvent(input$numIn, {
                   req(input$numIn, !identical(input$sliderIn, input$numIn))
                   updateSliderInput(session = session,
                                     "sliderIn",
                                     value = input$numIn)
                   result(input$numIn)
                 })

                 return(result)
               })
}


# Collection of helper functions for the modelling tabs ----

#' Extract Zoom From Long Range
#'
#' @param rangeLongitude (numeric) range of longitude vector
#' @param mapCentering (character) centering of the map, either "Europe" or "Pacific"
extractZoomFromLongRange <- function(rangeLongitude, mapCentering) {
  if (mapCentering == "Europe") {
    rangeLong <- diff(range(rangeLongitude, na.rm = TRUE) + c(-1, 1))
  } else {
    longRange <- rangeLongitude
    longRange[rangeLongitude < -20] <-
      longRange[rangeLongitude < -20] + 200
    longRange[rangeLongitude >= -20] <-
      (-160 + longRange[rangeLongitude >= -20])
    rangeLong <- diff(range(longRange, na.rm = TRUE) + c(-1, 1))
  }

  pmin(360, pmax(0, rangeLong, na.rm = TRUE)) %>% round()
}
