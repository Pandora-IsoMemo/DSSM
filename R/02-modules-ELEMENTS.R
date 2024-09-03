## Custom Inputs ----

#' Cluster Method UI
#'
#' UI for the cluster method
#'
#' @param ns namespace
#' @param timeRangeInput logical, if TRUE, the time range input is shown
clusterMethodUI <- function(ns, timeRangeInput = FALSE) {
  tagList(
    selectizeInput(inputId = ns("clusterMethod"),
                   label = "Cluster Method (optional):",
                   choices = c("kmeans","mclust","tclust"),
                   options = list(
                     placeholder = '',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    conditionalPanel(
      condition = "input.clusterMethod == 'kmeans'",
      ns = ns,
      selectInput(inputId = ns("kMeansAlgo"),
                  label = "K-means algorithm:",
                  choices = c("Hartigan-Wong", "Lloyd", "Forgy",
                              "MacQueen"))),
    conditionalPanel(
      condition = "input.clusterMethod == 'kmeans' | input.clusterMethod == 'tclust'",
      ns = ns,
      sliderInput(inputId = ns("nClust"),
                  label = "Number of clusters",
                  value = 5, min = 2, max = 15, step = 1),
      helpText("Please adjust the number of clusters depending on the data.")
    ),
    conditionalPanel(
      condition = "input.clusterMethod == 'tclust'",
      ns = ns,
      sliderInput(inputId = ns("trimRatio"),
                  label = "Proportion of observations to be trimmed by tclust",
                  value = 0.05, min = 0, max = 1, step = 0.05)
    ),
    conditionalPanel(
      condition = "input.clusterMethod == 'mclust'",
      ns = ns,
      sliderInput(inputId = ns("nClustRange"),
                  label = "Possible range for clusters",
                  value = c(2,10), min = 2, max = 50, step = 1)
    ),
    if (timeRangeInput) {
      conditionalPanel(
        condition = "input.clusterMethod == 'mclust' | input.clusterMethod == 'kmeans' | input.clusterMethod == 'tclust'",
        ns = ns,
        sliderInput(inputId = ns("timeClust"),
                    label = "Cluster time range",
                    min = 0, max = 15000, value = c(1000, 5000), step = 100)
      )
    } else NULL
  )
}

#' Smoothing UI
#'
#' @param ns namespace
#' @param label_slider label of the slider
#' @param label_info label of the info button
#' @param min minimum value of the slider
#' @param max maximum value of the slider
#' @param value default value of the slider
#' @param step step of the slider
smoothingUI <- function(ns,
                        label_slider = "No. of basis functions",
                        label_info = "Basis functions",
                        min = 20,
                        max = 1000,
                        value = 70,
                        step = 10) {
  tagList(
    sliderInput(
      ns("Smoothing"),
      label = label_slider,
      min = min,
      max = max,
      value = value,
      step = step
    ),
    uiOutput(ns("timeBasisFunctionsUI")),
    # info button for more information
    actionButton(
      ns("smoothing_info"),
      label = label_info,
      icon = icon("info-circle")
    ),
    tags$br(),
    tags$br()
  )
}

#' Smoothing Server
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param ns namespace
#' @param title title of the modal
#' @param map3D logical, if TRUE, the map is 3D (contains the time dimension)
#' @param label if `map3D == TRUE` label of the slider for the number of time basis functions
smoothingServer <- function(input,
                            output,
                            session,
                            ns,
                            title = "Number of Basis Functions (Smoothing)",
                            map3D = FALSE,
                            label = "No. of time basis functions") {
  # if 3D map and spline type is 2: show UI for TIME basis functions
  output$timeBasisFunctionsUI <- renderUI({
    logDebug("Render the number of TIME basis functions for 3D map")
    if (map3D && (input[["SplineType"]] == 2)) {
      sliderInput(
        inputId = ns("SmoothingT"),
        label = label,
        min = 4,
        max = 50,
        value = 12,
        step = 1
      )
    } else {
      NULL
    }
  })

  # if 3D map and the spline type changes: update default values for the number of SPATIAL basis functions
  if (map3D) {
    observe({
      logDebug("Update default values for the number of SPATIAL basis functions for 3D map")
      if (input[["SplineType"]] == 1) {
        # planar smooth type
        updateSliderInput(
          session,
          "Smoothing",
          min = 10,
          max = 1000,
          value = 150,
          step = 5
        )
      } else {
        # spherical smooth type
        updateSliderInput(
          session,
          "Smoothing",
          min = 10,
          max = 250,
          value = 30,
          step = 5
        )
      }
    }) %>%
      bindEvent(input[["SplineType"]])
  }

  observe({
    logDebug("Show modal with information about the smoothing")
    # Convert the markdown file to HTML
    markdown_html <- markdownToHTML("www/info_basis-functions.md", fragment.only = TRUE)

    showModal(modalDialog(
      title = title,
      HTML(markdown_html),
      # Include the converted HTML
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }) %>%
    bindEvent(input[["smoothing_info"]])
}

## Combined Inputs ----

#' Numeric Input Lat And Long UI
#'
#' UI of the numeric input of latitude and longitude
#'
#' @param id id of module
#' @param label label
#' @param valueLat (numeric) latitude
#' @param valueLong (numeric) longitude
numericInputLatAndLongUI <-
  function(id,
           label,
           valueLat = 50,
           valueLong = 10) {
    ns <- NS(id)
    tagList(fluidRow(
      tags$strong(label, style = "margin-left: 15px;"),
      tags$br(),
      column(
        6,
        numericInput(
          inputId = ns("latitude"),
          label = "Latitude",
          min = -90,
          max = 90,
          value = valueLat
        )
      ),
      column(
        6,
        numericInput(
          inputId = ns("longitude"),
          label = "Longitude",
          min = -180,
          max = 180,
          value = valueLong
        )
      )
    ))
  }

#' Numeric Input Lat And Long Server
#'
#' Server function of the numeric input of latitude and longitude module
#' @param id id of module
#' @param valueLat (numeric) latitude
#' @param valueLong (numeric) longitude
numericInputLatAndLongServer <- function(id,
                                         valueLat = reactive(50),
                                         valueLong = reactive(10)) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(valueLat())

                   updateNumericInput(session = session,
                                      "latitude",
                                      value = valueLat())
                 }) %>%
                   bindEvent(valueLat())

                 observe({
                   req(valueLong())

                   updateNumericInput(session = session,
                                      "longitude",
                                      value = valueLong())
                 }) %>%
                   bindEvent(valueLong())

                 list(longitude = reactive(input$longitude),
                      latitude = reactive(input$latitude))
               })
}


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
