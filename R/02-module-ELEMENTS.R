## Custom Inputs ----

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
                        min = 20, max = 1000, value = 70, step = 10) {
  tagList(
    sliderInput(ns("Smoothing"),
                label = label_slider,
                min = min, max = max, value = value, step = step),
    # info button for more information
    actionButton(ns("smoothing_info"), label = label_info, icon = icon("info-circle")),
    tags$br(), tags$br()
  )
}

#' Smoothing Server
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param ns namespace
#' @param title title of the modal
smoothingServer <- function(input, output, session, ns,
                            title = "Number of Basis Functions (Smoothing)") {
  # Show modal with information about the smoothing
  observeEvent(input$smoothing_info, {
    # Convert the markdown file to HTML
    markdown_html <- markdownToHTML("www/info_basis-functions.md", fragment.only = TRUE)

    showModal(
      modalDialog(
        title = title,
        HTML(markdown_html),  # Include the converted HTML
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
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
