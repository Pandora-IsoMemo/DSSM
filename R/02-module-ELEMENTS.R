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
