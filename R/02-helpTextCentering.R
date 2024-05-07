dataCenterUI <- function(ns, displayCondition = "input.SplineType == '1'", hideCorrection = FALSE) {
  conditionalPanel(
    ns = ns,
    condition = displayCondition,
    selectInput(inputId = ns("dataCenter"),
                label = "Center of data",
                choices = c("0th meridian" = "Europe", "180th meridian" = "Pacific")),
    if (!hideCorrection) {
      conditionalPanel(
        ns = ns,
        condition = "input.dataCenter == 'Europe'",
        checkboxInput(inputId = ns("correctionPac"),
                      label = "Border correction for Pacific",
                      value = FALSE)
      )
    } else NULL
  )
}

helpTextCenteringUI <- function(ns) {
  conditionalPanel(
    ns = ns,
    condition = "input.Centering != input.dataCenter",
    helpText(textOutput(ns("helpTextCentering")))
  )
}

outputHelpTextCentering <- function(input, output, session) {
  output$helpTextCentering <- renderText({
    if (input$dataCenter != input$Centering) {
      sprintf("The 'Center of data' was set to '%s' for modelling, but 'Map Center' is set to '%s'. Please use the same centering, or keep in mind that displayed predictions are based on '%s' centering.",
              input$dataCenter, input$Centering,  input$Centering)
    } else {
      ""
    }
  })
}
