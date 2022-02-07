#' ui function of saved maps module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
savedMapsTabUI <- function(id, title = ""){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    mainPanel(
      uiOutput(ns("mapTable")),
      div(
        style = 'display:inline-block',
        class = "save-plot-container",
        textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
        numericInput(ns("meanMap"), "Mean of map", value = 0),
        numericInput(ns("sdMap"), "Sd of map", value = 0, min = 0),
        radioButtons(ns("userMapType"), "Type", choices = c("all" = "1", "region - circle" = "2", "region - square" = "3")),
        conditionalPanel(
          ns = ns,
          condition = "input.userMapType == '2' || input.userMapType == '3'",
          sliderInput(inputId = ns("userRadius"),
                      label = "Radius in km",
                      min = 1, max = 10000, value = c(3000),
                      width = "100%", step = 100),
          numericInput(inputId = ns("centerLatitude"),
                       label = "Set Latitude of center",
                       min = -90, max = 90, value = c(50), width = "20%"),
          numericInput(inputId = ns("centerLongitude"),
                       label = "Set Longitude of center",
                       min = -180, max = 180, value = c(10), width = "20%")
        ),
        actionButton(ns("createMap"), "Create new map")
      )
    )
  )
}
