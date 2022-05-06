#' ui function of leaflet settings module
#'
#' @param id namespace
#' @param title title in tab
leafletSettingsUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    selectInput(
      ns("LeafletType"),
      "Map type",
      choices = c(
        "Type 1" = "1",
        "Type 2" = "2",
        "Type 3" = "3",
        "Type 4" = "4",
        "Type 5" = "5",
        "Type 6" = "6",
        "Type 7" = "7"
      )
    ),
    fluidRow(column(6, checkboxInput(
      ns("includeNorthArrow"), "North Arrow"
    )),
    column(
      6,
      selectInput(
        ns("northArrowPosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    fluidRow(column(6, checkboxInput(
      ns("includeScale"), "Scale"
    )),
    column(
      6,
      selectInput(
        ns("scalePosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    checkboxInput(ns("fitBounds"), "Fit boundaries"),
    conditionalPanel(
      condition = "input.fitBounds == true",
      tags$hr(),
      sliderInput(
        ns("boundsLat"),
        "Latitude: South - North",
        value = c(15, 60),
        min = -90,
        max = 90
      ),
      sliderInput(
        ns("boundsLng"),
        "Longitude: West - East",
        value = c(-15, 60),
        min = -180,
        max = 180
      ),
      fluidRow(
        column(5, actionButton(
          ns("applyBounds"), "Apply"
        )),
        column(
          7, checkboxInput(ns("showBounds"), "Show boundaries")
        )
        ),
      tags$hr(),
      ns = ns
    )
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
leafletSettings <- function(input, output, session) {
  values <- reactiveValues(applyBounds = 0)

  values$bounds <-
    reactiveValues(
      north = 60,
      south = 15,
      east = 60,
      west = -15
    )

  observeEvent(input$LeafletType, {
    values$leafletType <- input$LeafletType
  })

  observe({
    values$scalePosition <-
      ifelse(input$includeScale, input$scalePosition, NA_character_)
  })

  observe({
    values$northArrowPosition <-
      ifelse(input$includeNorthArrow,
             input$northArrowPosition,
             NA_character_)
  })

  observeEvent(input$applyBounds, {
    values$applyBounds <- input$applyBounds

    values$bounds <-
      reactiveValues(
        north = input$boundsLat[[2]],
        south = input$boundsLat[[1]],
        east = input$boundsLng[[2]],
        west = input$boundsLng[[1]]
      )
  })

  observeEvent({
    input$showBounds & input$fitBounds
  }, {
    values$showBounds <- input$showBounds & input$fitBounds
  })

  reactive({
    values
  })
}
