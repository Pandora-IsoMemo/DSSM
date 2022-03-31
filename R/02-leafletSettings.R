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
      ns("includeLogo"), "Logo"
    )),
    column(
      6,
      selectInput(
        ns("logoPosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "topleft"
      )
    ))
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
leafletSettings <- function(input, output, session) {
  values <- reactiveValues(pointRadius = 20000)

  observeEvent(input$map_zoom, {
    values$pointRadius <- (20000 * (4 / input$map_zoom) ^ 3)
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

  observe({
    values$logoPosition <-
      ifelse(input$includeLogo, input$logoPosition, NA_character_)
  })

  reactive(
    list(
      leafletType = input$LeafletType,
      pointRadius = values$pointRadius,
      scalePosition = values$scalePosition,
      northArrowPosition = values$northArrowPosition,
      logoPosition = values$logoPosition
    )
  )
}
