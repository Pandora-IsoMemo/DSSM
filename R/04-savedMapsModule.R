#' ui function of saved maps module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
savedMapsTabUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(title,
           id = id,
           value = id,
           sidebarLayout(sidebarPanel(
             width = 2,
             div(
               style = 'display:inline-block',
               class = "save-plot-container",
               textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
               numericInput(ns("meanMap"), "Mean of map", value = 0),
               numericInput(ns("sdMap"), "Sd of map", value = 0, min = 0),
               radioButtons(
                 ns("userMapType"),
                 "Type",
                 choices = c(
                   "all" = "1",
                   "region - circle" = "2",
                   "region - square" = "3"
                 ),
                 inline = FALSE
               ),
               conditionalPanel(
                 ns = ns,
                 condition = "input.userMapType == '2' || input.userMapType == '3'",
                 numericInput(
                   inputId = ns("userRadius"),
                   label = "Radius in km",
                   min = 1,
                   max = 10000,
                   value = c(3000),
                   width = "100%",
                   step = 100
                 ),
                 tags$h5("Center"),
                 numericInput(
                   inputId = ns("centerLatitude"),
                   label = "Latitude",
                   min = -90,
                   max = 90,
                   value = c(50),
                   width = "49%"
                 ),
                 numericInput(
                   inputId = ns("centerLongitude"),
                   label = "Longitude",
                   min = -180,
                   max = 180,
                   value = c(10),
                   width = "49%"
                 )
               ),
               actionButton(ns("createMap"), "Create new map")
             )
           ),
           mainPanel(uiOutput(ns(
             "mapTable"
           )))))
}



#' server function of saved maps module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param savedMaps saved maps
#'
#' @export
savedMapsTab <- function(input, output, session, savedMaps) {
  ns <- session$ns
  parentSession <- get("session", envir = parent.frame(2))

  output$mapTable <- renderUI({
    mapTable(savedMaps(), ns = ns)
  })

  observeEvent(input$deleteMap, {
    savedMaps(savedMaps()[-input$deleteMap$i])
  })

  observeEvent(input$createMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == "") {
      alert("Please provide a map name")
      return()
    }

    if (input$userMapType == "1") {
      XPred <- as.numeric(c(input$meanMap, input$sdMap))
    }
    if (input$userMapType == "2") {
      lo <- seq(-180, 180, by = input$userRadius / 10000)
      la <- seq(-90, 90, by = input$userRadius / 10000)
      coord <- expand.grid(lo, la)
      coord <-
        coord[sqrt((coord[, 2] - input$centerLatitude) ^ 2 +  (coord[, 1] - input$centerLongitude) ^
                     2) < input$userRadius / 111,]
      XPred <- data.frame(
        Est = input$meanMap,
        Sd = input$sdMap,
        Longitude = coord[, 1],
        Latitude = coord[, 2]
      )
    }
    if (input$userMapType == "3") {
      lo <- seq(-180, 180, by = input$userRadius / 10000)
      la <- seq(-90, 90, by = input$userRadius / 10000)
      coord <- expand.grid(lo, la)
      coord <-
        coord[pmax(abs(coord[, 2] - input$centerLatitude),
                   abs(coord[, 1] - input$centerLongitude)) < input$userRadius / 111,]
      XPred <- data.frame(
        Est = input$meanMap,
        Sd = input$sdMap,
        Longitude = coord[, 1],
        Latitude = coord[, 2]
      )
    }

    map <- createSavedMap(
      model = NULL,
      predictions = XPred,
      plot =  recordPlot({
        plot.new()
        text(
          x = 0.5,
          y = 0.5,
          paste0("Mean = ", input$meanMap, ", Sd = ", input$sdMap),
          cex = 5
        )
      }),
      type = "user",
      name = mapName
    )
    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })


  observeEvent(savedMaps(), {
    lapply(seq_along(savedMaps()), function(i) {
      rr <- savedMaps()[[i]]
      output[[paste0("thumbnail_", i)]] <-
        renderImage(list(
          src = rr$file,
          height = 100,
          width = 160
        ), deleteFile = FALSE)
    })
  })
}
