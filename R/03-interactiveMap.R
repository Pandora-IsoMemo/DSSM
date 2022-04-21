#' ui function of interactiveMap module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
interactiveMapUI <- function(id, title = ""){
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    div(
      class = "map-container",
      leafletOutput(ns("map"), width = "100%", height = "100%")
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      absolutePanel(
        tags$script(paste0(
          '
            $(document).on("shiny:visualchange", function(e) {
              let box = document.querySelector("#', ns('map'),'");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("', ns('map_width'), '", width);
              Shiny.setInputValue("', ns('map_height'), '", height);
            });
            $(window).resize(function(e) {
              let box = document.querySelector("#', ns('map'),'");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("', ns('map_width'), '", width);
              Shiny.setInputValue("', ns('map_height'), '", height);
            });
          '
        )),
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 110, left = "auto", right = 40, bottom = "auto",
        width = 330, height = "auto",
        h2("Statistics"),
        selectizeInput(ns("var1"), "Variable 1", choices = character(0),
                       options = list(allowEmptyOption = TRUE)),
        selectizeInput(ns("var2"), "Variable 2", choices = character(0),
                    options = list(allowEmptyOption = TRUE)),
        div(
          id = "stats-sidebar-container",
          sidebarPlotOutput(
            ns("plot1"),
            condition = paste0("input['", ns("var1"), "'] != ''")
          ),
          sidebarPlotOutput(
            ns("plot2"),
            condition = paste0("input['", ns("var2"), "'] != ''")
          ),
          sidebarPlotOutput(
            ns("plot3"),
            condition = paste0(
              "input['", ns("var1"), "'] != '' && input['", ns("var2"), "'] != ''"
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = "auto", right = "auto", left = 40, bottom = 100,
        width = 330, height = "auto",
        leafletSettingsUI(ns("mapSettings"), "Map Settings"),
        tags$br(),
        leafletExportButton(ns("exportLeaflet"))
      )
    )
  )
}


#' server funtion of interactive map module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData data
#'
#' @export
interactiveMap <- function(input, output, session, isoData){
  ns <- session$ns

  leafletValues <- callModule(leafletSettings, "mapSettings",
                              zoom = reactive(input$map_zoom),
                              center = reactive(input$map_center))
  leafletMap <- reactiveVal(leaflet())

  # Create the map

  # set map type
  observeEvent(leafletValues()$leafletType, {
    req(leafletValues()$leafletType)
    leafletMap(leafletMap() %>%
                 drawType(type = leafletValues()$leafletType)
    )
  })

  # add icons to map
  observeEvent(is.na(leafletValues()$scalePosition) |
                 is.na(leafletValues()$northArrowPosition) |
                 is.na(leafletValues()$logoPosition), {
                   leafletMap(leafletMap() %>%
                                drawIcons(scale = !is.na(leafletValues()$scalePosition),
                                          scalePosition = leafletValues()$scalePosition,
                                          northArrow = !is.na(leafletValues()$northArrowPosition),
                                          northArrowPosition = leafletValues()$northArrowPosition,
                                          logoPosition = leafletValues()$logoPosition
                                )
                   )
                 })

  # adjust map center
  observeEvent(leafletValues()$center, {
    req(leafletValues()$center)
    leafletMap(leafletMap() %>%
                 setView(lng = leafletValues()$center$lng,
                         lat = leafletValues()$center$lat,
                         zoom = input$map_zoom
                         ))
  })

  # adjust map bounds fit
  observeEvent(leafletValues()$bounds, {
    req(leafletValues()$bounds)
    #input$map_bounds
    leafletMap(leafletMap() %>%
                 fitBounds(lng1 = leafletValues()$bounds$west,
                           lng2 = leafletValues()$bounds$east,
                           lat1 = leafletValues()$bounds$south,
                           lat2 = leafletValues()$bounds$north
                 ))
  })

  # render output map ####
  output$map <- renderLeaflet({
    leafletMap()
  })

  # Add Circles relative to zoom
  observe({
    req(isoData(), leafletMap())
    new_zoom <- input$map_zoom
    if (is.null(new_zoom)) return()
    isolate({
      addCirclesRelativeToZoom(leafletProxy("map"), isoData(), #pointRadius = 20000,
                               newZoom = new_zoom, zoom = 4)
    })

  })


  # When map is clicked, show a popup with info
  observe({
    req(input$map_shape_click)
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event)) return()

    isolate({
      showIDPopup(isoData(), event$id, event$lat, event$lng)
    })
  })

  # Show Histograms and Scatterplot in Sidebar
  observe({
    req(isoData())
    numVars <- unlist(lapply(names(isoData()), function(x){
      if (is.integer(isoData()[[x]]) | is.numeric(isoData()[[x]]))
        x
      else
        NULL
    }))

    updateSelectInput(session, "var1", choices = c("", numVars))
    updateSelectInput(session, "var2", choices = c("", numVars))
  })

  var1 <- reactive({
    if (is.null(isoData()) | is.null(input$var1) | input$var1 == "")
      return(NULL)

    isoData()[[input$var1]]
  })

  var2 <- reactive({
    if (is.null(isoData()) | is.null(input$var2) | input$var2 == "")
      return(NULL)

    isoData()[[input$var2]]
  })

  callModule(leafletExport, "exportLeaflet", leafletMap = leafletMap,
             width = reactive(input$map_width), height = reactive(input$map_height),
             zoom = reactive(input$map_zoom), isoData = isoData)

  callModule(sidebarPlot, "plot1", x = var1, nameX = reactive(input$var1))
  callModule(sidebarPlot, "plot2", x = var2, nameX = reactive(input$var2))
  callModule(sidebarPlot, "plot3", x = var1, y = var2,
             nameX = reactive(input$var1), nameY = reactive(input$var2))
}


# helper functions ####

#'  draw Interactive Map
#' @param isoData isoData data
#' @param zoom zoom
#' @param type map type
#' @param northArrow show north arrow?
#' @param northArrowPosition position of north arrow
#' @param scale show scale?
#' @param scalePosition position of scale
#' @param logoPosition character position of logo if selected, else NA
#' @param center where to center map (list of lat and lng)
#' @param bounds map bounds (list of north, south, east, west)
#'
#' @export
draw <- function(isoData, zoom = 5, type = "1",
                 northArrow = FALSE, northArrowPosition = "bottomright",
                 scale = FALSE, scalePosition = "topleft",
                 logoPosition = NA,
                 center = NULL,
                 bounds = NULL){

  map <- leaflet() %>% drawType(type = type)
  map <- map %>% drawIcons(northArrow = northArrow, northArrowPosition = northArrowPosition,
                           scale = scale, scalePosition = scalePosition,
                           logoPosition = logoPosition)

  if (!is.null(center)) {
    map <- map %>% setView(lng = center$lng,
                           lat = center$lat,
                           zoom = zoom
    )
  }

  if (!is.null(bounds)) {
    map <- map %>% fitBounds(lng1 = bounds$west,
                             lng2 = bounds$east,
                             lat1 = bounds$south,
                             lat2 = bounds$north
    )
  }

  map <- map %>% addCirclesRelativeToZoom(isoData, newZoom = zoom, zoom = zoom)
}


#' Draw Type of Interactive Map
#' @param map leaflet map
#' @param type map type
drawType <- function(map, type = "1"){

  if (type == "1"){
    mType <- "CartoDB.Positron"
  }
  if (type == "2"){
    mType <- "OpenStreetMap.Mapnik"
  }
  if (type == "3"){
    mType <- "OpenStreetMap.DE"
  }
  if (type == "4"){
    mType <- "OpenTopoMap"
  }
  if (type == "5"){
    mType <- "Stamen.TonerLite"
  }
  if (type == "5"){
    mType <-  "Esri"
  }
  if (type == "6"){
    mType <- "Esri.WorldTopoMap"
  }
  if (type == "7"){
    mType <-  "Esri.WorldImagery"
  }

  map <- map %>%
    addProviderTiles(mType)

  map
}



#' Draw Icons on Interactive Map
#' @param map leaflet map
#' @param northArrow show north arrow?
#' @param northArrowPosition position of north arrow
#' @param scale show scale?
#' @param scalePosition position of scale
#' @param logoPosition character position of logo if selected, else NA
drawIcons <- function(map,
                      northArrow = FALSE, northArrowPosition = "bottomright",
                      scale = FALSE, scalePosition = "topleft",
                      logoPosition = NA){

  map <- map %>%
    clearControls() %>%
    removeScaleBar()

  if (!is.na(logoPosition)) {
    map <- map %>%
      addControl(
        tags$img(src = "https://isomemo.com/images/logo.jpg", width = "75", height = "50"),
        position = logoPosition,
        className = ""
      )
  }

  if (northArrow && (northArrowPosition %in% c("bottomright", "bottomleft"))) {
    if (scale) {
      map <- addScaleBar(
        map,
        position = scalePosition,
        options = scaleBarOptions()
      )
    }

    if (northArrow) {
      map <- addControl(
        map,
        tags$img(src = "https://isomemodb.com/NorthArrow.png", width = "80", height = "80"),
        position = northArrowPosition,
        className = ""
      )
    }
  } else {
    if (northArrow) {
      map <- addControl(
        map,
        tags$img(src = "https://isomemodb.com/NorthArrow.png", width = "80", height = "80"),
        position = northArrowPosition,
        className = ""
      )
    }

    if (scale) {
      map <- addScaleBar(
        map,
        position = scalePosition,
        options = scaleBarOptions()
      )
    }
  }

  map
}


addCirclesRelativeToZoom <- function(map, isoData,
                                     newZoom, zoom = 5){
  if (is.null(isoData$latitude) || all(is.na(isoData$latitude))) return(map)

  isoData <- isoData[!is.na(isoData$longitude), ]

  numColors <- length(unique(isoData$source))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  pal <- colorFactor(colors, isoData$Source)

  set.seed(20180213)
  isoData$Latitude_jit <- jitter(isoData$latitude, amount = 0.05 * (zoom / newZoom) ^ 2)
  isoData$Longitude_jit <- jitter(isoData$longitude, amount = 0.05 * (zoom / newZoom) ^ 2)

  map %>%
    clearShapes() %>%
    addCircles(data = isoData,
               lat = ~ Latitude_jit,
               lng =  ~ Longitude_jit,
               layerId = ~ id,
               stroke = F,
               fillOpacity = 0.7,
               color = pal(isoData$source),
               fillColor = pal(isoData$source),
               radius = 20000 * (zoom / newZoom) ^ 3
    ) %>%
    addLegend("topleft", pal = pal, values = isoData$source, title = "Database",
              layerId = "colorLegend")

}



# Show a popup at the given location
showIDPopup <- function(dat, id, lat, lng) {
  selectedId <- dat[which(dat$id == id), ]

  popup <- paste(
    names(selectedId),
    unlist(lapply(selectedId, as.character)),
    sep = ": ",
    collapse = "</br>")

  leafletProxy("map") %>%
    addPopups(lng, lat, popup, layerId = id,
              options = popupOptions(maxHeight = 200))

}
