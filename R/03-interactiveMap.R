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
        draggable = TRUE, top = 110, right = "auto", left = 50, bottom = "auto",
        width = 330, height = "auto",
        leafletSettingsUI(ns("mapSettings"), "Map Settings"),
        leafletPointSettingsUI(ns("mapPointSettings")),
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

  leafletValues <- callModule(leafletSettings, "mapSettings", zoom = reactive(input$map_zoom))
  leafletPointValues <- leafletPointSettingsServer("mapPointSettings")
  leafletMap <- reactiveVal(leaflet())

  newZoom <- reactive({
    if (is.null(input$map_zoom))
      4
    else
      input$map_zoom
  })

  #zoomSlow <- newZoom %>% debounce(1000)
  #zoomSlow <- newZoom %>% throttle(1000)
  zoomSlow <- newZoom

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
                 is.na(leafletValues()$northArrowPosition), {
                   leafletMap(leafletMap() %>%
                                drawIcons(scale = !is.na(leafletValues()$scalePosition),
                                          scalePosition = leafletValues()$scalePosition,
                                          northArrow = !is.na(leafletValues()$northArrowPosition),
                                          northArrowPosition = leafletValues()$northArrowPosition
                                )
                   )
                 })

  # set legend
  observeEvent(list(leafletValues()$showLegend), {
    leafletMap(
      leafletMap() %>%
        setColorLegend(showLegend = leafletValues()$showLegend,
                       values = isoData()$source)
    )
  })

  # adjust map bounds fit
  observeEvent(leafletValues()$bounds, {
    req(leafletValues()$bounds)
    # not exact bounds, only fit to input$map_bounds
    leafletMap(leafletMap() %>%
                 fitBounds(lng1 = leafletValues()$bounds$west,
                           lng2 = leafletValues()$bounds$east,
                           lat1 = leafletValues()$bounds$south,
                           lat2 = leafletValues()$bounds$north
                 ))
  })

  observeEvent(list(leafletValues()$showBounds, leafletValues()$bounds), {
    req(leafletValues()$bounds)

    leafletMap(leafletMap() %>%
                 drawFittedBounds(showBounds = leafletValues()$showBounds,
                                  bounds = leafletValues()$bounds)
                 )
  })

  # render output map ####
  output$map <- renderLeaflet({
    req(leafletMap())
    isolate({
      if (!is.null(input$map_center) & !(leafletValues()$applyBounds)) {
        leafletMap() %>%
          setView(lng = input$map_center$lng,
                  lat = input$map_center$lat,
                  zoom = input$map_zoom
          )
      } else {
        leafletMap()
      }

    })
  })

  # Add Circles with jitter relative to zoom
  observeEvent(
    list(isoData(), leafletMap(), zoomSlow(),
         leafletPointValues()$useJitter,
         leafletPointValues()$pointRadius),
    {
      req(isoData(), leafletMap(), leafletPointValues()$useJitter, zoomSlow())

      addCirclesToMap(leafletProxy("map"),
                      addJitterCoords(isoData(), zoom = 4, newZoom = zoomSlow(), amount = 0.05),
                      pointRadius = leafletPointValues()$pointRadius)
    })


  # Add Circles
  observeEvent(
    list(isoData(), leafletMap(),
         leafletPointValues()$useJitter,
         leafletPointValues()$pointRadius),
    {
      req(isoData(), leafletMap(), !leafletPointValues()$useJitter)

      addCirclesToMap(leafletProxy("map"),
                      isoData(),
                      pointRadius = leafletPointValues()$pointRadius)
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
             zoom = reactive(input$map_zoom), center = reactive(input$map_center),
             isoData = isoData, leafletPointValues = leafletPointValues)

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
#' @param center where to center map (list of lat and lng)
#' @param bounds map bounds (list of north, south, east, west)
#'
#' @export
draw <- function(isoData, zoom = 5, type = "1",
                 northArrow = FALSE, northArrowPosition = "bottomright",
                 scale = FALSE, scalePosition = "topleft",
                 center = NULL,
                 bounds = NULL){

  map <- leaflet() %>% drawType(type = type)
  map <- map %>% drawIcons(northArrow = northArrow, northArrowPosition = northArrowPosition,
                           scale = scale, scalePosition = scalePosition)

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
drawIcons <- function(map,
                      northArrow = FALSE, northArrowPosition = "bottomright",
                      scale = FALSE, scalePosition = "topleft"
                      ){

  if (northArrow && (northArrowPosition %in% c("bottomright", "bottomleft"))) {
    if (scale) {
      map <- addScaleBar(
        map,
        position = scalePosition,
        options = scaleBarOptions()
      )
    } else {
      map <- map %>%
        removeScaleBar()
    }

    if (northArrow) {
      map <- addControl(
        map,
        tags$img(src = "https://isomemodb.com/NorthArrow.png", width = "80", height = "80"),
        position = northArrowPosition,
        layerId = "northArrowIcon",
        className = ""
      )
    } else {
      map <- map %>% removeControl("northArrowIcon")
    }
  } else {
    if (northArrow) {
      map <- addControl(
        map,
        tags$img(src = "https://isomemodb.com/NorthArrow.png", width = "80", height = "80"),
        position = northArrowPosition,
        layerId = "northArrowIcon",
        className = ""
      )
    } else {
      map <- map %>% removeControl("northArrowIcon")
    }

    if (scale) {
      map <- addScaleBar(
        map,
        position = scalePosition,
        options = scaleBarOptions()
      )
    } else {
      map <- map %>%
        removeScaleBar()
    }
  }

  map
}


addCirclesRelativeToZoom <- function(map, isoData, newZoom, zoom = 5){
  addCirclesToMap(
    map = map,
    isoData = isoData,
    pointRadius = relateToZoom(radius = 20, zoom = zoom, newZoom = newZoom)
  )
}


addCirclesToMap <- function(map, isoData, pointRadius){
  if (is.null(isoData$latitude) || all(is.na(isoData$latitude))) return(map)

  isoData <- isoData[!is.na(isoData$longitude), ]

  numColors <- length(unique(isoData$source))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  pal <- colorFactor(colors, isoData$Source)

  if (!is.null(isoData$Latitude_jit)) isoData$latitude <- isoData$Latitude_jit
  if (!is.null(isoData$Longitude_jit)) isoData$longitude <- isoData$Longitude_jit

  map <- map %>%
    removeShape(layerId = isoData$id)

  map %>%
    addCircles(data = isoData,
               lat = ~ latitude,
               lng =  ~ longitude,
               layerId = ~ id,
               stroke = F,
               fillOpacity = 0.7,
               color = pal(isoData$source),
               fillColor = pal(isoData$source),
               radius = 100 * pointRadius
    )
}


addJitterCoords <- function(dat, zoom, newZoom, amount = 0.05){
  set.seed(20180213)
  dat$Latitude_jit <- jitter(dat$latitude, amount = 0.05 * (zoom / newZoom) ^ 2)
  dat$Longitude_jit <- jitter(dat$longitude, amount = 0.05 * (zoom / newZoom) ^ 2)

  dat
}


relateToZoom <- function(radius, zoom, newZoom){
  radius * (zoom / newZoom) ^ 3
}


#' Add Colour Legend
#'
#' @param map leaflet map
#' @param showLegend logical show/hide legend
#' @param values possible values that can be mapped, e.g. isoData$source
setColorLegend <- function(map, showLegend, values){

  if (showLegend) {

    map <- map %>%
      addLegend("topleft",
                pal = getColourPal(values),
                values = values,
                title = "Database",
                layerId = "colorLegend")
  } else {
    map <- map %>% removeControl("colorLegend")
  }

  map
}


#' Get Colour Palette
#'
#' Get colour palette for the points and the legend
#'
#' @inheritParams setColorLegend
getColourPal <- function(values){
  numColors <- length(unique(values))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  colorFactor(colors, values)
}

#' Draw Fitted Bounds
#'
#' @param map leaflet map
#' @param showBounds logical show/hide fitted bounds
#' @param bounds list of (west, east, south, north) boundaries to be drawn
drawFittedBounds <- function(map, showBounds, bounds){
  if (showBounds) {
    map <- map %>%
      addRectangles(
        layerId = "mapBoundsFrame",
        lng1 = bounds$west,
        lng2 = bounds$east,
        lat1 = bounds$south,
        lat2 = bounds$north,
        color = "grey",
        weight = 1,
        fillColor = "transparent"
      )
  } else {
    map <- map %>%
      removeShape(layerId = "mapBoundsFrame")
  }

  map
}


#' Show a popup at the given location
#'
#' @param dat dat contains data to show
#' @param id id of what to show
#' @param lat lat for popup
#' @param lng lng for popup
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
