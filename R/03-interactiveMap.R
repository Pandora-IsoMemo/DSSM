#' ui function of interactiveMap module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
interactiveMapUI <- function(id, title = "") {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    useShinyjs(),
    div(class = "map-container",
        leafletOutput(
          ns("map"), width = "100%", height = "100%"
        )),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      div(
        id = "show_checkbox",
        style = "top:60px; left:60px; z-index: 9999; position:fixed",
        checkboxInput(ns("show_panel"), "Show Panels", value = TRUE)
      )
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      div(
        id = "stats_panel",  # ID to control visibility
        absolutePanel(
          tags$script(
            paste0(
              '
            $(document).on("shiny:visualchange", function(e) {
              let box = document.querySelector("#',
              ns('map'),
              '");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("',
              ns('map_width'),
              '", width);
              Shiny.setInputValue("',
              ns('map_height'),
              '", height);
            });
            $(window).resize(function(e) {
              let box = document.querySelector("#',
              ns('map'),
              '");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("',
              ns('map_width'),
              '", width);
              Shiny.setInputValue("',
              ns('map_height'),
              '", height);
            });
          '
            )
          ),
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 110,
          left = "auto",
          right = 40,
          bottom = "auto",
          width = 330,
          height = "auto",
          h2("Statistics"),
          selectizeInput(
            ns("var1"),
            "Variable 1",
            choices = character(0),
            options = list(allowEmptyOption = TRUE)
          ),
          selectizeInput(
            ns("var2"),
            "Variable 2",
            choices = character(0),
            options = list(allowEmptyOption = TRUE)
          ),
          div(
            id = "stats-sidebar-container",
            sidebarPlotOutput(ns("plot1"),
                              condition = paste0("input['", ns("var1"), "'] != ''")),
            sidebarPlotOutput(ns("plot2"),
                              condition = paste0("input['", ns("var2"), "'] != ''")),
            sidebarPlotOutput(
              ns("plot3"),
              condition = paste0(
                "input['",
                ns("var1"),
                "'] != '' && input['",
                ns("var2"),
                "'] != ''"
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      div(
        id = "leaflet_panel",  # ID to control visibility
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          draggable = TRUE,
          top = 110,
          right = "auto",
          left = 60,
          bottom = "auto",
          style = "position:fixed; width:330px; overflow-y:auto; height:85%",
          leafletSettingsUI(ns("mapSettings"), "Map Settings"),
          leafletPointSettingsUI(ns("mapPointSettings")),
          leafletExportButton(ns("exportLeaflet"))
        )
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
interactiveMap <- function(input, output, session, isoData) {
  ns <- session$ns

  observe({
    if (input$show_panel) {
      shinyjs::show("leaflet_panel", asis = TRUE)
      shinyjs::show("stats_panel", asis = TRUE)
    } else {
      shinyjs::hide("leaflet_panel", asis = TRUE)
      shinyjs::hide("stats_panel", asis = TRUE)
    }
  }) %>%
    bindEvent(input$show_panel)

  leafletValues <- callModule(leafletSettings, "mapSettings")

  leafletPointValues <-
    leafletPointSettingsServer("mapPointSettings", loadedData = isoData)

  # Create the map
  leafletMap <- reactiveVal({
    leaflet() %>%
      setView(lng = defaultCenter()$lng,
              lat = defaultCenter()$lat,
              zoom = 4) %>%
      addProviderTiles("CartoDB.Positron")
  })

  # newZoom <- reactive({
  #   if (is.null(input$map_zoom))
  #     4
  #   else
  #     input$map_zoom
  # })
  #
  #zoomSlow <- newZoom %>% debounce(1000)
  #zoomSlow <- newZoom %>% throttle(1000)

  # render output map ####
  output$map <- renderLeaflet({
    req(leafletMap())

    isolate({
      if (is.null(isoData())) {
        leafletMap()
      } else {
        withProgress({
        # add data to new map with given (e.g. default) point values
        leafletMap() %>%
          updateDataOnLeafletMap(isoData = isoData(),
                                 leafletPointValues = leafletPointValues)
        }, min = 0, max = 1, value = 0.8, message = "Updating map ...")
      }
    })
  })


  # adjust the zoom
  observeEvent(leafletValues()$bounds, {
    req(leafletValues()$bounds)
    # not exact bounds, only fit to input$map_bounds
    leafletProxy("map") %>%
      fitBounds(
        lng1 = leafletValues()$bounds$west,
        lng2 = leafletValues()$bounds$east,
        lat1 = leafletValues()$bounds$south,
        lat2 = leafletValues()$bounds$north
      )
  })

  # adjust map type
  observe({
    leafletProxy("map") %>%
      addProviderTiles(provider = input[["mapSettings-LeafletType"]])
  }) %>%
    bindEvent(input[["mapSettings-LeafletType"]])


  # add icons to map
  observe({
    leafletProxy("map") %>%
      drawIcons(
        zoom = input$map_zoom,
        scalePosition = leafletValues()$scalePosition,
        scaleSize = leafletValues()$scaleSize,
        scaleLng = leafletValues()$`scale-lng`,
        scaleLat = leafletValues()$`scale-lat`,
        northArrowPosition = leafletValues()$northArrowPosition,
        northArrowSize = leafletValues()$northArrowSize,
        northArrowLng = leafletValues()$`northArrow-lng`,
        northArrowLat = leafletValues()$`northArrow-lat`
      )
  })


  # draw/hide a square at bounds
  observeEvent(list(leafletValues()$showBounds, leafletValues()$bounds), {
    # not using direct input values but prepared values for bounds
    req(leafletValues()$bounds)

    leafletProxy("map") %>%
      drawFittedBounds(showBounds = leafletValues()$showBounds,
                       bounds = leafletValues()$bounds)
  })


  # adjust map center
  observe({
    center <- defaultCenter(centerType = input[["mapPointSettings-leafletCenter"]]) %>%
      shiftCenter(centerType = input[["mapPointSettings-leafletCenter"]],
                  isoData = isoData())
    leafletProxy("map") %>%
      setView(lng = center$lng,
              lat = center$lat,
              zoom = input$map_zoom)
  }) %>%
    bindEvent(input[["mapPointSettings-leafletCenter"]])


  # Update data ----
  observe({
    req(isoData(), isoData()[["latitude"]], isoData()[["longitude"]],
        input$map_width > 0, input$map_height > 0)

    withProgress({
      leafletProxy("map") %>%
        updateDataOnLeafletMap(isoData = isoData(), leafletPointValues = leafletPointValues)
    }, min = 0, max = 1, value = 0.8, message = "Plotting points ...")
  })

  # show / hide legend ----
  observe({
    req(isoData(), isoData()[["latitude"]], isoData()[["longitude"]],
        input$map_width > 0, input$map_height > 0,
        !is.null(leafletPointValues$showColourLegend),
        !is.null(leafletPointValues$pointColourPalette))

    leafletProxy("map") %>%
      setColorLegend(
        showLegend = leafletPointValues$showColourLegend,
        title = leafletPointValues$columnForPointColour,
        pal = leafletPointValues$pointColourPalette,
        values = getColourCol(isoData(),
                              colName = leafletPointValues$columnForPointColour)
      )
  })

  observe({
    req(isoData(), isoData()[["latitude"]], isoData()[["longitude"]],
        input$map_width > 0, input$map_height > 0,
        !is.null(leafletPointValues$showSizeLegend),
        !is.null(leafletPointValues$sizeLegendValues))

    leafletProxy("map") %>%
      setSizeLegend(
        sizeLegend = leafletPointValues$sizeLegendValues,
        showLegend = leafletPointValues$showSizeLegend
      )
  })

  observe({
    req(isoData(), isoData()[["latitude"]], isoData()[["longitude"]],
        input$map_width > 0, input$map_height > 0,
        !is.null(leafletPointValues$showSymbolLegend),
        !is.null(leafletPointValues$symbolLegendValues))

    leafletProxy("map") %>%
      setSymbolLegend(
        symbolLegend = leafletPointValues$symbolLegendValues,
        showLegend = leafletPointValues$showSymbolLegend
      )
  })

  # When map is clicked, show a popup with info
  observe({
    req(input$map_shape_click)
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showIDPopup(isoData(), event$id, event$lat, event$lng)
    })
  })

  # Show Histograms and Scatterplot in Sidebar
  observe({
    req(isoData())
    numVars <- unlist(lapply(names(isoData()), function(x) {
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

  # export leaflet ----
  callModule(
    leafletExport,
    "exportLeaflet",
    leafletMap = leafletMap,
    width = reactive(input$map_width),
    height = reactive(input$map_height),
    zoom = reactive(input$map_zoom),
    center = reactive(input$map_center),
    isoData = isoData,
    leafletValues = leafletValues,
    leafletPointValues = leafletPointValues
  )

  callModule(sidebarPlot,
             "plot1",
             x = var1,
             nameX = reactive(input$var1))
  callModule(sidebarPlot,
             "plot2",
             x = var2,
             nameX = reactive(input$var2))
  callModule(
    sidebarPlot,
    "plot3",
    x = var1,
    y = var2,
    nameX = reactive(input$var1),
    nameY = reactive(input$var2)
  )
}


# helper functions ####

shiftCenter <- function(center, centerType, isoData) {
  if (length(isoData) == 0 || is.null(centerType) || centerType != "data") return(center)

  center = list(
    lng = isoData$longitude %>%
      range() %>%
      mean(),
    lat = isoData$latitude %>%
      range() %>%
      mean()
  )

  return(center)
}

#'  draw Interactive Map
#' @param isoData isoData data
#' @param zoom zoom
#' @param type map type
#' @param northArrowPosition position of north arrow
#' @param scalePosition position of scale
#' @param center where to center map (list of lat and lng)
#' @param bounds map bounds (list of north, south, east, west)
#'
#' @export
draw <- function(isoData,
                 zoom = 5,
                 type = "1",
                 northArrowPosition = "none",
                 scalePosition = "none",
                 center = NULL,
                 bounds = NULL) {
  map <- leaflet() %>% drawType(type = type)
  map <-
    map %>% drawIcons(
      zoom = zoom,
      northArrowPosition = northArrowPosition,
      scalePosition = scalePosition
    )

  if (!is.null(center)) {
    map <- map %>% setView(lng = center$lng,
                           lat = center$lat,
                           zoom = zoom)
  }

  if (!is.null(bounds)) {
    map <- map %>% fitBounds(
      lng1 = bounds$west,
      lng2 = bounds$east,
      lat1 = bounds$south,
      lat2 = bounds$north
    )
  }

  map <-
    map %>% addCirclesRelativeToZoom(isoData, newZoom = zoom, zoom = zoom)
}


# Draw Type of Interactive Map
# @param map leaflet map
# @param type map type
drawType <- function(map, type = "1") {
  if (type == "1") {
    mType <- "CartoDB.Positron"
  }
  if (type == "2") {
    mType <- "OpenStreetMap.Mapnik"
  }
  if (type == "3") {
    mType <- "OpenStreetMap.DE"
  }
  if (type == "4") {
    mType <- "OpenTopoMap"
  }
  if (type == "5") {
    mType <- "Stamen.TonerLite"
  }
  if (type == "5") {
    mType <-  "Esri"
  }
  if (type == "6") {
    mType <- "Esri.WorldTopoMap"
  }
  if (type == "7") {
    mType <-  "Esri.WorldImagery"
  }

  map <- map %>%
    addProviderTiles(mType)

  map
}

getBestScaleValue <- function(max_km) {
  nice_values <- c(10000, 5000, 2000, 1000, 500, 200, 100, 50, 25, 10, 5, 2, 1, 0.5)
  max(nice_values[nice_values <= max_km])
}

addScaleBarAt <- function(map, lat, lng, zoom, size = 100, value = NULL, color = "black", weight = 1, opacity = 1, group = "customScaleBar") {
  if (!is.null(size) && is.numeric(size) && size > 0) {
    # Estimate meters per pixel at current zoom & latitude
    m_per_px <- 156543.03392 * cos(lat * pi / 180) / (2 ^ zoom)
    max_km <- size * m_per_px / 1000  # convert to km

    value <- getBestScaleValue(max_km)
  }

  # Determine default numeric value from zoom level
  if (is.null(value)) {
    value <- switch(as.character(zoom),
                    "1" = 1000, "2" = 1000, "3" = 750,
                    "4" = 500, "5" = 200, "6" = 100, "7" = 50,
                    "8" = 25, "9" = 10, "10" = 5, "11" = 2,
                    "12" = 1, "13" = 1, "14" = 0.5, "15" = 0.2,
                    "16" = 0.1, "17" = 0.1, "18" = 0.05,
                    1
    )
  }

  # Vertical spacing for ticks and labels
  spacing <- switch(as.character(zoom),
                    "1" = 1.5, "2" = 1, "3" = 0.6,
                    "4" = 0.3, "5" = 0.2, "6" = 0.1, "7" = 0.08,
                    "8" = 0.05, "9" = 0.03, "10" = 0.02, "11" = 0.01,
                    "12" = 0.008, "13" = 0.005, "14" = 0.003,
                    "15" = 0.002, "16" = 0.001, "17" = 0.0008,
                    "18" = 0.0005, 0.002
  )

  # Convert scale lengths to degrees longitude
  km_m <- value * 1000
  mi_m <- value * 1609.344

  # # approximation of meters per degree longitude at given latitude
  # # this slightly diverges from Leaflets Mercator projection
  # m_per_deg_long <- 111320 * cos(lat * pi / 180)
  # delta_lng_km <- km_m / m_per_deg_long
  # delta_lng_mi <- mi_m / m_per_deg_long

  # Leaflet's projection uses Mercator, so we can use geosphere to calculate the destination point
  delta_lng_km <- geosphere::destPoint(c(lng, lat), 90, value * 1000)[1] - lng
  delta_lng_mi <- geosphere::destPoint(c(lng, lat), 90, value * 1609.344)[1] - lng

  # Helper to draw background rectangle
  drawBackground <- function(map, lng2, lat1, lat2) {
    addPolygons(map,
                lng = c(lng, lng2, lng2, lng),
                lat = c(lat1, lat1, lat2, lat2),
                fillColor = "white", fillOpacity = 0.5,
                stroke = FALSE, smoothFactor = 0,
                group = group
    )
  }

  # Backgrounds
  map <- map %>%
    drawBackground(lng + delta_lng_mi, lat, lat - spacing * 4) %>%
    drawBackground(lng + delta_lng_km, lat, lat + spacing * 4)

  # Horizontal lines (both at lat)
  map <- map %>%
    addPolylines(lng = c(lng, lng + delta_lng_km), lat = c(lat, lat),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = c(lng, lng + delta_lng_mi), lat = c(lat, lat),
                 color = color, weight = weight, opacity = opacity, group = group)

  # Vertical ticks
  map <- map %>%
    addPolylines(lng = c(lng, lng),
                 lat = c(lat - spacing * 4, lat + spacing * 4),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = rep(lng + delta_lng_km, 2),
                 lat = c(lat, lat + spacing * 4),
                 color = color, weight = weight, opacity = opacity, group = group) %>%
    addPolylines(lng = rep(lng + delta_lng_mi, 2),
                 lat = c(lat - spacing * 4, lat),
                 color = color, weight = weight, opacity = opacity, group = group)

  # Invisible icon for labels
  emptyIcon <- icons(
    iconUrl = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8Xw8AApMBgVQe57oAAAAASUVORK5CYII=",
    iconWidth = 1, iconHeight = 1
  )

  # Labels (centered)
  map <- map %>%
    addMarkers(lng = lng + delta_lng_km / 2,
               lat = lat + spacing * 2,
               label = paste(value, "km"),
               labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE),
               icon = emptyIcon, group = group) %>%
    addMarkers(lng = lng + delta_lng_mi / 2,
               lat = lat - spacing * 3,
               label = paste(value, "mi"),
               labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE),
               icon = emptyIcon, group = group)

  return(map)
}



setScaleBar <- function(map, zoom, position = "none", size = 100, positionCoords = c()) {
  # reset scale bar
  map <- map %>%
    removeScaleBar() %>%
    clearGroup(group = "customScaleBar")

  if (is.null(position) || position == "none") {
    return(map)
  }

  if (position %in% c("topright", "topleft", "bottomright", "bottomleft")) {
    map <- addScaleBar(map,
                       position = position,
                       options = scaleBarOptions(maxWidth = size))
    return(map)
  }

  if (position == "custom" &&
      "lng" %in% names(positionCoords) &&
      "lat" %in% names(positionCoords)) {
    map <- map %>%
      addScaleBarAt(lat = positionCoords[["lat"]],
                    lng = positionCoords[["lng"]],
                    zoom = zoom,
                    size = size)
  }

  return(map)
}


setNorthArrow <- function(map, position, layerId = NULL, height = 80, width = 80, positionCoords = c()) {
  # reset layerId
  map <- map %>%
    removeControl("northArrowIcon") %>%
    removeMarker("northArrowIcon")

  if (is.null(position) ||
      position == "none" ||
      (position == "custom" &&
       !("lng" %in% names(positionCoords) && "lat" %in% names(positionCoords)))
      ) return(map)

  if (position %in% c("topright", "topleft", "bottomright", "bottomleft")) {
    map <- map %>%
      addControl(
        tags$img(
          #src = "NorthArrow.png",
          src = "https://isomemodb.com/NorthArrow.png",
          width = as.character(width),
          height = as.character(height)
        ),
        position = position,
        layerId = layerId,
        className = ""
      )
    return(map)
  }

  if (position == "custom" &&
      "lng" %in% names(positionCoords) &&
      "lat" %in% names(positionCoords)) {
    map <- map %>%
      addMarkers(
        lng = positionCoords[["lng"]],
        lat = positionCoords[["lat"]],
        layerId = layerId,
        icon = icons(
          #iconUrl = "NorthArrow.png",
          iconUrl = "https://isomemodb.com/NorthArrow.png",
          iconWidth = width,
          iconHeight = height
        ))
    return(map)
  }

  return(map)
}

getCoordsIfCustom <- function(position, lng = NA, lat = NA) {
  if (position == "custom" && !is.na(lng) && !is.na(lat)) {
    return(c(lng = lng, lat = lat))
  }
  return(c())
}

# Draw Icons on Interactive Map
# @param map leaflet map
# @param northArrowPosition position of north arrow
# @param scalePosition position of scale
drawIcons <- function(map,
                      zoom,
                      scalePosition = "none",
                      scaleSize = 100,
                      scaleLng = NA,
                      scaleLat = NA,
                      northArrowPosition = "none",
                      northArrowSize = 80,
                      northArrowLng = NA,
                      northArrowLat = NA) {
  # Check if north arrow position is valid
  if (is.null(northArrowPosition)) {
    return(map)
  }

  # Prepare coordinates if using custom positioning
  northArrowCoords <- getCoordsIfCustom(northArrowPosition, northArrowLng, northArrowLat)
  scaleCoords <- getCoordsIfCustom(scalePosition, scaleLng, scaleLat)

  if (northArrowPosition %in% c("bottomright", "bottomleft")) {
    map <- map %>%
      setScaleBar(zoom = zoom,
                  position = scalePosition,
                  size = scaleSize,
                  positionCoords = scaleCoords
                  ) %>%
      setNorthArrow(position = northArrowPosition,
                    layerId = "northArrowIcon",
                    height = northArrowSize,
                    width = northArrowSize,
                    positionCoords = northArrowCoords)
  } else {
    map <- map %>%
      setNorthArrow(position = northArrowPosition,
                    layerId = "northArrowIcon",
                    height = northArrowSize,
                    width = northArrowSize,
                    positionCoords = northArrowCoords) %>%
      setScaleBar(zoom = zoom,
                  position = scalePosition,
                  size = scaleSize,
                  positionCoords = scaleCoords
                  )
  }

  map
}

addCirclesRelativeToZoom <-
  function(map, isoData, newZoom, zoom = 5) {
    relateToZoom <- function(radius) {
      radius * (zoom / newZoom) ^ 3
    }

    if (is.null(isoData$latitude) || all(is.na(isoData$latitude))) return(map)

    isoData <- isoData[(!is.na(isoData$longitude) & !is.na(isoData$latitude)), ]
    map <- map %>%
      cleanDataFromMap()

    if (!is.null(isoData$Latitude_jit)) isoData$latitude <- isoData$Latitude_jit
    if (!is.null(isoData$Longitude_jit)) isoData$longitude <- isoData$Longitude_jit

    colors <- appColors(c("red", "green", "purple", "black"),
                        names = FALSE)[1:length(unique(isoData$source))]

    drawCirclesOnMap(
      map = map,
      isoData = isoData,
      pointRadius = relateToZoom(radius = 20),
      colourPal = colorFactor(
        palette = colors,
        domain = isoData$source
      ),
      columnForColour = "source"
    )
  }


# Show a popup at the given location
#
# @param dat dat contains data to show
# @param id id of what to show
# @param lat lat for popup
# @param lng lng for popup
showIDPopup <- function(dat, id, lat, lng) {
  selectedId <- dat[which(dat$id == id), ]

  popup <- paste(names(selectedId),
                 unlist(lapply(selectedId, as.character)),
                 sep = ": ",
                 collapse = "</br>")

  leafletProxy("map") %>%
    addPopups(lng,
              lat,
              popup,
              layerId = id,
              options = popupOptions(maxHeight = 200))

}
