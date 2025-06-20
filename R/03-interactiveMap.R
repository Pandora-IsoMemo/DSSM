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
    # not using direct input values but prepared values for positions
    leafletProxy("map") %>%
      drawIcons(
        scalePosition = leafletValues()$scalePosition,
        scaleSize = leafletValues()$scaleSize,
        northArrowPosition = leafletValues()$northArrowPosition,
        northArrowSize = leafletValues()$northArrowSize
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

addNorthArrow <- function(map, position, layerId = NULL, height = 80, width = 80) {
  addControl(
    map,
    tags$img(
      src = "https://isomemodb.com/NorthArrow.png",
      width = as.character(width),
      height = as.character(height)
    ),
    position = position,
    layerId = layerId,
    className = ""
  )
}

# Draw Icons on Interactive Map
# @param map leaflet map
# @param northArrowPosition position of north arrow
# @param scalePosition position of scale
drawIcons <- function(map,
                      northArrowPosition = "none",
                      northArrowSize = 80,
                      scalePosition = "none",
                      scaleSize = 100) {
  if (!is.null(northArrowPosition) && northArrowPosition %in% c("bottomright", "bottomleft")) {
    if (!is.null(scalePosition) && scalePosition != "none") {
      map <- addScaleBar(map,
                         position = scalePosition,
                         options = scaleBarOptions(maxWidth = scaleSize))
    } else {
      map <- map %>%
        removeScaleBar()
    }

    if (!is.null(northArrowPosition) && northArrowPosition != "none") {
      #map <- map %>% leaflet.extras2::addNorthArrow(layerId = "northArrowIcon", position = northArrowPosition, height = 50, width = 50)
      map <- map %>%
        addNorthArrow(position = northArrowPosition,
                      layerId = "northArrowIcon",
                      height = northArrowSize,
                      width = northArrowSize)
    } else {
      map <- map %>% removeControl("northArrowIcon")
    }
  } else {
    if (!is.null(northArrowPosition) && northArrowPosition %in% c("topright", "topleft")) {
      #map <- map %>% leaflet.extras2::addNorthArrow(layerId = "northArrowIcon", position = northArrowPosition, height = 50, width = 50)
      map <- map %>%
        addNorthArrow(position = northArrowPosition,
                      layerId = "northArrowIcon",
                      height = northArrowSize,
                      width = northArrowSize)
    } else {
      map <- map %>% removeControl("northArrowIcon")
    }

    if (!is.null(scalePosition) && scalePosition != "none") {
      map <- addScaleBar(map,
                         position = scalePosition,
                         options = scaleBarOptions(maxWidth = scaleSize))
    } else {
      map <- map %>%
        removeScaleBar()
    }
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
