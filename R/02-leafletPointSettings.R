#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    radioButtons(ns("leafletCenter"),
                 "Map Centering",
                 choices = c("Atlantic" = "atlantic", "Pacific" = "pacific", "Data" = "data"),
                 selected = "atlantic",
                 inline = TRUE),
    checkboxInput(ns("clusterPoints"), "Cluster points"),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      ns = ns,
      tags$hr(),
      fluidRow(
        column(8, tags$h4("Point Settings")),
        column(4,
               align = "right",
               actionButton(ns("applyPointSettings"), "Apply"))
      ),
      fluidRow(column(
        8,
        checkboxInput(ns("useJitter"), "Use jitter (in max. km)")
      ),
      column(
        4,
        #style = "margin-top: -1em;",
        conditionalPanel(
          condition = "input.useJitter == true",
          numericInput(
            ns("jitterMaxKm"),
            label = NULL,
            value = 25,
            min = 0,
            max = 100
          ),
          ns = ns
        )
      )),
      sliderInput(
        ns("pointOpacity"),
        "Opacity",
        value = 0.7,
        min = 0,
        max = 1,
        step = 0.1
      ),
      # show legend pickerInput ...
      pointColourUI(ns("pointColor")),
      pointSizeUI(ns("pointSize")),
      pointSymbolUI(ns("pointSymbol")),
      tags$hr()
    )
  )
}

#' server funtion of leaflet point settings module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param loadedData (reactive) loaded data
leafletPointSettingsServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 values <- reactiveValues()

                 # following parameters are used in updateDataOnLeafletMap()
                 observe({
                   values$leafletCenter <- input$leafletCenter
                 })

                 observe({
                   values$clusterPoints <- input$clusterPoints
                 })

                 pointColorVals <-
                   pointColourServer("pointColor", loadedData, apply = reactive(input$applyPointSettings))
                 observe({
                   for (i in names(pointColorVals)) {
                     values[[i]] <- pointColorVals[[i]]
                   }
                 })

                 pointSizeVals <-
                   pointSizeServer("pointSize", loadedData, apply = reactive(input$applyPointSettings))
                 observe({
                   for (i in names(pointSizeVals)) {
                     values[[i]] <- pointSizeVals[[i]]
                   }
                 })

                 pointSymbolVals <-
                   pointSymbolServer("pointSymbol", loadedData, apply = reactive(input$applyPointSettings))
                 observe({
                   for (i in names(pointSymbolVals)) {
                     values[[i]] <- pointSymbolVals[[i]]
                   }
                 })

                 observe({
                   values$pointOpacity <- input$pointOpacity
                   values$jitterMaxKm <- ifelse(input$useJitter,
                                                input$jitterMaxKm,
                                                NA_real_)
                 }) %>%
                   bindEvent(input$applyPointSettings, ignoreNULL = FALSE)

                 values
               })
}


pointAestheticUI <- function(id) {
  capitalize <- function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  }

  ns <- NS(id)
  aesthetic <- sub(".*-", "", id) %>% capitalize()

  tagList(
    fluidRow(
      column(8,
             checkboxInput(ns("fixed"), sprintf("Fixed %s", aesthetic), value = TRUE),
             conditionalPanel(
               ns = ns,
               condition = "input.fixed == false",
               selectInput(
                 ns("column"),
                 sprintf("Point %s Variable", aesthetic),
                 choices = c("Select a column ..." = "")
               )
             )),
      column(4, checkboxInput(
        ns("legend"), "Legend", value = FALSE
      ))
    ),

  )
}


pointAesteticsServer <- function(id, loadedData, onlyNumCols = FALSE, onlyFacCols = FALSE) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   logDebug("%s: Update loadedData()", id)

                   data <- loadedData()
                   if (is.null(data)) {
                     choices <- c("No data ..." = "")
                   } else {
                     if (onlyNumCols) {
                       cols <- numericColumns(data)
                       label <- if (length(cols) == 0) "No numeric columns ..." else "Select a numeric column ..."
                     } else if (onlyFacCols) {
                       cols <- factorColumns(data)
                       label <- if (length(cols) == 0) "No character columns ..." else "Select a character column ..."
                     } else {
                       cols <- colnames(data)
                       label <- "Select a column ..."
                     }
                     choices <- c(label = "", cols)
                   }

                   updateSelectInput(
                     session = session,
                     "column",
                     choices = choices,
                     selected = ""
                   )
                   updateCheckboxInput(session = session,
                                       "legend",
                                       value = FALSE)
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   logDebug("%s: Update input$column", id)

                   if (input$fixed) {
                     updateSelectInput(session = session, "column", selected = "")
                   }
                 }) %>%
                   bindEvent(input$fixed)
               })
}

#' ui function of leaflet point settings module
#'
#' @param id namespace
pointColourUI <- function(id) {
  ns <- NS(id)

  colourPalettes <- leaflet_point_colours()

  tagList(
    pointAestheticUI(ns("colour")),
    conditionalPanel(
      ns = ns,
      condition = "input['colour-fixed'] == false",
      fluidRow(
        column(8,
               selectInput(
                 ns("paletteName"),
                 label = NULL,
                 choices = colourPalettes,
                 selected = "Dark2"
               )),
        column(4,
               style = "margin-top: -0.25em;",
               checkboxInput(
                 ns("isReversePalette"), "Reverse", value = FALSE
               ))
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input['colour-fixed'] == true",
      fluidRow(column(8, colourInput(
        ns("fixedPointColour"),
        "Fixed colour",
        value = "#459778"
      )))
    )
  )
}


#' server funtion of leaflet point settings module
#'
#' @param apply (reactive) apply button input
#' @inheritParams leafletPointSettingsServer
pointColourServer <- function(id, loadedData, apply) {
  moduleServer(id,
               function(input, output, session) {
                 # use reactiveValues to pass values only after "apply"
                 colourValues <- reactiveValues()

                 pointAesteticsServer("colour", loadedData)

                 observe({
                   logDebug("%s: Initialize after loadedData()", id)
                   colourValues$columnForPointColour <- ""
                   colourValues$showColourLegend <- TRUE
                   colourValues$pointColourPalette <- getColourCol(loadedData(), colName = "") %>%
                     getColourPal(paletteName = input$fixedPointColour,
                                  isReversePalette = input$isReversePalette)
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   logDebug("%s: Update colourValues", id)
                   colourValues$showColourLegend <- input[["colour-legend"]]

                   if (is.null(input[["colour-column"]]) || input[["colour-column"]] == "") {
                     colourValues$columnForPointColour <- ""
                     paletteName <- input$fixedPointColour
                   } else {
                     colourValues$columnForPointColour <- input[["colour-column"]]
                     paletteName <- input$paletteName
                   }

                   colourValues$pointColourPalette <- getColourCol(
                     loadedData(),
                     colName = input[["colour-column"]]
                   ) %>%
                     getColourPal(paletteName = paletteName,
                                  isReversePalette = input$isReversePalette)
                 }) %>%
                   bindEvent(apply())

                 return(colourValues)
               })
}


#' ui function of leaflet point settings module
#'
#' @param id namespace
pointSizeUI <- function(id) {
  ns <- NS(id)

  tagList(
    pointAestheticUI(ns("size")),
    sliderInput(
      ns("sizeFactor"),
      "Size Factor",
      min = 0.5,
      max = 5.5,
      value = 1,
      step = 0.5
    )
  )
}


#' server funtion of leaflet point settings module
#'
#' @param apply (reactive) apply button input
#' @inheritParams leafletPointSettingsServer
pointSizeServer <- function(id, loadedData, apply) {
  moduleServer(id,
               function(input, output, session) {
                 # use reactiveValues to pass values only after "apply"
                 sizeValues <- reactiveValues()

                 pointAesteticsServer("size", loadedData, onlyNumCols = TRUE)

                 observe({
                   logDebug("%s: Initialize after loadedData()", id)
                   radiusAndLegend <- getPointSize(
                     df = loadedData(),
                     columnForPointSize = "",
                     sizeFactor = input$sizeFactor
                   )
                   sizeValues$pointRadius <-
                     radiusAndLegend$pointSizes
                   sizeValues$sizeLegendValues <-
                     radiusAndLegend$sizeLegendValues
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   req(loadedData())
                   radiusAndLegend <- getPointSize(
                     df = loadedData(),
                     columnForPointSize = input[["size-column"]],
                     sizeFactor = input$sizeFactor
                   )
                   sizeValues$pointRadius <-
                     radiusAndLegend$pointSizes
                   sizeValues$sizeLegendValues <-
                     radiusAndLegend$sizeLegendValues
                 }) %>%
                   bindEvent(apply(), ignoreInit = TRUE)

                 observe({
                   sizeValues$showSizeLegend <- input[["size-legend"]]
                 }) %>%
                   bindEvent(apply())

                 return(sizeValues)
               })
}


#' ui function of leaflet point symbol settings module
#'
#' @param id namespace
pointSymbolUI <- function(id) {
  ns <- NS(id)

  tagList(
    pointAestheticUI(ns("symbol")),
    fluidRow(
      column(8,
             pickerInput(
               ns("pointSymbol"),
               label = "Point symbol",
               choices = pchChoices(),
               selected = "",
               options = list(
                 `actions-box` = TRUE,
                 size = 25,
                 `selected-text-format` = "count > 8",
                 `none-selected-text` = "Select symbols ..."
               ),
               multiple = TRUE
             )
      ),
      column(4,
             numericInput(
               ns("pointWidth"),
               "Line width",
               value = 1,
               min = 1,
               max = 10
             )
      )
    )
  )
}


#' server function of leaflet point symbol settings module
#'
#' @param apply (reactive) apply button input
#' @inheritParams leafletPointSettingsServer
pointSymbolServer <- function(id, loadedData, apply) {
  moduleServer(id,
               function(input, output, session) {
                 # use reactiveValues to pass values only after "apply"
                 symbolValues <- reactiveValues(pointSymbol = 19)

                 pointAesteticsServer("symbol", loadedData, onlyFacCols = TRUE)

                 observe({
                   logDebug("%s: Initialize after loadedData()", id)

                   updatePickerInput(session = session,
                                     "pointSymbol",
                                     selected = 19)

                   symbolsAndLegend <- getPointSymbols(
                     df = loadedData(),
                     columnForPointSymbol = "",
                     symbols = 19
                   )
                   symbolValues$pointSymbol <-
                     symbolsAndLegend$pointSymbol
                   symbolValues$symbolLegendValues <-
                     symbolsAndLegend$symbolLegendValues

                   symbolValues$columnForPointSymbol <- ""
                   symbolValues$showSymbolLegend <- input[["symbol-legend"]]
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   req(loadedData(), input$pointSymbol)
                   symbolsAndLegend <- getPointSymbols(
                     df = loadedData(),
                     columnForPointSymbol = input[["symbol-column"]],
                     symbols = as.numeric(input$pointSymbol)
                   )
                   symbolValues$pointSymbol <-
                     symbolsAndLegend$pointSymbol
                   symbolValues$symbolLegendValues <-
                     symbolsAndLegend$symbolLegendValues

                   symbolValues$columnForPointSymbol <- input[["symbol-column"]]
                   symbolValues$showSymbolLegend <- input[["symbol-legend"]]
                   symbolValues$pointWidth <- input$pointWidth
                 }) %>%
                   bindEvent(apply(), ignoreInit = TRUE)

                 return(symbolValues)
               })
}


# Helper functions ----

leaflet_point_colours <- function() {
  # using colours from: RColorBrewer::brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
  # adding full names manually
  list(
    `diverging palettes` = c(
      "Brown-BlueGreen" = "BrBG",
      "Pink-Green" = "PiYG",
      "Purple-Green" = "PRGn",
      "Orange-Purple" = "PuOr",
      "Red-Blue" = "RdBu",
      "Red-Yellow-Blue" = "RdYlBu"
    ),
    `qualitive palettes` = c(
      "Dark" = "Dark2",
      "Paired" = "Paired",
      "Set" = "Set2"
    ),
    `sequential palettes` = c(
      "Blue" = "Blues",
      "BlueGreen" = "BuGn",
      "BluePurple" = "BuPu",
      "GreenBlue" = "GnBu",
      "Green" = "Greens",
      "Grey" = "Greys",
      "Orange" = "Oranges",
      "OrangeRed" = "OrRd",
      "PurpleBlue" = "PuBu",
      "PurpleBlueGreen" = "PuBuGn",
      "PurpleRed" = "PuRd",
      "Purple" = "Purples",
      "RedPurple" = "RdPu",
      "Red" = "Reds",
      "YellowGreen" = "YlGn",
      "YellowGreenBlue" = "YlGnBu",
      "YellowOrangeBrown" = "YlOrBr",
      "YellowOrangeRed" = "YlOrRd"
    )
  )
}

#' Update Data On Map
#'
#' @param map reactive leaflet map object
#' @param isoData reactive isoData data
#' @param leafletPointValues reactive settings for points on map
updateDataOnLeafletMap <-
  function(map, isoData, leafletPointValues) {
    map <- map %>%
      cleanDataFromMap()

    if (is.null(isoData) ||
        is.null(isoData[["latitude"]]) ||
        all(is.na(isoData[["latitude"]])) ||
        is.null(isoData[["longitude"]]) ||
        all(is.na(isoData[["longitude"]])))
      return(map)

    isoData <-
      isoData[(!is.na(isoData[["longitude"]]) &
                 !is.na(isoData[["latitude"]])), ]

    if (nrow(isoData) == 0)
      return(map)

    isoData[["longitude"]] <- isoData[["longitude"]] %>%
        centerLongitudes(center = leafletPointValues$leafletCenter)

    if (leafletPointValues$clusterPoints) {
      return(drawClustersOnMap(map, isoData))
    }

    plotData <- setJitterCoords(isoData,
                                km = leafletPointValues$jitterMaxKm)

    if (!is.null(plotData$Latitude_jit))
      plotData$latitude <- plotData$Latitude_jit
    if (!is.null(plotData$Longitude_jit))
      plotData$longitude <- plotData$Longitude_jit

    map %>%
      drawSymbolsOnMap(
        plotData,
        pointRadius = leafletPointValues$pointRadius,
        colourPal = leafletPointValues$pointColourPalette,
        columnForColour = leafletPointValues$columnForPointColour,
        pointOpacity = leafletPointValues$pointOpacity,
        pointSymbol = leafletPointValues$pointSymbol,
        pointWidth = leafletPointValues$pointWidth
      ) %>%
      setColorLegend(
        showLegend = leafletPointValues$showColourLegend,
        title = leafletPointValues$columnForPointColour,
        pal = leafletPointValues$pointColourPalette,
        values = getColourCol(plotData,
                              colName = leafletPointValues$columnForPointColour)
      ) %>%
      setSizeLegend(
        sizeLegend = leafletPointValues$sizeLegendValues,
        showLegend = leafletPointValues$showSizeLegend
      ) %>%
      setSymbolLegend(
        symbolLegend = leafletPointValues$symbolLegendValues,
        showLegend = leafletPointValues$showSymbolLegend
      ) %>%
      addLayersControl(
        overlayGroups = c("Data Points"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      )
  }


drawClustersOnMap <- function(map, isoData) {
  map %>%
    addMarkers(
      data = isoData,
      lat = ~ latitude,
      lng =  ~ longitude,
      group = "dataPoints",
      clusterOptions = markerClusterOptions()
    )
}


setJitterCoords <- function(dat, km) {
  # no jitter should be used: km == NA
  if (is.na(km))
    return(dat)

  withProgress({
    set.seed(20180213)

    dat$Latitude_jit <-
      jitter_latlong(dat$latitude,
                     type = "lat",
                     dat$latitude,
                     km = km)
    dat$Longitude_jit <-
      jitter_latlong(dat$longitude,
                     type = "long",
                     dat$latitude,
                     km = km)
    dat
  },
  value = 0.9,
  message = 'Add jitter ...')
}


drawCirclesOnMap <-
  function(map,
           isoData,
           pointRadius,
           colourPal,
           columnForColour,
           pointOpacity) {
    if (is.null(colourPal) | is.null(pointRadius))
      return(map)

    map %>%
      addCircleMarkers(
        data = isoData,
        lat = ~ latitude,
        lng =  ~ longitude,
        group = "dataPoints",
        stroke = F,
        fillOpacity = pointOpacity,
        color = colourPal(isoData[[columnForColour]]),
        fillColor = colourPal(isoData[[columnForColour]]),
        radius = pointRadius
      )
  }


drawSymbolsOnMap <-
  function(map,
           isoData,
           pointRadius,
           colourPal,
           columnForColour,
           pointOpacity,
           pointSymbol,
           pointWidth) {
    if (is.null(colourPal) | is.null(pointRadius))
      return(map)

    # create colour for each point
    colourCol <- getColourCol(isoData, colName = columnForColour)
    colourList <-
      lapply(colourPal(colourCol), col2rgb)
    colourVec <- sapply(1:nrow(isoData), function(i) {
      rgb(
        red = colourList[[i]][1],
        green = colourList[[i]][2],
        blue = colourList[[i]][3],
        maxColorValue = 255,
        alpha = pointOpacity * 255
      )
    })

    # create icon for each point
    iconFiles <- sapply(1:nrow(isoData), function(x) {
      createPchPoints(
        pch = pointSymbol[[x]],
        # pointSymbol is a list, while others are vectors
        width = pointRadius[x] * 2,
        height = pointRadius[x] * 2,
        col = colourVec[x],
        lwd = pointWidth
      )
    })

    map %>%
      addMarkers(
        data = isoData,
        lat = ~ latitude,
        lng =  ~ longitude,
        group = "Data Points",
        icon = ~ icons(
          iconUrl = iconFiles,
          popupAnchorX = 20,
          popupAnchorY = 0
        )
      )
  }


cleanDataFromMap <- function(map) {
  map %>%
    clearGroup("Data Points") %>%
    clearMarkerClusters()
}

# Colour ----

#' Set Colour Legend
#'
#' @param map leaflet map
#' @param showLegend logical show/hide legend
#' @param title legend title
#' @param pal colour palette
#' @param values possible values that can be mapped, e.g. isoData$source
setColorLegend <- function(map, showLegend, title, pal, values) {
  if (showLegend && !is.null(pal) && !is.null(values)) {
    map <- map %>%
      addLegend("topleft",
                pal = pal,
                values = values,
                #title = title, # hide title here, since title is not available for addControl()
                layerId = "colorLegend")
  } else {
    map <- map %>% removeControl("colorLegend")
  }

  map
}

getColourPal <- function(colourCol, paletteName, isReversePalette) {
  if (is.null(colourCol)) return(NULL)

  if (is.numeric(colourCol)) {
    pal <- colorNumeric(
      palette = paletteName,
      domain = colourCol,
      reverse = isReversePalette
    )
    return(pal)
  }

  if (all(colourCol == "all")) {
    # if fixed colour was selected (which sets colourCol <- "all" for all values), then the
    # paletteName is set to a value from colourInput()
    pal <- colorFactor(
      palette = rep(paletteName, length(colourCol)),
      domain = colourCol,
      reverse = FALSE
    )

    return(pal)
  }

  # if colourCol contains different character values create colour from a palette
  pal <- colorFactor(
    palette = paletteName,
    domain = colourCol,
    reverse = isReversePalette
  )

  pal
}

getColourCol <- function(dat, colName) {
  if (is.null(colName) || is.null(dat)) return(NULL)

  colourCol <- dat[[colName]]
  if (is.null(colourCol)) {
    colourCol <- rep("all", nrow(dat))
  }

  colourCol
}

# Point Size ----

setSizeLegend <- function(map, showLegend, sizeLegend) {
  if (is.null(sizeLegend) || !showLegend) {
    map <- map %>%
      removeControl("sizeLegend")

    return(map)
  }

  htmlString <- getSizeLegend(sizeLegend)

  map %>%
    addControl(html = htmlString,
               position = "topleft",
               layerId = "sizeLegend")
}

#' Get Point Size
#'
#' Get point size in pixel
#'
#' @param df (data.frame) loaded data
#' @param columnForPointSize (character) name of the column that determines the point size
#' @param sizeFactor (numeric) general factor for point size
getPointSize <- function(df, columnForPointSize, sizeFactor = 1) {
  if (is.null(df) ||
      is.null(columnForPointSize) || is.null(sizeFactor))
    return(NULL)

  defaultPointSizeInPxl <- 5

  nPoints <- nrow(df)
  pointSizes <-
    rep(sizeFactor * defaultPointSizeInPxl, nPoints)

  if (columnForPointSize %in% c("",  "none"))
    return(list(pointSizes = pointSizes))

  sizeColumn <- df[, columnForPointSize] %>%
    as.numeric() %>%
    suppressWarnings()

  if (length(unique(na.omit(sizeColumn))) < 2)
    return(list(pointSizes = pointSizes))

  minSize <- min(sizeColumn, na.rm = TRUE)
  maxSize <- max(sizeColumn, na.rm = TRUE)

  pointSizes <- mapToPixel(
    val = sizeColumn,
    minVal = minSize,
    maxVal = maxSize,
    defaultPxlVal = defaultPointSizeInPxl,
    sizeFactor = sizeFactor
  )

  # get sizes for legend
  legendLabels <-
    seq(minSize, maxSize, by = (maxSize - minSize) / 3) %>%
    signif(digits = 2) %>%
    unique()
  legendValues <- mapToPixel(
    val = legendLabels,
    minVal = minSize,
    maxVal = maxSize,
    defaultPxlVal = defaultPointSizeInPxl,
    sizeFactor = sizeFactor
  )

  sizeLegendValues <- legendValues
  names(sizeLegendValues) <- legendLabels

  list(pointSizes = pointSizes, sizeLegendValues = sizeLegendValues)
}

mapToPixel <-
  function(val,
           minVal,
           maxVal,
           defaultPxlVal,
           sizeFactor) {
    # normalize sizes to intervall [0,1]
    pointSizes <- val %>%
      shiftToZero(minVal = minVal)

    pointSizes <- pointSizes / shiftToZero(maxVal, minVal = minVal)

    # map to intervall: [1/defaultPxlVal, 1-(1/defaultPxlVal)] instead of (0,1)
    # because the minimal factor should be at least 1/defaultPxlVal
    # smaller values produce errors in the plotting function
    pointSizes <-
      (1 - 2 / defaultPxlVal) * pointSizes + 1 / defaultPxlVal

    # the mean of the data (== 0.5) should have a factor of 1
    pointSizes <- 2 * pointSizes

    # give missing values zero factor
    pointSizes[is.na(pointSizes)] <- 0

    # multiply with default
    pointSizes * sizeFactor * defaultPxlVal
  }

shiftToZero <- function(val, minVal) {
  if (minVal >= 0) {
    val <- val - minVal
  } else {
    val <- val + abs(minVal)
  }

  return(val)
}

# Symbols ----

setSymbolLegend <- function(map, showLegend, symbolLegend) {
  if (is.null(symbolLegend) || !showLegend) {
    map <- map %>%
      removeControl("symbolLegend")

    return(map)
  }

  htmlString <- getSymbolLegend(symbolLegend)

  map %>%
    addControl(html = htmlString,
               position = "topleft",
               layerId = "symbolLegend")
}

# from: https://stackoverflow.com/questions/41372139/using-diamond-triangle-and-star-shapes-in-r-leaflet
#' Create PCH Points Vector
#'
#' @param pch plotting ‘character’, i.e., symbol to use. See graphics::points for details
#' @param width width in pixel
#' @param height height in pixel
#' @param bg initial background colour
#' @param col color code or name
#' @param ... Further graphical parameters that are passed to graphics::points()
createPchPointsVec <-
  function(pch = 16,
           width = 50,
           height = 50,
           bg = "transparent",
           col = "black",
           ...) {
    n = length(pch)
    files = character(n)
    # create a sequence of png images
    for (i in seq_len(n)) {
      f = tempfile(fileext = '.png')
      png(f,
          width = width,
          height = height,
          bg = bg)
      par(mar = c(0, 0, 0, 0))
      plot.new()
      points(
        .5,
        .5,
        pch = pch[i],
        col = col[i],
        cex = min(width, height) / 8,
        ...
      )
      dev.off()
      files[i] = f
    }
    files
  }

#' Create PCH Points
#'
#' @param pch plotting ‘character’, i.e., symbol to use. See graphics::points for details
#' @param width width in pixel
#' @param height height in pixel
#' @param bg initial background colour
#' @param col color code or name
#' @param tmpDir directory for storing the icons
#' @param pattern pattern to be used in filenames
#' @param ... Further graphical parameters that are passed to graphics::points()
createPchPoints <-
  function(pch = 16,
           width = 50,
           height = 50,
           bg = "transparent",
           col = "black",
           tmpDir = tempdir(),
           pattern = "symbolFile",
           ...) {
    file <-
      tempfile(pattern = pattern,
               fileext = '.png',
               tmpdir = tmpDir)

    png(
      file,
      width = max(width, 1),
      height = max(height, 1),
      bg = bg,
      units = "px"
    )
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(
      .5,
      .5,
      pch = pch,
      col = col,
      cex = min(width, height) / 8,
      ...
    )
    dev.off()
    file
  }

pchChoices <- function() {
  list(
    `simple symbols` = c(
      "square" = 0,
      "circle" = 1,
      "triangle point up" = 2,
      "plus" = 3,
      "cross" = 4,
      "diamond" = 5,
      "triangle point down" = 6
    ),
    `combined symbols` = c(
      "square cross" = 7,
      "star" = 8,
      "diamond plus" = 9,
      "circle plus" = 10,
      "triangles up and down" = 11,
      "square plus" = 12,
      "circle cross" = 13,
      "square and triangle down" = 14
    ),
    `filled symbols` = c(
      "filled square" = 15,
      "filled circle" = 16,
      "filled triangle point-up" = 17,
      "filled diamond" = 18,
      "solid circle" = 19,
      "bullet (smaller circle)" = 20
    )
  )
}

#' Get Point Size
#'
#' @param df (data.frame) loaded data
#' @param columnForPointSymbol (character) name of the column that determines the point symbol
#' @param symbols (numeric) selected symbols
getPointSymbols <-
  function(df,
           columnForPointSymbol,
           symbols = unlist(pchChoices())) {
    if (is.null(df) || is.null(columnForPointSymbol))
      return(NULL)

    # create default symbols
    pointSymbol <- 19

    if (length(symbols) > 0 && !any(symbols %in% c("",  "none"))) {
      pointSymbol <- symbols[1] %>%
        as.numeric()
    }

    symbolLegendValues <- c("all" = pointSymbol)

    # create a list of symbols, one symbol for each point
    # use list to enable different types of values, we need numeric and ""
    pointSymbols <- rep(pointSymbol, nrow(df)) %>%
      as.list()

    # create symbols based on columnForPointSymbol if there are more than one unique values
    if (!(columnForPointSymbol %in% c("",  "none"))) {
      symbolColumn <- df[, columnForPointSymbol] #%>%
      #as.numeric() %>%
      #suppressWarnings()

      uniqueValues <- unique(na.omit(symbolColumn))
      if (length(uniqueValues) > 1) {
        if (length(uniqueValues) > length(symbols)) {
          # add more symbols if not selected enough, repeat values to fill to full length if needed
          symbols <- pchChoices() %>%
            unlist() %>%
            orderBySelection(pchSel = symbols) %>%
            rep_len(length.out = length(uniqueValues))
        } else {
          # remove symbols if selected too many
          symbols <- symbols[1:length(uniqueValues)]
        }

        # overwrite default symbols based on factors from the symbolColumn
        pointSymbols <- symbols[symbolColumn %>% as.factor()] %>%
          as.numeric() %>%
          as.list()

        # hide missing values: pch == "" means no point is displayed
        pointSymbols[sapply(pointSymbols, is.na)] <- ""

        # create legend values
        names(symbols) <- symbolColumn %>% as.factor() %>% levels()
        symbolLegendValues <- symbols
      }
    }

    list(pointSymbols = pointSymbols, symbolLegendValues = symbolLegendValues)
  }

orderBySelection <-
  function(pchSel, pchAll = unlist(pchChoices())) {
    index <- match(pchSel, pchAll)
    c(pchAll[index], pchAll[-index])
  }


# get Legend HTML String ----
getSizeLegend <- function(sizeLegend) {
  path <- system.file("dist", package = "DSSM")

  # remove old icons: remove all files with the pattern "sizeFile"
  removeOldIcons(pattern = "sizeFile", path = path)

  # create icon for each point
  iconFiles <- sapply(sizeLegend, function(x) {
    createPchPoints(
      pch = 19,
      width = 2 * x,
      height = 2 * x,
      lwd = 1,
      tmpDir = path,
      pattern = "sizeFile"
    )
  })

  # create one html string over all used icons
  getHTMLFromPath(paths = iconFiles)
}

getSymbolLegend <- function(symbolLegend) {
  path <- system.file("dist", package = "DSSM")

  # remove old icons: remove all files with the pattern "symbolFile"
  removeOldIcons(pattern = "symbolFile", path = path)

  # create icon for each point
  iconFiles <- sapply(symbolLegend, function(x) {
    createPchPoints(
      pch = x,
      width = 10,
      height = 10,
      lwd = 1,
      tmpDir = path,
      pattern = "symbolFile"
    )
  })

  # create one html string over all used icons
  getHTMLFromPath(paths = iconFiles)
}

#' Remove Old Icons
#'
#' Remove files of previous icons
#'
#' @param pattern pattern of the files
#' @param path path to the folder that contains the files
removeOldIcons <- function(pattern, path = "www") {
  oldFiles <- dir(path)
  oldFiles <- oldFiles[grepl(pattern, oldFiles)]
  sapply(oldFiles, function(oldFile) {
    file.remove(file.path(path, oldFile))
  })
}

#' Get HTML from path
#'
#' Creates the HTML string that defines the legend
#'
#' @param paths named list of paths to the files containing an icon, names of the items are used as
#'  labels
getHTMLFromPath <- function(paths) {
  sapply(seq_along(paths), function(x) {
    label <- names(paths[x])
    path <- paths[x]
    path <- path %>%
      gsub(pattern = ".*dist", replacement = "IsoMemo")
    sprintf("<img src='%s'> %s", path, label)
  }) %>%
    paste0(collapse = "<br/>")
}
