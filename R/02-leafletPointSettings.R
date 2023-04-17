#' ui function of leaflet point settings module
#'
#' @param id namespace
leafletPointSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("clusterPoints"), "Cluster data points"),
    conditionalPanel(
      condition = "input.clusterPoints == false",
      ns = ns,
      tags$hr(),
      fluidRow(
        column(8,
               checkboxInput(
                 ns("useJitter"), "Use jitter (in max. km)"
               )),
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
        )
      ),
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

                 observe({
                   values$clusterPoints <- input$clusterPoints
                 })

                 pointColorVals <-
                   pointColourServer("pointColor", loadedData)
                 observe({
                   for (i in names(pointColorVals)) {
                     values[[i]] <- pointColorVals[[i]]
                   }
                 })

                 pointSizeVals <-
                   pointSizeServer("pointSize", loadedData)
                 observe({
                   for (i in names(pointSizeVals)) {
                     values[[i]] <- pointSizeVals[[i]]
                   }
                 })

                 pointSymbolVals <-
                   pointSymbolServer("pointSymbol", loadedData)
                 observe({
                   for (i in names(pointSymbolVals)) {
                     values[[i]] <- pointSymbolVals[[i]]
                   }
                 })

                 observeEvent(input$pointOpacity, {
                   values$pointOpacity <- input$pointOpacity
                 })

                 observe({
                   values$jitterMaxKm <- ifelse(input$useJitter,
                                                input$jitterMaxKm,
                                                NA_real_)
                 })

                 values
               })
}

#' ui function of leaflet point settings module
#'
#' @param id namespace
pointColourUI <- function(id) {
  ns <- NS(id)

  # using colours from: RColorBrewer::brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
  # adding full names manually
  colourPalettes <- list(
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

  tagList(
    fluidRow(
    column(8,
           selectInput(
             ns("columnForPointColour"),
             "Point colour",
             choices = c("Add data ..." = "")
           )),
    column(4,
           style = "margin-top: 1.5em;",
           checkboxInput(ns("showColourLegend"), "Legend", value = FALSE))
  ),
  fluidRow(
    column(
      8,
      selectInput(
        ns("paletteName"),
        label = NULL,
        choices = colourPalettes,
        selected = "Dark2"
      )
    ),
    column(4,
           style = "margin-top: -0.25em;",
           checkboxInput(
             ns("isReversePalette"), "Reverse", value = FALSE
           ))
  ))
}


#' server funtion of leaflet point settings module
#'
#' @inheritParams leafletPointSettingsServer
pointColourServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 colourValues <- reactiveValues()

                 observeEvent(input$showColourLegend, {
                   colourValues$showColourLegend <- input$showColourLegend
                 })

                 observeEvent(loadedData(), {
                   if (!is.null(loadedData())) {
                     selectedDefault <- ifelse("source" %in% colnames(loadedData()),
                                               "source",
                                               colnames(loadedData())[1])
                   } else {
                     selectedDefault <- character(0)
                   }

                   updateSelectInput(
                     session = session,
                     "columnForPointColour",
                     choices = colnames(loadedData()),
                     selected = selectedDefault
                   )
                   updateCheckboxInput(session = session, "showColourLegend", value = TRUE)
                 })

                 observeEvent(input$columnForPointColour, {
                   colourValues$columnForPointColour <- input$columnForPointColour
                 })

                 observeEvent(
                   list(
                     input$paletteName,
                     input$isReversePalette,
                     input$columnForPointColour
                   ),
                   {
                     if (is.null(loadedData()) ||
                         is.null(input$columnForPointColour))
                       colourValues$pointColourPalette <- NULL

                     if (!is.null(loadedData()) &&
                         !is.null(input$columnForPointColour)) {
                       colourColumn <- loadedData()[[input$columnForPointColour]]

                       if (is.numeric(colourColumn)) {
                         pal <- colorNumeric(
                           palette = input$paletteName,
                           domain = colourColumn,
                           reverse = input$isReversePalette
                         )
                       } else {
                         pal <- colorFactor(
                           palette = input$paletteName,
                           domain = colourColumn,
                           reverse = input$isReversePalette
                         )
                       }

                       colourValues$pointColourPalette <- pal
                     }
                   }
                 )

                 return(colourValues)
               })
}


#' ui function of leaflet point settings module
#'
#' @param id namespace
pointSizeUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
    column(
      8,
      selectInput(
        ns("columnForPointSize"),
        "Point size",
        choices = c("Add data ..." = "")
      )
    ),
    column(4,
           #style = "margin-top: -0.5em;",
           #checkboxInput(ns("showLegend"), "Legend", value = FALSE)
           numericInput(
             ns("sizeFactor"),
             "Factor",
             value = 1,
             min = 0.1,
             max = 20,
             step = 0.1,
             width = "100%"
           )
           )
  ))
}


#' server funtion of leaflet point settings module
#'
#' @inheritParams leafletPointSettingsServer
pointSizeServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 sizeValues <- reactiveValues()

                 # observe({
                 #   sizeValues$showLegend <- input$showLegend
                 # }) %>%
                 #   bindEvent(input$showLegend)

                 observe({
                   if (is.null(loadedData())) {
                     choices <- c("Add data ..." = "")
                     selectedDefault <- ""
                     #showLegendVal <- FALSE
                   } else {
                     numCols <- partialNumericColumns(loadedData())
                     if (length(numCols) == 0) {
                       choices <- c("No numeric columns ..." = "")
                       selectedDefault <- ""
                       #showLegendVal <- FALSE
                     } else {
                       choices <- c("Size variable ..." = "", numCols)
                     }
                     selectedDefault <- ""
                     #showLegendVal <- TRUE
                   }

                   updateSelectInput(
                     session = session,
                     "columnForPointSize",
                     choices = choices,
                     selected = selectedDefault
                   )
                   #updateCheckboxInput(session = session, "showLegend", value = showLegendVal)

                   sizeValues$pointRadius <- getPointSize(
                     df = loadedData(),
                     columnForPointSize = selectedDefault,
                     sizeFactor = input$sizeFactor
                   )
                   #sizeValues$showLegend <- input$showLegend
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   sizeValues$pointRadius <- getPointSize(
                     df = loadedData(),
                     columnForPointSize = input$columnForPointSize,
                     sizeFactor = input$sizeFactor
                   )
                 }) %>%
                   bindEvent(list(input$columnForPointSize, input$sizeFactor))

                 return(sizeValues)
               })
}


#' ui function of leaflet point symbol settings module
#'
#' @param id namespace
pointSymbolUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(8,
             selectInput(
               ns("columnForPointSymbol"),
               "Point symbol",
               choices = c("Add data ..." = "")
             )
      ),
      column(4,
             #style = "margin-top: 1.5em;",
             numericInput(
               ns("pointWidth"),
               "Line width",
               value = 1,
               min = 1,
               max = 10
             ))
    ),
    fluidRow(
      column(8,
             #style = "margin-top: -0.5em;",
             pickerInput(
               ns("pointSymbol"),
               label = NULL,
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
             checkboxInput(ns("showSymbolLegend"), "Legend", value = FALSE))
    )
  )
}


#' server function of leaflet point symbol settings module
#'
#' @inheritParams leafletPointSettingsServer
pointSymbolServer <- function(id, loadedData) {
  moduleServer(id,
               function(input, output, session) {
                 symbolValues <- reactiveValues(
                   pointSymbol = 19
                 )

                 observe({
                   if (is.null(loadedData())) {
                     choices <- c("Add data ..." = "")
                     selectedDefault <- ""
                     showLegendVal <- FALSE
                   } else {
                     facCols <- factorColumns(loadedData())
                     if (length(facCols) == 0) {
                       choices <- c("No character columns ..." = "")
                       selectedDefault <- ""
                       showLegendVal <- FALSE
                     } else {
                       choices <- c("Symbol variable ..." = "", facCols)
                     }
                     selectedDefault <- ""
                     showLegendVal <- TRUE
                   }

                   updateSelectInput(
                     session = session,
                     "columnForPointSymbol",
                     choices = choices,
                     selected = selectedDefault
                   )
                   updatePickerInput(
                     session = session,
                     "pointSymbol",
                     selected = 19
                   )
                   updateCheckboxInput(session = session, "showSymbolLegend", value = showLegendVal)

                   symbolsAndLegend <- getPointSymbols(
                     df = loadedData(),
                     columnForPointSymbol = selectedDefault,
                     symbols = 19
                   )
                   symbolValues$pointSymbol <- symbolsAndLegend$pointSymbol
                   symbolValues$symbolLegendValues <- symbolsAndLegend$symbolLegendValues
                   symbolValues$columnForPointSymbol <- selectedDefault
                   symbolValues$showSymbolLegend <- showLegendVal
                 }) %>%
                   bindEvent(loadedData())

                 observe({
                   req(input$pointSymbol)
                   symbolsAndLegend <- getPointSymbols(
                     df = loadedData(),
                     columnForPointSymbol = input$columnForPointSymbol,
                     symbols = as.numeric(input$pointSymbol)
                   )
                   symbolValues$pointSymbol <- symbolsAndLegend$pointSymbol
                   symbolValues$symbolLegendValues <- symbolsAndLegend$symbolLegendValues
                   symbolValues$columnForPointSymbol <- input$columnForPointSymbol
                 }) %>%
                   bindEvent(list(input$columnForPointSymbol, input$pointSymbol), ignoreInit = TRUE)

                 observe({
                   symbolValues$pointWidth <- input$pointWidth
                 }) %>%
                   bindEvent(input$pointWidth)

                 observe({
                   symbolValues$showSymbolLegend <- input$showSymbolLegend
                 }) %>%
                   bindEvent(input$showSymbolLegend)

                 return(symbolValues)
               })
}


# Helper functions ----

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

    if (nrow(isoData) == 0) return(map)

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
      addLayersControl(
        overlayGroups = c("Data Points"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE))
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
    colourList <- lapply(colourPal(isoData[[columnForColour]]), col2rgb)
    colourVec <- sapply(1:nrow(isoData), function(i) {
      rgb(red = colourList[[i]][1],
          green = colourList[[i]][2],
          blue = colourList[[i]][3],
          maxColorValue = 255,
          alpha = pointOpacity * 255)
    })

    # create icon for each point
    iconFiles <- sapply(1:nrow(isoData), function(x) {
      createPchPoints(pch = pointSymbol[[x]], # pointSymbol is a list, while others are vectors
                      width = pointRadius[x] * 2,
                      height = pointRadius[x] * 2,
                      col = colourVec[x],
                      lwd = pointWidth)
    })

    map %>%
      addMarkers(
        data = isoData,
        lat = ~ latitude,
        lng =  ~ longitude,
        group = "Data Points",
        icon = ~ icons(
          iconUrl = iconFiles,
          popupAnchorX = 20, popupAnchorY = 0
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
  if (showLegend && !is.null(pal)) {
    map <- map %>%
      addLegend(
        "topleft",
        pal = pal,
        values = values,
        title = title,
        layerId = "colorLegend"
      )
  } else {
    map <- map %>% removeControl("colorLegend")
  }

  map
}

# Point Size ----

#' Get Point Size
#'
#' @param df (data.frame) loaded data
#' @param columnForPointSize (character) name of the column that determines the point size
#' @param sizeFactor (numeric) general factor for point size
getPointSize <- function(df, columnForPointSize, sizeFactor = 1) {
  if (is.null(df) || is.null(columnForPointSize) || is.null(sizeFactor))
    return(NULL)

  defaultPointSizeInPxl <- 5

  nPoints <- nrow(df)
  defaultPointSize <-
    rep(sizeFactor * defaultPointSizeInPxl, nPoints)

  if (columnForPointSize %in% c("",  "none"))
    return(defaultPointSize)

  sizeColumn <- df[, columnForPointSize] %>%
    as.numeric() %>%
    suppressWarnings()

  if (length(unique(na.omit(sizeColumn))) < 2)
    return(defaultPointSize)

  # normalize sizes to intervall [0,1]
  varSizeFactor <- sizeColumn - min(sizeColumn, na.rm = TRUE)
  varSizeFactor <- varSizeFactor / max(varSizeFactor, na.rm = TRUE)

  # map to intervall: [1/defaultPointSizeInPxl, 1-(1/defaultPointSizeInPxl)] instead of (0,1)
  # because the minimal factor should be at least 1/defaultPointSizeInPxl
  varSizeFactor <- (1 - 2/defaultPointSizeInPxl) * varSizeFactor + 1/defaultPointSizeInPxl

  # the mean of the data (== 0.5) should have a factor of 1
  varSizeFactor <- 2 * varSizeFactor

  # give missing values zero factor
  varSizeFactor[is.na(varSizeFactor)] <- 0

  # multiply with default
  pointSizes <- varSizeFactor * sizeFactor * defaultPointSizeInPxl

  pointSizes
}

# Symbols ----

setSymbolLegend <- function(map, showLegend, symbolLegend, isTest = FALSE) {
  if (is.null(symbolLegend) || !showLegend) {
    map <- map %>%
      removeControl("symbolLegend")

    return(map)
  }

  if (isTest) {
    pathToSymbols <- file.path("inst", "app", "www")
  } else {
    pathToSymbols <- "www"
  }

  htmlString <- getSymbolLegend(symbolLegend, pathToSymbols = pathToSymbols)

  map %>%
    addControl(
      html = htmlString,
      position = "topleft",
      layerId = "symbolLegend"
    )
}

getSymbolLegend <- function(symbolLegend, pathToSymbols) {
  # remove old icons: remove all files with the pattern "symbolFile"
  oldSymbolFiles <- dir(pathToSymbols)
  oldSymbolFiles <- oldSymbolFiles[grepl("symbolFile", oldSymbolFiles)]
  sapply(oldSymbolFiles, function(oldFile) {
    file.remove(file.path(pathToSymbols, oldFile))
  })

  # create icon for each point
  iconFiles <- sapply(symbolLegend, function(x) {
    createPchPoints(pch = x,
                    width = 10,
                    height = 10,
                    lwd = 1,
                    tmpDir = pathToSymbols)
    })

  # create one html string over all used icons
  sapply(seq_along(symbolLegend), function(x) {
    label <- names(iconFiles[x])
    pathToIcon <- iconFiles[x]
    pathToIcon <- pathToIcon %>%
      gsub(pattern = ".*www", replacement = "")
    sprintf("<img src='%s'> %s", pathToIcon, label)
  }) %>%
    paste0(collapse = "<br/>")
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
createPchPointsVec <- function(pch = 16, width = 50, height = 50, bg = "transparent", col = "black", ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
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
#' @param ... Further graphical parameters that are passed to graphics::points()
createPchPoints <- function(pch = 16, width = 50, height = 50, bg = "transparent",
                            col = "black", tmpDir = tempdir(), ...) {
  file <- tempfile(pattern = "symbolFile", fileext = '.png', tmpdir = tmpDir)

  png(file, width = width, height = height, bg = bg)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  points(.5, .5, pch = pch, col = col, cex = min(width, height) / 8, ...)
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
getPointSymbols <- function(df, columnForPointSymbol, symbols = unlist(pchChoices())) {
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

orderBySelection <- function(pchSel, pchAll = unlist(pchChoices())) {
  index <- match(pchSel, pchAll)
  c(pchAll[index], pchAll[-index])
}
