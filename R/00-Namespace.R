#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom animation saveGIF
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom colourpicker colourInput
#' @importFrom rcarbon calibrate sampleDates
#' @importFrom dplyr bind_rows everything group_by group_indices inner_join left_join select
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom geometry convhulln inhulln
#' @importFrom ggplot2 ggplot theme theme_light coord_cartesian geom_point theme_light theme labs
#'  geom_errorbar aes_ element_blank element_text position_dodge aes geom_boxplot xlab
#' @importFrom graphics axis filled.contour hist plot points title boxplot par image lines
#'  polygon text .filled.contour Axis box layout lcm plot.new plot.window rect legend strwidth text
#' @importFrom grDevices colorRampPalette chull png pdf tiff svg dev.off recordPlot replayPlot
#'  cm.colors pdfFonts
#' @importFrom IsoAppTools importDataUI importDataServer
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom ks kde Hpi.diag Hpi
#' @importFrom leaflet addAwesomeMarkers addCircleMarkers addControl addLegend addMarkers
#'  addPopups addProviderTiles addRectangles addScaleBar addTiles awesomeIcons
#'  clearMarkerClusters clearGroup clearPopups colorFactor colorNumeric fitBounds
#'  leaflet leafletProxy leafletOutput markerClusterOptions popupOptions
#'  removeControl removeScaleBar removeShape renderLeaflet scaleBarOptions setMaxBounds setView
#' @importFrom magrittr "%>%"
#' @importFrom maps map map.scale
#' @importFrom MASS mvrnorm kde2d
#' @importFrom mgcv gam gamm smoothCon s Predict.matrix rig te
#' @importFrom openxlsx loadWorkbook read.xlsx write.xlsx
#' @importFrom raster raster getValues terrain writeRaster
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr guess_encoding
#' @importFrom readxl excel_sheets read_excel
#' @importFrom Rfast spdinv Crossprod rmvnorm
#' @importFrom rgeos gCentroid
#' @importFrom rlang .data
#' @importFrom RSQLite dbConnect dbGetQuery dbListTables dbWriteTable SQLite
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs alert reset runjs useShinyjs
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats aggregate as.formula dunif model.matrix na.exclude na.omit predict
#'  quantile rgamma rnorm runif sd setNames mahalanobis pchisq cov pf rbeta
#' @importFrom sp point.in.polygon plot SpatialPoints
#' @importFrom stats aggregate as.formula model.matrix na.exclude na.omit predict quantile dnorm
#'  qnorm residuals pnorm var sd setNames cor density median kmeans
#' @importFrom stringi stri_escape_unicode
#' @importFrom templates tmpl
#' @importFrom tidyr separate unite
#' @importFrom utils available.packages compareVersion install.packages head
#'  packageVersion read.csv read.csv2 write.table installed.packages capture.output download.file
#' @importFrom zip zipr
NULL
