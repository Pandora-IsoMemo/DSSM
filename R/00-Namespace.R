#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom animation saveGIF
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom colourpicker colourInput
#' @importFrom rcarbon calibrate sampleDates
#' @importFrom DataTools cutAllLongStrings downloadModelUI downloadModelServer fillIsoData
#' @importFrom DataTools handleDescription has_internet importDataUI importDataServer importUI
#' @importFrom DataTools  importServer importOptions prefixSysTime previewDataUI previewDataServer
#' @importFrom DataTools updateNameEntryIfDuplicate
#' @importFrom dplyr "%>%" arrange desc distinct filter group_by group_by_at left_join mutate
#' @importFrom dplyr select summarise ungroup
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom elevatr get_elev_point
#' @importFrom futile.logger DEBUG flog.debug flog.info flog.threshold flog.warn INFO
#' @importFrom gdistance transition transition accCost shortestPath geoCorrection
#' @importFrom geometry convhulln inhulln
#' @importFrom geosphere destPoint
#' @importFrom ggplot2 ggplot theme theme_light coord_cartesian geom_point theme_light theme labs
#' @importFrom ggplot2 geom_errorbar aes_ element_blank element_text position_dodge aes
#' @importFrom ggplot2 geom_boxplot xlab ylab
#' @importFrom gifski gifski
#' @importFrom graphics axis abline contour filled.contour hist plot points title boxplot par
#' @importFrom graphics image lines polygon text .filled.contour Axis box layout lcm plot.new
#' @importFrom graphics plot.window rect legend strwidth text
#' @importFrom grDevices as.raster cm.colors colorRampPalette col2rgb chull dev.off jpeg png
#' @importFrom grDevices pdf pdfFonts recordPlot replayPlot rgb svg tiff
#' @importFrom grid grid.raster
#' @importFrom htmlwidgets saveWidget
#' @importFrom IsoMemo getData getDatabaseList getFields getMappings
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom ks kde Hpi.diag Hpi
#' @importFrom leaflet addAwesomeMarkers addCircleMarkers addControl addLayersControl addLegend
#' @importFrom leaflet addMarkers addPolygons addPolylines addPopups addProviderTiles addRectangles
#' @importFrom leaflet addScaleBar addTiles awesomeIcons clearGroup clearMarkerClusters clearGroup
#' @importFrom leaflet clearPopups colorFactor colorNumeric fitBounds icons labelOptions
#' @importFrom leaflet layersControlOptions leaflet leafletProxy leafletOutput markerClusterOptions
#' @importFrom leaflet popupOptions removeControl removeMarker removeScaleBar removeShape
#' @importFrom leaflet renderLeaflet scaleBarOptions setMaxBounds setView
#' @importFrom magick image_info image_read
#' @importFrom maps map map.scale
#' @importFrom markdown markdownToHTML
#' @importFrom MASS mvrnorm kde2d
#' @importFrom mgcv gam gamm smoothCon s Predict.matrix rig te
#' @importFrom mice mice complete
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom pgdraw pgdraw
#' @importFrom raster raster getValues terrain writeRaster rasterFromXYZ adjacent ncell
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RefManageR ReadBib
#' @importFrom Rfast spdinv Crossprod rmvnorm
#' @importFrom rlang .data
#' @importFrom sf as_Spatial st_as_sf st_as_sfc st_bbox st_centroid st_combine st_coordinates
#' @importFrom sf st_difference st_crs st_geometry st_intersects st_intersection st_sf st_sfc
#' @importFrom sf st_union
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs alert runjs useShinyjs hide show enable disable
#' @importFrom shinyTools headerButtonsUI shinyTryCatch
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats aggregate as.formula cor cov density dist dnorm dunif kmeans
#' @importFrom stats mahalanobis median model.matrix na.exclude na.omit pchisq  pf pnorm predict
#' @importFrom stats qnorm quantile rbeta residuals rgamma rnorm runif sd var
#' @importFrom sp point.in.polygon plot SpatialPoints SpatialPixelsDataFrame
#' @importFrom stringi stri_escape_unicode
#' @importFrom tclust tclust
#' @importFrom utils available.packages capture.output compareVersion install.packages head
#' @importFrom utils packageVersion read.csv read.csv2 write.table installed.packages object.size
#' @importFrom utils zip
#' @importFrom webshot2 webshot
#' @importFrom yaml read_yaml
#' @importFrom zip zipr
NULL
