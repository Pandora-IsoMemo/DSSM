# Plots map of a spatial average or spread model from estimateMap() or estimateMapSpread() functions

Plots map of a spatial average or spread model from estimateMap() or
estimateMapSpread() functions

## Usage

``` r
plotMap(
  model,
  IndSelect = NULL,
  estType = "Mean",
  estQuantile = 0.9,
  points = TRUE,
  pointSize = 1,
  StdErr = Inf,
  rangex = range(model$data$Longitude, na.rm = TRUE),
  rangey = range(model$data$Latitude, na.rm = TRUE),
  rangez = NULL,
  contourType = c("filled.contour", "contour"),
  limitz = NULL,
  resolution = 100,
  interior = TRUE,
  ncol = 10,
  colors = "RdYlGn",
  reverseColors = FALSE,
  colorsP = NULL,
  centerMap = "Europe",
  pointShape = 4,
  showScale = TRUE,
  textLabels = NULL,
  pointLabels = NULL,
  pointColLabels = NULL,
  fontSize = 11,
  showModel = TRUE,
  fontType = "sans",
  fontCol = "black",
  mask = FALSE,
  maskRadius = 100,
  pColor = "#000000",
  dataCenter = NULL,
  RadiusBatch = NULL,
  terrestrial = 1,
  grid = TRUE,
  showBorders = TRUE,
  arrow = TRUE,
  scale = TRUE,
  mapType = "Map",
  titleMain = TRUE,
  titleScale = TRUE,
  setAxisLabels = FALSE,
  mainLabel = "",
  yLabel = "Latitude",
  xLabel = "Longitude",
  northSize = 0.2,
  scalSize = 0.1,
  scaleX = 0,
  scaleY = 0.1,
  NorthX = 0.025,
  NorthY = 0.925,
  scLabel = "",
  AxisSize = 1,
  AxisLSize = 1,
  cluster = FALSE,
  clusterCol = "Set1",
  pointDat = NULL,
  MinMax = "Min",
  nMin = 3,
  minDist = 250,
  showMinOnMap = FALSE,
  OLat = NULL,
  OLong = NULL,
  DestLat = NULL,
  DestLong = NULL
)
```

## Arguments

- model:

  return object of spatial or spread model from estimateMap() or
  estimateMapSpread() functions

- IndSelect:

  for categorical model: selected category; shifts between categories in
  the center

- estType:

  one of "Mean", "SE" and "Quantile". defaults to "Mean"

- estQuantile:

  quantile (only applicable if estType = "Quantile")

- points:

  show points on map TRUE/FALSE

- pointSize:

  point size (if points = TRUE)

- StdErr:

  maximum standard error to which estimates are displayed

- rangex:

  range of longitude values (x axis limits)

- rangey:

  range of latitude values (y axis limits)

- rangez:

  range of estimated values (z axis limits)

- contourType:

  one of "filled.contour" or "contour"

- limitz:

  restrict range of z (possible values "0-1", "0-100")

- resolution:

  spatial grid resolution of displayed (higher is slower but better
  quality)

- interior:

  show only convex hull TRUE/FALSE

- ncol:

  number of colors for estimates

- colors:

  color scheme of estimates from RColorBrewer. defaults to "RdYlGn"

- reverseColors:

  inverse colour scheme

- colorsP:

  color scale of location marks

- centerMap:

  center of map, one of "Europe" and "Pacific"

- pointShape:

  pch shape of location marks

- showScale:

  show colour scale

- textLabels:

  text labels

- pointLabels:

  point size labels

- pointColLabels:

  point colour labels

- fontSize:

  font size

- showModel:

  show model

- fontType:

  font type

- fontCol:

  font color

- mask:

  boolean: Show output within range of points

- maskRadius:

  numeric: Show output within range of points in km

- pColor:

  color of location marks drawn

- dataCenter:

  optional data.frame with two columns (longitude / latitude) for batch
  estimates of a series of spatial points

- RadiusBatch:

  radius of batch spatial point estimates

- terrestrial:

  show only estimates on land masses (1), oceans (-1) or all (0)

- grid:

  show coordinate grid TRUE/FALSE

- showBorders:

  show country borders TRUE/FALSE

- arrow:

  display north arrow TRUE/FALSE

- scale:

  display scale TRUE/FALSE

- mapType:

  Type of map. "Map" for simple estimates. "speed" for gradient (mainly
  for spread model)

- titleMain:

  show main title

- titleScale:

  show scale title

- setAxisLabels:

  show axis/main/scale labels

- mainLabel:

  main label

- yLabel:

  y lab label

- xLabel:

  y lab label

- northSize:

  size of north arrow

- scalSize:

  size of scale

- scaleX:

  scale x orientation

- scaleY:

  scale y orientation

- NorthX:

  north arrow x orientation

- NorthY:

  north arrow y orientation

- scLabel:

  scale lab label

- AxisSize:

  axis title font size

- AxisLSize:

  axis label font size

- cluster:

  show clusters

- clusterCol:

  Cluster colors

- pointDat:

  data frame of points to add to plot

- MinMax:

  Min or Max, only for spread model

- nMin:

  Number of minima to compare, only for spread model

- minDist:

  Distance between minima/maxima, only for spread model

- showMinOnMap:

  Show minima on map yes/no, only for spread model if maptype =
  "Minima/Maxima"

- OLat:

  Origin Latitude for Cost-Surface Plot, only for spread model if
  maptype = "Cost-Surface"

- OLong:

  Origin Longitude for Cost-Surface Plot, only for spread model if
  maptype = "Cost-Surface"

- DestLat:

  Origin Latitude for Cost-Surface Plot, only for spread model if
  maptype = "Cost-Surface"

- DestLong:

  Origin Longitude for Cost-Surface Plot, only for spread model if
  maptype = "Cost-Surface"
