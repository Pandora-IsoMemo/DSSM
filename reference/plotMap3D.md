# Plots time slice map of a spatio-temporal model from estimateMap3D() function

Plots time slice map of a spatio-temporal model from estimateMap3D()
function

## Usage

``` r
plotMap3D(
  model,
  time,
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
  limitz = "none",
  centerMap = "Europe",
  addU = 0,
  pointShape = 4,
  pColor = "#000000",
  colorsP = NULL,
  showModel = TRUE,
  textLabels = NULL,
  pointLabels = NULL,
  pointColLabels = NULL,
  fontSize = 11,
  mask = FALSE,
  maskRadius = 100,
  showScale = TRUE,
  fontType = "sans",
  fontCol = "black",
  resolution = 100,
  interior = TRUE,
  ncol = 10,
  colors = "RdYlGn",
  reverseColors = FALSE,
  dataCenter = NULL,
  RadiusBatch = 100,
  terrestrial = 1,
  grid = TRUE,
  showBorders = TRUE,
  arrow = TRUE,
  scale = TRUE,
  titleMain = TRUE,
  titleScale = TRUE,
  setAxisLabels = FALSE,
  mainLabel = "",
  yLabel = "Latitude",
  xLabel = "Longitude",
  scLabel = "",
  northSize = 0.2,
  scalSize = 0.1,
  scaleX = 0,
  scaleY = 0.1,
  NorthX = 0.025,
  NorthY = 0.925,
  AxisSize = 1,
  AxisLSize = 1,
  cluster = FALSE,
  clusterAll = "0",
  clusterResults = 0,
  clusterCol = "Set1",
  pointDat = NULL,
  plotRetNull = FALSE
)
```

## Arguments

- model:

  return object of a spatio-temporal model from estimateMap3D() function

- time:

  time slice value for map

- IndSelect:

  for categorical model: selected category

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

- centerMap:

  center of map, one of "Europe" and "Pacific"

- addU:

  numeric: years of added uncertainty for time sliced 2D-convex hull

- pointShape:

  pch shape of location marks

- pColor:

  color of location marks drawn

- colorsP:

  color scale of location marks

- showModel:

  show model

- textLabels:

  text labels

- pointLabels:

  point size labels

- pointColLabels:

  point colour labels

- fontSize:

  font size

- mask:

  boolean: Show output within range of points

- maskRadius:

  numeric: Show output within range of points in km

- showScale:

  show colour scale

- fontType:

  font type

- fontCol:

  font color

- resolution:

  spatial grid resolution of displayed (higher is slower but better
  quality)

- interior:

  show only convex hull 0 "none", 1 "3D", 2 "time sliced 2D"

- ncol:

  number of colors for estimates

- colors:

  color scheme of estimates from RColorBrewer. defaults to "RdYlGn"

- reverseColors:

  inverse colour scheme

- dataCenter:

  optional data.frame with three columns (longitude / latitude / time)

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

- scLabel:

  scale lab label

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

- AxisSize:

  axis title font size

- AxisLSize:

  axis label font size

- cluster:

  show clusters

- clusterAll:

  show all cluster points

- clusterResults:

  temporal groups or spatial clusters

- clusterCol:

  Cluster colors

- pointDat:

  data frame of points to add to plot

- plotRetNull:

  return predictions
