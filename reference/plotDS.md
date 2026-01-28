# Plots difference or similarity map

Plots difference or similarity map

## Usage

``` r
plotDS(
  XPred,
  estType = "mean",
  estQuantile = 0.95,
  type = "similarity",
  independent = "",
  rangex = range(XPred$Longitude, na.rm = TRUE),
  rangey = range(XPred$Latitude, na.rm = TRUE),
  rangez = range(XPred$Est, na.rm = TRUE),
  contourType = c("filled.contour", "contour"),
  showModel = TRUE,
  showScale = TRUE,
  ncol = 10,
  colors = "RdYlGn",
  centerMap = "Europe",
  reverseColors = FALSE,
  terrestrial = 1,
  grid = TRUE,
  showBorders = TRUE,
  arrow = TRUE,
  scale = TRUE,
  simValues = NULL,
  showValues = FALSE,
  dataCenter = NULL,
  RadiusBatch = NULL,
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
  pointDat = NULL
)
```

## Arguments

- XPred:

  return object of similarityMap or createDifferenceMap functions

- estType:

  one of "Mean", "SE" and "Quantile". defaults to "Mean"

- estQuantile:

  quantile (only applicable if estType = "Quantile")

- type:

  one of "similarity" or "difference" determining the type of map

- independent:

  name of independent variable shown in plot

- rangex:

  range of longitude values (x axis limits)

- rangey:

  range of latitude values (y axis limits)

- rangez:

  range of estimated values (z axis limits)

- contourType:

  one of "filled.contour" or "contour"

- showModel:

  show model

- showScale:

  show colour scale

- ncol:

  number of colors for estimates

- colors:

  color scheme of estimates from RColorBrewer. defaults to "RdYlGn"

- centerMap:

  center of map, one of "Europe" and "Pacific"

- reverseColors:

  inverse colour scheme

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

- simValues:

  if showValues: list of simulated values

- showValues:

  boolean show values in plot?

- dataCenter:

  data for batch estimation

- RadiusBatch:

  radius

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

  scale lab labels

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

- pointDat:

  data frame of points to add to plot
