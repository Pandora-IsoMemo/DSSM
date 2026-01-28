# Plots time course map of a spatio-temporal model from estimateMap3D() function

Plots time course map of a spatio-temporal model from estimateMap3D()
function

## Usage

``` r
plotTimeCourse(
  model,
  IndSelect = NULL,
  independent = "",
  trange = range(model$data$Date),
  resolution = 500,
  centerX = NA,
  centerY = NA,
  rangey = NULL,
  seType = "2",
  pointDat = NULL,
  pointsTime = FALSE,
  returnPred = FALSE,
  intTime = FALSE,
  rangePointsTime = 500,
  limitz = NULL,
  formatTimeCourse = NULL
)
```

## Arguments

- model:

  return object of a spatio-temporal model from estimateMap3D() function

- IndSelect:

  for categorical model: selected category

- independent:

  name of independent variable shown in plot

- trange:

  range of longitude values (x axis limits)

- resolution:

  temporal grid resolution of displayed (higher is slower but better
  quality)

- centerX:

  longitude value to display time course plot for

- centerY:

  latitude value to display time course plot for

- rangey:

  2-element vector of time interval to show

- seType:

  setype

- pointDat:

  add points/lines to plot

- pointsTime:

  should nearby points be plotted?

- returnPred:

  should the prediction be returned?

- intTime:

  should uncertainty intervals of nearby points be plotted?

- rangePointsTime:

  range of nearby points in km

- limitz:

  z limit range

- formatTimeCourse:

  parameters for the plot format, e.g. axesDecPlace, nLabelsX, nLabelsY
