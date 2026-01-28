# Estimates spatial spread model (first or latest occurence of event)

Estimates spatial spread model (first or latest occurence of event)

## Usage

``` r
estimateMapSpread(
  data,
  Longitude,
  Latitude,
  DateOne,
  DateTwo,
  center = c("Europe", "Pacific"),
  burnin = 500,
  iter = 2000,
  nChains = 1,
  K = 50,
  MinMax = "Max",
  DateType = "Interval",
  dateUnc = "mid point",
  CoordType = "decimal degrees",
  smoothConst = 1,
  penalty = 1,
  splineType = 2,
  shinyApp = FALSE,
  outlier = FALSE,
  outlierValue = 4,
  outlierD = FALSE,
  outlierValueD = 4,
  restriction = c(-90, 90, -180, 180),
  correctionPac = FALSE,
  thinning = 2,
  spreadQ = 0.01,
  minValue = -Inf
)
```

## Arguments

- data:

  data.frame: data

- Longitude:

  character: name of longitude variable

- Latitude:

  character: name of latitude variable

- DateOne:

  character: name of date variable 1 (lower interval point / mean /
  single point)

- DateTwo:

  character: name of date variable 2 (upper interval point / sd / )

- center:

  (character) center to shift data to, either "Europe" or "Pacific"

- burnin:

  integer: number of burn-in iterations for Bayesian model (default =
  500)

- iter:

  integer: number of iterations for Bayesian model (default = 2000)

- nChains:

  integer: number of chains for Bayesian model (default = 1)

- K:

  integer: number of basis functions for tprs (thin plate regression
  spline)

- MinMax:

  character: estimate minimum or maximum of distribution. choices:
  "Max", "Min"

- DateType:

  character: one of "Interval", "Mean + 1 SD uncertainty" and "Single
  Point"

- dateUnc:

  character: one of "uniform", "normal", "point"

- CoordType:

  character: type of longitude/latitude coordinates. One of "decimal
  degrees", "degrees minutes seconds" and "degrees decimal minutes"

- smoothConst:

  numeric: adjust smoothing parameter for Bayesian model (optional)

- penalty:

  numeric: 1 for constant extrapolation, 2 for linear extrapolation

- splineType:

  numeric: 1 for classical tprs, 2 for spherical spline

- shinyApp:

  boolean: If called inside shinyApp: Set to true

- outlier:

  boolean: outlier removal TRUE/FALSE

- outlierValue:

  numeric: if outlier removal is TRUE, threshold for removals in sd

- outlierD:

  boolean: data outlier removal TRUE/FALSE

- outlierValueD:

  numeric: if outlierD removal is TRUE, threshold for removals in sd

- restriction:

  numeric vector: spatially restricts model data 4 entries for latitude
  (min/max) and longitude(min/max)

- correctionPac:

  boolean: correction (data augmentation) for pacific centering

- thinning:

  numeric: mcmc thinning for bayesian models

- spreadQ:

  numeric: exceedance quantile as buffer

- minValue:

  numeric: minValue restriction

## Examples

``` r
if (FALSE) { # \dontrun{
# load data
data <- readRDS(system.file("extData", "exampleData.Rds", package = "DSSM"))
# estimate model-map
map <- estimateMapSpread(data = data, Longitude = "longitude",
Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", iter = 200)
# Plot the map
plotMap(model = map)
} # }
```
