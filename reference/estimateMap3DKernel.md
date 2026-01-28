# Estimates spatio-temporal kernel density model

Estimates spatio-temporal kernel density model

## Usage

``` r
estimateMap3DKernel(
  data,
  Longitude,
  Latitude,
  DateOne,
  DateTwo,
  center = c("Europe", "Pacific"),
  independent = NULL,
  DateType = "Interval",
  CoordType = "decimal degrees",
  Weighting = NULL,
  clusterMethod = NULL,
  nClust = 5,
  nClustRange = c(2, 10),
  kMeansAlgo = "Hartigan-Wong",
  trimRatio = 0.05,
  restr.fact = 12,
  clusterTimeRange = c(0, 1000),
  modelUnc = FALSE,
  dateUnc = "point",
  restriction = c(-90, 90, -180, 180),
  nSim = 10,
  smoothness = 1,
  kdeType = "1"
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

- independent:

  character: name of presence/absence variable (optional)

- DateType:

  character: one of "Interval", "Mean + 1 SD uncertainty" and "Single
  Point"

- CoordType:

  character: type of longitude/latitude coordinates. One of "decimal
  degrees", "degrees minutes seconds" and "degrees decimal minutes"

- Weighting:

  character: name of weighting variable

- clusterMethod:

  character: cluster method

- nClust:

  numeric: how many clusters

- nClustRange:

  numeric: range of potential mclust cluster

- kMeansAlgo:

  character: kmeans algorithm as in stats:kmeans

- trimRatio:

  numeric: proportion of observations to be trimmed by tclust

- restr.fact:

  numeric: clustering restriction factor

- clusterTimeRange:

  numeric vector: time range of cluster

- modelUnc:

  boolean: Include dating uncertainty

- dateUnc:

  character: one of "uniform", "normal", "point"

- restriction:

  numeric vector: spatially restricts model data 4 entries for latitude
  (min/max) and longitude(min/max)

- nSim:

  numeric: number of bootstrap samples

- smoothness:

  numeric: smoothness adjustment

- kdeType:

  character: "1" for correlated bandwidth, "2" for diagonal bandwidth,
  "3" for diagonal, equal long/lat bandwidth

## Examples

``` r
if (FALSE) { # \dontrun{
# load data
data <- readRDS(system.file("extData", "exampleData.Rds", package = "DSSM"))
# estimate model-map
map <- estimateMap3D(data = data, independent = "d13C", Longitude = "longitude",
Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", Site = "site")
# Plot the map
plotMap3D(model = map, time = median(data$dateLower, na.rm = TRUE))
} # }
```
