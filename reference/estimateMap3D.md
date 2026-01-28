# Estimates spatio-temporal average model with (optional) random effects (GAMM /Generalized Additive Mixed Model)

Note regarding IndependentType = "categorical": This follows a one vs.
all approach using logistic regression, which in the Bayesian case is
performed using a Polya-Gamma latent variable during Gibbs-sampling
(https://arxiv.org/abs/1205.0310).

## Usage

``` r
estimateMap3D(
  data,
  independent,
  Longitude,
  Latitude,
  DateOne,
  DateTwo,
  center = c("Europe", "Pacific"),
  IndependentType = "numeric",
  Site = "",
  DateType = "Interval",
  dateUnc = "uniform",
  independentUncertainty = "",
  CoordType = "decimal degrees",
  burnin = 500,
  iter = 2000,
  nChains = 1,
  splineType = 1,
  K = 25,
  KT = 10,
  Bayes = FALSE,
  penalty = 1,
  smoothConst = 1,
  outlier = FALSE,
  outlierValue = 4,
  outlierD = FALSE,
  outlierValueD = 4,
  restriction = c(-90, 90, -180, 180),
  sdVar = FALSE,
  correctionPac = FALSE,
  thinning = 2
)
```

## Arguments

- data:

  data.frame: data

- independent:

  character: name of independent variable

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

- IndependentType:

  character: type ("numeric" or "categorical") of independent variable

- Site:

  character: name of site variable (optional)

- DateType:

  character: one of "Interval", "Mean + 1 SD uncertainty" and "Single
  Point"

- dateUnc:

  character: one of "uniform", "normal", "point"

- independentUncertainty:

  character: uncertainty of independent variable in sd (optional)

- CoordType:

  character: type of longitude/latitude coordinates. One of "decimal
  degrees", "degrees minutes seconds" and "degrees decimal minutes"

- burnin:

  integer: number of burn-in iterations for Bayesian model (default =
  500)

- iter:

  integer: number of iterations for Bayesian model (default = 2000)

- nChains:

  integer: number of chains for Bayesian model (default = 1)

- splineType:

  numeric: 1 for classical tprs, 2 for spherical spline

- K:

  integer: number of basis functions for sos (spline on a sphere)

- KT:

  integer: number of basis functions for tprs (thin plate regression
  spline)

- Bayes:

  boolean: Bayesian model TRUE/FALSE?

- penalty:

  numeric: 1 for constant extrapolation, 2 for linear extrapolation

- smoothConst:

  numeric: adjust smoothing parameter(\>0) for Bayesian model (optional)

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

- sdVar:

  boolean: variable standard deviation

- correctionPac:

  boolean: correction (data augmentation) for pacific centering

- thinning:

  numeric: mcmc thinning for bayesian models

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
