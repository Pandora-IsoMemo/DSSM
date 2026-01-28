# Estimates spatial average model with (optional) random effects (GAMM /Generalized Additive Mixed Model)

Note regarding IndependentType = "categorical": This follows a one vs.
all approach using logistic regression, which in the Bayesian case is
performed using a Polya-Gamma latent variable during Gibbs-sampling
(https://arxiv.org/abs/1205.0310).

## Usage

``` r
estimateMap(
  data,
  independent,
  Longitude,
  Latitude,
  center = c("Europe", "Pacific"),
  IndependentType = "numeric",
  Site = "",
  independentUncertainty = "",
  burnin = 500,
  iter = 2000,
  nChains = 1,
  K = 50,
  Bayes = FALSE,
  CoordType = "decimal degrees",
  smoothConst = 1,
  penalty = 2,
  splineType = 2,
  outlier = FALSE,
  outlierValue = 4,
  outlierD = FALSE,
  outlierValueD = 4,
  restriction = c(-90, 90, -180, 180),
  correctionPac = FALSE,
  sdVar = FALSE,
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

- center:

  (character) center to shift data to, either "Europe" or "Pacific"

- IndependentType:

  character: type ("numeric" or "categorical") of independent variable

- Site:

  character: name of site variable (optional)

- independentUncertainty:

  character: uncertainty of independent variable in sd (optional)

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

- Bayes:

  boolean: Bayesian model TRUE/FALSE?

- CoordType:

  character: type of longitude/latitude coordinates. One of "decimal
  degrees", "degrees minutes seconds" and "degrees decimal minutes"

- smoothConst:

  numeric: adjust smoothing parameter (\> 0) for Bayesian model
  (optional)

- penalty:

  numeric: 1 for constant extrapolation, 2 for linear extrapolation

- splineType:

  numeric: 1 for classical tprs, 2 for spherical spline

- outlier:

  boolean: model outlier removal TRUE/FALSE

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

- sdVar:

  boolean: variable standard deviation

- thinning:

  numeric: mcmc thinning for bayesian models

## Examples

``` r
if (FALSE) { # \dontrun{
#load data
data <- readRDS(system.file("extData", "exampleData.Rds", package = "DSSM"))
# estimate model-map
map <- estimateMap(data = data, independent = "d13C", Longitude = "longitude",
Latitude = "latitude", Site = "site")
# Plot the map
plotMap(model = map)

# Alternative: use app
shiny::runApp(paste0(system.file(package = "DSSM"),"/app"))

} # }
```
