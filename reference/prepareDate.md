# Prepare Date

Adds new columns 'Date' and 'Uncertainty' that are used in the model
dependent on user inputs.

## Usage

``` r
prepareDate(data, DateOne, DateTwo, DateType, dateUnc, useMaxUnc = TRUE)
```

## Arguments

- data:

  data.frame: data

- DateOne:

  character: name of date variable 1 (lower interval point / mean /
  single point)

- DateTwo:

  character: name of date variable 2 (upper interval point / sd / )

- DateType:

  character: one of "Interval", "Mean + 1 SD uncertainty" and "Single
  Point"

- dateUnc:

  character: one of "uniform", "normal", "point"

- useMaxUnc:

  (logical) True if max uncertainty should be used.
