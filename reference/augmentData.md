# Augment Data

Data augmentation to add data at the borders of the map

## Usage

``` r
augmentData(data, restriction = c(-90, 90, -320, 320))
```

## Arguments

- data:

  (data.frame) data containing columns "Latitude" and "Longitude"

- restriction:

  (numeric) restriction in the form c(minLatitude, maxLatitude,
  minLongitude, maxLongitude)

## Value

(data.frame) augmented data
