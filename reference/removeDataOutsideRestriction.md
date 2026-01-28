# Remove Data Outside Restriction

Remove all data outside of the restriction

## Usage

``` r
removeDataOutsideRestriction(data, Latitude, Longitude, restriction)
```

## Arguments

- data:

  (data.frame) data containing columns "Latitude" and "Longitude"

- Latitude:

  (character) column name of Latitude column

- Longitude:

  (character) column name of Longitude column

- restriction:

  (numeric) restriction in the form c(minLatitude, maxLatitude,
  minLongitude, maxLongitude)

## Value

(data.frame) data containing columns "Latitude" and "Longitude" with
data outside of restriction removed
