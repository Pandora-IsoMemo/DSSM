# Center Data

Transfer data to the center of the map, either Europe or Pacific by
adding or substracting 360° from the Longitude. If "Europe" longitudes
are transposed to the range (-180, 180), if "Pacific" longitudes are
transposed to the range (0, 360).

## Usage

``` r
centerData(data, center = c("Europe", "Pacific"))
```

## Arguments

- data:

  (data.frame) data containing column "Longitude"

- center:

  (character) center to shift data to, either "Europe" or "Pacific"

## Value

(data.frame) data shifted to the center
