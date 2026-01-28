# Jitter latitude and longitude coordinates

Given a latitude or longitude coordinate, return that coordinate
jittered plus or minus a certain maximum amount. This function assumes a
spherical earth when calculating the maximum amount to jitter.

## Usage

``` r
jitter_latlong(coord, type = c("lat", "long"), latitude, km = 1)
```

## Arguments

- coord:

  The coordinate in degrees

- type:

  Whether the coordinate is latitude or longitude

- latitude:

  If the coordinate is longitude, then the latitude to use for
  calculating the maximum amount to jitter.

- km:

  The maximum number of kilometers to jitter a point plus or minus.

## Value

The jittered coordinate in degrees.

## See also

[`length_of_degree`](https://pandora-isomemo.github.io/DSSM/reference/length_of_degree.md)

## Examples

``` r
jitter_latlong(-73, type = "long", lat = 43, km = 1)
#> [1] -73.01152
jitter_latlong(42, type = "lat", km = 1)
#> [1] 41.99675
```
