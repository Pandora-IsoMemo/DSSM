# server funtion of leaflet map export module

server funtion of leaflet map export module

## Usage

``` r
leafletExport(
  input,
  output,
  session,
  leafletMap,
  width,
  height,
  zoom,
  center,
  isoData,
  leafletValues,
  leafletPointValues
)
```

## Arguments

- input:

  input

- output:

  output

- session:

  session

- leafletMap:

  reactive leaflet map object

- width:

  reactive width of map in px

- height:

  reactive height of map in px

- zoom:

  map zoom

- center:

  map center (list of lat and lng)

- isoData:

  reactive isoData data

- leafletValues:

  map settings, e.g. scalePosition, show/hide bounds

- leafletPointValues:

  reactive settings for points on map
