# North Arrow for Interactive Maps

Creates a North Arrow object for use in interactive maps.

## Usage

``` r
new_NorthArrow(
  position = "none",
  width = 80,
  height = 80,
  lat = NULL,
  lng = NULL,
  layerId = "northArrowIcon"
)
```

## Arguments

- position:

  Position of the North Arrow. Options are "topright", "topleft",
  "bottomright", "bottomleft", "custom", or "none".

- width:

  Width of the North Arrow in pixels.

- height:

  Height of the North Arrow in pixels.

- lat:

  Latitude for custom positioning (required if position is "custom").

- lng:

  Longitude for custom positioning (required if position is "custom").

- layerId:

  Unique identifier for the North Arrow layer.

## Value

A NorthArrow object.
