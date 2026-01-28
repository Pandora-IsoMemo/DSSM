# ScaleBar Class

This class represents a scale bar for interactive maps.

## Usage

``` r
new_ScaleBar(
  position = "none",
  zoom = NULL,
  size = 100,
  lat = NULL,
  lng = NULL
)
```

## Arguments

- position:

  The position of the scale bar on the map. Options are "topright",
  "topleft", "bottomright", "bottomleft", "custom", or "none".

- zoom:

  The zoom level at which the scale bar is displayed. If \`NULL\`, it
  defaults to the current zoom level of the map.

- size:

  The size of the scale bar in pixels. Default is 100.

- lat:

  The latitude for a custom position of the scale bar. Required if
  \`position\` is "custom".

- lng:

  The longitude for a custom position of the scale bar. Required if
  \`position\` is "custom".

## Value

An object of class \`ScaleBar\`.
