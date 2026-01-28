# Filled Countour 2 Wrapper for contour plot

Filled Countour 2 Wrapper for contour plot

## Usage

``` r
filled.contour2(
  x = seq(0, 1, length.out = nrow(z)),
  y = seq(0, 1, length.out = ncol(z)),
  z,
  contourType = c("filled.contour", "contour"),
  xlim = range(x, finite = TRUE),
  ylim = range(y, finite = TRUE),
  zlim = range(z, finite = TRUE),
  levels = pretty(zlim, nlevels),
  nlevels = 20,
  showScale = TRUE,
  color.palette = cm.colors,
  col = color.palette(length(levels) - 1),
  plot.title,
  plot.axes,
  key.title,
  key.axes,
  asp = NA,
  xaxs = "i",
  yaxs = "i",
  las = 1,
  axes = TRUE,
  frame.plot = axes,
  ...
)
```

## Arguments

- x:

  x values

- y:

  y values

- z:

  z values

- contourType:

  one of "filled.contour" or "contour"

- xlim:

  x limits

- ylim:

  y limits

- zlim:

  z limits

- levels:

  levels

- nlevels:

  number of levels

- showScale:

  show colour scale

- color.palette:

  color palette

- col:

  colors

- plot.title:

  plot title

- plot.axes:

  plot axes

- key.title:

  key title

- key.axes:

  key axes

- asp:

  aspect ratio

- xaxs:

  x axis style

- yaxs:

  y axis style

- las:

  label style

- axes:

  show axes

- frame.plot:

  frame plot

- ...:

  additional arguments
