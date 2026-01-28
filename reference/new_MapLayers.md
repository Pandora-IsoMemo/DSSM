# MapLayers S3 Class

A lightweight S3 class to manage and draw map layers (ocean, land,
coastlines, grid lines, country borders, and an optional center
meridian) on an existing plot. Supports "Europe" and "Pacific" map
centers. When centered on "Pacific", layers are split into `-180` and
`+180` halves to handle wrapping.

## Usage

``` r
new_MapLayers(
  Maps,
  terrestrial,
  centerMap,
  showBorders = TRUE,
  grid = FALSE,
  centerLine = FALSE,
  xlim = NULL,
  ylim = NULL
)
```

## Arguments

- Maps:

  A named list of `sf` spatial objects containing map layers.

- terrestrial:

  Integer; show only estimates on land masses (1), oceans (-1) or all
  (0); `1` to draw ocean fill, `-1` to draw land fill.

- centerMap:

  Character; either `"Europe"` or `"Pacific"` (or any non-"Europe"
  string to trigger split logic).

- showBorders:

  Logical; add country borders.

- grid:

  Logical; add graticule grid lines.

- centerLine:

  Logical; add a vertical line at the center longitude (0 in the
  plotting coordinate space you provide).

- xlim, ylim:

  Numeric length-2 vectors with plot limits in degrees.

## Value

An object of class `"MapLayers"`.

## Expected `Maps` structure

For **centerMap == "Europe"**:

- `$ocean`, `$land`, `$coast`, `$grids`, `$borders`

For **centerMap != "Europe"** (e.g. "Pacific"):

- `` $`ocean-180` ``, `` $`ocean+180` ``, `` $`land-180` ``,
  `` $`land+180` ``

- `` $`coast-180` ``, `` $`coast+180` ``, `` $`grids-180` ``,
  `` $`grids+180` ``

- `` $`borders-180` ``, `` $`borders+180` ``

All layers are expected as `sf` objects (polygons/lines). Internally we
convert to `sp` for plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
ml <- new_MapLayers(Maps, terrestrial = 1, centerMap = "Pacific",
                    showBorders = TRUE, grid = TRUE, centerLine = FALSE,
                    xlim = c(-180, 180), ylim = c(-60, 75))
plot(0, 0, type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xlab = "lon", ylab = "lat")
plot(ml)  # draws ocean, coastlines, optional grid/borders
} # }
```
