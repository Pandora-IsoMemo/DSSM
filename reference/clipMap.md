# Clip map to bounding box

Clips an `sf` layer to `xlim`/`ylim`. For `layer="ocean"`, subtracts
unioned land to explicitly define ocean polygons inside the bbox.

## Usage

``` r
clipMap(map, layer, xlim, ylim, mapLand = NULL)
```

## Arguments

- map:

  `sf` object to clip.

- layer:

  One of `"ocean"`, `"land"`, `"coast"`, `"grid"`, `"borders"`.

- xlim, ylim:

  Numeric length-2 vectors; if `NULL`, returns full layer.

- mapLand:

  `sf` object of land polygons (required when `layer == "ocean"` to
  subtract land).

## Value

An `sp` object suitable for plotting.
