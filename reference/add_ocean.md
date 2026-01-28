# Add ocean layer (generic)

Generic for adding an ocean layer to a map plot.

## Usage

``` r
add_ocean(x, ...)

# S3 method for class 'MapLayers'
add_ocean(x, ...)
```

## Arguments

- x:

  A `MapLayers` object.

- ...:

  Further arguments passed to methods.

## Details

For `MapLayers`, `add_ocean()` fills the ocean area respecting
`xlim`/`ylim` and `centerMap` wrapping.
