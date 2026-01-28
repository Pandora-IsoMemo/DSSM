# Draw all map layers

S3 plot method that draws (in order) the filled base (ocean or land),
coastlines, optional grids, optional borders, and optional center line.
All drawing is done with `add = TRUE`; initialize your plotting device
beforehand (e.g., `plot(0,0,type="n", ...)`).

## Usage

``` r
# S3 method for class 'MapLayers'
plot(x, ...)
```

## Arguments

- x:

  A `MapLayers` object.

- ...:

  Unused.
