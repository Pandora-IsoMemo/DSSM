# Create PCH Points

Create PCH Points

## Usage

``` r
createPchPoints(
  pch = 16,
  width = 50,
  height = 50,
  bg = "transparent",
  col = "black",
  tmpDir = tempdir(),
  pattern = "symbolFile",
  ...
)
```

## Arguments

- pch:

  plotting ‘character’, i.e., symbol to use. See graphics::points for
  details

- width:

  width in pixel

- height:

  height in pixel

- bg:

  initial background colour

- col:

  color code or name

- tmpDir:

  directory for storing the icons

- pattern:

  pattern to be used in filenames

- ...:

  Further graphical parameters that are passed to graphics::points()
