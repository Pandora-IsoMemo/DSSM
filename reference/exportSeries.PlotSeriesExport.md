# Export the series of plots to files or a GIF

This function handles the export of the generated plots to either a ZIP
file or a GIF, depending on the specified type of series.

## Usage

``` r
# S3 method for class 'PlotSeriesExport'
exportSeries(
  obj,
  file,
  modelType,
  typeOfSeries,
  fpsGif = 1,
  reverseGif = FALSE,
  ...
)
```

## Arguments

- obj:

  A PlotSeriesExport object.

- file:

  The name of the output file (ZIP or GIF).

- modelType:

  A string indicating the type of model (e.g., "local-average",
  "spread", etc.) for naming conventions.

- typeOfSeries:

  A string indicating the type of series to export ("onlyZip",
  "onlyGif", or "gifAndZip").

- fpsGif:

  The frames per second for the GIF (default is 1).

- reverseGif:

  A boolean indicating whether to reverse the order of the GIF frames
  (default is FALSE).

- ...:

  Additional arguments (not used).

## Value

The modified PlotSeriesExport object with updated status.
