# PlotSeriesExport Class

This class is used to manage the export of a series of plots generated
by a plotting function. It allows for the generation of plots at
specified time points, saving them in various formats, and exporting
them as a series of files or a GIF.

## Usage

``` r
new_PlotSeriesExport(
  plotFun,
  Model,
  times,
  exportType = "png",
  modelType = "",
  width = 1280,
  height = 800
)
```

## Arguments

- plotFun:

  A function that generates a plot given a model and a time point.

- Model:

  The model object that the plotting function will use.

- times:

  A vector of time points at which to generate the plots.

- exportType:

  The type of file to export the plots as (e.g., "png", "jpeg", "pdf",
  etc.).

- modelType:

  A string indicating the type of model (e.g., "local-average",
  "spread", etc.) for naming conventions.

- width:

  The width of the exported plots in pixels.

- height:

  The height of the exported plots in pixels.
