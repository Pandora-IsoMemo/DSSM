# Changelog

## DSSM 25.09.0

### Bug Fixes

- *Interactive Map*: fixed brackets in export module (#284)

## DSSM 25.08.0

### New Features

- *Modeling tabs*: Added a “Show borders” toggle in the right sidebar,
  below “Show map grid”. (#281)

### Updates

- *Map layers*: refactored to S3 (MapLayers class + per-layer methods);
  replace addMapLayers() with plot(new_MapLayers(…)).

## DSSM 25.07.3

### New Features

- *Interactive Map*: Added option to shift the *North Arrow* or the
  *Scale* to a *custom* latitude and longitude position (#275)

## DSSM 25.07.2

### Updates

- export of time series plots: fixed bug (#268)
  - Added asynchronous creation of single plots for a time series. Now,
    users must press “Generate Plot Files” before the download of a
    series of plots.
  - Switched to using the **`gifski`** package for animated GIF
    creation, replacing the `magick` package, which crashed when
    handling a large number of input images.

## DSSM 25.07.1

### Bug Fixes

- *Estimates for (Bayesian) TimeR models*: Fixed a sign-error in the
  Metropolis Hastings Algorithm for the date uncertainty
  - This bug could lead to overly wide estimate ranges (#276)

## DSSM 25.07.0

### Bug Fixes

- reduced buffer that is added to the range of the default scale for the
  estimates (#276)

## DSSM 25.06.0

### New Features

- added logging of *high* and *critical* memory usage when running DSSM
  in a local Docker container (#206)

## DSSM 25.05.1

### Updates

- *interactive Map*:
  - added a preview to the export modal, allowing users to check the
    effect of width and height settings before exporting the map (#267)
  - option to set the *North Arrow* and *Scale Bar* size (#267)
  - Renamed input “Fit boundaries” to “Zoom into boundaries” to more
    clearly indicate the option for zoom fine-tuning (#267)

### Bug Fixes

- *interactive Map*:
  - removed maps from the dropdown that can no longer be accessed (#267)
  - fixed issue with scrolling bar in the window list (#267)

## DSSM 25.05.0

### Updates

- *interactive Map*:
  - *Map Settings*: separate checkbox for “Fixed” point aesthetics
    (#267)
  - option to close and open the view of the *Map settings* and
    *Statistics* panels (#267)

### Bug Fixes

- *interactive Map export*:
  - switch from deprecated phantomjs to chromium for webshot2 (#267)
  - fix issue with PDF export of maps
- *modelling tabs*:
  - update all maps to fix an issues with the plotting of maps for
    different ranges (#270)

## DSSM 25.04.0

### Updates

- use most recent shinyTools version with smaller header logos

## DSSM 25.03.1

### Updates

- skip large data tests in CI and add large test data to the
  `.Rbuildignore`

## DSSM 25.03.0

### Updates

- shift content of the help popup into a new vignette “How to use DSSM”
- update links in ReadMe and in app header
- reduce package size by optimizing test data and add example files to
  the `.Rbuildignore`

## DSSM 25.01.0

### Bug Fixes

- remove `Cairo::CairoSVG` since it did not help
- clip map layers to ranges xlim, ylim before adding them to the map
  (#259)
- handle the case of the *ocean* layer where geometries are added
  implicitly
  - cannot use intersection of the map with a bounding box
  - the layer must be subtracted from the bounding box
  - for pacific centering xlim must be split into parts smaller and
    larger zero

## DSSM 24.12.1

### Bug Fixes

- use `Cairo::CairoSVG` for export of `.svg` plots (#259)

## DSSM 24.12.0

### Updates

- *Modeling tabs*: updated the process of map centering (#248)
  - now the Pacific center is at 180° longitudes instead of 160°
    longitudes
  - old maps for Pacific were replaced by new maps
  - the code to create plots had to be refactored significantly

## DSSM 24.10.1

### Bug Fixes

- fix issue with penalty parameter when using Bayesian Modelling (#251)

## DSSM 24.10.0

### New Features

- *KernelR* + *KernelTimeR*: New restriction factor input for tclust
  clustering (#204)

## DSSM 24.09.2

### New Features

- *Centerpoint estimates*: Radius input is now hidden if the center
  coordinates are not set (#252)

## DSSM 24.09.1

### New Features

- *KernelR* + *KernelTimeR*: New option to adjust smoothness of kernel
  density estimator

## DSSM 24.09.0

### Bug Fixes

- fix issue with failing modeling for “Smooth Type” = “planar” with
  ‘number of spatial basis functions’ input (#247)

## DSSM 24.08.5

### New Features

- Adds tclust as clustering method in KernelR and KernelTimeR (#204)

### Bug Fixes

- solves issue with the plotting and export of cluster data in KernelR

## DSSM 24.08.4

### New Features

- *OperatoR*: New plot option (“estimation type”): ‘Significance
  (Overlap)’: Shows which non-significant overlap in difference maps

## DSSM 24.08.3

- Adds info button with rule of thumb information on selection of number
  of basis functions to “AverageR”, “TimeR” and “SpreadR” tabs (#236)

## DSSM 24.08.2

### New Features

- *Cost Surface and least-cost path for SpreadR*:
  - Estimate cost surface and shortest path using the gdistance package

## DSSM 24.08.1

### Bug Fixes

- fixes cluster ids being non continuous in some cases (#238)

## DSSM 24.08.0

### New Features

- adds option to TimeR and KernelTimeR to download a zipm file that can
  be uploaded in MapR (#203)

## DSSM 24.06.0

### New Features

- replace `rgeos::gCentroids()` with
  [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
  because of retired packages `rgeos` and `rgdal` (#228)
- Renaming of the Package
- R-CMD check workflow
- pkgdown Documentation
