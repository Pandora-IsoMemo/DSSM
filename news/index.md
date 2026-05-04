# Changelog

## DSSM 26.04.1

### Updates

- *Interactive map export*: added a notice about third-party basemap
  licenses/terms and attribution requirements, and added a corresponding
  note to `README.md`.

## DSSM 26.04.0

### Updates

- Added rnaturalearth attribution in `RScripts/update_maps.R`, and added
  an acknowledgments section in `README.md`.
- Appended a “Third-Party Licenses” section (including MIT license text
  for rnaturalearth) to `LICENSE.md`.
- Added a recommended citation for mclust in `README.md`.

## DSSM 26.03.2

### Updates

- Refactored the summarise() call in findDuplicates() to remove the
  deprecated unnamed column expression, fixing compatibility with recent
  dplyr versions.

## DSSM 26.03.1

### Updates

- Replaced `pryr::mem_used()` with a new helper based on gc() and
  `pryr::object_size()` with utils::object.size().
- Removed `pryr` dependency (not available on CRAN anymore).

## DSSM 26.03.0

### Updates

- Removed the deprecated fileExtension argument from
  [`DataTools::importServer()`](https://pandora-isomemo.github.io/data-tools/reference/importServer.html)
  calls across model/map modules.
- Added additional logging around 3D model execution.
- Adjusted variance-spline construction in
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html) to
  improve smoothing stability (#296).

## DSSM 26.01.1

### Updates

- Refactored colour palette handling by introducing a centralized,
  reusable colour palette module (#293).
  - Replaced duplicated UI and server logic across multiple
    visualisation modules.
  - Added support for single-colour, multi-colour, white-start, and
    diverging palettes.
  - Updated map plotting functions to use the new palette
    infrastructure.

## DSSM 26.01.0

### Updates

- Added logging of object sizes also to the modelling tabs *KernelR,
  KernelTimeR, SpreadR AssignR* (#206)

### Bug Fixes

- Fixed hidden UI for setting the center estimates in *KernelTimeR* time
  course plots (#292)
- Fixed decimal-place settings for axis labels: x and y axes now have
  separate inputs, with an improved default for the y axis (1 decimal
  place) to prevent rounding issues (#292)

## DSSM 25.12.0

### Bug Fixes

- Fixed an issue where date columns were parsed as character instead of
  numeric, causing errors when calculating date ranges and means in the
  UI (#289).
- Added logging of object sizes for easier debugging of memory issues.
  (#289)
- Extracted common code into helper functions to reduce code
  duplication.

## DSSM 25.10.0

### New Features

- BibTeX Citation Formatting: Added support for formatting and exporting
  BibTeX citations. Users can now select citation styles (APA, Chicago,
  Harvard) and output formats (text, HTML, LaTeX, etc.) for BibTeX
  entries. New UI modules allow configuration of citation styles and
  columns, and citation export supports user-selected formatting.

## DSSM 25.09.1

### New Features

- *Modeling tabs - TimeR*: option to export a series of geotiff files
  for each selected time slice (#286)

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
