# How to use DSSM

Welcome to Pandora & IsoMemo Data Search and Spatiotemporal Modeling!

The [“Data”](#data) tab serves for selection of data from various
isotopic databases.

With the [“Interactive map”](#map) tab, the selected data can be viewed
in an interactive interface on the world map.

The [“Modeling”](#modeling) tab serves for the computation of models
using the selected data (or user data provided in a data-file) for
estimating a [spatial average model](#local-average-model), a
[spatio-temporal average model](#spatio-temporal-average-model) or a
[spread model](#spread-model).

For a video introduction please have a look at:  
<https://www.youtube.com/watch?time_continue=1&v=B-isiVu09zQ>

## Data

On the left panel the user can select the databases and load them into
the app by using the **“Load data”** button. Under **“Select
categories”** the user may select categories or groups of fields. Each
category has multiple fields or variables which can be selected
individually by clicking on the drop down menu.

A certain selection of fields can be saved and re-loaded by the **“Save
data selection”** and **“Load data selection”** buttons.

Once the data is loaded, a spreadsheet appears on the screen. It can be
sorted using the values by certain fields by clicking on the field name
and filtered by clicking on the entry below the field name.

The selected and filtered data set in the spreadsheet can be exported in
`.xlsx`, `.csv`, or `.json` format using the **“Export”** button at the
bottom of the left panel.

### Radiocarbon Calibration

On the left panel the calibration options for radiocarbon dates can be
found. By selecting a method all uncalibrated radiocarbon dates in the
displayed spreadsheet are calibrated by the selected method. The
coverage of the credible intervals in percent can be selected in the
**“Calibration range”** field.

The calibration is done with the help of the
[Bchron](https://cran.r-project.org/web/packages/Bchron/index.html) R
package.

## Interactive Map

Once data is loaded into the dashboard view in the [“Data”](#data) tab,
all locations of the selected data are displayed on the world map. Keep
in mind that the `longitude` and `latitude` variables have to be
available and selected.

By clicking on a location the field values selected under **“Select
categories”** in the [“Data”](#data) tab can be seen.

On the right panel one or two numeric fields of the selected data can be
chosen. One- and two-dimensional summary statistics and plots (with
different options) are computed and displayed. The statistics can be
copied into the clipboard for further use. The plots can be exported in
various formats.

## Modeling

### Local Average Model

Here, a spatial or local average of a user-defined independent variable
is modeled. A GAM (Generalized Additive Model) or GAMM (Generalized
Additive Mixed Model) is fitted here.

#### Left Panel - Data and Model Options

- **Data selection**: Under “Data” source the user can select if they
  want to use the data from the “Data” tab or upload a `.xlsx` or `.csv`
  file. For uploads, the first row must contain variable names, and the
  coordinate format must be chosen from: “decimal degrees”, “degrees
  decimal minutes”, or “degrees minutes seconds”. If a `.csv` is
  selected, column and decimal separators must also be specified.
- **Variable selection**: Specify the independent variable, its
  uncertainty in standard deviations (optional), the longitude and
  latitude variable, and optionally a site/ID variable. If selected, a
  random intercept is added to the model.
- **Model options**:
  - The “Number of basis functions” governs the number of basis
    functions used for the approximation of the surface, so called
    approximate thin-plate-splines (see
    e.g. [doi:10.1007/3-540-47977-5_2](https://doi.org/10.1007/3-540-47977-5_2)
    or
    [doi:10.1111/1467-9868.00374](https://doi.org/10.1111/1467-9868.00374)
    for further information). A higher number delivers more exact
    results and enables the estimation of more complex 2-D surfaces, but
    uses much more computational power. In general, there is little gain
    from very high numbers and there is no additional gain from
    selecting a higher number than there are unique combinations of
    longitude and latitude.
  - A Bayesian option is available, but increases computation time.
    Select how much MCMC (Markov Chain Monte Carlo) iterations should be
    computed. Higher numbers deliver more exact results at the expense
    of computation time (usually 1000 iterations already give a pretty
    usable results for testing). The smoothing degree can be adjusted,
    the model already selects an optimal amount of smoothing itself, but
    this provides the option to estimate a more or less smoother
    surface.
  - Click **“Run”** to estimate the model.

#### Central Panel - Display, Export, and Map Selection

The estimated spatial average is displayed in the center. An export
button is provided below, together with sliders for map section
selection.

#### Right Panel - Graphic Options

- Options to control the displayed plot include range restriction
  settings (0–1 or 0–100), surface type (terrestrial, aquatic, or
  complete), markers, grid, scale, north arrow.
- The “Apply convex hull” checkbox marks, whether estimates of locations
  within the convex hull of the data points should be displayed. In
  general, this is indicated as extrapolation beyond areas with
  available data is highly speculative.
- The slider “Display up to max standard error” governs the area where
  we have a standard error below the selected mark to prevent areas with
  low certainty being shown.
- Right under the colour options, a slider “Plot resolution” is
  available. A higher number delivers a higher quality picture of the
  coloured area with less pixaleted borders but needs much more drawing
  time. A lower value is recommended for setting the right picture
  options and map section and a high value for publication.

#### Location/Batch Estimate

The options on the bottom of the rightern panel enable the user to get
an estimate of a specified location (**“Center point
latitude/longitude”**) within a certain radius. The estimate with
standard error will appear below the x-axis in the displayed plot.

For getting estimates for a batch of points the **“Batch Estimates”**
button opens a pop-up window, where a user-specified data set can be
uploaded. A local estimate including standard error and 95 percent
uncertainty intervals is added and can be exported.

------------------------------------------------------------------------

### Spatio-Temporal Average Model

Here, a spatio-temporal average of a user-defined independent variable
is modeled. A GAM (Generalized Additive Model) or GAMM (Generalized
Additive Mixed Model) is fitted.

#### Left Panel - Data and Model Options

- **Data and variable selection**: Same setup as
  [above](#local-average-model), but with additional date variables and
  a **“Date type”** selection (Interval, Mean + 1 SD, or Single point).
  Depending on type, appropriate fields must be filled.

- **Model options**: Modeling options are identical: basis functions,
  Bayesian model toggle, MCMC iterations, and smoothing level.

#### Central Panel - Display, Export, and Map Selection

The spatial average is displayed, exportable via button below. Map
section adjustable with sliders.

#### Right Panel - Graphic Options

Includes a **“Plot type = time course”** option to show estimate time
series at a chosen location with uncertainty intervals instead of a 2-D
slice at a certain point in time. All other display controls match the
local model.

#### Location/Batch Estimate

Same procedure as above: location-based and batch-based estimation with
export support.

------------------------------------------------------------------------

### Spread Model

Here, a time spread of a user defined date variable is modeled. An
extremal quantile regression model (see
e.g. [doi:10.1214/009053604000001165](https://doi.org/10.1214/009053604000001165)
is fitted here to get an estimate very close to the local minimum or
maximum. This is done using a Bayesian quantile approach (see
e.g. [doi:10.1016/S0167-7152(01)00124-9](https://doi.org/10.1016/S0167-7152(01)00124-9))
with a quantile set near 0/1.

#### Left Panel - Data and Model Options

- **Data and variable selection**: The user selects the data source and
  specifies the date variables, longitude, latitude, and optionally a
  site/ID variable, see setup as [above](#local-average-model). The
  **“Date type”** must be specified (Interval, Mean ± SD, Single point).

- **Model options**: Model direction (**Min** or **Max**) must be
  chosen. Then, basis functions, Bayesian toggle, MCMC count, and
  smoothing can be set, see setup as mentioned
  [above](#local-average-model)

#### Central Panel - Display, Export, and Map Selection

The estimated spatial average is shown in the center. Export options are
available. A **“Time selection”** slider allows selection of the
estimate’s time point. Sliders adjust the map section.

#### Right Panel - Graphic Options

Optional display of **spreading speed (gradient, in km/year)**. All
other controls — surface type, standard error filtering, convex hull
display, plot resolution — match previous models.

#### Location/Batch Estimate

As with other models, location-based estimation and batch uploads are
supported. Estimates with uncertainty intervals can be exported.

------------------------------------------------------------------------

### Other Tabs

Help not yet available for other models.
