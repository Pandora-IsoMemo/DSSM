# MpiIsoApp development version

## MpiIsoApp 22.10.1

### New features
- specify the sheet of an xlsx/xls file in the import dialog
- new button "Center Map" in the _Interactive map_ section that centers the map at the mean 
longitude and latitude of the displayed data

### Enhancements
- for Pandora skin: set default Latitude and Longitude columns after data load if match was 
found (#67)

### Bug fixes
- fix failing of import of xlsx files with sheets (-> option to select the sheet)
- fix missing columns in the selection of `Longitude` and `Latitude` if `Coordinate format` was not
equal "decimal degrees" (#62)
- fix in the removal of points on the interactive map after using filters (#63)

## MpiIsoApp 22.09.3

### New features
- export a time series of _spatio-temporal-average_ plots in the selected file format condensed
into a zip file

## MpiIsoApp 22.09.2

### New features
- option to adjust the format of the axes in time course plots (decimal places and number of labels)
- option to adjust the decimal places of the center estimates for mapType == "Map" or "Spread"

## MpiIsoApp 22.09.1

### Enhancements
- In the _Modelling_ tab under _TimeR_ and _KernelTimeR_: new (numeric) input field for the
`Time selection` in addition to the slider input `Time selection`

## MpiIsoApp 22.08.2

### Enhancements
- when using _Pandora_ skin: updates of the UI in the _Import Data_ pop-up menu (#39)
- update label names in the _maps_ tab (#48)

### Bug fixes

- fixes in the modelling tab _AssignR_: (#46)
