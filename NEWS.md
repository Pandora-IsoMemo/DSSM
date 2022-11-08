# MpiIsoApp development version

## MpiIsoApp 22.11.2

### New features

- when using _Pandora_ skin, in _Import Data_:
  - option to merge two data tables before import (#37)
  - option to prepare data before import or merge
    - rename columns
    - join columns
    - split columns
    - delete columns
  - checks for correct column names in file imports
  - update column names of files if naming conflicts
  - new tests

### Updates
- disable accept button(s) by default in _Import Data_

## MpiIsoApp 22.11.1

### Updates

- extract z scale settings ("Estimation type", "Show estimates", Min/Max range, "Restrict range")
into their own module
- new module zScale applied in the modelling tabs: _AverageR_, _KernelR_, _TimeR_, _KernelTimeR_,
_SpreadR_, _OperatoR_, _LocateR_
- switch the title for min/max range dependent on the "Plot type"

### Bug fixes
- fix update of min/max values after removing a restriction (min/max values had been overwritten
before, that is, removing a restriction did not reset min/max values) (#27)
- remove duplicated re-rendering of the map when changing "Estimation type" or restricting the range
- update choices of "Estimation type" when switching the "Plot type" in _TimeR_ since some types
are not implemented for "Time course"
- debounce inputs for min/max range to prevent re-rendering of the map when typing min/max values 

## MpiIsoApp 22.10.3

### Updates
- new UI to specify the section of maps of modelling tabs (_AverageR, TimeR, SpreadR, KernelR,
KernelTimeR, OperatorR, LocateR_)
  - dynamically use the buttons up, down, center, ...
  - apply a button "Set (Time and) Map Section" to set
    - time (only for _TimeR_ and _KernelTimeR_),
    - zoom in degrees of longitude, or
    - lat/long of upper left corner
- new modules that encapsulate the functionality behind the new UI
- applying the new modules in all modelling tabs and removing copy-paste code

### Bug fixes
- removes the slow **automatic** updates for inputs of time and zoom (#8)

## MpiIsoApp 22.10.2

### Bug fixes
- fixing the point radius changing with latitude, now set radius in pixel (#44)

## MpiIsoApp 22.10.1

### New features
- specify the sheet of an xlsx/xls file in the import dialog
- new button "Center Map" in the _Interactive map_ section that centers the map at the mean 
longitude and latitude of the displayed data

### Enhancements
- for Pandora skin: set default Latitude and Longitude columns after data load if match was 
found (#67)
  - note: the default format is "decimal degrees", here we still apply the check for numeric columns

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

# MpiIsoApp 22.08.2.1

## Updates

- text update of the modeling tab for the _pandora / isomemo search app_ (#9)

## MpiIsoApp 22.08.2

### Enhancements
- when using _Pandora_ skin: updates of the UI in the _Import Data_ pop-up menu (#39)
- update label names in the _maps_ tab (#48)

### Bug fixes

- fixes in the modeling tab _AssignR_: (#46)
