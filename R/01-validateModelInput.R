validInput <- function(model){
  if (is.list(model)){
     NULL
  } else if (is.null(model)) {
    "Please select a dependent and other necessary variables in the drop-down menu on the
    left sidebar panel and click the Start-Button."
  } else if (model == "non-numeric latitude or longitude") {
    "Non-numeric latitude or longitude variables in your data. Please check the coordinate format
    and your variables in the \"Longitude\" and \"Latitude\" fields."
  } else if (model == "Longitude or Latitude not available.") {
    "Longitude or Latitude not available. Please check your data, the coordinate format
    and your variables in the \"Longitude\" and \"Latitude\" fields."
  } else if (model == "less than 4 rows") {
    "Not enough data, less than 4 rows or unique long/lat combinations. Please check your data!"
  } else if (model == "less than 11 rows") {
    "Not enough data, less than 11 rows or unique long/lat/time combinations.
    Please check your data!"
  } else if (model == "non-numeric independent variable") {
    "Non-numeric independent variable. Please check your data!"
  } else if (model == "non-numeric date field 1 variable") {
  "Non-numeric or NA date field 1 variable. Please check your data!"
  } else if (model == "non-numeric date field 2 variable") {
  "Non-numeric or NA date field 2 variable. Please check your data!"
  } else if (model == "wrong site variable") {
  "NA Site ID variable. Please check your data!"
  } else if (model == "Error in Model Fitting.") {
  "Error in model fitting. Please check your input variables assignments and your data!"
  } else if (model == "Weights must be non-negative numeric values."){
    "Weights must be non-negative numeric values."
  } else if (model == "presence/absence variable must be numeric 0 or 1 values"){
    "Presence/absence variable must be numeric 0 or 1 values."
  } else if (model == "At least one present observation must be included in presence/absence variable."){
    "At least one present observation must be included in presence/absence variable."
  } else if (model == "Not enough data available."){
    "Not enough data available."
  }
}
