validateImport <- function(data, showModal = FALSE){
  warnings <- character(0)

  if (!inherits(data, "data.frame"))
    warnings <- append(
      warnings,
      "Data has wrong type"
    )

  if (NROW(data) < 3)
    warnings <- append(
      warnings,
      "Data has less than 3 rows or missing/wrong formatted coordinate variables, check your data and/or coordinate format settings and
       - in case of .csv file format - the column and decimal separators"
    )

  if (showModal & length(warnings) > 0){
    showModal(modalDialog(
      footer = modalButton("OK"),
      lapply(warnings, div)
    ))
  } else {
    lapply(warnings, warning)
  }
  messageSucc <- paste("Data import successful, imported", NROW(data),
                       "rows and", NCOL(data), "variables.
                         If these numbers deviate from your original file, please check if
                         you selected the right coordinate format and - in case of .csv file format -
                         the column and decimal separators")

  if (showModal & length(warnings) == 0){
    showModal(modalDialog(messageSucc, footer = modalButton("OK")))
  } else {
    message(messageSucc)
  }

  length(warnings) == 0
}
