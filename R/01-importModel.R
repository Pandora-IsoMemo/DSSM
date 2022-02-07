readFile <- function(file, type, sep = ",", dec = "."){
  encTry <- as.character(guess_encoding(file)[1,1])

  tryCatch(
    switch(
      type,
      csv = read.csv(file, sep = sep, dec = dec, stringsAsFactors = FALSE, fileEncoding = encTry),
      xlsx = convertNumeric(read.xlsx(file))
    ),
    error = function(e){
      warning("Could not read file")
      FALSE
    }
  )
}

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
convertNumeric <- function(data){
  suppressWarnings(data.num <- as.data.frame(lapply(1:ncol(data), function(x){
    y <- as.numeric(data[,x])
    if(sum(is.na(y)) == sum(is.na(data[,x]))){
      return(y)
    } else {
      return(data[,x])
    }
  } )))
  names(data.num) <- names(data)
  data.num
}
