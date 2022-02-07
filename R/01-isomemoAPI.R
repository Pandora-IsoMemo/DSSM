callAPI <- function(action, ...) {
  # return(NULL)
  params <- list(...)
  paramString <- paste(names(params), params, sep = "=", collapse = "&")

  apiBaseURL <- Sys.getenv("API_BASE_URL")
  url <- paste(apiBaseURL, action, "?", paramString, sep = "")
  data <- fromJSON(url)

  if (data$status == 200) data
  else if (!is.null(data$message)) {
    warning(data$message)
    NULL
  } else if (!is.null(data$error)) {
    warning(data$error)
    NULL
  }
  else {
    warning("An error occured")
    NULL
  }
}

getDatabaseList <- function() {
  res <- callAPI("dbsources")
  if (!is.null(res)) res$dbsource
  else res
}

getRemoteDataAPI <- function(db = NULL) {
  res <- callAPI("iso-data", dbsource = paste(db, collapse = ","))
  if (!is.null(res)) {
    attr(res$isodata, "updated") <- res$updated
    fillIsoData(res$isodata, getMappingAPI())
  } else res
}

getMappingAPI <- function(){
  res <- callAPI("mapping")
  if (!is.null(res)) res$mapping
  else res
}

fillIsoData <- function(data, mapping){
  colToFill <- mapping$shiny[!(mapping$shiny %in% names(data))]
  data[colToFill] <- NA
  data
}

#' getMappingTable
#'
#' @export
getMappingTable <- function() {
  getMappingAPI()
}

#' getRemoteData
#'
#' @param db database
#'
#' @export
getRemoteData <- function(db) {
  if (is.null(db)) return(NULL)

  isoData <- getRemoteDataAPI(db = db)
  isoData[sapply(isoData, is.character)] <- lapply(isoData[sapply(isoData, is.character)], as.factor)
  isoData <- handleDescription(isoData)

  isoData
}


handleDescription <- function(isoData, maxChar = 20){
  isoData$description <- as.character(isoData$description)
  isoData$descriptionFull <- isoData$description
  isoData$description <- paste0(substr(isoData$description, 1, maxChar),
                                ifelse(nchar(isoData$description) > maxChar, " ...", ""))
  isoData

}
