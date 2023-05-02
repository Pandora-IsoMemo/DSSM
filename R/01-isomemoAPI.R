callAPI <- function(action, ...) {
  # return(NULL)
  params <- list(...)
  paramString <- paste(names(params), params, sep = "=", collapse = "&")

  if (Sys.getenv("API_BASE_URL") == "" && Sys.getenv("API_BASE_URL_DEFAULT") == "") {
    stop(paste0("Cannot reach API. Environment variable 'API_BASE_URL' is missing. ",
                "Please add the API_BASE_URL to your .Renviron file ",
                "(e.g. 'API_BASE_URL=https://isomemodb.com/testapi/v1/'), or provide ",
                "API_BASE_URL as a parameter to docker ",
                "(e.g. 'docker run -p 3838:3838 -e API_BASE_URL=https://isomemodb.com/api/v1/ ghcr.io/pandora-isomemo/iso-app:main')."))
  }

  if (Sys.getenv("API_BASE_URL") != "") {
    apiBaseURL <- Sys.getenv("API_BASE_URL")
  } else {
    apiBaseURL <- Sys.getenv("API_BASE_URL_DEFAULT")
  }

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
