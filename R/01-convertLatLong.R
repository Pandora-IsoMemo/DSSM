#' Convert Lat Long Wrapper
#'
#' @param showMessage (logical) if TRUE showNotification() displays success or failure
#' @inheritParams estimateMap
convertLatLongWrapper <- function(data, Longitude, Latitude, CoordType, showMessage = TRUE) {
  if (is.null(Longitude) || is.null(Latitude) || Longitude == "" || Latitude == "")
    return(data)

  dCoord <-
    try({
      convertLatLong(data,
                     CoordType = CoordType,
                     Latitude = Latitude,
                     Longitude = Longitude)
    }, silent = TRUE)

  if (inherits(dCoord, "try-error")) {
    ### Conversion failure ----
    message <- list(
      text = paste(
        "Conversion of coordinates failed.",
        "Please select appropriate longitude / latitude fields and coordinate format.",
        "Columns longitude and latitude not available.",
        sep = "<br>"
      ),
      type = "warning")

    convertedDat <- data

    # remove original Longitude and Latitude
    convertedDat[[Longitude]] <- NULL
    convertedDat[[Latitude]] <- NULL
  } else {
    ### Conversion success ----
    message <- list(
      text = paste(
        "Conversion of coordinates succeeded.",
        "Columns longitude and latitude set successfully.",
        sep = "<br>"
      ),
      type = "message"
    )

    convertedDat <- dCoord
    convertedDat$id <- as.character(1:nrow(convertedDat))
    convertedDat$longitude <- convertedDat[[Longitude]]
    convertedDat$latitude <- convertedDat[[Latitude]]

    # put longitude and latitude to beginning
    oldColNames <- colnames(convertedDat)
    oldColNames <-
      oldColNames[!(oldColNames %in% c("longitude", "latitude"))]
    convertedDat <-
      convertedDat[, c("longitude", "latitude", oldColNames)]
  }

  if(showMessage) showNotification(HTML(message$text), type = message$type)

  return(convertedDat)
}

convertLatLong <- function(isoData, CoordType, Latitude = "Latitude", Longitude = "Longitude"){
  isoData[, Longitude] <- convertCoordinates(isoData[, Longitude], CoordType)
  isoData[, Latitude] <- convertCoordinates(isoData[, Latitude], CoordType)
  if (all(is.na(isoData[, Longitude])) || all(is.na(isoData[, Latitude]))){
    stop("Coordinate transformation failed")
  }
  isoData
}

convertCoordinates <- function(x, from = "decimal degrees"){
  x <- gsub(",", ".", x)
  if (from == "decimal degrees"){
    return(as.numeric(x))
  }
  x <- gsub("\u2032", "'", x)
  x <- gsub("`", "'", x)

  if(from == "degrees decimal minutes"){
    deg <- sapply(strsplit(x, c("\u00B0")), function(k) k[1])
    min <- sapply(strsplit(x, c("\u00B0")), function(k) k[2])
    min <- sapply(strsplit(min, split = "[']+"), function(k) k[1])
    dd <- as.numeric(deg) + as.numeric(min) / 60
    dd[grepl("W", x) | grepl("S", x)] <- -dd[grepl("W", x) | grepl("S", x)]
  }
  if(from == "degrees minutes seconds"){
    deg <- sapply(strsplit(x, c("\u00B0")), function(k) k[1])
    rest <- sapply(strsplit(x, c("\u00B0")), function(k) k[2])
    min <- sapply(strsplit(rest, c("'")), function(k) k[1])
    sec <- sapply(strsplit(rest, c("'")), function(k) k[2])
    sec <- unlist(regmatches(sec, gregexpr("[-+.e0-9]*\\d", sec)))
    dd <- as.numeric(deg) + as.numeric(min) / 60 + as.numeric(sec) / 3600
    dd[grepl("W", x) | grepl("S", x)] <- -dd[grepl("W", x) | grepl("S", x)]
  }
  return(dd)
}
