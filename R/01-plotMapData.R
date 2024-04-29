#' Center Plot Data
#'
#' @param data data frame
#' @param centerMap (character) center of map, one of "Europe" and "Pacific"
#'
#' @return data frame
centerPlotData <- function(data, centerMap = c("Europe", "Pacific")) {
  centerMap <- match.arg(centerMap)
  if (centerMap == "Europe") return(data)

  # if centerMap != "Europe":
  dataPac <- data
  dataPac$Longitude[data$Longitude < -20] <- dataPac$Longitude[data$Longitude < -20] + 200
  dataPac$Longitude[data$Longitude >= -20] <- (- 160 + dataPac$Longitude[data$Longitude >= -20])

  return(dataPac)
}

#' Filter Time
#'
#' @param data (data.frame) data to be filtered by time containing columns "Date" and "Uncertainty"
#' @param addU (numeric) additional uncertainty to be added to time frame
#' @param time (numeric) time for filter
#'
#' @return (data.frame) filtered data
filterT <- function(data, addU, time) {
  data[data$Date + 2 * data$Uncertainty + addU >= time  &
         data$Date - 2 * data$Uncertainty - addU <= time , ]
}

#' Extract Mask Draw
#'
#' @param dat (data.frame) data frame
#' @param maskRadius (numeric) mask radius
#' @param XPred (matrix) matrix of predictions
#'
#' @return (numeric) mask draw
extractMaskDraw <- function(dat, maskRadius, XPred) {
  if (length(dat) == 0 || nrow(dat) == 0) return(rep(0, nrow(XPred)))

  maskDraw <- sapply(1:nrow(dat), function(x) sqrt((dat$Longitude[x] - XPred[, "Longitude"])^2 + (dat$Latitude[x] - XPred[, "Latitude"])^2) < maskRadius)
  maskDraw <- apply(maskDraw, 1, max)

  return(maskDraw)
}

#' Center XPred
#'
#' @param XPred (matrix) matrix of predictions
#' @param XPredPac (matrix) matrix of predictions for Pacific
#' @param centerMap (character) center of map, one of "Europe" and "Pacific"
#'
#' @return (matrix) matrix of predictions
centerXPred <- function(XPred, XPredPac, centerMap) {
  if(centerMap != "Europe"){
    return(XPredPac)
  } else {
    return(XPred)
  }
}
