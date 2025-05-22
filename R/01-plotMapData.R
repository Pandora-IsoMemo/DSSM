# Center the plot data to Pacific if centerMap is "Pacific"
#
# @param data data frame
# @param centerMap (character) center of map, one of "Europe" and "Pacific"
#
# @return data frame
shiftDataToCenter <- function(data, centerMap = c("Europe", "Pacific"), threshold = 0) {
  centerMap <- match.arg(centerMap)
  if (centerMap == "Europe") return(data)

  # if centerMap != "Europe":
  dataPac <- data
  dataPac$Longitude <- dataPac$Longitude %>% shiftLongitudesToPacific(threshold = threshold)

  return(dataPac)
}

# Shift to Pacific
#
# @param longitudes (numeric) longitudes to be shifted
# @param threshold (numeric) threshold value
# @param order (logical) whether to order the shifted values
shiftLongitudesToPacific <- function(longitudes, threshold = 0, order = FALSE) {
  shifted <- longitudes
  shifted[longitudes < threshold] <- shifted[longitudes < threshold] + 180 - threshold
  shifted[longitudes >= threshold] <- shifted[longitudes >= threshold] - 180 - threshold
  if (order) {
    # Order the shifted values
    shifted <- shifted[order(shifted)]
  }

  return(shifted)
}

extractRangeFromData <- function(data, column = c("Longitude", "Latitude"), move = 0) {
  column <- match.arg(column)

  - diff(range(data[[column]], na.rm = TRUE)) / 2 +
    max(data[[column]], na.rm = TRUE) + move
}

zoomLongitudeRange <- function(rangex, zoom, upperLeftLongitude, center = c("Europe", "Pacific"), move = 0) {
  center <- match.arg(center)

  if (is.na(upperLeftLongitude)) return(rangex + c(-zoom / 2, zoom / 2))

  # set custom range
  upperLeftLongitude <- upperLeftLongitude + move
  if (center != "Europe") {
    upperLeftLongitude <- upperLeftLongitude %>% shiftLongitudesToPacific()
  }

  return(upperLeftLongitude + c(0, zoom))
}

zoomLatitudeRange <- function(rangey, zoom, upperLeftLatitude, move = 0) {
  if (is.na(upperLeftLatitude)) return(rangey + c(-zoom / 4, zoom / 4))

  rangey <- upperLeftLatitude + move + c(-zoom / 2, 0)
  return(rangey)
}

constrainLongitudeRange <- function(rangex, zoom = 50, center = c("Europe", "Pacific")) {
  center = match.arg(center)
  if (center == "Europe") {
    if (rangex[2] > 180) rangex <- c(180 - zoom, 180)
    if (rangex[1] < -180) rangex <- c(-180, -180 + zoom)
  } else {
    if (rangex[2] > 360) rangex <- c(360 - zoom, 360)
    if (rangex[1] < 0) rangex <- c(0, 0 + zoom)
  }

  rangex
}

constrainLatitudeRange <- function(rangey) {
  rangey <- pmin(90, pmax(-90, rangey))
  if (rangey[2] > 90) rangey <- pmin(90, rangey - (rangey[2] - 90))
  if (rangey[1] < -90) rangey <- pmax(-90, rangey - (rangey[1] + 90))

  rangey
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

# Center XPred
#
# @param XPred (matrix) matrix of predictions
# @param XPredPac (matrix) matrix of predictions for Pacific
# @param centerMap (character) center of map, one of "Europe" and "Pacific"
#
# @return (matrix) matrix of predictions
centerXPred <- function(XPred, XPredPac, centerMap) {
  if(centerMap != "Europe"){
    return(XPredPac)
  } else {
    return(XPred)
  }
}
