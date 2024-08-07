#' Augment Data
#'
#' Data augmentation to add data at the borders of the map
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @param restriction (numeric) restriction in the form c(minLatitude, maxLatitude, minLongitude, maxLongitude)
#' @return (data.frame) augmented data
augmentData <- function(data, restriction = c(-90, 90, -320, 320)) {
  if (!(
    max(data$Latitude) > 75 ||
    max(data$Longitude) > 150 ||
    min(data$Longitude) < -150 || min(data$Latitude) < -75
  )) {
    return(data)
  }

  dataBottom  <- data
  dataTop  <- data
  dataLeft  <- data
  dataRight  <- data
  dataLeftBottom <- data
  dataLeftTop <- data
  dataRightBottom <- data
  dataRightTop <- data

  dataBottom$Latitude <- -90 - (dataBottom$Latitude + 90)
  dataBottom$Longitude <- dataBottom$Longitude + 180
  dataBottom$Longitude[dataBottom$Longitude >= 180] <- dataBottom$Longitude[dataBottom$Longitude >= 180] - 360

  dataTop$Latitude <- 90 + (90 - dataTop$Latitude)
  dataTop$Longitude <- dataTop$Longitude + 180
  dataTop$Longitude[dataTop$Longitude >= 180] <- dataTop$Longitude[dataTop$Longitude >= 180] - 360

  dataLeft$Longitude <- -180 - (-dataLeft$Longitude + 180)
  dataRight$Longitude <- 180 + (180 + dataRight$Longitude)

  dataLeftBottom$Longitude <- dataBottom$Longitude - 360
  dataLeftBottom$Latitude <- dataBottom$Latitude

  dataLeftTop$Longitude <- dataTop$Longitude - 360
  dataLeftTop$Latitude <- dataTop$Latitude

  dataRightBottom$Longitude <- dataBottom$Longitude + 360
  dataRightBottom$Latitude <- dataBottom$Latitude

  dataRightTop$Longitude <- dataTop$Longitude + 360
  dataRightTop$Latitude <- dataTop$Latitude

  data2 <- rbind(
    data,
    dataLeft,
    dataRight,
    dataBottom,
    dataTop,
    dataLeftBottom,
    dataLeftTop,
    dataRightBottom,
    dataRightTop
  )
  #data2 <- rbind(data, dataLeft, dataRight, dataBottom, dataTop)

  data2 <- data2[data2$Latitude > restriction[1] & data2$Latitude < restriction[2] &
                   data2$Longitude > restriction[3] &
                   data2$Longitude < restriction[4], ]

  if (length(unique(data$Site)) == nrow(data)) {
    data2$Site = 1:nrow(data2)
  }

  return(data2)
}

#' Shift Data To Default Restriction
#'
#' Shift data such that it is in the range of -180 to 180 and -90 to 90
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @return (data.frame) data containing columns "Latitude" and "Longitude" shifted to the default restriction
shiftDataToDefaultRestriction <- function(data) {
  data$Longitude[data$Longitude > 180 & !is.na(data$Longitude)] <- data$Longitude[data$Longitude > 180 & !is.na(data$Longitude)] - 360
  data$Longitude[data$Longitude < -180 & !is.na(data$Longitude)] <- data$Longitude[data$Longitude < -180 & !is.na(data$Longitude)] + 360
  data$Latitude[data$Latitude > 90 & !is.na(data$Latitude)] <- data$Latitude[data$Latitude > 90 & !is.na(data$Latitude)] - 180
  data$Latitude[data$Latitude < -90 & !is.na(data$Latitude)] <- data$Latitude[data$Latitude < -90 & !is.na(data$Latitude)] + 180

  return(data)
}

#' Remove Data Outside Restriction
#'
#' Remove all data outside of the restriction
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @param Latitude (character) column name of Latitude column
#' @param Longitude (character) column name of Longitude column
#' @param restriction (numeric) restriction in the form c(minLatitude, maxLatitude, minLongitude, maxLongitude)
#' @return (data.frame) data containing columns "Latitude" and "Longitude" with data outside of restriction removed
removeDataOutsideRestriction <- function(data, Latitude, Longitude, restriction) {
  if (restriction[4] >= restriction[3]) {
    data <- data[data[, Latitude] <= restriction[2] &
                   data[, Latitude] >= restriction[1] &
                   data[, Longitude] <= restriction[4] &
                   data[, Longitude] >= restriction[3], ]
  } else {
    data <- data[data[, Latitude] <= restriction[2] &
                   data[, Latitude] >= restriction[1] &
                   !(data[, Longitude] <= restriction[3] &
                       data[, Longitude] >= restriction[4]), ]
  }

  return(data)
}

#' Center Data
#'
#' Transfer data to the center of the map, either Europe or Pacific by adding or substracting 360°
#' from the Longitude. If "Europe" longitudes are transposed to the range (-180, 180),  if
#' "Pacific" longitudes are transposed to the range (0, 360).
#'
#' @param data (data.frame) data containing column "Longitude"
#' @param center (character) center to shift data to, either "Europe" or "Pacific"
#' @return (data.frame) data shifted to the center
centerData <- function(data, center = c("Europe", "Pacific")) {
  center <- match.arg(center)

  if (center == "Pacific") {
    data$Longitude[data$Longitude < 0] <- data$Longitude[data$Longitude < 0] + 360
    data$Longitude[data$Longitude > 360] <- data$Longitude[data$Longitude > 360] - 360
  }

  if (center == "Europe") {
    data$Longitude[data$Longitude < -180] <- data$Longitude[data$Longitude < -180] + 360
    data$Longitude[data$Longitude > 180] <- data$Longitude[data$Longitude > 180] - 360
  }

  return(data)
}
