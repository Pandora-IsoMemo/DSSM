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
