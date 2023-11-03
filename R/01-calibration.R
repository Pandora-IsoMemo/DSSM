calibrateRadiocarbon <- function(
  isoData, calMethod = "intcal13", level = 0.95,
  dateFields = list("dateMean" = "dateMean", "dateUncertainty" = "dateUncertainty", "datingType" = "datingType")
  ){
  isoData$calibratedDate <- NA
  isoData$calibratedDateLower <- NA
  isoData$calibratedDateUpper <- NA
  if(!is.null(dateFields$dateMean)){
    if(is.null(isoData[[dateFields$dateMean]]) |
       is.null(isoData[[dateFields$dateUncertainty]])){
      return(isoData)
    }} else {
      if(is.null(isoData[[dateFields$dateLower]]) |
         is.null(isoData[[dateFields$dateUpper]])){
        return(isoData)
      }
    }
  if(is.null(isoData[[dateFields$datingType]])){
    isoData$datingType <- "radiocarbon"
  }
  if(!is.null(dateFields$dateLower)){
    radiodata <- isoData[, c(dateFields$dateLower, dateFields$dateUpper, "datingType")]
    radiodata$dateMean <- (isoData[, dateFields$dateLower] + isoData[, dateFields$dateUpper]) / 2
    radiodata$dateUncertainty <- abs(isoData[, dateFields$dateLower] - (isoData[, dateFields$dateUpper])) / 4
    radiodata <- radiodata[, c("dateMean", "dateUncertainty", "datingType")]
  } else {
    radiodata <- isoData[, c(dateFields$dateMean, dateFields$dateUncertainty, "datingType")]
  }
  names(radiodata) <- c("dateMean", "dateUncertainty", "datingType")
  radiodata[, 1:2] <- sapply(radiodata[, 1:2], as.numeric) %>%
    suppressWarnings()

  selectData <- which(!is.na(radiodata$dateMean) &
                        !is.na(radiodata$dateUncertainty) &
                        radiodata$datingType == "radiocarbon")

  if (length(selectData) == 0) return(isoData)

  radiodata <- radiodata[selectData, , drop = FALSE]

  ages <- try(calibrate(x = radiodata$dateMean,
                    errors = radiodata$dateUncertainty,
                    calCurves = calMethod,
                    timeRange = c(100000, 0)))

  if (inherits(ages, "try-error")) return(isoData)

  age_samples <- sampleDates(ages)[[1]]

  radiodata$calibratedDate <- sapply(age_samples, mean)
  radiodata$calibratedDateUpper <-
    sapply(age_samples,
           function(x) {quantile(x, 1 - (1 - level) / 2 )})

  radiodata$calibratedDateLower <-
    sapply(age_samples,
           function(x) {quantile(x, (1 - level) / 2 )})

  isoData$calibratedDate[selectData] <- round(radiodata$calibratedDate)
  isoData$calibratedDateLower[selectData] <- round(radiodata$calibratedDateLower)
  isoData$calibratedDateUpper[selectData] <- round(radiodata$calibratedDateUpper)

  return(isoData)
}

