findDuplicates <- function(data, userSimilaritySelection) {
  cols <- userSimilaritySelection$cols
  numericCols <- numericColumns(data)[numericColumns(data) %in% cols]
  characterCols <- characterColumns(data)[characterColumns(data) %in% cols]

  preparedData <- data

  preparedData[numericCols] <- sapply(numericCols, function(x) {
    if (userSimilaritySelection[cols == x, "numericSimilarity"] == "Rounded Match") {
      round(preparedData[, x], userSimilaritySelection[cols == x, "rounding"])
    } else {
      preparedData[, x]
    }
  })

  preparedData[characterCols] <- sapply(characterCols, function(x) {
    if (userSimilaritySelection[cols == x, "textSimilarity"] == "Case Insensitive") {
      tolower(preparedData[, x])
    } else {
      preparedData[, x]
    }
  })

  checkData <- preparedData[, cols]

  # check for ignore empty
  allDuplicatesDF <- data[duplicated(checkData) | duplicated(checkData, fromLast = TRUE), ]
  keepNotEmpty <- !apply(data.frame(allDuplicatesDF[, userSimilaritySelection$cols[userSimilaritySelection$ignoreEmpty]]), MARGIN = 1, function(x) any(is.na(x) | x == ""))
  allDuplicatesDF <- allDuplicatesDF[as.vector(keepNotEmpty), ]

  allDuplicateRows <- rownames(allDuplicatesDF)
  uniqueData <- data[!duplicated(checkData), ]

  list(
    allDuplicatesDF = allDuplicatesDF,
    allDuplicatesRows = allDuplicateRows,
    uniqueData = uniqueData
  )
}
