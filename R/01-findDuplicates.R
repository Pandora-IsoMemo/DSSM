#' Finds duplicates in data using user specified similarity rules
#'
#' @param data dataframe in which duplicates are searched for
#' @param userSimilaritySelection dataframe containing similarity rules for each column to be considered
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

  checkData <- data.frame(preparedData[, cols], row.names = row.names(preparedData))

  # check for ignore empty
  allDuplicatesDF <- data[duplicated(checkData) | duplicated(checkData, fromLast = TRUE), ]
  keepNotEmpty <- !apply(data.frame(allDuplicatesDF[, userSimilaritySelection$cols[userSimilaritySelection$ignoreEmpty]]), MARGIN = 1, function(x) any(is.na(x) | x == ""))
  allDuplicatesDF <- allDuplicatesDF[as.vector(keepNotEmpty), ]

  allDuplicateRows <- rownames(allDuplicatesDF)
  uniqueData <- data[!duplicated(checkData), ]

  # add column with duplicate rows
  rowCheckData <- checkData
  rowCheckData$row <- rownames(rowCheckData)
  duplicateRows <-
  rowCheckData %>%
    dplyr::group_by_at(cols) %>%
    dplyr::summarise(row,
                     duplicateRows = paste0(row, collapse = ","),
                     .groups = "drop") %>%
    dplyr::select(row,duplicateRows)

  data$duplicateRows <- NULL
  data$row <- row.names(data)
  data <- data %>%
    left_join(duplicateRows, by = "row")
  data[!data$row %in% as.numeric(allDuplicateRows),"duplicateRows"] <- ""
  row.names(data) <- data$row
  data$row <- NULL

  uniqueData$duplicateRows <- NULL
  uniqueData$row <- row.names(uniqueData)
  uniqueData <- uniqueData %>%
    left_join(duplicateRows, by = "row")
  uniqueData[!uniqueData$row %in% as.numeric(allDuplicateRows),"duplicateRows"] <- ""
  row.names(uniqueData) <- uniqueData$row
  uniqueData$row <- NULL

  allDuplicatesDF$duplicateRows <- NULL
  allDuplicatesDF$row <- row.names(allDuplicatesDF)
  allDuplicatesDF <- allDuplicatesDF %>%
    left_join(duplicateRows, by = "row")
  row.names(allDuplicatesDF) <- allDuplicatesDF$row
  allDuplicatesDF$row <- NULL

  list(
    inputData = data,
    allDuplicatesDF = allDuplicatesDF,
    allDuplicatesRows = (1:nrow(data))[rownames(data) %in% allDuplicateRows],
    uniqueData = uniqueData
  )
}
