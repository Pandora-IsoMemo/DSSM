#' Finds duplicates in data using user specified similarity rules
#'
#' @param data dataframe in which duplicates are searched for
#' @param userSimilaritySelection dataframe containing similarity rules for each column to be considered
#' @param addColumn logical should a column with duplicate row indices be added
findDuplicates <- function(data, userSimilaritySelection, addColumn) {
  cols <- userSimilaritySelection$cols
print(userSimilaritySelection)
  preparedData <- data

  preparedData <- data.frame(lapply(preparedData, function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  }))

  numericCols <- numericColumns(preparedData)[numericColumns(preparedData) %in% cols]
  characterCols <- characterColumns(preparedData)[characterColumns(preparedData) %in% cols]

  preparedData[numericCols] <- sapply(numericCols, function(x) {
    if (userSimilaritySelection[cols == x, "numericSimilarity"] == "Rounded Match") {
      round(preparedData[, x], userSimilaritySelection[cols == x, "rounding"])
    } else {
      preparedData[, x]
    }
  })

  preparedData[characterCols] <- sapply(characterCols, function(x) {
    if (userSimilaritySelection[cols == x, "textSimilarity"] == "Case Insensitive") {
    adjustedData <- tolower(preparedData[, x])
    } else {
      adjustedData <- preparedData[, x]
    }
    if (userSimilaritySelection[cols == x, "ignoreSpaces"]) {
      adjustedData <- gsub(" ","",adjustedData)
    }
    if(userSimilaritySelection[cols == x, "specificString"] != ""){
      ignore_case <- ifelse(userSimilaritySelection[cols == x, "textSimilarity"] == "Case Insensitive", TRUE, FALSE)
      containIndex <- grepl(userSimilaritySelection[cols == x, "specificString"], adjustedData, ignore.case = ignore_case)
      adjustedData <- 1:length(adjustedData)
      adjustedData[containIndex] <- 0
    }
    adjustedData
  })

  checkData <- data.frame(preparedData[, cols], row.names = row.names(data))

  # check for ignore empty
  allDuplicatesDF <- data[duplicated(checkData) | duplicated(checkData, fromLast = TRUE), ]
  keepNotEmpty <- !apply(data.frame(allDuplicatesDF[, userSimilaritySelection$cols[userSimilaritySelection$ignoreEmpty]]), MARGIN = 1, function(x) any(is.na(x) | x == ""))
  allDuplicatesDF <- allDuplicatesDF[as.vector(keepNotEmpty), ]

  allDuplicateRows <- rownames(allDuplicatesDF)
  uniqueData <- data[!duplicated(checkData), ]

  if(addColumn){
  # add column with duplicate rows
  rowCheckData <- checkData
  rowCheckData$row <- rownames(rowCheckData)
  duplicateRows <-
    rowCheckData %>%
    group_by_at(cols) %>%
    summarise(row,
      duplicateRows = paste0(row, collapse = ","),
      .groups = "drop"
    ) %>%
    select(row, duplicateRows)

  data$duplicateRows <- NULL
  data$row <- row.names(data)
  data <- data %>%
    left_join(duplicateRows, by = "row")
  data[!data$row %in% as.numeric(allDuplicateRows), "duplicateRows"] <- ""
  row.names(data) <- data$row
  data$row <- NULL

  uniqueData$duplicateRows <- NULL
  uniqueData$row <- row.names(uniqueData)
  uniqueData <- uniqueData %>%
    left_join(duplicateRows, by = "row")
  uniqueData[!uniqueData$row %in% as.numeric(allDuplicateRows), "duplicateRows"] <- ""
  row.names(uniqueData) <- uniqueData$row
  uniqueData$row <- NULL

  allDuplicatesDF$duplicateRows <- NULL
  allDuplicatesDF$row <- row.names(allDuplicatesDF)
  allDuplicatesDF <- allDuplicatesDF %>%
    left_join(duplicateRows, by = "row")
  row.names(allDuplicatesDF) <- allDuplicatesDF$row
  allDuplicatesDF$row <- NULL

  # relocate duplicate column as first column
  col_order_data <- c("duplicateRows",names(data)[names(data) != "duplicateRows"])
  data <- data[, col_order_data]

  col_order_uniqueData <- c("duplicateRows",names(uniqueData)[names(uniqueData) != "duplicateRows"])
  uniqueData <- uniqueData[, col_order_uniqueData]

  col_order_allDuplicatesDF <- c("duplicateRows",names(allDuplicatesDF)[names(allDuplicatesDF) != "duplicateRows"])
  allDuplicatesDF <- allDuplicatesDF[, col_order_allDuplicatesDF]
  }

  # return result
  list(
    inputData = data,
    allDuplicatesDF = allDuplicatesDF,
    allDuplicatesRows = (1:nrow(data))[rownames(data) %in% allDuplicateRows],
    uniqueData = uniqueData
  )
}
