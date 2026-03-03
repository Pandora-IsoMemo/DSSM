context("findDuplicates function with detectDuplicates Module")

test_that("findDuplicates", {
  testdata <- structure(list(
    id1 = c(
    "string1", "STRING1", "string2", "string2",
    "String2", "String3", "String3", "strIng3", "", "", "sstrIng3strIng3strIng3strIng3strIng3strIng3strIng3strIng3trIng3"
  ),
  id2 = c(1, 1, 1, 0, 1, 1, 1, 1.2, 1, 1, 1.2),
  id3 = c(1, 1, 1, 1, 1, 1, 1, 1.23, 1, 1, 1.23),
  id4 = c(1, 1, 1, 1, 1, 1, 1, 1.234, NA, NA, 1.234),
  id5 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  id6 = c(
    "string1", "STRING1", "string2", "string2",
    "String2", "String3", "String3", "strIng3", "", "", "strIng3"
  ),
  id7 = c(1, 1, 1, 0, 1, 1, 1, 1.2, 1, 1, 1.2),
  id8 = c(1, 1, 1, 1, 1, 1, 1, 1.23, 1, 1, 1.23),
  id9 = c(1, 1, 1, 1, 1, 1, 1, 1.234, NA, NA, 1.234),
  id10 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  id11 = c(
    "string1", "STRING1", "string2", "string2",
    "String2", "String3", "String3", "strIng3", "", "", "strIng3"
  ),
  id12 = c(1, 1, 1, 1, 1, 1, 1, 1.23, 1, 1, 1.23)),
  class = "data.frame",
  row.names = c(NA,-11L)
  )

  userSimilaritySelection <- data.frame(
    cols = c("id1"),
    textSimilarity = "Case Sensitive",
    specificString = "",
    numericSimilarity = NA,
    rounding = NA,
    ignoreEmpty = TRUE,
    ignoreSpaces = FALSE
  )

  findDuplicatesResult <- findDuplicates(
    data = testdata,
    userSimilaritySelection = userSimilaritySelection,
    addColumn = TRUE,
    keepFirst = TRUE
  )

  expect_equal(length(findDuplicatesResult), 4)
  expect_true(nrow(findDuplicatesResult$inputData) == 11)
  expect_true(nrow(findDuplicatesResult$allDuplicatesDF) == 4)
  expect_true(identical(findDuplicatesResult$allDuplicatesRows,as.integer(c(3,4,6,7))))
  expect_true(nrow(findDuplicatesResult$uniqueData) == 8)
  expect_true("duplicateRows" %in% names(findDuplicatesResult$inputData))
  expect_true("duplicateRows" %in% names(findDuplicatesResult$allDuplicatesDF))
  expect_true("duplicateRows" %in% names(findDuplicatesResult$uniqueData))

  # duplicateRows should be filled only for actual duplicates (3,4,6,7)
  expect_equal(findDuplicatesResult$inputData$duplicateRows[3], "3,4")
  expect_equal(findDuplicatesResult$inputData$duplicateRows[4], "3,4")
  expect_equal(findDuplicatesResult$inputData$duplicateRows[6], "6,7")
  expect_equal(findDuplicatesResult$inputData$duplicateRows[7], "6,7")

  # empty-string group should NOT be considered duplicates due to ignoreEmpty=TRUE
  expect_equal(findDuplicatesResult$inputData$duplicateRows[9], "")
  expect_equal(findDuplicatesResult$inputData$duplicateRows[10], "")

  userSimilaritySelection <- data.frame(
    cols = c("id4"),
    textSimilarity = NA,
    specificString = "",
    numericSimilarity = "Rounded Match",
    rounding = 1,
    ignoreEmpty = FALSE,
    ignoreSpaces = FALSE
  )

  findDuplicatesResult <- findDuplicates(
    data = testdata,
    userSimilaritySelection = userSimilaritySelection,
    addColumn = TRUE,
    keepFirst = TRUE
  )

  expect_equal(length(findDuplicatesResult), 4)
  expect_true(nrow(findDuplicatesResult$inputData) == 11)
  expect_true(nrow(findDuplicatesResult$allDuplicatesDF) == 11)
  expect_true(identical(findDuplicatesResult$allDuplicatesRows,as.integer(1:11)))
  expect_true(nrow(findDuplicatesResult$uniqueData) == 3)
  expect_true("duplicateRows" %in% names(findDuplicatesResult$inputData))
  expect_true("duplicateRows" %in% names(findDuplicatesResult$allDuplicatesDF))
  expect_true("duplicateRows" %in% names(findDuplicatesResult$uniqueData))

  expect_true(nchar(findDuplicatesResult$inputData$duplicateRows[1]) > 0)
  expect_true(grepl("1", findDuplicatesResult$inputData$duplicateRows[1]))

  # rows 8 and 11 both have id4=1.234 which rounds to 1.2 — they must be grouped
  # correctly even though the join is done on preprocessed (rounded) values
  expect_equal(findDuplicatesResult$inputData$duplicateRows[8], "8,11")
  expect_equal(findDuplicatesResult$inputData$duplicateRows[11], "8,11")
})
