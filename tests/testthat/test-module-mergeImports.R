test_that("Test module mergeImportData", {
  testMergeList <- readRDS(testthat::test_path("data-module-mergeImports.rds"))

  testServer(mergeImportsServer, args = list(mergeList = reactive(testMergeList)),
               {
                 # Arrange
                 print("test merge import data from ckan")
                 # Act
                 session$setInputs(
                   mergeNames = names(testMergeList)
                 )

                 testthat::expect_true(typeof(output$colNames) == "character")
               })
})
