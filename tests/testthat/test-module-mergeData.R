# testthat::test_that("Test module mergeImportData", {
#   testMergeList <-
#     readRDS(testthat::test_path("data-module-mergeImports.rds"))
#
#   shiny::testServer(mergeDataServer, args = list(mergeList = reactive(testMergeList)),
#              {
#                # Arrange
#                print("test merge import data from ckan")
#                # Act
#                session$setInputs(
#                  tableX = getTableChoices(testMergeList)[1],
#                  tableY = getTableChoices(testMergeList)[2],
#                  mergeOperation = "left_join",
#                  addAllCommonColumns = FALSE,
#                  commonColumns = NULL,
#                  showColnames = FALSE,
#                  xColumnsToJoin = c("col1", "col2"),
#                  yColumnsToJoin = c("col1", "col2"),
#                  mergeCommand = "joinedData <- inner_join(table1, table2, by = c(\"col2\" = \"col2\"), na_matches = \"never\")",
#                  applyMerge = TRUE
#                )
#                browser()
#
#                testthat::expect_true(typeof(output$colNames) == "character")
#                testthat::expect_equal(
#                  colnames(joinedData())[c(1:10, (ncol(joinedData()) - 10):ncol(joinedData()))],
#                  c("Human.Entry.ID", "Submitter.ID", "Context.ID", "Radiocarbon.ID",
#                    "Individual.ID", "Sample.ID", "Sex", "Age.Category", "Min..Age.(yrs)",
#                    "Max..Age.(yrs)", "IRMS.Lab.Institution.Stable.Oxygen.Phosphate.Measurement",
#                    "Nr..of.Samples.(δ18O)", "δ18O.Phosphate.(VPDB)", "δ18O.Phosphate.(VPDB).unc.",
#                    "δ18O.Phosphate.(VSMOW)", "δ18O.Phosphate.(VSMOW).unc.", "δ18O.Drinking.Water.(if.not.reported.differently)",
#                    "Lab.Institution.Stable.Strontium.Measurement", "Nr..of.Samples.(87Sr/86Sr)",
#                    "87Sr/86Sr", "87Sr/86Sr.unc.")
#
#                )
#              })
# })
