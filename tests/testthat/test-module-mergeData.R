testthat::test_that("Test module mergeImportData", {
  testMergeList <-
    readRDS(testthat::test_path("data-module-mergeImports.rds"))

  testCommonColumns <-
    c(
      # "Submitter.ID",
      # "Context.ID",
      # "Individual.ID",
      # "Sample.ID",
      "Sex",
      "Age.Category",
      "Min..Age.(yrs)",
      "Max..Age.(yrs)",
      "Sampled.Element",
      "Analysed.Component",
      "Modern.Country",
      "Site.Name",
      "Site.Description",
      "Central.Power.(Empire.or.Kingdom)",
      "Local.Power.(e.g..Vassal,.Petty.Kingdom,.Tribe,.etc.)",
      "Probable.Cultural.Context",
      "Culture.Mix.(Substratus,.Dependence,.External.Influence,.etc.)",
      "Latitude",
      "Longitude"#,
      # "Exact.Site.location?",
      # "unc..Radius.(km)",
      # "Min..Year.(95%)",
      # "Max..Year.(95%)",
      # "Dating.Method",
      # "General.Period(s)",
      # "Additional.Chronological.Tags",
      # "Social.Status.Rank",
      # "Elite?",
      # "Additional.Social.Information",
      # "Probable.Religious.Culture",
      # "Probable.Religious.Denomination",
      # "Reference",
      # "IRMS.Lab.Institution.Stable.Carbon.&.Nitrogen.Measurement",
      # "Nr..of.Samples.(Collagen.δ13C.&.δ15N)",
      # "IRMS.δ13C.Collagen",
      # "IRMS.δ13C.Collagen.unc",
      # "δ15N.Collagen",
      # "δ15N.Collagen.unc.",
      # "Collagen.Yield",
      # "%C",
      # "%N",
      # "Atomic.C:N.Ratio",
      # "IRMS.Lab.Institution.Stable.Carbon.&.Oxygen.Carbonate.Measurement",
      # "Nr..of.Samples.(Carbonate)",
      # "δ13C.Carbonate",
      # "δ13C.Carbonate.unc.",
      # "δ18O.Carbonate.(VPDB)",
      # "δ18O.Carbonate.(VPDB).unc."
    )

  shiny::testServer(mergeDataServer, args = list(mergeList = reactive(testMergeList)),
                    {
                      # Arrange
                      print("test merge import data from ckan")
                      # Act
                      session$setInputs(
                        tableX = getTableChoices(testMergeList)[1],
                        tableY = getTableChoices(testMergeList)[2],
                        mergeOperation = "left_join",
                        addAllCommonColumns = FALSE,
                        xColumnsToJoin = testCommonColumns,
                        yColumnsToJoin = testCommonColumns,
                        mergeCommand = "table1 %>%   left_join(table2,    by = c(\"Sex\"=\"Sex\", \"Age.Category\"=\"Age.Category\", \"Min..Age.(yrs)\"=\"Min..Age.(yrs)\", \"Max..Age.(yrs)\"=\"Max..Age.(yrs)\", \"Sampled.Element\"=\"Sampled.Element\", \"Analysed.Component\"=\"Analysed.Component\", \"Modern.Country\"=\"Modern.Country\", \"Site.Name\"=\"Site.Name\", \"Site.Description\"=\"Site.Description\", \"Central.Power.(Empire.or.Kingdom)\"=\"Central.Power.(Empire.or.Kingdom)\", \"Local.Power.(e.g..Vassal,.Petty.Kingdom,.Tribe,.etc.)\"=\"Local.Power.(e.g..Vassal,.Petty.Kingdom,.Tribe,.etc.)\", \"Probable.Cultural.Context\"=\"Probable.Cultural.Context\", \"Culture.Mix.(Substratus,.Dependence,.External.Influence,.etc.)\"=\"Culture.Mix.(Substratus,.Dependence,.External.Influence,.etc.)\", \"Latitude\"=\"Latitude\", \"Longitude\"=\"Longitude\"))",
                        applyMerge = FALSE
                      )

                      testthat::expect_equal(
                        mergeCommand(),
                        "table1 %>%   left_join(table2,    by = c(\"Sex\"=\"Sex\", \"Age.Category\"=\"Age.Category\", \"Min..Age.(yrs)\"=\"Min..Age.(yrs)\", \"Max..Age.(yrs)\"=\"Max..Age.(yrs)\", \"Sampled.Element\"=\"Sampled.Element\", \"Analysed.Component\"=\"Analysed.Component\", \"Modern.Country\"=\"Modern.Country\", \"Site.Name\"=\"Site.Name\", \"Site.Description\"=\"Site.Description\", \"Central.Power.(Empire.or.Kingdom)\"=\"Central.Power.(Empire.or.Kingdom)\", \"Local.Power.(e.g..Vassal,.Petty.Kingdom,.Tribe,.etc.)\"=\"Local.Power.(e.g..Vassal,.Petty.Kingdom,.Tribe,.etc.)\", \"Probable.Cultural.Context\"=\"Probable.Cultural.Context\", \"Culture.Mix.(Substratus,.Dependence,.External.Influence,.etc.)\"=\"Culture.Mix.(Substratus,.Dependence,.External.Influence,.etc.)\", \"Latitude\"=\"Latitude\", \"Longitude\"=\"Longitude\"))"
                      )

                      # testthat::expect_true(typeof(output$colNames) == "character")
                      # testthat::expect_equal(
                      #   colnames(joinedData())[c(1:10, (ncol(joinedData()) - 10):ncol(joinedData()))],
                      #   c(
                      #     "Human.Entry.ID",
                      #     "Submitter.ID",
                      #     "Context.ID",
                      #     "Radiocarbon.ID",
                      #     "Individual.ID",
                      #     "Sample.ID",
                      #     "Sex",
                      #     "Age.Category",
                      #     "Min..Age.(yrs)",
                      #     "Max..Age.(yrs)",
                      #     "IRMS.Lab.Institution.Stable.Oxygen.Phosphate.Measurement",
                      #     "Nr..of.Samples.(δ18O)",
                      #     "δ18O.Phosphate.(VPDB)",
                      #     "δ18O.Phosphate.(VPDB).unc.",
                      #     "δ18O.Phosphate.(VSMOW)",
                      #     "δ18O.Phosphate.(VSMOW).unc.",
                      #     "δ18O.Drinking.Water.(if.not.reported.differently)",
                      #     "Lab.Institution.Stable.Strontium.Measurement",
                      #     "Nr..of.Samples.(87Sr/86Sr)",
                      #     "87Sr/86Sr",
                      #     "87Sr/86Sr.unc."
                      #   )
                      #
                      # )
                    })
})
