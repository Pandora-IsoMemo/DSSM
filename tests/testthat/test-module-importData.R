test_that("Test module importData", {
  testServer(importDataServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(openPopup = TRUE)
               expect_equal(session$returned(), list())
             })

  testServer(importDataServer,
             {
               # Arrange
               print("test import data from ckan")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "ckan",
                 ckanRecord = "CIMA: Compendium Isotoporum Medii Aevi",
                 ckanResource = "CIMA Humans 29.05.2021",
                 type = "xlsx",
                 sheet = "1",
                 rownames = FALSE,
                 accept = TRUE
               )

               expect_equal(
                 names(session$returned()),
                 "cima-humans.xlsx"
               )
               expect_true(all(
                 c("Entry.ID", "Reference", "Link", "DOI") %in% names(session$returned()[[1]])
               ))
               expect_true(nrow(session$returned()[[1]]) > 100)

               expect_equal(
                 colnames(session$returned()[[1]])[1:10],
                 c(
                   "Entry.ID",
                   "Submitter.ID",
                   "Context.ID",
                   "Individual.ID",
                   "Sample.ID",
                   "Sex",
                   "Age.Category",
                   "Min..Age..yrs.",
                   "Max..Age..yrs.",
                   "Sampled.Element"
                 )
               )
             })
})
