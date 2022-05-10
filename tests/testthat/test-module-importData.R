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
                 rownames = FALSE,
                 accept = TRUE
               )

               expect_equal(
                 names(session$returned()),
                 "https://pandoradata.earth/dataset/cbbc35e0-af60-4224-beea-181be10f7f71/resource/f7581eb1-b2b8-4926-ba77-8bc92ddb4fdb/download/cima-humans.xlsx"
               )
               expect_true(all(
                 c("Entry.ID", "Reference", "Link", "DOI") %in% names(session$returned()[[1]])
               ))
               expect_true(nrow(session$returned()[[1]]) > 0)
             })
})
