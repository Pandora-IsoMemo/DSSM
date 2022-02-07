context("data explorer table")

testthat::test_that("creates table", {

  d <- data.frame(Source = letters[1:3],
                  ID = 1:3,
                  d13C = 1:3,
                  Latitude = NA,
                  Longitude = NA)

  testthat::expect_is(datTable(d), "datatables")
  testthat::expect_is(datTable(d, columns = "Source"), "datatables")
  testthat::expect_true(!"number" %in% names(datTable(d, columns = "Source")$x$data))

})

mapping <- getMappingTable()

testthat::test_that("function categoryChoices", {
  skip_if(is.null(mapping))
  testthat::expect_is(categoryChoices(mapping), "character")
  testthat::expect_true(all(categoryChoices(mapping) %in% mapping$category))
})

testthat::test_that("function columnChoices", {
  skip_if(is.null(mapping))
  testthat::expect_is(columnChoices("Sample description", mapping), "character")
  #testthat::expect_equal(columnChoices("Sample description", mapping), "Description")
  testthat::expect_true(all(columnChoices(unique(mapping$category), mapping) %in% mapping$shiny))
})

testthat::test_that("function columnDefault", {
  testthat::expect_is(columnDefault(), "character")

  skip_if(is.null(mapping))
  testthat::expect_true(all(setdiff(columnDefault(), "source") %in% mapping$shiny))
})

testthat::test_that("function getDataColumns", {
  skip_if(is.null(mapping))

  mapping <- getMappingTable()

  input <- list("selectCategorySampledescription" = TRUE,
                "selectCategoryIsotopicproxies" = FALSE,
                "selectCategoryLocation" = FALSE,
                "selectCategoryDatinginfo" = FALSE,
                "selectColumnsSampledescription" = "description")

  testthat::expect_setequal(getDataColumns(mapping, input), c(columnDefault(), "description"))

  input <- list("selectCategorySampledescription" = FALSE,
                "selectCategoryIsotopicproxies" = FALSE,
                "selectCategoryLocation" = FALSE,
                "selectCategoryDatinginfo" = FALSE,
                "selectColumnsSampledescription" = "description")

  testthat::expect_setequal(getDataColumns(mapping, input), columnDefault())

  input <- list("selectCategorySampledescription" = TRUE,
                "selectCategoryIsotopicproxies" = TRUE,
                "selectCategoryLocation" = FALSE,
                "selectCategoryDatinginfo" = FALSE,
                "selectColumnsSampledescription" = "description")

  testthat::expect_setequal(getDataColumns(mapping, input), c(columnDefault(), "description"))

  input <- list("selectCategorySampledescription" = TRUE,
                "selectCategoryIsotopicproxies" = TRUE,
                "selectCategoryLocation" = FALSE,
                "selectCategoryDatinginfo" = FALSE,
                "selectColumnsSampledescription" = "description",
                "selectColumnsIsotopicproxies" = c("d13C", "d15N"))

  testthat::expect_setequal(getDataColumns(mapping, input),
                         c(columnDefault(), "description", "d13C", "d15N"))


})
