#' loadMaps
#' @export
loadMaps <- function(){
  ocean <- readRDS(system.file("maps/ocean.RDS",
                               package = "DSSM"))
  ocean160 <- readRDS(system.file("maps/ocean160.RDS",
                               package = "DSSM"))
  ocean200 <- readRDS(system.file("maps/ocean200.RDS",
                                  package = "DSSM"))
  ocean180 <- readRDS(system.file("maps/ocean180.RDS",
                                  package = "DSSM"))
  grids <- readRDS(system.file("maps/grids.RDS",
                               package = "DSSM"))
  grids160 <- readRDS(system.file("maps/grids160.RDS",
                               package = "DSSM"))
  grids200 <- readRDS(system.file("maps/grids200.RDS",
                                  package = "DSSM"))
  grids180 <- readRDS(system.file("maps/grids180.RDS",
                                  package = "DSSM"))
  borders <- readRDS(system.file("maps/borders.RDS",
                                 package = "DSSM"))
  borders160 <- readRDS(system.file("maps/borders160.RDS",
                                 package = "DSSM"))
  borders200 <- readRDS(system.file("maps/borders200.RDS",
                                 package = "DSSM"))
  borders180 <- readRDS(system.file("maps/borders180.RDS",
                                    package = "DSSM"))
  coast <- readRDS(system.file("maps/coast.RDS",
                                    package = "DSSM"))
  coast160 <- readRDS(system.file("maps/coast160.RDS",
                                  package = "DSSM"))
  coast200 <- readRDS(system.file("maps/coast200.RDS",
                                    package = "DSSM"))
  coast180 <- readRDS(system.file("maps/coast180.RDS",
                                  package = "DSSM"))
  land <- readRDS(system.file("maps/land.RDS",
                               package = "DSSM"))
  land160 <- readRDS(system.file("maps/land160.RDS",
                                  package = "DSSM"))
  land200 <- readRDS(system.file("maps/land200.RDS",
                                  package = "DSSM"))
  land180 <- readRDS(system.file("maps/land180.RDS",
                                 package = "DSSM"))

  return(list(ocean = ocean, grids = grids, borders = borders, coast = coast, land = land,
              ocean160 = ocean160, grids160 = grids160, borders160 = borders160, coast160 = coast160, land160 = land160,
              ocean200 = ocean200, grids200 = grids200, borders200 = borders200, coast200 = coast200, land200 = land200,
              ocean180 = ocean180, grids180 = grids180, borders180 = borders180, coast180 = coast180, land180 = land180))
}

createMap180 <- function(layer = c("ocean", "grids", "borders", "coast", "land")) {
  layer <- match.arg(layer)

  message("Reading layer: ", layer)
  # we use the file "*160.RDS" as basis for the transformation
  # there had been error messages for "*200.RDS" files and for "*.RDS" files
  map <- readRDS(system.file(sprintf("maps/%s160.RDS", layer), package = "DSSM"))

  message("Transforming layer: ", layer)
  # try create a map centered on 180th Meridian
  tryCatch({
    # Perform the operations
    res <- map %>%
      st_as_sf() %>%
      sf::st_wrap_dateline(options = c("WRAPDATELINE=YES"), quiet = TRUE) %>%
      sf::as_Spatial()
  }, error = function(e) {
    # Handle errors
    message("Error processing layer: ", layer)
    message("Error message: ", e$message)
  })

  # Save the result to an RDS file if no errors occur
  if (exists("res") && !inherits(res, "try-error")) {
    saveRDS(res, file = paste0(layer, "180.RDS"))
    message("Successfully processed and saved: ", paste0(getwd(), "/", layer, "180.RDS"))
  }
}

createAllLayers180 <- function() {
  layers <- c("ocean", "grids", "borders", "coast", "land")

  for (layer in layers) {
    createMap180(layer)
  }
}
