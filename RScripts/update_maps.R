library(rnaturalearth)
library(sf) # load whole library to enable syntax for overwriting of st_geometry and st_crs

# Update maps of package
#
# This function downloads maps from the package 'rnaturalearth' and saves them into the folder for
# maps of the package 'DSSM'. Only needed if the maps are outdated or if new maps are needed.
# This function is only for maintenance of the package and not needed for running the shiny app.
#
# @param layers character vector of layers to update
updateMapsOfPackage <- function(layers = c("ocean", "grids", "borders", "coast", "land")) {
  # check if package rnaturalearth is installed
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Package 'rnaturalearth' is not installed. Please install it first.")
  }

  library(rnaturalearth)

  for (layer in layers) {
    message("Downloading layer: ", layer, " ...")
    map_loaded <- switch(
      layer,
      "ocean" = rnaturalearth::ne_download(scale = "medium", type = "ocean", category = "physical", returnclass = "sf"),
      "grids" = rnaturalearth::ne_download(scale = "medium", type = "graticules_10", category = "physical", returnclass = "sf"),
      "borders" = rnaturalearth::ne_download(scale = "medium", type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf"),
      "coast" = rnaturalearth::ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf"),
      "land" = rnaturalearth::ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")
    )

    message("Creating layers: ", layer, " ...")
    map <- map_loaded %>% moveMap(x = 0, y = 0)
    map_left <- map_loaded %>% moveMap(x = -180, y = 0)
    map_right <- map_loaded %>% moveMap(x = 180, y = 0)

    message("Saving layers: ", layer, " ...")
    saveRDS(map, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s_sf.RDS", layer)))
    saveRDS(map_left, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s-180_sf.RDS", layer)))
    saveRDS(map_right, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s+180_sf.RDS", layer)))
    message("... updated layer: ", layer)
  }
}

moveMap <- function(map, x = 0, y = 0) {
  sf::st_geometry(map) <- sf::st_geometry(map) + c(x, y)
  sf::st_crs(map) <- sf::st_crs(map)
  return(map)
}

# trigger update of maps
updateMapsOfPackage()
