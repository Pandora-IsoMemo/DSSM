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

#' loadMaps from sf
#' @export
loadMaps_sf <- function(layers = c("ocean", "grids", "borders", "coast", "land")){
  all_layers <- c(layers, paste0(layers, "-180"), paste0(layers, "+180"))
  maps <- lapply(all_layers, function(layer) {
    readRDS(system.file(sprintf("maps/%s_sf.RDS", layer), package = "DSSM")) %>%
      sf::as_Spatial()
  })
  names(maps) <- all_layers
  return(maps)
}

# Update maps of package
#
# This function downloads maps from the package 'rnaturalearth' and saves them in the folder for
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
    map <- switch(layer,
                  "ocean" = rnaturalearth::ne_download(scale = "medium", type = "ocean", category = "physical", returnclass = "sf"),
                  "grids" = rnaturalearth::ne_download(scale = "medium", type = "graticules_10", category = "physical", returnclass = "sf"),
                  "borders" = rnaturalearth::ne_download(scale = "medium", type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf"),
                  "coast" = rnaturalearth::ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf"),
                  "land" = rnaturalearth::ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf"))

    message("Creating layers moved by -/+180° : ", layer, " ...")
    map_left <- map %>% moveMap(x = -180, y = 0)
    map_right <- map %>% moveMap(x = 180, y = 0)

    message("Saving layers(original and original -/+ c(180, 0): ", layer, " ...")
    saveRDS(map, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s_sf.RDS", layer)))
    saveRDS(map_left, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s-180_sf.RDS", layer)))
    saveRDS(map_right, file = file.path(system.file("maps", package = "DSSM"), sprintf("%s+180_sf.RDS", layer)))
    message("... updated layer: ", layer)
  }
}

moveMap <- function(map, x = 0, y = 0) {
  library(sf) # load library to enable overwriting of st_geometry and st_crs

  st_geometry(map) <- st_geometry(map) + c(x, y)
  st_crs(map) <- st_crs(map)
  return(map)
}
