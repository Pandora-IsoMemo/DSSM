#' loadMaps
#' @export
loadMaps <- function(){
  layers <- c("ocean", "grids", "borders", "coast", "land")
  all_layers <- c(layers, paste0(layers, "-180"), paste0(layers, "+180"))

  maps <- lapply(all_layers, function(layer) {
    readRDS(system.file(sprintf("maps/%s_sf.RDS", layer), package = "DSSM")) %>%
      sf::as_Spatial()
  })
  names(maps) <- all_layers
  return(maps)
}
