#' loadMaps
#' @export
loadMaps <- function(){
  ocean <- readRDS(system.file("maps/ocean.RDS",
                               package = "DSSM"))
  grids <- readRDS(system.file("maps/grids.RDS",
                               package = "DSSM"))
  borders <- readRDS(system.file("maps/borders.RDS",
                                 package = "DSSM"))
  ocean160 <- readRDS(system.file("maps/ocean160.RDS",
                               package = "DSSM"))
  ocean200 <- readRDS(system.file("maps/ocean200.RDS",
                                  package = "DSSM"))
  grids160 <- readRDS(system.file("maps/grids160.RDS",
                               package = "DSSM"))
  grids200 <- readRDS(system.file("maps/grids200.RDS",
                                  package = "DSSM"))
  borders160 <- readRDS(system.file("maps/borders160.RDS",
                                 package = "DSSM"))
  borders200 <- readRDS(system.file("maps/borders200.RDS",
                                 package = "DSSM"))
  coast <- readRDS(system.file("maps/coast.RDS",
                                    package = "DSSM"))
  coast160 <- readRDS(system.file("maps/coast160.RDS",
                                  package = "DSSM"))
  coast200 <- readRDS(system.file("maps/coast200.RDS",
                                    package = "DSSM"))
  land <- readRDS(system.file("maps/land.RDS",
                               package = "DSSM"))
  land160 <- readRDS(system.file("maps/land160.RDS",
                                  package = "DSSM"))
  land200 <- readRDS(system.file("maps/land200.RDS",
                                  package = "DSSM"))

  return(list(ocean = ocean, grids = grids, borders = borders, coast = coast,
              land = land,
              ocean160 = ocean160, grids160 = grids160, borders160 = borders160,
              ocean200 = ocean200, grids200 = grids200, borders200 = borders200,
              coast160 = coast160, coast200 = coast200,
              land160 = land160, land200 = land200))
}
