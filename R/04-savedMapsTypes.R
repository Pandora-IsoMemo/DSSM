getTabId <- function(mapType){
  switch(
    mapType,
    localAvg = "AverageR",
    temporalAvg = "TimeR",
    spread = "SpreadR",
    difference = "OperatoR",
    similarity = "LocateR",
    kernel2d = "KernelR",
    kernel3d = "KernelTimeR"
  )
}
