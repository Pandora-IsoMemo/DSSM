logging <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

logDebug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

logWarn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}

#' Convert bytes to human-readable units
#'
#' @param bytes (numeric) bytes
#' @export
format_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- if (bytes > 0) floor(log(bytes, 1024)) else 0
  sprintf("%.2f %s", bytes / 1024^power, units[power + 1])
}

#' Get total available system memory on linux
#'
#' @export
get_total_memory <- function() {
  if (Sys.info()["sysname"] != "Linux") return(NULL)

  # Works on Linux
  meminfo <- readLines("/proc/meminfo")
  memtotal_kb <- as.numeric(sub(".*:\\s+([0-9]+)\\s+kB", "\\1", grep("MemTotal", meminfo, value = TRUE)))
  return(memtotal_kb * 1024)  # return in bytes
}
