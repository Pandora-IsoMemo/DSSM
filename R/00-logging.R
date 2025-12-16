logging <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

logDebug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

logWarn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}

init_logging <- function() {
  if (as.logical(Sys.getenv("SHOW_DEBUG", unset = "FALSE"))) {
    futile.logger::flog.threshold(futile.logger::DEBUG)
    logging("Logger initialized: threshold = DEBUG")
  } else {
    futile.logger::flog.threshold(futile.logger::INFO)
    logging("Logger initialized: threshold = INFO")
  }
}

get_total_memory <- function() {
  if (Sys.info()["sysname"] != "Linux") return(NULL)

  # on Linux
  meminfo <- readLines("/proc/meminfo")
  memtotal_kb <- as.numeric(sub(".*:\\s+([0-9]+)\\s+kB", "\\1", grep("MemTotal", meminfo, value = TRUE)))
  return(memtotal_kb * 1024)  # return in bytes
}

format_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- if (bytes > 0) floor(log(bytes, 1024)) else 0
  sprintf("%.2f %s", bytes / 1024^power, units[power + 1])
}

log_memory_usage <- function() {
  total_mem <- get_total_memory()

  if (is.null(total_mem)) return()

  current_mem <- pryr::mem_used()
  current_mem_msg <- sprintf("usage: %s / %s (%.1f%%)",
                             format_bytes(current_mem),
                             format_bytes(total_mem),
                             100 * current_mem / total_mem)
  mem_warning <- "-- Calculation may fail due to insufficient RAM"

  if (current_mem <= 0.8 * total_mem)
    logging("Memory %s.", current_mem_msg)

  if (0.8 * total_mem < current_mem && current_mem <= 0.9 * total_mem)
    logging("High memory %s %s.", current_mem_msg, mem_warning)

  if (0.9 * total_mem < current_mem)
    logWarn("Critical memory %s %s.", current_mem_msg, mem_warning)
}

log_object_size <- function(object, object_name = deparse(substitute(object))) {
  logDebug(sprintf("Size of %s: %s", object_name, pryr::object_size(object) |> format(units = "auto")))
}
