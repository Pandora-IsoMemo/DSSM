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
  power <- max(0, min(power, length(units) - 1))
  sprintf("%.2f %s", bytes / 1024^power, units[power + 1])
}

mem_used_bytes <- function() {
  # Use a lighter-weight GC and select the "used (Mb)" column by name where possible
  g <- gc(full = FALSE)

  colname_used_mb <- "used (Mb)"
  if (colname_used_mb %in% colnames(g)) {
    used_mb <- sum(g[, colname_used_mb, drop = TRUE], na.rm = TRUE)
  } else {
    # Fallback for R versions where column 2 corresponds to used memory in MB
    used_mb <- sum(g[, 2], na.rm = TRUE)
  }
  used_mb * 1024^2
}

log_memory_usage <- function() {
  total_mem <- get_total_memory()

  if (is.null(total_mem)) return(invisible(NULL))

  current_mem <- mem_used_bytes()
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

  invisible(NULL)
}

log_object_size <- function(object, object_name = deparse(substitute(object)), log_memory = TRUE) {
  # Only compute object size if debug logging is enabled
  if (identical(futile.logger::flog.threshold(), "DEBUG")) {
    logDebug(sprintf(
      "Size of %s: %s",
      object_name,
      format(utils::object.size(object), units = "auto")
    ))
    # if logging object size then also log memory usage
    if (log_memory) log_memory_usage()
  }
}
