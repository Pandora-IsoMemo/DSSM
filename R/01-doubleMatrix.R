listToDoubleMatrix <- function(ll, names) {
  lapply(names[!(names %in% names(ll))], function(nn) {
    ll[[nn]] <<- matrix(NA, 0, 2, dimnames = list(
      NULL,
      paste(nn, c("mean", "sd"), sep = "||")
    ))
  })

  m <- do.call(
    "cbind",
    ll[names]
  )

  if (is.null(m)) matrix(NA, 0, 0)
  else {
    m <- as.matrix(m)
    colnames(m) <- paste(rep(names, each = 2), c("mean", "sd"), sep = "||")
    m
  }
}

doubleMatrixToList <- function(m) {
  mode(m) <- "numeric"
  naRow <- apply(m, 1, function(rr) {all(is.na(rr))})
  m <- m[!naRow, , drop = FALSE]

  ii <- seq(1, ncol(m), by = 2)
  ll <- lapply(ii, function(i) {
    df <- as.data.frame(m[, i:(i+1), drop = FALSE])
    names(df) <- c("mean", "sd")
    df
  })

  names(ll) <- unlist(lapply(strsplit(colnames(m)[ii], "||", fixed = TRUE), `[[`, 1))

  ll
}
