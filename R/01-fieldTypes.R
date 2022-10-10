numericColumns <- function(df) {
    i <- unlist(lapply(df, isNumeric))
    names(df)[i]
}
partialNumericColumns <- function(df) {
    i <- unlist(lapply(df, function(x) !all(is.na(suppressWarnings(as.numeric(x))))))
    names(df)[i]
}

characterColumns <- function(df) {
    i <- unlist(lapply(df, is.character))
    names(df)[i]
}

isNumeric <- function(x) {
    is.numeric(x) || is.integer(x)
}
