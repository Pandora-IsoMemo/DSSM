convertNumeric <- function(data){
  suppressWarnings(data.num <- as.data.frame(lapply(1:ncol(data), function(x){
    y <- as.numeric(data[,x])
    if(sum(is.na(y)) == sum(is.na(data[,x]))){
      return(y)
    } else {
      return(data[,x])
    }
  } )))
  names(data.num) <- names(data)
  data.num
}

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
