getCKANFiles <- function () {
    res <- try({
        list <- getCKANFileList()
        filterCKANFileList(list)
    })

    if (inherits(res, "try-error")) {
        shinyjs::alert("Could not retrieve file metadata from pandoradata.earth")
        list()
    } else {
        res
    }
}

getCKANFileList <- function() {
    res <- httr::GET("https://pandoradata.earth/api/3/action/current_package_list_with_resources?limit=1000")
    httr::content(res)$result
}

filterCKANFileList <- function (fileList) {
    files <- lapply(fileList, filterSingleCKANRecord)
    keyBy(files, "title")
}

filterSingleCKANRecord <- function(record) {
    if (is.null(record$resources)) resources <- list()
    else resources <- lapply(record$resources, filterSingleCKANResource)

    list(
        title = record$title,
        resources = keyBy(resources, "name")
    )
}

filterSingleCKANResource <- function(resource) {
    list (
        name = resource$name,
        format = resource$format,
        url = resource$url
    )
}

keyBy <- function(l, key) {
    n <- unlist(lapply(l, `[[`, key))
    names(l) <- n
    l
}