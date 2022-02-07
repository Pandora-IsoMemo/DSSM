#' Skin helper functions
#' 
#' @param skin one of isomemo / pandora
#' 
#' @export
#' @rdname skin
setSkin <- function(skin) {
    session <- getDefaultReactiveDomain()
    session$userData$skin <- skin
}

#' @export
#' @rdname skin
getSkin <- function() {
    session <- getDefaultReactiveDomain()

    if (!is.null(session$userData$skin)) session$userData$skin
    else "isomemo"
}

#' @rdname skin
#' @export
allowedSkins <- function () {
    c("pandora", "isomemo")
}