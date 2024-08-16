predictNearestCluster <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

#' Make cluster ids continuous
#'
#' When Mclust is used with e.g. 10 clusters, it can still happen that some clusters are empty
#' In this case we would see a jump in cluster ids e.g. 1,2,5,...
#' To prevent this, we change the cluster ids in the last step.
#'
#' @param column_with_ids A vector with cluster ids
makeClusterIdsContinuous <- function(column_with_ids) {
  if (length(column_with_ids) == 0) {
    return(column_with_ids)
  }

  match(column_with_ids, sort(unique(column_with_ids)))
}
