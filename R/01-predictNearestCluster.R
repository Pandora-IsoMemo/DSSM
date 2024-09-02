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

#' Get centroid of clusters
#'
#' @param dataC Filtered data
#' @param clust_centroid Centroid of clusters
#' @param removeClusterCol Remove cluster column from data
#'
#' @return Filtered data with centroid
getCentroid <- function(dataC, cluster, centers, removeClusterCol = FALSE) {
  clust_centroid = data.frame(cluster = 1:nrow(centers), centers)
  names(clust_centroid) <- c("cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")

  dataC$cluster <- cluster
  dataC <- merge(dataC, clust_centroid, by = "cluster", sort = FALSE)
  dataC <- dataC[order(dataC$id),]

  if (removeClusterCol) {
    dataC$cluster <- NULL
  }

  dataC
}

#' Join centroid to data
#'
#' @param data Data
#' @param dataC Filtered data with centroid
joinCentroid <- function(data, dataC) {
  data <- data %>% left_join(dataC[,c("id","cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")], by = "id")
  data$id <- NULL
  colnames(data)[colnames(data) == "cluster"] <- "spatial_cluster"

  data
}
