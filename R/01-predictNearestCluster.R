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

#' Add centroid of clusters
#'
#' @param dataC Filtered data
#' @param cluster Cluster ids
#' @param centers Cluster centers
#' @param type Type of centroid
#'
#' @return Data with centroids
addCentroids <- function(dataC, cluster, centers, type = c("centroid_spatial_cluster", "temporal_group_reference_point")) {
  type <- match.arg(type)

  clust_centroid = data.frame(cluster = 1:nrow(centers), centers)
  names(clust_centroid) <- c("cluster", sprintf("long_%s", type), sprintf("lat_%s", type))

  dataC$cluster <- cluster
  dataC <- merge(dataC, clust_centroid, by = "cluster", sort = FALSE)

  if (!is.null(dataC$id)) {
    dataC <- dataC[order(dataC$id),]
  }

  dataC
}

#' Join centroid data to data
#'
#' @param data Data
#' @param dataC Filtered data with centroid
#'
#' @return Data with centroid
joinCentroidData <- function(data, dataC) {
  data <- data %>% left_join(dataC[,c("id", "cluster", "long_centroid_spatial_cluster", "lat_centroid_spatial_cluster")], by = "id")
  data$id <- NULL
  colnames(data)[colnames(data) == "cluster"] <- "spatial_cluster"

  data
}
