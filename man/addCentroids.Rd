% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-predictNearestCluster.R
\name{addCentroids}
\alias{addCentroids}
\title{Add centroid of clusters}
\usage{
addCentroids(
  dataC,
  cluster,
  centers,
  type = c("centroid_spatial_cluster", "temporal_group_reference_point")
)
}
\arguments{
\item{dataC}{Filtered data}

\item{cluster}{Cluster ids}

\item{centers}{Cluster centers}

\item{type}{Type of centroid}
}
\value{
Data with centroids
}
\description{
Add centroid of clusters
}
