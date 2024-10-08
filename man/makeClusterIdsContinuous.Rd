% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-predictNearestCluster.R
\name{makeClusterIdsContinuous}
\alias{makeClusterIdsContinuous}
\title{Make cluster ids continuous}
\usage{
makeClusterIdsContinuous(column_with_ids)
}
\arguments{
\item{column_with_ids}{A vector with cluster ids}
}
\description{
When Mclust is used with e.g. 10 clusters, it can still happen that some clusters are empty
In this case we would see a jump in cluster ids e.g. 1,2,5,...
To prevent this, we change the cluster ids in the last step.
}
